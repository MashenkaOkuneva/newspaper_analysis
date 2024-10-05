function [backcast, nowcast, forecast1, forecast2, date_backcast, date_nowcast, date_forecast1, date_forecast2] = run_forecast(filename, dirname, Nr, Np)
    
    %-------------------------------------------------------------------------%
    %- This function uses EM algorithm to estimate a mixed frequency factor
    %- model with daily and quarterly data. 
    %- These are stored in the csv-file filename.csv
    %- produced with the R script vintages.R. The file also contains the 
    %- W's and Xi's needed for the estimation. 
    %- Currently 10 topics that have the highest correlation with GDP
    %- growth are used in the estimation. 
    %- The function returns backcast, nowcast, and forecasts
    %- (1Q and 2Q) for the particular vintage.
    %- Additionally, the function returns the corresponding dates for each of these forecasts.
    %-------------------------------------------------------------------------%
    
    %-------------------------------------------------------------------------%
    % load data
    %-------------------------------------------------------------------------%
    tmp = importdata([dirname, filename]);
    
    % offset as there are less numeric columns!
    offset_numcols = size(tmp.textdata, 2) - size(tmp.data, 2);
    
    % back out data for estimation and forecasting
    aux.ind_sample = logical(tmp.data(:, find(strcmp('ind_sample', tmp.textdata(1,:))) - offset_numcols));
    
    % daily data
    ind_y_d = find(contains(tmp.textdata(1,:), 'y_d_')) - offset_numcols;
    %ind_y_d = setdiff(ind_y_d, ind_y_d([6, 9, 23])); % manually remove T05, T07, T21
    %ind_y_d = [1 11 37 45] + 4; % topics T0, T10, T21, T36, T44 => highest correlated with GDP
    y_d = tmp.data(aux.ind_sample, ind_y_d)';
    y_d_fore = tmp.data(~aux.ind_sample, ind_y_d)';
    
    % quarterly data
    ind_y_q = find(contains(tmp.textdata(1,:), 'y_q_')) - offset_numcols;
    y_q = tmp.data(aux.ind_sample, ind_y_q)';
    y_q_fore = tmp.data(~aux.ind_sample, ind_y_q)';
    aux.ind_q_flow = true; 
    
    % weights and Xi
    aux.Xi_qd = tmp.data(:, find(strcmp('Xi_qd', tmp.textdata(1,:))) - offset_numcols);
    aux.W_qd_p = tmp.data(:, find(strcmp('W_qd_p', tmp.textdata(1,:))) - offset_numcols);
    aux.W_qd_c = tmp.data(:, find(strcmp('W_qd_c', tmp.textdata(1,:))) - offset_numcols);
    
    % inds for back-, now- and forecasts
    ind_backcast = logical(tmp.data(:, find(strcmp('ind_backcast', tmp.textdata(1,:))) - offset_numcols));
    ind_nowcast = logical(tmp.data(:, find(strcmp('ind_nowcast', tmp.textdata(1,:))) - offset_numcols));
    ind_forecast = logical(tmp.data(:, find(strcmp('ind_forecast1Q', tmp.textdata(1,:))) - offset_numcols));
    
    % back out dates for plot
    ind_plot = find(tmp.data(:, 2) == 1 & tmp.data(:, 3) == 1 & tmp.data(:, 4) == 1);
    dates_plot = tmp.data(ind_plot, 1); 
    
    %-------------------------------------------------------------------------%
    % prepare data for estimation
    %-------------------------------------------------------------------------%
    
    % standardize
    y_d_stand = (y_d - nanmean(y_d, 2)) ./ nanstd(y_d, [], 2);
    y_d_fore_stand = (y_d_fore - nanmean(y_d, 2)) ./ nanstd(y_d, [], 2);
    mean_gdp = nanmean(y_q);
    std_gdp = nanstd(y_q);
    y_q_stand = (y_q - mean_gdp) / std_gdp; 
    y_q_fore_stand = (y_q_fore - mean_gdp) / std_gdp; 
    
    % starting values
    params = f_start_vals(y_d_stand, [], [], y_q_stand, aux, Nr, Np);
    
    %-------------------------------------------------------------------------%
    % EM algorithm
    %-------------------------------------------------------------------------%
    
    params_init = params; 
    tol = 1e-5;
    params = f_EMalg(y_d_stand, [], [], y_q_stand, aux, params, tol); 
    
    %-------------------------------------------------------------------------%
    % run KF/KS to get back-, now- and forecasts
    %-------------------------------------------------------------------------%
      
    dat = [[y_d_stand y_d_fore_stand]; [y_q_stand y_q_fore_stand]]; 
    [Z, H, T, R, Q] = f_state_space_params(params, aux, size(dat, 2));
    s0 = zeros(size(T,1),1); 
    P0 = 100 * eye(size(T,1)); 
    [stT, ~, ~] = f_KS_DK_logL(dat,T,Z,H,R,Q,s0,P0);
    
    %-------------------------------------------------------------------------%
    % forecasts 
    %-------------------------------------------------------------------------%
      
    chi_q_stand = params.lam_q_flow * stT(end-2*Nr+1:end-Nr,:);
    chi_q = chi_q_stand * std_gdp + mean_gdp;

    backcast = chi_q(1, ind_backcast); 
    nowcast = chi_q(1, ind_nowcast);
    forecast1 = chi_q(1, ind_forecast); 
    forecast2 = chi_q(1, end);
    date_backcast = tmp.textdata{find(ind_backcast == 1) + 1, 1};
    date_nowcast = tmp.textdata{find(ind_nowcast == 1) + 1, 1};
    date_forecast1 = tmp.textdata{find(ind_forecast == 1) + 1, 1};
    date_forecast2 = tmp.textdata{size(ind_backcast,1) + 1, 1};
end