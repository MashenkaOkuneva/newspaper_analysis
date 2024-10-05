clear; close all; clc;
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%
%- This script tests the EM algorithm for a mixed-frequency dynamic factor
%- model using daily, monthly and quarterly data. The input data is stored in the CSV 
%- file `vint_2010_3_30.csv`, which is produced by the `construct_vintage_hard.R`
%- script. The file also contains the W's and Xi's. 
%- Currently five daily and twelve monthly series are used in the estimation.  
%- Main output is a plot displaying 
%- a) the standardized daily series and quarterly GDP growth
%- b) the estimated daily factor
%- c) the daily GDP growth as well as now- and forecasts
%-    (1Q and 2Q) along with the realizations for the particular vintage. 
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%
%-------------------------------------------------------------------------%

%-------------------------------------------------------------------------%
% user settings
%-------------------------------------------------------------------------%
filename = 'vint_2010_3_30.csv';
%filename = 'vint_2013_3_30.csv'
%dirname = '..\..\hard_data\';
dirname = '..\..\hard_data\vintages_hard\';
Nr = 1;
Np = 10;

%-------------------------------------------------------------------------%
% load data
%-------------------------------------------------------------------------%
tmp = importdata([dirname, filename]);

% offset as there are less numeric columns!
offset_numcols = size(tmp.textdata, 2) - size(tmp.data, 2);

% back out data for estimation and forecasting
aux.ind_sample = logical(tmp.data(:, find(strcmp('ind_sample', tmp.textdata(1,:))) - offset_numcols));

% daily data
%ind_y_d = find(contains(tmp.textdata(1,:), 'y_d_')) - offset_numcols;
ind_y_d = [5 7 8 9];
%ind_y_d = setdiff(ind_y_d, ind_y_d([6, 9, 23])); % manually remove T05, T07, T21
%ind_y_d = [1 11 37 45] + 4; % topics T0, T10, T21, T36, T44 => highest correlated with GDP
y_d = tmp.data(aux.ind_sample, ind_y_d)';
y_d_fore = tmp.data(~aux.ind_sample, ind_y_d)';

% monthly data
ind_y_m = find(contains(tmp.textdata(1,:), 'y_m_')) - offset_numcols;
%ind_y_m = [10 11 12];
y_m = tmp.data(aux.ind_sample, ind_y_m)';
y_m_fore = tmp.data(~aux.ind_sample, ind_y_m)';
aux.ind_m_flow = [repelem(true, size(y_m,1))];

% quarterly data
ind_y_q = find(contains(tmp.textdata(1,:), 'y_q_')) - offset_numcols;
y_q = tmp.data(aux.ind_sample, ind_y_q)';
y_q_fore = tmp.data(~aux.ind_sample, ind_y_q)';
aux.ind_q_flow = true; 

% weights and Xi
aux.Xi_md = tmp.data(:, find(strcmp('Xi_md', tmp.textdata(1,:))) - offset_numcols);
aux.W_md_p = tmp.data(:, find(strcmp('W_md_p', tmp.textdata(1,:))) - offset_numcols);
aux.W_md_c = tmp.data(:, find(strcmp('W_md_c', tmp.textdata(1,:))) - offset_numcols);
aux.Xi_qd = tmp.data(:, find(strcmp('Xi_qd', tmp.textdata(1,:))) - offset_numcols);
aux.W_qd_p = tmp.data(:, find(strcmp('W_qd_p', tmp.textdata(1,:))) - offset_numcols);
aux.W_qd_c = tmp.data(:, find(strcmp('W_qd_c', tmp.textdata(1,:))) - offset_numcols);

% inds for now- and forecasts
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
y_m_stand = (y_m - nanmean(y_m, 2)) ./ nanstd(y_m, [], 2);
y_m_fore_stand = (y_m_fore - nanmean(y_m, 2)) ./ nanstd(y_m, [], 2);
mean_gdp = nanmean(y_q);
std_gdp = nanstd(y_q);
y_q_stand = (y_q - mean_gdp) / std_gdp; 
y_q_fore_stand = (y_q_fore - mean_gdp) / std_gdp; 

% starting values
params = f_start_vals(y_d_stand, [], y_m_stand, y_q_stand, aux, Nr, Np);

%-------------------------------------------------------------------------%
% EM algorithm
%-------------------------------------------------------------------------%

params_init = params; 
tol = 1e-5;
params = f_EMalg(y_d_stand, [], y_m_stand, y_q_stand, aux, params, tol); 

%-------------------------------------------------------------------------%
% run KF/KS to get back-, now- and forecasts
%-------------------------------------------------------------------------%
  
dat = [[y_d_stand y_d_fore_stand]; [y_m_stand y_m_fore_stand]; [y_q_stand y_q_fore_stand]]; 
[Z, H, T, R, Q] = f_state_space_params(params, aux, size(dat, 2));
s0 = zeros(size(T,1),1); 
P0 = 100 * eye(size(T,1)); 
[stT, ~, ~] = f_KS_DK_logL(dat,T,Z,H,R,Q,s0,P0);

%-------------------------------------------------------------------------%
% plot 
%-------------------------------------------------------------------------%

figure;
subplot(3,1,1)
p1 = plot(y_d_stand(1,:)', 'b-');
hold on
plot(y_d_stand(2:end,:)', 'b-');
p2 = plot(y_q_stand', 'kd', 'MarkerFaceColor', 'k', 'MarkerSize',4);
xticks(ind_plot(1:5:end))
xticklabels(dates_plot(1:5:end))
legend([p1, p2], {'daily series', 'GDP growth'}, 'Location', 'best')
title('(standardized) daily series and GDP growth')

subplot(3,1,2)
plot(stT(1:Nr,:)', 'b-')
xticks(ind_plot(1:5:end))
xticklabels(dates_plot(1:5:end))
title('daily factor(s)')

chi_q_stand = params.lam_q_flow * stT(end-2*Nr+1:end-Nr,:);
chi_q = chi_q_stand * std_gdp + mean_gdp;
Nt = sum(aux.ind_sample); 
Nh = length(aux.ind_sample) - Nt; % sum(aux.ind_sample == 0)
y_eoq_hat = NaN(1, Nt);
y_eoq_hat(:, ~isnan(y_q)) = chi_q(:, ~isnan(y_q)); 

y_eoq_fore = NaN(1, Nt+Nh); 
y_eoq_fore(1, ind_nowcast) = chi_q(1, ind_nowcast); 
y_eoq_fore(1, ind_forecast) = chi_q(1, ind_forecast); 
y_eoq_fore(1, end) = chi_q(1, end);

% actuals
acts = [y_q NaN(1, Nh)];
acts(1, ind_nowcast) = 0.64;
%acts(1, ind_forecast) = 9.1;
acts(1, ind_forecast) = 8.63;
acts(1, end) = 2.8;

subplot(3,1,3)
plot([chi_q(:, 1: sum(aux.ind_sample)) NaN(1, Nh)]', 'b-');
hold on
p1 = plot([y_eoq_hat NaN(1, Nh)]', 'bo',  'MarkerFaceColor', 'b');
plot([NaN(1, Nt), chi_q(:, sum(aux.ind_sample)+1:end)]', 'r-')
p2 = plot(y_eoq_fore', 'ro', 'MarkerFaceColor', 'r');
p3 = plot(acts, 'kd', 'MarkerFaceColor', 'k', 'MarkerSize',4);
xticks(ind_plot(1:5:end))
xticklabels(dates_plot(1:5:end))
legend([p1, p2, p3], {'in-sample', 'out-of-sample', 'actuals'}, 'Location','SouthWest', 'Fontsize', 10)
title('quarterly GDP growth (ann.), forecasts and actuals')

% save figure
fig = gcf;
orient(fig,'landscape')
fig.Units = 'inches';
fig.OuterPosition = [0.25 0.25 10 7];
exportgraphics(fig, 'results_2010_03_30_1_factor_hard_no_trafo_1345_li.pdf'); 


