% This script generates direct multi-step AR(1) forecasts of GDP growth, 
% including backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts. 
% The resulting forecasts are saved into separate CSV files.

clear; close all; clc;

tic; % Start timer

% Define the directory path
dir_path = 'vintages_AR1';
% Get all CSV files in the directory
files = dir(fullfile(dir_path, '*.csv'));

% Arrays to hold results
backcasts = [];
nowcasts = [];
forecasts_1step = [];
forecasts_2step = [];

backcast_dates = [];
nowcast_dates = [];
forecast1_dates = [];
forecast2_dates = [];

for i = 1:length(files)
    % Read the data from the CSV file
    data = readtable(fullfile(dir_path, files(i).name));
    
    % Extract the 'date' and 'd_gdp' columns
    dates = data.date;
    d_gdp = data.d_gdp;
    
    % Keep only non-nan values of d_gdp and corresponding dates
    valid_indices = ~isnan(d_gdp);
    dates_sample = dates(valid_indices);
    d_gdp_sample = d_gdp(valid_indices);
    
    %---------------------------------------------------
    % Start of AR(1) model specification for direct multi-step forecast
    % Backcast
    % gdp_growth(t) = alpha1 + beta1*gdp_growth(t-1) + e(t)
    % Nowcast
    % gdp_growth(t) = alpha2 + beta2*gdp_growth(t-2) + e(t)
    % One-step-ahead forecast
    % gdp_growth(t) = alpha3 + beta3*gdp_growth(t-3) + e(t)
    % Two-step-ahead forecast
    % gdp_growth(t) = alpha4 + beta4*gdp_growth(t-4) + e(t)
    
    % Initialize storage for the forecasts and their dates
    forecasts = zeros(4, 1);
    forecast_dates = dates(isnan(d_gdp));
    
    T=length(d_gdp_sample);
    
    % Loop through the 4 models
    for i = 1:4
        % Current lag
        nlag = i;
        
        % Dependent variable for regression
        Y = d_gdp_sample(1+nlag:T);
        
        % Lagged values for the independent variable in regression
        Ylag = d_gdp_sample(1:T-nlag);
        
        % Independent variable matrix for regression
        X = [ones(length(Y),1) Ylag];
        
        % Parameter estimation
        param_est = inv(X'*X)*X'*Y;
        
        % Forecasting
        forecasts(i) = [1 d_gdp_sample(end-i+1)]*param_est;
    end
    
    % Extract individual forecasts and their dates
    backcast_date = forecast_dates(1);
    backcast = forecasts(1);
    nowcast_date = forecast_dates(2);
    nowcast = forecasts(2);
    one_step_ahead_date = forecast_dates(3);
    one_step_ahead = forecasts(3);
    two_step_ahead_date = forecast_dates(4);
    two_step_ahead = forecasts(4);
    
    backcasts = [backcasts; backcast];
    nowcasts = [nowcasts; nowcast];
    forecasts_1step = [forecasts_1step; one_step_ahead];
    forecasts_2step = [forecasts_2step; two_step_ahead];

    backcast_dates = [backcast_dates; backcast_date];
    nowcast_dates = [nowcast_dates; nowcast_date];
    forecast1_dates = [forecast1_dates; one_step_ahead_date];
    forecast2_dates = [forecast2_dates; two_step_ahead_date];
end

% Convert forecasts to cell
backcasts_cell = num2cell(backcasts);
nowcasts_cell = num2cell(nowcasts);
forecasts_1step_cell = num2cell(forecasts_1step);
forecasts_2step_cell = num2cell(forecasts_2step);

% Convert the `char` array of dates to a cell array
backcast_dates = cellstr(backcast_dates);
nowcast_dates = cellstr(nowcast_dates);
forecast1_dates = cellstr(forecast1_dates);
forecast2_dates = cellstr(forecast2_dates);

% Combine dates and forecasts
backcast_csv = [backcast_dates, backcasts_cell];
nowcast_csv = [nowcast_dates, nowcasts_cell];
forecast1_csv = [forecast1_dates, forecasts_1step_cell];
forecast2_csv = [forecast2_dates, forecasts_2step_cell];

% Sort the rows based on the dates
backcast_csv_sorted = sortrows(backcast_csv, 1);
nowcast_csv_sorted = sortrows(nowcast_csv, 1);
forecast1_csv_sorted = sortrows(forecast1_csv, 1);
forecast2_csv_sorted = sortrows(forecast2_csv, 1);

% Save to CSV files
writecell(backcast_csv_sorted, 'backcasts_ar1.csv');
writecell(nowcast_csv_sorted, 'nowcasts_ar1.csv');
writecell(forecast1_csv_sorted, 'forecasts_1step_ar1.csv');
writecell(forecast2_csv_sorted, 'forecasts_2step_ar1.csv');

elapsedTime = toc; % End timer and get elapsed time in seconds
fprintf('Time taken: %f seconds\n', elapsedTime);