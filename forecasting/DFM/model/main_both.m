clear; close all; clc;
%-------------------------------------------------------------------------%
% This script produces backcasts, nowcasts, and 1-step- and 2-step-ahead 
% forecasts for 34 real-time data vintages using a dynamic factor model. 
% The forecasts are generated in parallel using a combination of 
% sign-adjusted topics and hard economic data.
%
% Key Details:
% - For each vintage, it runs the `run_forecast_both` function, which estimates 
%   the mixed-frequency model using a mix of daily sign-adjusted topics and 
%   traditional economic indicators.
% - The script collects and stores the resulting forecasts and their corresponding 
%   dates in separate CSV files:
%   a) `backcasts_both_2fac_K_30_10_stable.csv`
%   b) `nowcasts_both_2fac_K_30_10_stable.csv`
%   c) `forecasts_1step_both_2fac_K_30_10_stable.csv`
%   d) `forecasts_2step_both_2fac_K_30_10_stable.csv`
%-------------------------------------------------------------------------%

tic; % Start timer

% Define the directory path
dir_path = '..\..\hard_data\vintages_both\';

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

parfor i = 1:length(files)
    [b, n, f1, f2, db, dn, df1, df2] = run_forecast_both(files(i).name, dir_path, 2, 10);
    
    backcasts = [backcasts; b];
    nowcasts = [nowcasts; n];
    forecasts_1step = [forecasts_1step; f1];
    forecasts_2step = [forecasts_2step; f2];

    backcast_dates = [backcast_dates; db];
    nowcast_dates = [nowcast_dates; dn];
    forecast1_dates = [forecast1_dates; df1];
    forecast2_dates = [forecast2_dates; df2];
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
writecell(backcast_csv_sorted, 'backcasts_both_2fac_K_30_10_stable.csv');
writecell(nowcast_csv_sorted, 'nowcasts_both_2fac_K_30_10_stable.csv');
writecell(forecast1_csv_sorted, 'forecasts_1step_both_2fac_K_30_10_stable.csv');
writecell(forecast2_csv_sorted, 'forecasts_2step_both_2fac_K_30_10_stable.csv');

elapsedTime = toc; % End timer and get elapsed time in seconds
fprintf('Time taken: %f seconds\n', elapsedTime);