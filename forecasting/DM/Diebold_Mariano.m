clear
clc

% This script performs the Diebold-Mariano test to compare forecast accuracy 
% between different models over four forecast horizons. The script evaluates 
% two types of comparisons:

% 1. Model vs. Benchmarks: It compares the forecast errors from different DFM 
% or MIDAS models (text, hard, or combined models) against benchmarks such as AR(1) 
% or professional survey forecasts.
% 2. Combination vs. Single Source Models: It also compares forecast errors of combined 
% models (text + hard data) against models using only hard data.

% The test decisions are calculated using both the standard DM statistic and the modified DM 
% statistic, applying both normal and t-distribution 
% critical values. Additionally, the script computes and reports Root Mean Squared Errors (RMSE) 
% and the RMSE ratio between models for each forecast horizon.

% Initialization
hmax=4;

%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_choose_10stable_1fac_K_30.csv', 1, 0); 
%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_both_2fac_K_30_10_topics.csv', 1, 0);
%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_both_2fac_K_30_10_stable.csv', 1, 0); 
%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_hard_1fac_no_trafo_1345_li.csv', 1, 0); 
%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_choose_20var_1fac_outlier.csv', 1, 0); 
%fe_ms_model1 = csvread('..\DFM\data\forecast_errors_dfm_choose_7var_1fac_K_30.csv', 1, 0);
%fe_ms_model1 = csvread('..\MIDAS\forecast_errors_midas_lasso_0lag.csv', 1, 0);
%fe_ms_model1 = csvread('..\combination\forecast_errors_combination_dfm_text_hard_10stable.csv', 1, 0); 
%fe_ms_model1 = csvread('..\combination\forecast_errors_combination_midas_text_hard_10stable_equal.csv', 1, 0); 
fe_ms_model1 = csvread('..\combination\forecast_errors_combination_midas_text_hard_10stable_ridge_3_equal.csv', 1, 0); 
%fe_ms_model2 = csvread('..\AR1\forecast_errors_ar1.csv', 1, 0);  
%fe_ms_model2 = csvread('..\reuters-poll-eval\forecast_errors_professional.csv', 1, 0);
%fe_ms_model2 = csvread('..\DFM\data\forecast_errors_dfm_hard_1fac_no_trafo_1345_li.csv', 1, 0);  
%fe_ms_model2 = csvread('..\MIDAS\forecast_errors_midas_lasso_2lags_hard.csv', 1, 0);
fe_ms_model2 = csvread('..\MIDAS\forecast_errors_midas_ridge_2lags_hard.csv', 1, 0);
for h=1:hmax 
    e1 = fe_ms_model1(:,h);
    e2 = fe_ms_model2(:,h);
    tau = length(e1(:,1)); % number of windows
    % Define the loss differential and calculate its mean
    d = e1.^2 - e2.^2;
    dMean=mean(d);
    % Calculate the variance of the loss differential, 
    % taking into account autocorrelation.
    Sigma_Ir=neweywest(d,h-1);    
    % Calculate the diebold mariano statistic DM ~N(0,1) 
    % and the modified DM statistic
    DM(1,h) = dMean/sqrt((1/tau)*Sigma_Ir);
    MDM(1,h)=tau^(-0.5)*sqrt(tau+1-2*h+tau^(-1)*h*(h-1))*DM(h);
end
%Test decisions using DM
    Dec_equal_DM=abs(DM)>norminv(0.975);
    pval_equal_DM=(1-normcdf(abs(DM)))*2
    Dec_model1better_DM=DM<norminv(0.05) 
    pval_model1better_DM=normcdf(DM)
% Test decisions using DM and t distribution
    Dec_equal_DM_t=abs(DM)>tinv(0.975,tau-1);
    pval_equal_DM_t=(1-tcdf(abs(DM),tau-1))*2
    Dec_model1_better_DM_t=DM<tinv(0.05,tau-1)
    pval_model1better_DM_t=tcdf(DM,tau-1)
% Test decisions using MDM (for h>1)
    Dec_equal_MDM=abs(MDM)>norminv(0.975);
    pval_equal_MDM=(1-normcdf(abs(MDM)))*2
    Dec_model1better_MDM=MDM<norminv(0.05)
    pval_model1better_MDM=normcdf(MDM)
% Test decisions using MDM and t distribution
    Dec_equal_MDM_t=abs(MDM)>tinv(0.975,tau-1);
    pval_equal_MDM_t=(1-tcdf(abs(MDM),tau-1))*2
    Dec_model1_better_MDM_t=MDM<tinv(0.05,tau-1)
    pval_model1better_MDM_t=tcdf(MDM,tau-1)


% Mean squared errors
MSE_model1 = zeros(1,hmax);

% Compute mean squared errors for each forecast horizon
for h=1:hmax
    squared_fe_ms_model1=fe_ms_model1(:,h).^2;
    MSE_model1(1,h)=mean(squared_fe_ms_model1);
    RMSE_model1(1,h)= MSE_model1(1,h).^0.5;
end

% Mean squared errors
MSE_model2 = zeros(1,hmax);

% Compute mean squared errors for each forecast horizon
for h=1:hmax
    squared_fe_ms_model2=fe_ms_model2(:,h).^2;
    MSE_model2(1,h)=mean(squared_fe_ms_model2);
    RMSE_model2(1,h)= MSE_model2(1,h).^0.5;
end

RMSE_ratio = RMSE_model1./RMSE_model2;
