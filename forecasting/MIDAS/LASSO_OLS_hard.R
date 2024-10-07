# This script implements forecasting using a MIDAS model with hard data only. 
# LASSO is applied for dimensionality reduction to select the most relevant predictors 
# among a set of financial and economic series, which are then used in an Ordinary Least Squares 
# (OLS) regression for prediction. 
# The function `perform_forecast` handles the forecasting for different horizons 
# (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts).
# The script reads in a single vintage and generates OLS-based forecasts using 
# the LASSO-selected predictors for each horizon.

# Required libraries
library(glmnet)
library(midasr)
# Source the function
source("cross_validation_ts.R")

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

perform_forecast <- function(data, nlag, p, mon, forecast_indicator) {
  
  # Function Parameters:
  # data: The dataframe coresponding to a particular vintage, containing GDP growth and hard economic data.
  # nlag: Specifies the lag structure:
  #       nlag = 0 for backcast (looking backward),
  #       nlag = 1 for nowcast (current period),
  #       nlag = 2 for one-step-ahead forecast, and so on.
  # p: Specifies the number of lags used in the MIDAS regression.
  # mon: Specifies the number of months of available data.
  # forecast_indicator: A string that indicates the column name in 'data' that identifies the forecast date.
  
  # Get column names starting with "financial" or "economic"
  selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
  
  # Filter out rows with all NA values in the financial/economic columns
  data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
  
  # Remove the first (1+mon) observations and the last (mon + 1) observations.
  #   - The first (1+mon) observations are removed to ensure the time series is compatible
  #     with a MIDAS approach, as the number of observations
  #     must be divisible by 3 to represent a quarterly series effectively.
  #   - The last (mon + 1) observations are removed to guarantee that the
  #     subsequent analysis only uses data points where all economic and financial 
  #     series are available.
  data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]
  
  # Extract the GDP growth data series and remove NA values
  y <- data$d_gdp
  y <- y[!is.na(y)]
  y <- y[(2 + nlag):length(y)]
  
  # Create an empty dataframe for the lagged values of each series
  lags_df <- data.frame()
  
  # Create lagged features for each economic/financial series
  for (series_name in selected_columns) {
    # Get the series
    X <- standardize_data(data_rm_na[[series_name]])
    # Create the lags for this series
    mlags_series <- fmls(X, k = p, m = 3)
    # Rename the columns based on the series name
    colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
    # Bind the lags to the lags_df data frame
    lags_df <- if (nrow(lags_df) == 0) mlags_series else cbind(lags_df, mlags_series)
  }
  
  # Filter out rows with NA values
  num_na_rows <- sum(apply(lags_df[1:length(y), 1:ncol(lags_df)], 1, function(row) all(is.na(row))))
  y <- y[(1 + num_na_rows):length(y)]
  
  # Split the data into estimation and prediction datasets
  estimation_data <- lags_df[(1 + num_na_rows):(length(y) + num_na_rows), ]
  prediction_data <- lags_df[(length(y) + 1 + num_na_rows + nlag):(length(y) + 1 + num_na_rows + nlag), ]
  
  # Create a sequence of lambda values for cross-validation
  grid = 10^seq(10,-2, length=100)
  
  # Set a random seed for reproducibility
  #set.seed(1)
  
  # Perform k-fold cross-validation (default k=10) to determine the best lambda
  #cv.out = cv.glmnet(estimation_data, y,
  #                   alpha = 1, lambda=grid)
  
  # Extract the lambda value that gave the smallest mean cross-validated error
  #bestlam=cv.out$lambda.min
  
  # Cross-validaiton for time-series data
  cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)
  
  # Retrieve and use the optimal lambda
  bestlam <- cv_results$best_lambda
  
  # Fit the LASSO model on the estimation data
  out = glmnet(estimation_data, y, alpha = 1, lambda=grid)
  
  # Identify the predictors that have non-zero coefficients
  selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]
  selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]
  
  # Filter the estimation dataset to only use selected predictors
  ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]
  
  # OLS regression using the selected predictors
  ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))
  
  # Prepare the data for prediction
  prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)
  ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]
  ols_prediction_data$`(Intercept)` <- 1
  ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]
  
  # Generate forecast using the OLS model
  ols_pred <- predict(ols_model, newdata = ols_prediction_data)
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = ols_pred, date = forecast_date))
}

# Read in the data
data <- read.csv("../hard_data/vint_2010_1_30_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)

# Perform Backcast
backcast_result <- perform_forecast(data, nlag = 0, p = 2, mon = 1, forecast_indicator = 'ind_backcast')
backcast <- backcast_result$forecast
backcast_date <- backcast_result$date

# Perform Nowcast
nowcast_result <- perform_forecast(data, nlag = 1, p = 2, mon = 1, forecast_indicator = 'ind_nowcast')
nowcast <- nowcast_result$forecast
nowcast_date <- nowcast_result$date

# Perform One-step-ahead forecast
one_step_result <- perform_forecast(data, nlag = 2, p = 2, mon = 1, forecast_indicator = 'ind_forecast1Q')
forecast1 <- one_step_result$forecast
one_step_date <- one_step_result$date

# Perform Two-step-ahead forecast
two_step_result <- perform_forecast(data, nlag = 3, p = 2, mon = 1, forecast_indicator = 'ind_forecast2Q')
forecast2 <- two_step_result$forecast
two_step_date <- two_step_result$date

# Print results
print(paste("Backcast date:", backcast_date, "Forecast:", backcast))
print(paste("Nowcast date:", nowcast_date, "Forecast:", nowcast))
print(paste("One-step-ahead date:", one_step_date, "Forecast:", forecast1))
print(paste("Two-step-ahead date:", two_step_date, "Forecast:", forecast2))
