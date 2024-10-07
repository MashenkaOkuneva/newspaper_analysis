# This script performs forecasting using a MIDAS model with hard data only. 
# LASSO is applied for dimensionality reduction to select the most relevant predictors.
# The function `perform_forecast` is used to handle forecasting for different horizons 
# (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts) for each vintage.
#
# The script iterates through all 34 real-time data vintages in the specified folder, 
# generating LASSO-based forecasts across all horizons using a fixed number of lags (p).
# Forecast results for each horizon (e.g., backcasts, nowcasts) are combined across all vintages 
# and saved into separate CSV files.

rm(list = ls())
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
  
  # Generate forecast using the LASSO model
  lasso_pred = predict(out, s=bestlam,
                       newx = t(prediction_data))
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = lasso_pred, date = forecast_date))
}

# Create vectors to store results for all vintages
backcasts <- c()
nowcasts <- c()
forecasts_1step <- c()
forecasts_2step <- c()

backcast_dates <- c()
nowcast_dates <- c()
forecast1_dates <- c()
forecast2_dates <- c()

# Get the list of all .csv files in the 'vintages_hard_MIDAS' directory
files <- list.files(path="../hard_data/vintages_hard_MIDAS/", pattern="*.csv")

# Iterate over each file and perform forecasting
for (file in files) {
  
  # Read in the data
  data <- read.csv(paste0("../hard_data/vintages_hard_MIDAS/", file), header = TRUE, stringsAsFactors = FALSE)
  
  backcast_result <- perform_forecast(data, nlag = 0, p = 2, mon = 1, forecast_indicator = 'ind_backcast')
  nowcast_result <- perform_forecast(data, nlag = 1, p = 2, mon = 1, forecast_indicator = 'ind_nowcast')
  one_step_result <- perform_forecast(data, nlag = 2, p = 2, mon = 1, forecast_indicator = 'ind_forecast1Q')
  two_step_result <- perform_forecast(data, nlag = 3, p = 2, mon = 1, forecast_indicator = 'ind_forecast2Q')
  
  # Append results to the result vectors
  backcasts <- c(backcasts, backcast_result$forecast)
  nowcasts <- c(nowcasts, nowcast_result$forecast)
  forecasts_1step <- c(forecasts_1step, one_step_result$forecast)
  forecasts_2step <- c(forecasts_2step, two_step_result$forecast)
  
  backcast_dates <- c(backcast_dates, backcast_result$date)
  nowcast_dates <- c(nowcast_dates, nowcast_result$date)
  forecast1_dates <- c(forecast1_dates, one_step_result$date)
  forecast2_dates <- c(forecast2_dates, two_step_result$date)
  
}

# Convert lists to data frames and order them by date
backcasts_df <- data.frame(date = unlist(backcast_dates), forecast = unlist(backcasts))
nowcasts_df <- data.frame(date = unlist(nowcast_dates), forecast = unlist(nowcasts))
forecasts_1step_df <- data.frame(date = unlist(forecast1_dates), forecast = unlist(forecasts_1step))
forecasts_2step_df <- data.frame(date = unlist(forecast2_dates), forecast = unlist(forecasts_2step))

# Sort the data frames by date
backcasts_df <- backcasts_df[order(backcasts_df$date), ]
nowcasts_df <- nowcasts_df[order(nowcasts_df$date), ]
forecasts_1step_df <- forecasts_1step_df[order(forecasts_1step_df$date), ]
forecasts_2step_df <- forecasts_2step_df[order(forecasts_2step_df$date), ]

# Save the results to CSV files without column names
write.table(backcasts_df, "backcasts_midas_lasso_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(nowcasts_df, "nowcasts_midas_lasso_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(forecasts_1step_df, "forecasts_1step_midas_lasso_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(forecasts_2step_df, "forecasts_2step_midas_lasso_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)