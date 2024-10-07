# This script performs forecasting using a MIDAS model with text data only. 
# Random Forests are used for dimensionality reduction and to capture nonlinear relationships 
# between the predictors and the target variable. 
# The function `perform_forecast` handles the forecasting for different horizons 
# (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts).
# The script reads in a single vintage and generates Random Forest-based forecasts for each horizon.

rm(list = ls())
# Required libraries
library(midasr)
library(randomForest)

perform_forecast <- function(data, nlag, p, mon, forecast_indicator, is_backcast = FALSE) {
  
  # Function Parameters:
  # data: The dataframe coresponding to a particular vintage, containing GDP growth and topics data.
  # nlag: Specifies the lag structure:
  #       nlag = 0 for backcast (looking backward),
  #       nlag = 1 for nowcast (current period),
  #       nlag = 2 for one-step-ahead forecast, and so on.
  # p: Specifies the number of lags used in the MIDAS regression.
  # mon: Specifies the number of months of available data.
  # forecast_indicator: A string that indicates the column name in 'data' that identifies the forecast date.
  # is_backcast: A boolean flag; set to TRUE if the function is performing a backcast. Default is FALSE.
  
  # Create a vector of the column names
  selected_columns <- names(data)[startsWith(names(data), "T")]
  
  # Filter out rows with all NA values in the topic columns
  data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
  
  # If it's backcasting, modify the data removing the last 'mon' observations
  if (is_backcast) {
    data_rm_na <- data_rm_na[1:(dim(data_rm_na)[1]-mon), 1:dim(data_rm_na)[2]]
  } else {
    data_rm_na <- data_rm_na[(1 + mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]
  }
  
  # Extract the GDP growth data series and remove NA values
  y <- data$d_gdp
  y <- y[!is.na(y)]
  y <- y[(1 + nlag):length(y)]
  
  # Create an empty dataframe for the lagged values of each topic
  lags_df <- data.frame()
  
  # Create lagged features for each topic
  for (topic_name in selected_columns) {
    # Get the topic series
    X <- data_rm_na[[topic_name]]
    # Create the lags for this topic
    mlags_topic <- fmls(X, k = p, m = 3)
    # Rename the columns based on the topic name
    colnames(mlags_topic) <- gsub("^X", topic_name, colnames(mlags_topic))
    # Bind the lags to the lags_df data frame
    lags_df <- if (nrow(lags_df) == 0) mlags_topic else cbind(lags_df, mlags_topic)
  }
  
  # Filter out rows with NA values
  num_na_rows <- sum(apply(lags_df[1:length(y), 1:ncol(lags_df)], 1, function(row) all(is.na(row))))
  y <- y[(1 + num_na_rows):length(y)]
  
  # Split the data into estimation and prediction datasets
  estimation_data <- lags_df[(1 + num_na_rows):(length(y) + num_na_rows), ]
  prediction_data <- lags_df[(length(y) + 1 + num_na_rows + nlag):(length(y) + 1 + num_na_rows + nlag), ]
  
  # Fit the Random Forest model on the estimation data
  set.seed(1)
  rf_model <- randomForest(x = estimation_data, 
                           y = y, 
                           ntree = 500, 
                           mtry = floor(ncol(estimation_data) / 3), 
                           importance = TRUE)
  
  # Prediction using Random Forests
  rf_pred <- predict(rf_model, newdata = prediction_data)
  
  # Variable importance:
  #importance(rf_model)
  
  # To visualize the variable importance:
  #varImpPlot(rf_model)
  
  # Retrieve the variable importance data
  importance_data <- importance(rf_model)
  
  # Ordering the variables based on their importance
  # The first column (%IncMSE) indicates a variable's importance
  ordered_importance <- importance_data[order(importance_data[,1], decreasing = TRUE),]
  
  # Retrieve names of 10 most important predictors
  important_predictors <- rownames(ordered_importance)[1:10]
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = rf_pred, date = forecast_date))
}

# Read in the data
data <- read.csv("vint_2010_1_30.csv", header = TRUE, stringsAsFactors = FALSE)

# Perform Backcast
backcast_result <- perform_forecast(data, nlag = 0, p = 2, mon = 1, forecast_indicator = 'ind_backcast', is_backcast = TRUE)
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
