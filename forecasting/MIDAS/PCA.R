# This script performs forecasting using a MIDAS model with text data only. 
# Principal Component Analysis (PCA) is applied for dimensionality reduction, using the EM 
# algorithm from Stock and Watson (2002) to extract common factors from the text data. 
# The function `perform_forecast` handles the forecasting for different horizons 
# (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts).
# The script reads in a single vintage and generates forecasts for each horizon based on the
# extracted factors and OLS regression.

rm(list = ls())
# Load the necessary libraries
library(fredmdr)
library(midasr)

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
  
  topics <- data_rm_na[, selected_columns]
  
  # PCA based on EM algorithm following Stock and Watson (2002).
  pca <- f_emalg(scale(topics), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
  # Manually set number of factors to 2!
  #pca <- f_emalg(scale(topics), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)
  
  num_pcs <- ncol(pca$f)
  
  # Creating new column names based on the number of PCs
  new_colnames <- paste0("PC", 1:num_pcs)
  
  # Replace the first num_pcs series with PCs
  data_rm_na[, selected_columns[1:num_pcs]] <- pca$f
  
  # Rename the replaced columns
  colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames
  
  # Removing the rest of the T columns (T[num_pcs]-T199)
  data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]
  
  # Extract the GDP growth data series and remove NA values
  y <- data$d_gdp
  y <- y[!is.na(y)]
  y <- y[(1 + nlag):length(y)]
  
  # Create an empty dataframe for the lagged values of each topic
  lags_df <- data.frame()
  
  # Loop through all components to create the lags
  for (pc_num in 1:num_pcs) {
    
    # Get the name of the component
    pc_name <- paste0("PC", pc_num)
    
    # Get the component series
    X <- data_rm_na[[pc_name]]
    
    # Create the lags for this component
    mlags_component <- fmls(X, k = p, m = 3)
    
    # Rename the columns based on the component name
    colnames(mlags_component) <- gsub("^X", pc_name, colnames(mlags_component))
    
    # Bind the lags to the lags_df data frame
    if (pc_num == 1) {
      lags_df <- mlags_component
    } else {
      lags_df <- cbind(lags_df, mlags_component)
    }
  }
  
  # Filter out rows with NA values
  num_na_rows <- sum(apply(lags_df[1:length(y), 1:ncol(lags_df), drop = FALSE], 1, function(row) all(is.na(row))))
  y <- y[(1 + num_na_rows):length(y)]
  
  # Split the data into estimation and prediction datasets
  estimation_data <- lags_df[(1 + num_na_rows):(length(y) + num_na_rows), , drop = FALSE]
  prediction_data <- lags_df[(length(y) + 1 + num_na_rows + nlag):(length(y) + 1 + num_na_rows + nlag), ]
  
  # Run the linear regression
  ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, estimation_data)))
  
  # Convert prediction_data to data frame if it isn't already, then transpose
  ols_prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)
  
  # Insert an intercept term
  ols_prediction_data$`(Intercept)` <- 1
  
  # Reorder columns so intercept is first
  ols_prediction_data <- ols_prediction_data[, c('(Intercept)', colnames(ols_prediction_data)), drop = FALSE]
  
  # Remove the (Intercept).1 column if it exists
  ols_prediction_data <- ols_prediction_data[, !colnames(ols_prediction_data) %in% '(Intercept).1', drop = FALSE]
  
  # Make a prediction using the OLS model
  ols_pred <- predict(ols_model, newdata = ols_prediction_data)
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = ols_pred, date = forecast_date))
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
