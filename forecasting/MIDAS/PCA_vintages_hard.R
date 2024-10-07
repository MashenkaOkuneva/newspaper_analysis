# This script performs forecasting using a MIDAS model with hard data only. 
# Principal Component Analysis (PCA) is used for dimensionality reduction, applying the EM 
# algorithm from Stock and Watson (2002) to extract common factors from a set of financial and economic series.
# The function `perform_forecast` is used to handle forecasting for different horizons 
# (backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts) for each vintage.
#
# The script iterates through all 34 real-time data vintages in the specified folder, 
# generating OLS-based forecasts using the extracted factors across all horizons with a fixed number of lags (p).
# Forecast results for each horizon (e.g., backcasts, nowcasts) are combined across all vintages 
# and saved into separate CSV files.

# Load the necessary libraries
library(fredmdr)
library(midasr)

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
  
  economic_series <- data_rm_na[, selected_columns]
  
  # PCA based on EM algorithm following Stock and Watson (2002).
  pca <- f_emalg(scale(economic_series), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
  # Manually set number of factors to 2!
  #pca <- f_emalg(scale(economic_series), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)
  
  num_pcs <- ncol(pca$f)
  
  # Creating new column names based on the number of PCs
  new_colnames <- paste0("PC", 1:num_pcs)
  
  # Replace the first num_pcs financial and economic series with PCs
  data_rm_na[, selected_columns[1:num_pcs]] <- pca$f
  
  # Rename the replaced columns
  colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames
  
  # Removing the rest of the economic and financial series
  data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]
  
  # Extract the GDP growth data series and remove NA values
  y <- data$d_gdp
  y <- y[!is.na(y)]
  y <- y[(2 + nlag):length(y)]
  
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
  num_na_rows <- sum(apply(lags_df[1:length(y), 1:ncol(lags_df)], 1, function(row) all(is.na(row))))
  y <- y[(1 + num_na_rows):length(y)]
  
  # Split the data into estimation and prediction datasets
  estimation_data <- lags_df[(1 + num_na_rows):(length(y) + num_na_rows), ]
  prediction_data <- lags_df[(length(y) + 1 + num_na_rows + nlag):(length(y) + 1 + num_na_rows + nlag), ]
  
  # Run the linear regression
  ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, estimation_data)))
  
  # Convert prediction_data to data frame if it isn't already, then transpose
  ols_prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)
  
  # Insert an intercept term
  ols_prediction_data$`(Intercept)` <- 1
  
  # Reorder columns so intercept is first
  ols_prediction_data <- ols_prediction_data[, c('(Intercept)', colnames(ols_prediction_data)), drop = FALSE]
  
  # Make a prediction using the OLS model
  ols_pred <- predict(ols_model, newdata = ols_prediction_data)
  
  # Extract the forecast date
  forecast_date <- data$date[data[[forecast_indicator]] == 1][1]
  
  return(list(forecast = ols_pred, date = forecast_date))
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

# Get the list of all .csv files in the 'vintages_MIDAS' directory
files <- list.files(path="../hard_data/vintages_hard_MIDAS/", pattern="*.csv")

# Iterate over each file and perform forecasting
for (file in files) {
  
  # Read in the data
  data <- read.csv(paste0("../hard_data/vintages_hard_MIDAS/", file), header = TRUE, stringsAsFactors = FALSE)
  
  backcast_result <- perform_forecast(data, nlag = 0, p = 0, mon = 1, forecast_indicator = 'ind_backcast')
  nowcast_result <- perform_forecast(data, nlag = 1, p = 0, mon = 1, forecast_indicator = 'ind_nowcast')
  one_step_result <- perform_forecast(data, nlag = 2, p = 0, mon = 1, forecast_indicator = 'ind_forecast1Q')
  two_step_result <- perform_forecast(data, nlag = 3, p = 0, mon = 1, forecast_indicator = 'ind_forecast2Q')
  
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
write.table(backcasts_df, "backcasts_midas_pca_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(nowcasts_df, "nowcasts_midas_pca_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(forecasts_1step_df, "forecasts_1step_midas_pca_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)
write.table(forecasts_2step_df, "forecasts_2step_midas_pca_hard.csv", row.names = FALSE, col.names=FALSE, sep=",", quote=FALSE)