# This script implements forecasting using a MIDAS model that combines both text and hard data.
# LASSO is applied for dimensionality reduction to select the most relevant predictors, 
# which are then used in an Ordinary Least Squares (OLS) regression for prediction.
# The implementation is tested across different forecasting horizons (backcasts, nowcasts, 
# 1-step-ahead, and 2-step-ahead forecasts) and for different numbers of lags applied uniformly 
# to all predictors, although the starting points for text and hard data may differ due to 
# varying data availability.

# Load the necessary libraries
library('glmnet')
library(midasr)
# Source the function
source("cross_validation_ts.R")


## BACKCAST
nlag = 0
p = 2
# How many months of data available
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_both_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
backcast_date <- data$date[data$ind_backcast == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[4:(dim(data_rm_na)[1]-mon), 1:dim(data_rm_na)[2]]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

# Loop through all topics to create the lags
for (series_name in selected_columns) {
  
  # Get the topic series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X",  series_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

# Loop through all series to create the lags
for (series_name in selected_columns) {
  
  # Get the series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this series
  mlags_series <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the series name
  colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_series
  } else {
    lags_df <- cbind(lags_df, mlags_series)
  }
}

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]
y <- y[(2 + nlag):length(y)]

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

# Visualize the cross-validation error across different lambda values
#plot(cv.out)

# Extract the lambda value that gave the smallest mean cross-validated error
#bestlam=cv.out$lambda.min

# Cross-validaiton for time-series data
cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)

# Retrieve and use the optimal lambda
bestlam <- cv_results$best_lambda

# Fit the LASSO model on the estimation data using the grid of lambda values
out = glmnet(estimation_data,y, alpha=1, lambda=grid)

# Predict coefficients for the best lambda value
lasso.coef = predict(out, type='coefficients',
                     s=bestlam)

# Print non-zero coefficients (variables selected by LASSO)
lasso.coef_matrix <- as.matrix(lasso.coef)
non_zero_coefs <- lasso.coef_matrix[lasso.coef_matrix != 0]

# Return the variable names (rownames) of the non-zero coefficients
#as.list(rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1]!= 0])

#lasso.pred = predict(out, s=bestlam,
#                     newx = t(prediction_data))

# Extract the predictors selected by LASSO
selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]

# Exclude the intercept
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]

# Extract only columns that match the selected predictors
ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]

# Run the linear regression
ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))

# Summarize the OLS model
#summary(ols_model)

# Convert prediction_data to data frame if it isn't already, then transpose
prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)

# Create prediction data with only the selected predictors
ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]

# Ensure columns in ols_prediction_data match model predictors
# colnames(ols_prediction_data) == colnames(ols_data)

# Insert an intercept term
ols_prediction_data$`(Intercept)` <- 1

# Reorder columns so intercept is first
ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]

# Make a prediction using the OLS model
ols_pred <- predict(ols_model, newdata = ols_prediction_data)


## NOWCAST

nlag = 1
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_both_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
nowcast_date <- data$date[data$ind_nowcast == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(4+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

# Loop through all topics to create the lags
for (series_name in selected_columns) {
  
  # Get the topic series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X",  series_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

# Loop through all series to create the lags
for (series_name in selected_columns) {
  
  # Get the series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this series
  mlags_series <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the series name
  colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_series
  } else {
    lags_df <- cbind(lags_df, mlags_series)
  }
}

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]
y <- y[(2 + nlag):length(y)]

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

# Visualize the cross-validation error across different lambda values
#plot(cv.out)

# Extract the lambda value that gave the smallest mean cross-validated error
#bestlam=cv.out$lambda.min

# Cross-validaiton for time-series data
cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)

# Retrieve and use the optimal lambda
bestlam <- cv_results$best_lambda

# Fit the LASSO model on the estimation data using the grid of lambda values
out = glmnet(estimation_data,y, alpha=1, lambda=grid)

# Predict coefficients for the best lambda value
lasso.coef = predict(out, type='coefficients',
                     s=bestlam)

# Print non-zero coefficients (variables selected by LASSO)
lasso.coef_matrix <- as.matrix(lasso.coef)
non_zero_coefs <- lasso.coef_matrix[lasso.coef_matrix != 0]

# Return the variable names (rownames) of the non-zero coefficients
#as.list(rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1]!= 0])

#lasso.pred = predict(out, s=bestlam,
#                     newx = t(prediction_data))

# Extract the predictors selected by LASSO
selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]

# Exclude the intercept
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]

# Extract only columns that match the selected predictors
ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]

# Run the linear regression
ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))

# Summarize the OLS model
#summary(ols_model)

# Convert prediction_data to data frame if it isn't already, then transpose
prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)

# Create prediction data with only the selected predictors
ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]

# Ensure columns in ols_prediction_data match model predictors
# colnames(ols_prediction_data) == colnames(ols_data)

# Insert an intercept term
ols_prediction_data$`(Intercept)` <- 1

# Reorder columns so intercept is first
ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]

# Make a prediction using the OLS model
ols_pred <- predict(ols_model, newdata = ols_prediction_data)


## ONE-STEP-AHEAD

nlag = 2
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_both_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
one_step_ahead_date <- data$date[data$ind_forecast1Q == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(4+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

# Loop through all topics to create the lags
for (series_name in selected_columns) {
  
  # Get the topic series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X",  series_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

# Loop through all series to create the lags
for (series_name in selected_columns) {
  
  # Get the series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this series
  mlags_series <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the series name
  colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_series
  } else {
    lags_df <- cbind(lags_df, mlags_series)
  }
}

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(2 + nlag):length(y)]

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

# Visualize the cross-validation error across different lambda values
#plot(cv.out)

# Extract the lambda value that gave the smallest mean cross-validated error
#bestlam=cv.out$lambda.min

# Cross-validaiton for time-series data
cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)

# Retrieve and use the optimal lambda
bestlam <- cv_results$best_lambda

# Fit the LASSO model on the estimation data using the grid of lambda values
out = glmnet(estimation_data,y, alpha=1, lambda=grid)

# Predict coefficients for the best lambda value
lasso.coef = predict(out, type='coefficients',
                     s=bestlam)

# Print non-zero coefficients (variables selected by LASSO)
lasso.coef_matrix <- as.matrix(lasso.coef)
non_zero_coefs <- lasso.coef_matrix[lasso.coef_matrix != 0]

# Return the variable names (rownames) of the non-zero coefficients
#as.list(rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1]!= 0])

#lasso.pred = predict(out, s=bestlam,
#                     newx = t(prediction_data))

# Extract the predictors selected by LASSO
selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]

# Exclude the intercept
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]

# Extract only columns that match the selected predictors
ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]

# Run the linear regression
ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))

# Summarize the OLS model
#summary(ols_model)

# Convert prediction_data to data frame if it isn't already, then transpose
prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)

# Create prediction data with only the selected predictors
ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]

# Ensure columns in ols_prediction_data match model predictors
# colnames(ols_prediction_data) == colnames(ols_data)

# Insert an intercept term
ols_prediction_data$`(Intercept)` <- 1

# Reorder columns so intercept is first
ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]

# Make a prediction using the OLS model
ols_pred <- predict(ols_model, newdata = ols_prediction_data)

## TWO-STEP-AHEAD

nlag = 3
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <-  read.csv("../hard_data/vint_2010_1_30_both_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
two_step_ahead_date <- data$date[data$ind_forecast2Q == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(4+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Function to standardize data
standardize_data <- function(data) {
  return(scale(data, center = TRUE, scale = TRUE))
}

# Loop through all topics to create the lags
for (series_name in selected_columns) {
  
  # Get the topic series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X",  series_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

# Loop through all series to create the lags
for (series_name in selected_columns) {
  
  # Get the series
  X <- standardize_data(data_rm_na[[series_name]])
  #X <- data_rm_na[[series_name]]
  
  # Create the lags for this series
  mlags_series <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the series name
  colnames(mlags_series) <- gsub("^X", series_name, colnames(mlags_series))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_series
  } else {
    lags_df <- cbind(lags_df, mlags_series)
  }
}

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(2 + nlag):length(y)]

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

# Visualize the cross-validation error across different lambda values
#plot(cv.out)

# Extract the lambda value that gave the smallest mean cross-validated error
#bestlam=cv.out$lambda.min

# Cross-validaiton for time-series data
cv_results <- cross_validation_ts(estimation_data, y, n_folds = 10, grid = grid, alpha = 1)

# Retrieve and use the optimal lambda
bestlam <- cv_results$best_lambda

# Fit the LASSO model on the estimation data using the grid of lambda values
out = glmnet(estimation_data,y, alpha=1, lambda=grid)

# Predict coefficients for the best lambda value
lasso.coef = predict(out, type='coefficients',
                     s=bestlam)

# Print non-zero coefficients (variables selected by LASSO)
lasso.coef_matrix <- as.matrix(lasso.coef)
non_zero_coefs <- lasso.coef_matrix[lasso.coef_matrix != 0]

# Return the variable names (rownames) of the non-zero coefficients
#as.list(rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1]!= 0])

#lasso.pred = predict(out, s=bestlam,
#                     newx = t(prediction_data))

# Extract the predictors selected by LASSO
selected_predictors <- rownames(coef(out, s = bestlam))[coef(out, s = bestlam)[,1] != 0]

# Exclude the intercept
selected_predictors <- selected_predictors[selected_predictors != "(Intercept)"]

# Extract only columns that match the selected predictors
ols_data <- estimation_data[, colnames(estimation_data) %in% selected_predictors, drop = FALSE]

# Run the linear regression
ols_model <- lm(y ~ ., data = as.data.frame(cbind(y = y, ols_data)))

# Summarize the OLS model
#summary(ols_model)

# Convert prediction_data to data frame if it isn't already, then transpose
prediction_data <- as.data.frame(t(prediction_data), stringsAsFactors=FALSE)

# Create prediction data with only the selected predictors
ols_prediction_data <- prediction_data[, colnames(prediction_data) %in% selected_predictors, drop = FALSE]

# Ensure columns in ols_prediction_data match model predictors
# colnames(ols_prediction_data) == colnames(ols_data)

# Insert an intercept term
ols_prediction_data$`(Intercept)` <- 1

# Reorder columns so intercept is first
ols_prediction_data <- ols_prediction_data[, c('(Intercept)', selected_predictors), drop = FALSE]

# Make a prediction using the OLS model
ols_pred <- predict(ols_model, newdata = ols_prediction_data)
