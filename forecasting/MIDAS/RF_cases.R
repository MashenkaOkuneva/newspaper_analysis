# This script implements forecasting using a MIDAS model with text data only. 
# A Random Forest model is used for prediction, which is effective in handling a large number of predictors 
# and capturing complex nonlinear relationships. 
# The implementation is tested across different forecasting horizons (backcasts, nowcasts, 
# 1-step-ahead, and 2-step-ahead forecasts) and across different numbers of lags 
# applied uniformly to all predictors.

rm(list = ls())
# Load the necessary libraries
#install.packages('randomForest')
library(randomForest)
library(midasr)

## BACKCAST
nlag = 0
p = 2
# How many months of data available
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("vint_2010_1_30.csv", header = TRUE, stringsAsFactors = FALSE)
backcast_date <- data$date[data$ind_backcast == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")] # Create a vector of the column names
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[1:(dim(data_rm_na)[1]-mon), 1:dim(data_rm_na)[2]]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]
y <- y[(1 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all topics to create the lags
for (topic_name in selected_columns) {
  
  # Get the topic series
  X <- data_rm_na[[topic_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X", topic_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## NOWCAST

nlag = 1
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("vint_2010_1_30.csv", header = TRUE, stringsAsFactors = FALSE)
nowcast_date <- data$date[data$ind_nowcast == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")] # Create a vector of the column names
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(1+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(1 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all topics to create the lags
for (topic_name in selected_columns) {
  
  # Get the topic series
  X <- data_rm_na[[topic_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X", topic_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## ONE-STEP-AHEAD

nlag = 2
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("vint_2010_1_30.csv", header = TRUE, stringsAsFactors = FALSE)
#data <- read.csv(paste0("./vintages_MIDAS/", "vint_2010_7_30.csv"), header = TRUE, stringsAsFactors = FALSE)
one_step_ahead_date <- data$date[data$ind_forecast1Q == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")] # Create a vector of the column names
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(1+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(1 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all topics to create the lags
for (topic_name in selected_columns) {
  
  # Get the topic series
  X <- data_rm_na[[topic_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X", topic_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## TWO-STEP-AHEAD

nlag = 3
p = 2
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("vint_2010_1_30.csv", header = TRUE, stringsAsFactors = FALSE)
two_step_ahead_date <- data$date[data$ind_forecast2Q == 1][1]

selected_columns <- names(data)[startsWith(names(data), "T")] # Create a vector of the column names
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(1+mon):(dim(data_rm_na)[1]), 1:dim(data_rm_na)[2]]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(1 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all topics to create the lags
for (topic_name in selected_columns) {
  
  # Get the topic series
  X <- data_rm_na[[topic_name]]
  
  # Create the lags for this topic
  mlags_topic <- fmls(X, k = p, m = 3)
  
  # Rename the columns based on the topic name
  colnames(mlags_topic) <- gsub("^X", topic_name, colnames(mlags_topic))
  
  # Bind the lags to the lags_df data frame
  if (nrow(lags_df) == 0) {
    lags_df <- mlags_topic
  } else {
    lags_df <- cbind(lags_df, mlags_topic)
  }
}

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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