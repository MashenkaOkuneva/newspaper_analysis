# This script implements forecasting using a MIDAS model with hard data only. 
# Principal Component Analysis (PCA) is applied for dimensionality reduction, using the EM 
# algorithm from Stock and Watson (2002) to extract common factors from the financial and economic data. 
# The implementation is tested across different forecasting horizons (backcasts, nowcasts, 
# 1-step-ahead, and 2-step-ahead forecasts) and across different numbers of lags 
# applied uniformly to all factors.

# Load the necessary libraries
library(fredmdr)
library(midasr)

## BACKCAST
nlag = 0
p = 0
# How many months of data available
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
backcast_date <- data$date[data$ind_backcast == 1][1]

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

economic_series <- data_rm_na[, selected_columns]

# Calculate eigenvalues and plot cumulative share of explained variance
#eigvals_pca <- eigen(cor(economic_series, use = "complete.obs"), only.values = T)

#plot(seq(1, length(eigvals_pca$values)), 
#     cumsum(eigvals_pca$values) / sum(eigvals_pca$values),
#     xlab = "eigenvalue", ylab = "share of explained variance",
#     main = "eigenvalue decomposition of economic_series")

# PCA based on EM algorithm following Stock and Watson (2002).
pca <- f_emalg(scale(economic_series), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
# Manually set number of factors to 2!
#pca <- f_emalg(scale(economic_series), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)

#cols <- rep(c("cornflowerblue", 
#              "darkorange1", 
#              "darkorchid3", 
#              "darkolivegreen4", 
#              "brown"), times = 2)
#ltys <- rep(c(1, 2), each = 5)
#matplot(as.Date(data_rm_na$date, format = "%Y-%m-%d"), pca$f, 
#        type = "l", lty = ltys, col = cols, 
#        main = "First 10 principal components of transformed series",
#        ylab = "", xlab = "")
#legend(as.Date("1995-01-01"), 2.0, 
#       legend = paste0("PC", seq(1, 10)), 
#       col = cols,
#       lty= ltys, cex= 1, box.lty = 0, y.intersp=2, bg="transparent", ncol = 5)

num_pcs <- ncol(pca$f)

# Creating new column names based on the number of PCs
new_colnames <- paste0("PC", 1:num_pcs)

# Replace the first num_pcs financial and economic series with PCs
data_rm_na[, selected_columns[1:num_pcs]] <- pca$f

# Rename the replaced columns
colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames

# Removing the rest of the economic and financial series
data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]
y <- y[(2 + nlag):length(y)]

# Create an empty data frame to store the lags
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

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## NOWCAST

nlag = 1
p = 0
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
nowcast_date <- data$date[data$ind_nowcast == 1][1]

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

economic_series <- data_rm_na[, selected_columns]

# Calculate eigenvalues and plot cumulative share of explained variance
#eigvals_pca <- eigen(cor(economic_series, use = "complete.obs"), only.values = T)

#plot(seq(1, length(eigvals_pca$values)), 
#     cumsum(eigvals_pca$values) / sum(eigvals_pca$values),
#     xlab = "eigenvalue", ylab = "share of explained variance",
#     main = "eigenvalue decomposition of economic_series")

# PCA based on EM algorithm following Stock and Watson (2002).
pca <- f_emalg(scale(economic_series), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
# Manually set number of factors to 2!
#pca <- f_emalg(scale(economic_series), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)

#cols <- rep(c("cornflowerblue", 
#              "darkorange1", 
#              "darkorchid3", 
#              "darkolivegreen4", 
#              "brown"), times = 2)
#ltys <- rep(c(1, 2), each = 5)
#matplot(as.Date(data_rm_na$date, format = "%Y-%m-%d"), pca$f, 
#        type = "l", lty = ltys, col = cols, 
#        main = "First 10 principal components of transformed series",
#        ylab = "", xlab = "")
#legend(as.Date("1995-01-01"), 2.0, 
#       legend = paste0("PC", seq(1, 10)), 
#       col = cols,
#       lty= ltys, cex= 1, box.lty = 0, y.intersp=2, bg="transparent", ncol = 5)

num_pcs <- ncol(pca$f)

# Creating new column names based on the number of PCs
new_colnames <- paste0("PC", 1:num_pcs)

# Replace the first num_pcs financial and economic series with PCs
data_rm_na[, selected_columns[1:num_pcs]] <- pca$f

# Rename the replaced columns
colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames

# Removing the rest of the economic and financial series
data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(2 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all series to create the lags
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

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## ONE-STEP-AHEAD

nlag = 2
p = 0
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
one_step_ahead_date <- data$date[data$ind_forecast1Q == 1][1]

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

economic_series <- data_rm_na[, selected_columns]

# Calculate eigenvalues and plot cumulative share of explained variance
#eigvals_pca <- eigen(cor(economic_series, use = "complete.obs"), only.values = T)

#plot(seq(1, length(eigvals_pca$values)), 
#     cumsum(eigvals_pca$values) / sum(eigvals_pca$values),
#     xlab = "eigenvalue", ylab = "share of explained variance",
#     main = "eigenvalue decomposition of economic_series")

# PCA based on EM algorithm following Stock and Watson (2002).
pca <- f_emalg(scale(economic_series), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
# Manually set number of factors to 2!
#pca <- f_emalg(scale(economic_series), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)

#cols <- rep(c("cornflowerblue", 
#              "darkorange1", 
#              "darkorchid3", 
#              "darkolivegreen4", 
#              "brown"), times = 2)
#ltys <- rep(c(1, 2), each = 5)
#matplot(as.Date(data_rm_na$date, format = "%Y-%m-%d"), pca$f, 
#        type = "l", lty = ltys, col = cols, 
#        main = "First 10 principal components of transformed series",
#        ylab = "", xlab = "")
#legend(as.Date("1995-01-01"), 2.0, 
#       legend = paste0("PC", seq(1, 10)), 
#       col = cols,
#       lty= ltys, cex= 1, box.lty = 0, y.intersp=2, bg="transparent", ncol = 5)

num_pcs <- ncol(pca$f)

# Creating new column names based on the number of PCs
new_colnames <- paste0("PC", 1:num_pcs)

# Replace the first num_pcs financial and economic series with PCs
data_rm_na[, selected_columns[1:num_pcs]] <- pca$f

# Rename the replaced columns
colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames

# Removing the rest of the economic and financial series
data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(2 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all series to create the lags
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

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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

## TWO-STEP-AHEAD

nlag = 3
p = 0
mon = 1

# Read in the data from the CSV file into a data frame
data <- read.csv("../hard_data/vint_2010_1_30_MIDAS.csv", header = TRUE, stringsAsFactors = FALSE)
two_step_ahead_date <- data$date[data$ind_forecast2Q == 1][1]

# Get column names starting with "financial" or "economic"
selected_columns <- names(data)[startsWith(names(data), "financial") | startsWith(names(data), "economic")]
data_rm_na <- data[rowSums(is.na(data[selected_columns])) != length(selected_columns), ]
data_rm_na <- data_rm_na[(2+mon):(dim(data_rm_na)[1]-mon-1), 1:dim(data_rm_na)[2]]

economic_series <- data_rm_na[, selected_columns]

# Calculate eigenvalues and plot cumulative share of explained variance
#eigvals_pca <- eigen(cor(economic_series, use = "complete.obs"), only.values = T)

#plot(seq(1, length(eigvals_pca$values)), 
#     cumsum(eigvals_pca$values) / sum(eigvals_pca$values),
#     xlab = "eigenvalue", ylab = "share of explained variance",
#     main = "eigenvalue decomposition of economic_series")

# PCA based on EM algorithm following Stock and Watson (2002).
pca <- f_emalg(scale(economic_series), Nr_max = 10, Niter = 50, ic = "PC_p1",  print_iter = TRUE)
# Manually set number of factors to 2!
#pca <- f_emalg(scale(economic_series), Nr_max = 2, Niter = 50, ic = "none",  print_iter = TRUE)

#cols <- rep(c("cornflowerblue", 
#              "darkorange1", 
#              "darkorchid3", 
#              "darkolivegreen4", 
#              "brown"), times = 2)
#ltys <- rep(c(1, 2), each = 5)
#matplot(as.Date(data_rm_na$date, format = "%Y-%m-%d"), pca$f, 
#        type = "l", lty = ltys, col = cols, 
#        main = "First 10 principal components of transformed series",
#        ylab = "", xlab = "")
#legend(as.Date("1995-01-01"), 2.0, 
#       legend = paste0("PC", seq(1, 10)), 
#       col = cols,
#       lty= ltys, cex= 1, box.lty = 0, y.intersp=2, bg="transparent", ncol = 5)

num_pcs <- ncol(pca$f)

# Creating new column names based on the number of PCs
new_colnames <- paste0("PC", 1:num_pcs)

# Replace the first num_pcs financial and economic series with PCs
data_rm_na[, selected_columns[1:num_pcs]] <- pca$f

# Rename the replaced columns
colnames(data_rm_na)[which(colnames(data_rm_na) %in% selected_columns[1:num_pcs])] <- new_colnames

# Removing the rest of the economic and financial series
data_rm_na <- data_rm_na[, !colnames(data_rm_na) %in% selected_columns[(num_pcs+1):length(selected_columns)], drop = FALSE]

# Extract the GDP growth data series
y <- data$d_gdp

# Remove NA values from the GDP growth series
y <- y[!is.na(y)]

y <- y[(2 + nlag):length(y)]

# Create an empty data frame to store the lags
lags_df <- data.frame()

# Loop through all series to create the lags
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

num_na_rows <- sum(apply(lags_df[1:length(y), 1: ncol(lags_df)], 1, function(row) all(is.na(row))))
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