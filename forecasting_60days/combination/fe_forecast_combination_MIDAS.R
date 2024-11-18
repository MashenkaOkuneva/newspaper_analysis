# This script combines forecasts from two different MIDAS models: one using only text data 
# and the other using only hard economic data. The combination is performed using optimal 
# weights that minimize the Mean Squared Error (MSE) of the forecast combination, constrained 
# to lie between 0 and 1.
# Two scenarios are considered: (1) equal weights for both models, and (2) weights optimized 
# for each forecasting horizon (nowcasts, 1-step-ahead, and 2-step-ahead forecasts).
#
# The script calculates the combined forecasts, merges them with actual GDP growth data, 
# and computes forecast errors for all horizons. The resulting forecast errors are saved 
# as a CSV file.

rm(list = ls())
current_dir <- getwd() # Get the current directory
library(quadprog)

# Load gdp_growth.csv
gdp_growth <- read.csv(file.path("../DFM/data", "gdp_growth.csv"), stringsAsFactors = FALSE)

# Load CSVs with the forecasts (model 1)
# Best-performing model
#nowcasts_m1 <- read.csv(file.path("../MIDAS","nowcasts_midas_lasso_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)
#forecasts_1step_m1 <- read.csv(file.path("../MIDAS", "forecasts_1step_midas_lasso_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)
#forecasts_2step_m1 <- read.csv(file.path("../MIDAS", "forecasts_2step_midas_lasso_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)

# Ridge (K=3)
nowcasts_m1 <- read.csv(file.path("../MIDAS","nowcasts_midas_ridge_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step_m1 <- read.csv(file.path("../MIDAS", "forecasts_1step_midas_ridge_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step_m1 <- read.csv(file.path("../MIDAS", "forecasts_2step_midas_ridge_10stable_2lags.csv"), header = FALSE, stringsAsFactors = FALSE)

# Combine all forecasts into a single dataframe
forecasts_m1 <- data.frame(
  nowcasts_m1 = nowcasts_m1$V2,
  forecasts_1step_m1 = forecasts_1step_m1$V2,
  forecasts_2step_m1 = forecasts_2step_m1$V2
)

# Load CSVs with the forecasts (model 2)
# Best-performing model
#nowcasts_m2 <- read.csv(file.path("../MIDAS", "nowcasts_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)
#forecasts_1step_m2 <- read.csv(file.path("../MIDAS", "forecasts_1step_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)
#forecasts_2step_m2 <- read.csv(file.path("../MIDAS", "forecasts_2step_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)

# Ridge (K=3)
nowcasts_m2 <- read.csv(file.path("../MIDAS", "nowcasts_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step_m2 <- read.csv(file.path("../MIDAS", "forecasts_1step_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step_m2 <- read.csv(file.path("../MIDAS", "forecasts_2step_midas_ridge_2lags_hard.csv"), header = FALSE, stringsAsFactors = FALSE)

# Combine all forecasts into a single dataframe
forecasts_m2 <- data.frame(
  nowcasts_m2 = nowcasts_m2$V2,
  forecasts_1step_m2 = forecasts_1step_m2$V2,
  forecasts_2step_m2 = forecasts_2step_m2$V2
)

# Load CSV with the forecast errors (model 2)
# Best-performing model
#fe_m2 <- read.csv(file.path("../MIDAS", "forecast_errors_midas_ridge_2lags_hard.csv"), header = TRUE, stringsAsFactors = FALSE)

# Ridge (K=3)
fe_m2 <- read.csv(file.path("../MIDAS", "forecast_errors_midas_ridge_2lags_hard.csv"), header = TRUE, stringsAsFactors = FALSE)

forecasts_dif = forecasts_m1-forecasts_m2

# Initialize vector for optimal weights
w_opt <- numeric(3)

# Calculate the optimal weights
#for (h in 1:4) {
#  Xreg <- forecasts_dif[, h] # Difference of forecast values (model 1 - model 2)
#  Yreg <- fe_m2[, h]
#  w_opt[h] <- solve(t(Xreg) %*% Xreg) %*% t(Xreg) %*% Yreg
#}

# Calculate the optimal weights using quadratic programming
for (h in 1:3) {
  Xreg <- forecasts_dif[, h] # Difference of forecast values (model 1 - model 2)
  Yreg <- fe_m2[, h]
  
  # Setting up the quadratic programming problem
  Dmat <- t(Xreg) %*% Xreg
  dvec <- -t(Xreg) %*% Yreg
  # Constraint matrices for the single weight (0 <= w <= 1)
  Amat <- matrix(c(1, -1), ncol = 1)
  bvec <- c(0, -1) # Upper and lower bounds for the weight
  
  # Solve the quadratic programming problem
  sol <- solve.QP(Dmat, dvec, t(Amat), bvec)
  
  # Extract the weight
  w_opt[h] <- sol$solution
}

# Initialize matrix for averaged forecasts
forecast_combination <- matrix(nrow = nrow(forecasts_dif), ncol = 3)

# Use the absolute values of the weights
#w_opt_abs <- abs(w_opt)

# Equal weights
#w_opt <- c(0.5, 0.5, 0.5)

# Compute the averaged forecasts for each horizon
for (h in 1:3) {
  forecast_combination[, h] <- w_opt[h] * forecasts_m1[, h] + (1 - w_opt[h]) * forecasts_m2[, h]
}

nowcasts <- data.frame(nowcasts_m1$V1, forecast_combination[, 1])
forecasts_1step <- data.frame(forecasts_1step_m1$V1, forecast_combination[, 2])
forecasts_2step <- data.frame(forecasts_2step_m1$V1, forecast_combination[, 3])

# Assigning column names
names(nowcasts) <- c("date", "forecast")
names(forecasts_1step) <- c("date", "forecast")
names(forecasts_2step) <- c("date", "forecast")

# Merging forecasts with gdp_growth based on date
nowcasts <- merge(nowcasts, gdp_growth, by = "date", all.x = TRUE)
forecasts_1step <- merge(forecasts_1step, gdp_growth, by = "date", all.x = TRUE)
forecasts_2step <- merge(forecasts_2step, gdp_growth, by = "date", all.x = TRUE)

# Calculate forecast errors
nowcasts$fe <- nowcasts$forecast - nowcasts$d_gdp
forecasts_1step$fe <- forecasts_1step$forecast - forecasts_1step$d_gdp
forecasts_2step$fe <- forecasts_2step$forecast - forecasts_2step$d_gdp

# Combine all forecast errors into a single dataframe
combined_errors <- data.frame(
  nowcasts_fe = nowcasts$fe,
  forecasts_1step_fe = forecasts_1step$fe,
  forecasts_2step_fe = forecasts_2step$fe
)

# Save the combined errors to a CSV
# Best-performing model
#write.csv(combined_errors, file.path(current_dir, "forecast_errors_combination_midas_text_hard_10stable.csv"), row.names = FALSE)

# Ridge (K=3)
write.csv(combined_errors, file.path(current_dir, "forecast_errors_combination_midas_text_hard_10stable_ridge_3.csv"), row.names = FALSE)