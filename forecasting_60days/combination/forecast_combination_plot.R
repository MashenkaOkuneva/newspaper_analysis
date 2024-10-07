# This script combines forecasts from two DFM models: one using only text data 
# and the other using only hard economic data. Forecast combinations are computed for both 
# equal weights and weights optimized using quadratic programming to minimize the Mean Squared Error (MSE).
#
# The combined forecasts for each horizon (nowcasts, 1-step-ahead, and 2-step-ahead) 
# are plotted against actual GDP growth. The resulting plots are saved as PDF files.

rm(list = ls())
current_dir <- getwd() # Get the current directory
library(quadprog)

# Load gdp_growth.csv
gdp_growth <- read.csv(file.path("../DFM/data", "gdp_growth.csv"), stringsAsFactors = FALSE)

# Load CSVs with the forecasts (model 1)
nowcasts_m1 <- read.csv(file.path("../DFM/model","nowcasts_choose_10stable_1fac_K_30.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step_m1 <- read.csv(file.path("../DFM/model", "forecasts_1step_choose_10stable_1fac_K_30.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step_m1 <- read.csv(file.path("../DFM/model", "forecasts_2step_choose_10stable_1fac_K_30.csv"), header = FALSE, stringsAsFactors = FALSE)

# Combine all forecasts into a single dataframe
forecasts_m1 <- data.frame(
  nowcasts_m1 = nowcasts_m1$V2,
  forecasts_1step_m1 = forecasts_1step_m1$V2,
  forecasts_2step_m1 = forecasts_2step_m1$V2
)

# Load CSVs with the forecasts (model 2)
nowcasts_m2 <- read.csv(file.path("../DFM/model", "nowcasts_hard_1fac_no_trafo_1345_li.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step_m2 <- read.csv(file.path("../DFM/model", "forecasts_1step_hard_1fac_no_trafo_1345_li.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step_m2 <- read.csv(file.path("../DFM/model", "forecasts_2step_hard_1fac_no_trafo_1345_li.csv"), header = FALSE, stringsAsFactors = FALSE)

# Combine all forecasts into a single dataframe
forecasts_m2 <- data.frame(
  nowcasts_m2 = nowcasts_m2$V2,
  forecasts_1step_m2 = forecasts_1step_m2$V2,
  forecasts_2step_m2 = forecasts_2step_m2$V2
)

# Load CSV with the forecast errors (model 2)
fe_m2 <- read.csv(file.path("../DFM/data", "forecast_errors_dfm_hard_1fac_no_trafo_1345_li.csv"), header = TRUE, stringsAsFactors = FALSE)

forecasts_dif = forecasts_m1-forecasts_m2

# Initialize vector for optimal weights
w_opt <- numeric(3)

# Calculate the optimal weights (no constraints)
#for (h in 1:3) {
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
w_opt <- c(0.5, 0.5, 0.5)

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

# Convert the date columns to Date class
nowcasts$date <- as.Date(nowcasts$date, format = "%Y-%m-%d")
forecasts_1step$date <- as.Date(forecasts_1step$date, format = "%Y-%m-%d")
forecasts_2step$date <- as.Date(forecasts_2step$date, format = "%Y-%m-%d")

# Define a function to generate a plot
generate_plot <- function(data, title_str, file_name) {
  pdf(file_name, width = 10, height = 7)
  
  plot(data$date, data$d_gdp, type = "l", col = "blue", lwd = 2, 
       xlab = "Date", ylab = "", ylim = range(c(data$d_gdp, data$forecast)),
       main = title_str)
  
  lines(data$date, data$forecast, col = "red", lwd = 2)
  legend("topleft", legend = c("d_gdp", "forecast"),
         col = c("blue", "red"), lty = c(1,1), lwd = 2)
  
  dev.off()
}

# Generate plots for each dataframe
generate_plot(nowcasts, "Nowcasts vs. GDP Growth", "nowcasts_plot_combination_dfm_text_hard_10stable_equal.pdf")
generate_plot(forecasts_1step, "One-step-ahead Forecasts vs. GDP Growth", "forecasts_1step_plot_combination_dfm_text_hard_10stable_equal.pdf")
generate_plot(forecasts_2step, "Two-step-ahead Forecasts vs. GDP Growth", "forecasts_2step_plot_combination_dfm_text_hard_10stable_equal.pdf")