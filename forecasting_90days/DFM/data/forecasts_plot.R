# This script creates plots of DFM forecasts for all forecasting horizons 
# (nowcasts, 1-step-ahead, and 2-step-ahead forecasts).
# The plots compare DFM forecasts against the actual GDP growth.

rm(list = ls())
current_dir <- getwd() # Get the current directory

# Load gdp_growth.csv
gdp_growth <- read.csv(file.path(current_dir, "gdp_growth.csv"), stringsAsFactors = FALSE)

# Construct path to "model" directory from the current "data" directory
model_dir <- gsub("data$", "model", current_dir)

# Load CSVs from the "model" directory
nowcasts <- read.csv(file.path(model_dir, "nowcasts_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step <- read.csv(file.path(model_dir, "forecasts_1step_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step <- read.csv(file.path(model_dir, "forecasts_2step_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)

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
generate_plot(nowcasts, "Nowcasts vs. GDP Growth", "nowcasts_plot_both_2fac_K_30_10_stable.pdf")
generate_plot(forecasts_1step, "One-step-ahead Forecasts vs. GDP Growth", "forecasts_1step_plot_both_2fac_K_30_10_stable.pdf")
generate_plot(forecasts_2step, "Two-step-ahead Forecasts vs. GDP Growth", "forecasts_2step_plot_both_2fac_K_30_10_stable.pdf")