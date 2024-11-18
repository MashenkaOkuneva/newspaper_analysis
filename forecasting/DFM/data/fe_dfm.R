# This R script calculates forecast errors for backcasts, nowcasts, 1-step-ahead, 
# and 2-step-ahead forecasts based on the Dynamic Factor Model (DFM). 

rm(list = ls())

current_dir <- getwd() # Get the current directory

# Load gdp_growth.csv
gdp_growth <- read.csv(file.path(current_dir, "gdp_growth.csv"), stringsAsFactors = FALSE)

# Construct path to "model" directory from the current "data" directory
model_dir <- gsub("data$", "model", current_dir)

# Load CSVs from the "model" directory
backcasts <- read.csv(file.path(model_dir, "backcasts_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)
nowcasts <- read.csv(file.path(model_dir, "nowcasts_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_1step <- read.csv(file.path(model_dir, "forecasts_1step_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)
forecasts_2step <- read.csv(file.path(model_dir, "forecasts_2step_both_2fac_K_30_10_stable.csv"), header = FALSE, stringsAsFactors = FALSE)

# Assigning column names
names(backcasts) <- c("date", "forecast")
names(nowcasts) <- c("date", "forecast")
names(forecasts_1step) <- c("date", "forecast")
names(forecasts_2step) <- c("date", "forecast")

# Merging forecasts with gdp_growth based on date
backcasts <- merge(backcasts, gdp_growth, by = "date", all.x = TRUE)
nowcasts <- merge(nowcasts, gdp_growth, by = "date", all.x = TRUE)
forecasts_1step <- merge(forecasts_1step, gdp_growth, by = "date", all.x = TRUE)
forecasts_2step <- merge(forecasts_2step, gdp_growth, by = "date", all.x = TRUE)

# Calculate forecast errors
backcasts$fe <- backcasts$forecast - backcasts$d_gdp
nowcasts$fe <- nowcasts$forecast - nowcasts$d_gdp
forecasts_1step$fe <- forecasts_1step$forecast - forecasts_1step$d_gdp
forecasts_2step$fe <- forecasts_2step$forecast - forecasts_2step$d_gdp

# Combine all forecast errors into a single dataframe
combined_errors <- data.frame(
  backcasts_fe = backcasts$fe,
  nowcasts_fe = nowcasts$fe,
  forecasts_1step_fe = forecasts_1step$fe,
  forecasts_2step_fe = forecasts_2step$fe
)

# Save the combined errors to a CSV 
write.csv(combined_errors, file.path(current_dir, "forecast_errors_dfm_both_2fac_K_30_10_stable.csv"), row.names = FALSE)