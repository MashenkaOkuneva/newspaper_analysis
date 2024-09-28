# This script calculates forecast errors for the Reuters poll forecasts of GDP growth. 
# Forecast errors are computed for backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts 
# by comparing the median poll forecast values to actual GDP growth. 
# The forecast errors are then combined and saved into a single CSV file for further analysis.

current_dir <- getwd() # Get the current directory

# Load gdp_growth.csv
gdp_growth <- read.csv(file.path("../DFM/data", "gdp_growth.csv"), stringsAsFactors = FALSE)

# Load CSVs with the forecasts
backcasts <- read.csv("backcasts_professional.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2)]
nowcasts <- read.csv( "nowcasts_professional.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2)]
forecasts_1step <- read.csv("forecasts_1step_professional.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2)]
forecasts_2step <- read.csv("forecasts_2step_professional.csv", header = TRUE, stringsAsFactors = FALSE)[,c(1,2)]

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
write.csv(combined_errors, file.path(current_dir, "forecast_errors_professional.csv"), row.names = FALSE)