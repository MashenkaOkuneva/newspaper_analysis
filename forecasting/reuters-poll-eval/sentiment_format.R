# This code reads an Excel file with multiple columns, extracts the sentiment index for the period 
# from Q1 2006 to Q4 2018, and saves it as a CSV file with two columns: 'quarter' and 'sentiment'.
# The resulting CSV file is saved in the folder "sentiment" as 'sentiment_lstm_quarter.csv'.

# Clear workspace
rm(list = ls())

# Required Libraries
library(readxl)
library(dplyr)

# Dates for which Reuters poll is available (Q1 2006 to Q4 2018)
dates <- seq(as.Date("2006-01-01"), as.Date("2018-10-01"), by = "quarters")

# Read in the Excel file with all columns
df_raw <- read_excel("sentiment_lstm_quarter_combined.xlsx", col_names = TRUE)

# Filter the Excel data to keep only the necessary columns and rows for the desired period
df_filtered <- df_raw %>%
  # Create a quarter column using the year and quarter data
  mutate(quarter = as.Date(paste0(year, "-", (quarter - 1) * 3 + 1, "-01"))) %>%
  # Filter for the quarters starting from Q1 2006 to Q4 2018
  filter(quarter >= as.Date("2006-01-01") & quarter <= as.Date("2018-10-01")) %>%
  # Select only the required columns: quarter and sentiment_index
  select(quarter, sentiment = sentiment_index)

# Create the 'sentiment' directory if it doesn't exist
if (!dir.exists("./sentiment")) {
  dir.create("./sentiment")
}

# Save the filtered data frame as a CSV file in the 'sentiment' folder
write.csv(df_filtered, file = "./sentiment/sentiment_lstm_quarter.csv", row.names = FALSE)
