# This script constructs a series of first-release annualized quarterly GDP growth rates 
# using real-time GDP vintages. 
# It identifies the earliest available non-missing value for each quarter and 
# calculates the corresponding growth rates, saving the results as `gdp_growth_actual.csv`.

library(dplyr)
library(tidyr)
library(lubridate)

load("../DFM/data/vintages_gdp.Rda")

df_gdp %>% 
  rownames_to_column(var = "date") %>%
  mutate(year = as.numeric(substr(date, 1, 4)), 
         month = as.numeric(substr(date, 6, 7)) + 3, 
         date_tmp = make_date(year = ifelse(month > 12, year + 1, year), month = ifelse(month > 12, 1, month), day = 1),
         date = date_tmp - days(1)) %>%
  select(-date_tmp, -year, -month) -> df_gdp_fr

# Function to get the first non-NA value after the given date
get_first_value <- function(row) {
  # Extract dates from the column names of df_gdp
  vintage_dates <- as.Date(names(df_gdp), format = "%Y-%m-%d")
  
  # Only consider vintages after the current row date
  relevant_vintages <- vintage_dates[vintage_dates > as.Date(row["date"])]
  
  # If there are no relevant vintages, return NA
  if (length(relevant_vintages) == 0) {
    return(NA)
  }
  
  # Extract the earliest available non-NA value after the given date
  earliest_value <- row[match(as.character(relevant_vintages), names(df_gdp)) + 1] # +1 to adjust for the "date" column
  return(earliest_value[!is.na(earliest_value)][1])

}

# Apply the function to each row of df_gdp_fr
df_gdp_fr$gdp <- apply(df_gdp_fr, 1, get_first_value)

# Helper function to get the end date of the previous quarter
previous_quarter_end <- function(date) {
  date <- as.Date(date)
  month <- as.numeric(format(date, "%m"))
  year <- as.numeric(format(date, "%Y"))
  
  # Determine previous quarter
  if (month <= 3) {
    month <- 12
    day <- 31
    year <- year - 1
  } else if (month <= 6) {
    month <- 3
    day <- 31
  } else if (month <= 9) {
    month <- 6
    day <- 30
  } else {
    month <- 9
    day <- 30
  }
  
  return(as.Date(paste(year, sprintf("%02d", month), sprintf("%02d", day), sep = "-")))
}

# Function to get gpt(t-1) from the same vintage
get_lag <- function(row) {
  # Extract dates from the column names of df_gdp
  vintage_dates <- as.Date(names(df_gdp), format = "%Y-%m-%d")
  
  # For the very first date, return NA for the lag
  if(row["date"] == min(df_gdp_fr$date)) {
    return(NA)
  }
  
  # For dates before "2005-06-30", return the lag from the first vintage
  if(as.Date(row["date"]) < as.Date("2005-06-30")) {
    prev_row_date <- previous_quarter_end(row["date"])
    prev_row <- df_gdp_fr[df_gdp_fr$date == prev_row_date, ]
    return(prev_row[2]) # 2nd column (1st vintage) because 1st column is "date"
  }
  
  prev_row_date <- previous_quarter_end(row["date"])
  prev_row <- df_gdp_fr[df_gdp_fr$date == prev_row_date, ]
  
  # Only consider vintages after the current row date
  relevant_vintages <- vintage_dates[vintage_dates > as.Date(prev_row[["date"]])]
  
  # If there are no relevant vintages, return NA
  if (length(relevant_vintages) == 0) {
    return(NA)
  }
  
  # Extract the second available non-NA value after the given date
  second_value <- prev_row[match(as.character(relevant_vintages), names(df_gdp)) + 1] # +1 to adjust for the "date" column
  return(second_value[!is.na(second_value)][2])
  
}

# Apply the function to each row of df_gdp_fr
df_gdp_fr$gdp_lag <- unlist(apply(df_gdp_fr, 1, get_lag))

# Select only the date and actual_gdp columns
df_gdp_fr <- df_gdp_fr %>% select(date, gdp, gdp_lag)

df_gdp_fr <- df_gdp_fr %>% 
  filter(date < as.Date("2019-01-01"))

# Convert gdp to numeric type
df_gdp_fr$gdp <- as.numeric(as.character(df_gdp_fr$gdp))

# Calculate annualized quarterly growth rate
df_gdp_fr <- df_gdp_fr %>%
  mutate(d_gdp = ifelse(is.na(gdp) | is.na(gdp_lag), NA, 400 * (log(gdp) - log(gdp_lag))))

# Subset and filter the dataframe
df_gdp_fr <- df_gdp_fr %>%
  select(date, d_gdp) %>%                       # Select date and gdp growth
  filter(date >= as.Date("1992-03-31") & 
           date <= as.Date("2018-12-31"))         # Filter by date range

# Save to CSV
write.csv(df_gdp_fr, "gdp_growth_actual.csv", row.names = FALSE)
