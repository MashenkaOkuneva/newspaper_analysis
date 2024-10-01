# This script creates a single vintage of real-time GDP data for Germany,
# representing the information available to a forecaster 30 days after the quarter started.
# The vintage is specified by the "vintage" parameter, and the script outputs a CSV file
# containing the corresponding annualized GDP growth rate for the sample period.

rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)

# SET-UP ----
#_____________________________________________________#
#_sample_start,
#_specify vintage, 
#_forecast horizon
#_____________________________________________________#

sample_start <- c("1992-01-01")
vintage <- c("2010-01-30") # requires backcast!
date_h <- c("2010-09-30") # 2010Q3

# DOWNLOAD GDP DATA ----

# load from file
load("../DFM/data/vintages_gdp.Rda")

# select vintage
dates_vintages <- as.Date(names(df_gdp))
ind_vintage <- sum(dates_vintages <= vintage)
df_gdp <- df_gdp[, ind_vintage, drop = F]

# convert dates to column
df_gdp %>% 
  select(tail(names(.), 1)) %>%
  rownames_to_column(var = "date") %>%
  mutate(year = as.numeric(substr(date, 1, 4)), 
         month = as.numeric(substr(date, 6, 7))+2,  
         quarter = ceiling(month/3),
         date_tmp = make_date(year = ifelse(month+1>12,year+1, year), month = ifelse(month+1>12,1,month+1), day = 1),
         date = date_tmp - days(1),
         day = day(date) # middle of the quarter, e.g. 15.2. for Q1
  )-> df_gdp

# adjust name of series
names(df_gdp)[2] <- "gdp"

# calculate annualized quarterly growth rate
df_gdp$d_gdp <- c(NA, 400 * diff(log(df_gdp$gdp)))

# filter rows in df_gdp to only include dates within the range of sample_start and date_h
df_gdp <- df_gdp %>%
  filter(date >= as.Date(sample_start) & date <= as.Date(date_h))

# retain only the 'date' and 'd_gdp' columns, removing all other columns from df_gdp
df_gdp <- df_gdp %>%
  select(date, d_gdp)

# EXPORT TO CSV ----
write.csv(df_gdp, file = paste0("vint_", year(vintage), "_", month(vintage), "_", day(vintage), ".csv"),
          row.names = F,
          quote = F,
          na = "NaN")
