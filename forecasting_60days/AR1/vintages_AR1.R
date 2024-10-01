# This script generates 34 vintages of real-time GDP data for Germany, 
# covering the period from 2010Q1 to 2018Q2. For each vintage, it creates 
# a CSV file containing the annualized quarterly GDP growth rate available 
# to a forecaster 60 days after the quarter started.

rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(tibble)

# The main function
prepare_vintage <- function(vintage, date_h, sample_start = c("1992-01-01")) {
  #_____________________________________________________#
  # sample_start: starting point of the data analysis period.
  # vintage: when the forecast is produced.
  # date_h: the date of a 2-step ahead forecast.
  #_____________________________________________________#
  
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
           day = day(date)# middle of the quarter, e.g. 15.2. for Q1
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
  write.csv(df_gdp, file = paste0("./vintages_AR1/", "vint_", year(vintage), "_", month(vintage), "_", day(vintage), ".csv"),
            row.names = F,
            quote = F,
            na = "NaN")
}

# Generate vintages
vintages <- seq(as.Date("2010-02-28"), as.Date("2018-05-28"), by = "quarter")
date_hs <- seq(as.Date("2010-09-30"), as.Date("2018-12-31"), by = "quarter")

# Adjusting to get the last day of each quarter in 'date_hs'
date_hs <- lapply(date_hs, function(date) {
  if (month(date) %in% c(6, 9)) {
    return(as.Date(paste0(year(date), "-", month(date), "-30")))
  } else if (month(date) %in% c(3, 12)) {
    return(as.Date(paste0(year(date), "-", month(date), "-31")))
  }
})

# Convert list to Date vector
date_hs <- do.call(c, date_hs)

library(parallel)

run_prepare_vintage <- function(i) {
  v <- vintages[i]
  dh <- date_hs[i]
  prepare_vintage(v, dh)
}

num_cores <- detectCores() - 4
cl <- makeCluster(num_cores)

# Load necessary libraries in each cluster node
clusterEvalQ(cl, {
  library(dplyr)
  library(lubridate)
  library(tidyr)
  library(bundesbank)
  library(tibble)
  
})

clusterEvalQ(cl, load("../DFM/data/vintages_gdp.Rda"))
clusterEvalQ(cl, setwd(getwd()))

clusterExport(cl, c("vintages", "date_hs", "prepare_vintage"))

if(!dir.exists("./vintages_AR1/")) {
  dir.create("./vintages_AR1/")
}

# Measure the time taken
timing <- system.time({
  result <- parLapply(cl, 1:length(vintages), run_prepare_vintage)
})

stopCluster(cl)

print(timing)
