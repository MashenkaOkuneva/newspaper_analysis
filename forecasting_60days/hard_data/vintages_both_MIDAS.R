rm(list = ls())
#_____________________________________________________#
#_This script processes and prepares a dataset containing annualized GDP growth 
# and monthly topic-based/economic series for macroeconomic forecasting.  
# It performs several key tasks:
# 1. Transforming topic-based daily series and converting them to monthly frequency.
# 2. Downloading and transforming economic variables.
# 3. Converting transformed financial daily series to monthly frequency.
# 4. Downloading GDP data and calculating the annualized quarterly growth rate.
# 5. Finally, the script exports the prepared dataset to a CSV file.
# This prepared dataset can then be used for economic forecasting with the MIDAS model.
#
# The current script performs these steps for 34 real-time data vintages in parallel.
#_____________________________________________________#

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(bundesbank)
library(tibble)
library(zoo)
library(purrr)

# FUNCTIONS ----
#_____________________________________________________#
#_ roll_mean 
#_ bw_filter
#_ f_outl
#_____________________________________________________#

rollmean <- function(x, k){
  xroll <- array(NA, c(length(x)))
  for (t in seq(k, length(x)))
    xroll[t] <- mean(x[(t-k+1):t], na.rm = TRUE)
  
  return(xroll)
}

bw_filter <- function(y, bw)
{
  # compute un-normalized weights
  j <- seq(-bw, bw, 1) 
  omeg = (1 - (j/bw) ^ 2) ^ 2  
  
  # check for NA's in y
  n_NA <- sum(is.na(y)) 
  y_noNA <- y[(n_NA + 1) : length(y)]
  Nt <- length(y_noNA)
  
  # loop over t
  tau <- mat.or.vec(length(y_noNA), 1)
  for (t in 1 : length(y_noNA)) {
    # case distinction
    if (t <= bw) {
      
      indY <- c(1 : (2 * bw - (bw - t)))
      indOmeg <- c((bw - t + 2):(2 * bw + 1)) 
      kap <- 1 / ( sum(omeg[indOmeg]))
      tau[ t ] <- kap * omeg[indOmeg] %*% y_noNA[indY] 
      
    } else if (t > (Nt - bw)) {
      
      indY <- c((t - bw) : Nt)
      indOmeg <- c(1 : (bw + 1 + (Nt - t)))
      kap <- 1 / ( sum( omeg[indOmeg] ) )
      tau[t] <- kap * omeg[indOmeg] %*% y_noNA[indY] 
      
    } else {
      
      indY <- seq(t - bw, t + bw, 1)
      indOmeg <- c( 1 : (2 * bw + 1))
      kap <- 1 / (sum(omeg[indOmeg]))
      tau[t] <- kap * omeg[indOmeg] %*% y_noNA[indY]  
    }
  }
  return(c(rep(NA, times = n_NA), tau))
}

f_outl <- function(y, aalpha)
{
  return(abs((y - median(y, na.rm = T))) > aalpha * IQR(y, na.rm = T))
}

# The main function
prepare_vintage <- function(vintage, date_h, sample_start = c("1992-01-01"), Ntopics = 4,
                            K = 30, bw = 1200, aalpha = 10, 
                            corr_select = 0.6) {
  #_____________________________________________________#
  # sample_start: starting point of the data analysis period.
  # vintage: when the forecast is produced.
  # date_h: the date of a 2-step ahead forecast.
  # Ntopics: number of topics to select.
  # K: window of moving average.
  # bw: bandwidth.
  # aalpha: number of IQR from the median to determine outlier.
  # corr_select: include topics with an absolute correlation larger than corr_select.
  
  
  # LOAD TOPICS ----
  #_____________________________________________________#
  #_select sample,
  #_extend to 7-day week,
  #_linearly interpolate to fill-in NA (commented out because not necessary)
  #_____________________________________________________#
  
  df_raw <- read.csv("../DFM/data/topics/sign_adjusted_daily_topics_format.csv")
  
  # add date and quarter variable
  df_raw %>%
    mutate(date = make_date(year = df_raw$year, 
                            month = df_raw$month, 
                            day = df_raw$day),
           quarter = ceiling(month / 3)) %>% 
    filter(date <= vintage) %>%
    select(date, year, quarter, month, day, everything()) -> df_topics 
  
  # get rid of raw df
  rm(df_raw)
  
  # extend series with NA to 7-day week 
  dates_tmp <- data.frame(date = seq(min(df_topics$date), 
                                     max(df_topics$date), 
                                     by = "days")
  )
  
  dates_tmp %>% 
    mutate(year = year(date),
           month = month(date),
           quarter = ceiling(month / 3),
           day = day(date)) %>%
    merge(df_topics, by = c("date", "year", "quarter", "month", "day"), all.x = T) -> df_topics
  
  # get rid of dates_tmp
  rm(dates_tmp)
  
  # col indices corresponding to topics
  ind_topics <- which(grepl("T", names(df_topics)))
  
  # adjust sample (leaving K additional rows at start which will be removed after smoothing)
  df_topics %>% filter(date >= as.Date(sample_start) - days(K)) -> df_topics
  
  # linear interpolate topics to fill-in missings, storing pattern of NA => commented out because moving average should take care of missings
  # ind_NA <- c()
  # for (n in ind_topics)
  # {
  #   x <- df_topics[, n]
  #   ind_NA <- cbind(ind_NA, is.na(x))
  #   x <- f_interpolate(x)
  #   df_topics[, n] <- x
  # }
  # 
  # colnames(ind_NA) <- names(df_topics)[grepl("T", names(df_topics))]
  
  # TRANSFORM TOPICS ---- 
  #_____________________________________________________#
  #_rm outlier,
  #_moving average,
  #_detrend using biweight filter
  #_reimpose NA pattern
  #_adjust sample
  #_____________________________________________________#
  
  # remove outlier
  #ind_outl <- apply(df_topics[, ind_topics], c(2), f_outl, aalpha = aalpha)
  #dat <- df_topics[, ind_topics]
  #dat[ind_outl] <- NA
  
  # store pattern of missings
  #ind_NA <- is.na(dat)
  #colnames(ind_NA) <- names(df_topics)[grepl("T", names(df_topics))]
  
  # moving average
  #dat_ma <- apply(dat, c(2), rollmean, k = K)
  dat_ma <- apply(df_topics[, ind_topics], c(2), rollmean, k = K)
  
  # detrend with biweight filter
  dat_bw <- apply(dat_ma, c(2), bw_filter, bw = bw)
  dat_detrend <- dat_ma - dat_bw # de-trended topics
  
  # reimpose NA pattern
  #dat_detrend_NA <- dat_detrend
  #dat_detrend_NA[ind_NA] <- NA
  
  # store in df_topics_trafo
  df_topics_trafo <- df_topics
  #df_topics_trafo[ind_topics] <- dat_detrend_NA
  df_topics_trafo[ind_topics] <- dat_detrend
  
  # rm first K rows
  df_topics_trafo <- df_topics_trafo[seq(K+1, nrow(df_topics_trafo)), ]
  
  # convert transformed topics to monthly frequency
  df_topics_trafo_M <- df_topics_trafo %>%
    group_by(year, month) %>%
    summarise(across(starts_with("T"), ~mean(., na.rm = TRUE))) %>%
    mutate(
      date = ceiling_date(as.Date(paste(year, month, "01"), format = "%Y %m %d"), unit="month") - days(1),
      quarter = quarter(date),
      day = day(date)
    ) %>%
    select(date, year, quarter, month, day, everything())  # Reordering columns so 'date' is first
  
  # LOAD DAILY FINANCIAL VARIABLES ----
  #_____________________________________________________#
  #_select sample,
  #_extend to 7-day week,
  #_____________________________________________________#
  
  # The DAX performance index (ticker: GDAXI) was downloaded from Yahoo Finance
  # (https://finance.yahoo.com/quote/%5EGDAXI/)
  # on 06.10.2023 and subsequently saved as a CSV file.
  financial1 <- read.csv("^GDAXI.csv")
  
  df_financial <- financial1 %>%
    # 1. Rename 'Date' to 'date'
    rename(date = Date) %>%
    # Ensure 'date' is in Date format
    mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
    # 2. Take only 'Close' and rename to 'financial_1', replacing null with NA
    select(date, financial_1 = Close) %>%
    mutate(financial_1 = ifelse(financial_1 == "null", NA, financial_1)) %>%
    # 3. Construct 'year', 'month', 'day', and 'quarter' columns
    mutate(
      year = year(date),
      month = month(date),
      day = day(date),
      quarter = quarter(date)
    ) %>%
    # 5. Filter data to be <= vintage by converting to the same format
    filter(format(date, "%Y-%m-%d") <= vintage) %>%
    # 4. Arrange columns
    select(date, year, quarter, month, day, financial_1)
  
  # get rid of raw df
  rm(financial1)
  
  # extend series with NA to 7-day week 
  dates_tmp <- data.frame(date = seq(min(df_financial$date), 
                                     as.Date(vintage), 
                                     by = "days")
  )
  
  dates_tmp %>% 
    mutate(year = year(date),
           month = month(date),
           quarter = ceiling(month / 3),
           day = day(date)) %>%
    merge(df_financial, by = c("date", "year", "quarter", "month", "day"), all.x = T) -> df_financial
  
  # get rid of dates_tmp
  rm(dates_tmp)
  
  # The other four daily financial series are downloaded from the Bundesbank:
  # Bundesbank API
  #url_base <- "https://api.statistiken.bundesbank.de/rest/download/"
  #url_params <- "?format=csv&lang=en"
  
  #download_series <- function(code, i) {
  #  series_code_category <- substr(code, 1, 5)
  #  series_code_series <- substr(code, 7, nchar(code))
  #  
  #  url <- paste0(url_base, series_code_category, "/", series_code_series, url_params)
  #  dat <- read.csv(url, skip = 8, stringsAsFactors = FALSE)
  #  
  #  # Additional data cleaning
  #  
  #  # 1. Rename columns and drop the third column
  #  colnames(dat) <- c("date", paste0("financial_", i))
  #  dat <- dat %>% select(-3)
  #  
  #  # 2. Replace "." with NA
  #  column_name <- paste0("financial_", i)
  #  dat <- dat %>% 
  #    mutate(!!sym(column_name) := ifelse(!!sym(column_name) == ".", NA, !!sym(column_name)))
  #  
  #  # 3. Remove rows where 'date' equals 'last update'
  #  dat <- dat %>% filter(date != "last update")
  #  
  #  # Rename the dataframe for saving with a specific name
  #  assign(paste0("financial", i), dat, envir = .GlobalEnv)
  #  
  #  # Save the data frame to a .Rda file
  #  save(list = paste0("financial", i), file = paste0("financial_", i, ".Rda"))
  #}
  
  #series_codes <- c("BBSSY.D.REN.EUR.A620.000000WT0505.A", # Federal notes yield (5 year)
  #                  "BBSSY.D.REN.EUR.A630.000000WT1010.A",  # Government bond yields (10-year)
  #                  "BBEE1.D.I9.AAA.XZE012.A.AABAN.M00", # Nominal effective exchange rate, narrow 
  #                  "BBEE1.D.I9.AAA.XZE022.A.AABAN.M00" # Nominal effective exchange rate, broad 
  #                  
  #)
  
  # Applying the function for all series codes
  #for (i in seq_along(series_codes)) {
  #  download_series(series_codes[i], i+1)
  #}
  
  # Specify the range of i
  i_values <- 2:5
  
  # Loop to load each Rda file
  for (i in i_values) {
    load(paste0("financial_", i, ".Rda"))
  }
  
  # Ensure all date columns are Date objects
  financial2$date <- as.Date(financial2$date, format = "%Y-%m-%d")
  financial3$date <- as.Date(financial3$date, format = "%Y-%m-%d")
  financial4$date <- as.Date(financial4$date, format = "%Y-%m-%d")
  financial5$date <- as.Date(financial5$date, format = "%Y-%m-%d")
  
  # Merging
  df_financial <- df_financial %>%
    left_join(financial2, by = "date") %>%
    left_join(financial3, by = "date") %>%
    left_join(financial4, by = "date") %>%
    left_join(financial5, by = "date")
  
  # TRANSFORM FINANCIAL VARIABLES ---- 
  #_____________________________________________________#
  #_apply appropriate transformation (diff or log, diff)
  #_rm outlier,
  #_moving average,
  #_detrend using biweight filter
  #_reimpose NA pattern
  #_adjust sample
  #_____________________________________________________#
  
  # Ensure financial columns are numeric
  df_financial <- df_financial %>%
    mutate(
      across(starts_with("financial_"), ~suppressWarnings(as.numeric(.)))
    )
  
  log_diff <- function(x) {
    x_log = log(x)
    x_log_filled = zoo::na.locf(x_log, na.rm = FALSE)
    100 * (x_log_filled - lag(x_log_filled))
  }
  
  simple_diff <- function(x) {
    x_filled = zoo::na.locf(x, na.rm = FALSE)
    x_filled - lag(x_filled)
  }
  
  df_financial <- df_financial %>%
    mutate(
      # Log diff transformation while preserving NAs
      across(c(financial_1, financial_4, financial_5), 
             ~ ifelse(is.na(.), NA, log_diff(.)), .names = "{col}"),
      # Simple diff transformation while preserving NAs
      across(c(financial_2, financial_3), 
             ~ ifelse(is.na(.), NA, simple_diff(.)), .names = "{col}")
    )
  
  # if no further transformations
  df_financial %>% filter(date >= as.Date(sample_start)) -> df_financial
  df_financial_trafo <- df_financial
  
  # adjust sample (leaving K additional rows at start which will be removed after smoothing)
  #df_financial %>% filter(date >= as.Date(sample_start) - days(K)) -> df_financial
  
  # col indices corresponding to financial series
  #ind_financial <- which(grepl("financial_", names(df_financial)))
  
  # remove outlier
  #ind_outl <- apply(df_financial[, ind_financial], c(2), f_outl, aalpha = aalpha)
  #dat <- df_financial[, ind_financial]
  #dat[ind_outl] <- NA
  
  # store pattern of missings
  #ind_NA <- is.na(dat)
  #colnames(ind_NA) <- names(df_financial)[grepl("financial_", names(df_financial))]
  
  # moving average
  #dat_ma <- apply(dat, c(2), rollmean, k = K)
  #dat_ma <- apply(df_financial[, ind_financial], c(2), rollmean, k = K)
  
  # detrend with biweight filter
  #dat_bw <- apply(dat_ma, c(2), bw_filter, bw = bw)
  #dat_detrend <- dat_ma - dat_bw # de-trended daily series
  
  # reimpose NA pattern
  #dat_detrend_NA <- dat_detrend
  #dat_detrend_NA[ind_NA] <- NA
  
  # store in df_financial_trafo
  #df_financial_trafo <- df_financial
  #df_financial_trafo[ind_financial] <- dat_detrend_NA
  #df_financial_trafo[ind_financial] <- dat_detrend
  
  # rm first K rows
  #df_financial_trafo <- df_financial_trafo[seq(K+1, nrow(df_financial_trafo)), ]
  
  # convert transformed daily series to monthly frequency
  #df_financial_trafo_M <- df_financial_trafo %>%
  #  group_by(year, month) %>%
  #  summarise(across(starts_with("financial"), ~mean(., na.rm = TRUE))) %>%
  #  mutate(
  #    date = ceiling_date(as.Date(paste(year, month, "01"), format = "%Y %m %d"), unit="month") - days(1),
  #    quarter = quarter(date),
  #    day = day(date)
  #  ) %>%
  #  select(date, everything())  # Reordering columns so 'date' is first
  
  # convert transformed daily series to monthly frequency
  df_financial_trafo_M <- df_financial_trafo %>%
    group_by(year, month) %>%
    summarise(
      # Cumulative sum for DAX
      financial_1 = ifelse(all(is.na(financial_1)), NA, sum(financial_1, na.rm = TRUE)),
      
      # Average for yields
      financial_2 = ifelse(all(is.na(financial_2)), NA, mean(financial_2, na.rm = TRUE)),
      financial_3 = ifelse(all(is.na(financial_3)), NA, mean(financial_3, na.rm = TRUE)),
      
      # Cumulative sum for nominal effective exchange rate indices
      financial_4 = ifelse(all(is.na(financial_4)), NA, sum(financial_4, na.rm = TRUE)),
      financial_5 = ifelse(all(is.na(financial_5)), NA, sum(financial_5, na.rm = TRUE))
    ) %>%
    mutate(
      date = ceiling_date(as.Date(paste(year, month, "01"), format = "%Y %m %d"), unit = "month") - days(1),
      quarter = quarter(date),
      day = day(date)
    ) %>%
    select(date, everything())
  
  # Identify the financial columns with any NA values
  cols_to_remove <- names(df_financial_trafo_M)[
    grepl("financial_", names(df_financial_trafo_M)) & 
      sapply(df_financial_trafo_M, function(col) any(is.na(col)))
  ]
  
  # Remove the identified columns
  df_financial_trafo_M <- df_financial_trafo_M %>%
    select(-all_of(cols_to_remove))
  
  # LOAD AND TRANSFORM MONTHLY ECONOMIC VARIABLES ----
  #_____________________________________________________#
  #_select sample,
  #_extend to 7-day week,
  #_apply appropriate transformation (log, diff)
  #_____________________________________________________#
  
  series_codes <- c("BBKRT.M.DE.Y.I.IP1.AA031.C.I", # Production in main construction industry
                    "BBKRT.M.DE.Y.I.IP1.ACM01.C.I", # Industrial production index
                    "BBKRT.M.DE.Y.I.IO1.AA031.C.I", # New orders for main construction industry
                    "BBKRT.M.DE.Y.I.IO1.ACM01.C.I", # New orders for industry
                    "BBKRT.M.DE.Y.I.IT1.AA031.V.A", # Main construction industry turnover
                    "BBKRT.M.DE.Y.I.IT1.ACM01.V.I", # Industry turnover
                    "BBKRT.M.DE.Y.P.PC1.PC100.R.I", # Consumer price index
                    "BBKRT.M.DE.S.P.PC1.PC110.R.I", # Consumer price index, excluding energy
                    "BBKRT.M.DE.S.P.PP1.PP100.R.I", # Producer price index
                    "BBKRT.M.DE.S.P.PP1.PP200.R.I", # Producer price index, excluding energy
                    "BBKRT.M.DE.Y.L.BE2.AA022.H.I", # Hours worked: manufacturing
                    "BBKRT.M.DE.Y.L.BE2.AA031.H.A"  # Hours worked: construction
  )
  
  # Loop through each series code and download data
  #for (i in seq_along(series_codes)) {
  #  code <- series_codes[i]
  #  
  #  # Get the series
  #  economic_data <- getSeries(code)
  #  
  #  # Assign this data to a new variable with a dynamic name
  #  assign(paste0("economic_", i), economic_data, envir = .GlobalEnv)
  #  
  #  # Save to an .Rda file
  #  save(list = paste0("economic_", i), file = paste0("economic_", i, ".Rda"))
  #}
  
  # Loop to load each .Rda file
  for (i in seq_along(series_codes)) {
    # Load the .Rda file into the environment
    load(file = paste0("economic_", i, ".Rda"))
  }
  
  for (i in seq_along(series_codes)) {
    
    # Construct variable name and get the dataframe
    df_name <- paste0("economic_", i)
    economic_i <- get(df_name)
    
    # select vintage
    dates_vintages <- as.Date(names(economic_i))
    ind_vintage <- sum(dates_vintages <= vintage)
    economic_i_series <- economic_i[, ind_vintage, drop = F]
    
    # convert dates to column
    economic_i_series %>% 
      select(tail(names(.), 1)) %>%
      rownames_to_column(var = "date") %>%
      mutate(year = as.numeric(substr(date, 1, 4)), 
             month = as.numeric(substr(date, 6, 7)),  
             quarter = ceiling(month/3),
             date_tmp = make_date(year = ifelse(month+1>12,year+1, year), month = ifelse(month+1>12,1,month+1), day = 1),
             date = date_tmp - days(1),
             day = day(date)
      )-> economic_i_series
    
    # adjust name of series
    names(economic_i_series)[2] <- "economic_i"
    
    # calculate monthly growth rate
    economic_i_series$economic_i <- c(NA, 100 * diff(log(economic_i_series$economic_i)))
    
    # select and reorder columns
    economic_i_series %>% select(date, year, quarter, month, day, economic_i) -> economic_i_series
    
    dates_tmp <- data.frame(date = df_financial_trafo_M$date)
    
    # Extract year, quarter, month, and day from the date column
    dates_tmp$year <- year(dates_tmp$date)
    dates_tmp$quarter <- quarter(dates_tmp$date)
    dates_tmp$month <- month(dates_tmp$date)
    dates_tmp$day <- day(dates_tmp$date)
    
    # Left join the economic_i_series with dates_tmp to get a monthly dataframe
    economic_i_series <- dates_tmp %>%
      left_join(economic_i_series, by = c("date", "year", "quarter", "month", "day"))
    
    # get rid of dates_tmp
    rm(dates_tmp)
    
    economic_i_series <- economic_i_series %>% 
      rename(!!paste0("economic_", i) := economic_i)
    
    # Assign transformed data to a new dynamically named dataframe
    assign(paste0(df_name, "_series"), economic_i_series, envir = .GlobalEnv)
  }
  
  # Merge dataframes
  
  # Start with df_financial_trafo_M
  economic_data <- df_financial_trafo_M
  
  # Loop to merge each economic_i_series with economic_data
  for (i in seq_along(series_codes)) {
    
    # Construct the name of the data frame and retrieve it
    series_name <- paste0("economic_", i, "_series")
    economic_i_series <- get(series_name, envir = .GlobalEnv)
    
    # Merge
    economic_data <- merge(economic_data, economic_i_series, 
                           by = c("date", "year", "quarter", "month", "day"), 
                           all.x = TRUE)
    
    # Remove economic_i_series from the environment to save memory
    rm(list = series_name, envir = .GlobalEnv)
  }
  
  # Remove economic_i dataframes from the environment
  for (i in seq_along(series_codes)) {
    rm(list = paste0("economic_", i), envir = .GlobalEnv)
  }
  
  # List of objects to remove
  #objects_to_remove <- c(
  #  "dat", "dat_bw", "dat_detrend", "dat_detrend_NA", 
  #  "dat_ma", "df_financial", "df_financial_trafo", "economic_i", 
  #  "economic_i_series", "financial2", "financial3", "financial4", 
  #  "financial5", "ind_NA", "ind_outl"
  #)
  
  # List of objects to remove
  objects_to_remove <- c(
    "dat_bw", "dat_detrend",
    "dat_ma", "df_financial", "df_financial_trafo", "economic_i", 
    "economic_i_series", "financial2", "financial3", "financial4", 
    "financial5"
  )
  
  # Remove objects
  rm(list = objects_to_remove, envir = .GlobalEnv)
  
  # Function to check if NA values are only in the last two positions or not
  na_check <- function(x) {
    !any(is.na(x[-c((length(x)-1), length(x))]))
  }
  
  # Getting column names that start with "economic"
  economic_colnames <- grep("^economic", names(economic_data), value = TRUE)
  
  # Checking each column with the NA condition
  valid_columns <- map_lgl(economic_data[economic_colnames], na_check)
  
  # Getting names of valid columns
  valid_economic_cols <- economic_colnames[valid_columns]
  
  # Keeping all columns that do not start with "economic_"
  non_economic_cols <- names(economic_data)[!names(economic_data) %in% economic_colnames]
  
  # Selecting non-economic columns and valid economic columns
  economic_data <- economic_data %>%
    select(all_of(c(non_economic_cols, valid_economic_cols)))
  
  # DOWNLOAD GDP DATA ----
  
  # load from file
  #load("../DFM/data/vintages_gdp.Rda")
  
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
  
  # select and reorder columns
  df_gdp %>% select(date, year, quarter, month, day, d_gdp) -> df_gdp
  
  # convert to "monthly" frequency
  dates_tmp <- data.frame(date = df_financial_trafo_M$date)
  
  # Extract year, quarter, month, and day from the date column
  dates_tmp$year <- year(dates_tmp$date)
  dates_tmp$quarter <- quarter(dates_tmp$date)
  dates_tmp$month <- month(dates_tmp$date)
  dates_tmp$day <- day(dates_tmp$date)
  
  # Left join the df_gdp with dates_tmp to get a monthly dataframe
  df_gdp <- dates_tmp %>%
    left_join(df_gdp, by = c("date", "year", "quarter", "month", "day"))
  
  # get rid of dates_tmp
  rm(dates_tmp)
  
  # APPEND ROWS FOR FORECASTS ----
  
  # Define the start and end dates
  start_date <- ceiling_date(max(df_gdp$date), 'month') + months(1) 
  end_date <- ceiling_date(as.Date(date_h), 'month')
  
  # Generate a sequence of dates for each month
  dates_tmp <- seq.Date(start_date, end_date, by = "month") - days(1)
  
  # Extract year, quarter, month, and day from the dates
  dates_tmp <- data.frame(date = dates_tmp)
  dates_tmp$year <- year(dates_tmp$date)
  dates_tmp$quarter <- quarter(dates_tmp$date)
  dates_tmp$month <- month(dates_tmp$date)
  dates_tmp$day <- day(dates_tmp$date)
  
  # Add NAs for d_gdp column
  dates_tmp$d_gdp <- NA
  
  # Combine the two dataframes
  df_gdp <- rbind(df_gdp, dates_tmp)
  
  # PRE-SELECT TOPICS ----
  
  # convert transformed topics to quarterly frequency
  df_topics_trafo %>% 
    pivot_longer(cols = -c(date, year, quarter, month, day), 
                 names_to = "topic", 
                 values_to = "vals") %>%
    group_by(topic, year, quarter) %>%
    summarise(avg_vals = mean(vals, na.rm = T)) %>%
    pivot_wider(id_cols = c(year, quarter), 
                names_from = topic, 
                values_from = avg_vals) -> df_topics_trafo_Q
  
  # merge with quarterly GDP
  df_gdp %>%
    filter(!is.na(d_gdp)) %>% 
    select(year, quarter, d_gdp) %>%
    merge(df_topics_trafo_Q, by = c("year", "quarter")) -> df_corr
  
  # calculate correlation between GDP and topics
  cor_topics_gdp <- cor(as.matrix(df_corr[, -c(1:2)]))
  cor_topics_gdp <- cor_topics_gdp[1, 2:ncol(cor_topics_gdp)] # first row contains correlations of topics with GDP growth
  
  # select those with a correlation of over corr_select in absolute terms
  #list_topics_select <- names(cor_topics_gdp)[abs(cor_topics_gdp) > corr_select]
  
  # sort by absolute correlation and select the top Ntopics
  #top_topics <- sort(abs(cor_topics_gdp), decreasing = TRUE)[1:Ntopics]
  
  # sort by absolute correlation
  #top_topics <- sort(abs(cor_topics_gdp), decreasing = TRUE)
  
  # Topics to be excluded
  #exclude_topics <- c("T16", "T196", "T98", "T179", "T138")
  
  # Remove the topics from top_topics
  #top_topics <- top_topics[!(names(top_topics) %in% exclude_topics)]
  
  #top_topics <- top_topics[1:Ntopics]
  
  # Create a dataframe with the desired columns
  #corr_df <- data.frame(
  #  topic = names(top_topics),
  #  correlation = as.numeric(top_topics)
  #)
  
  # Rename columns based on the vintage
  #colnames(corr_df) <- c(paste0(vintage, "_topics"), paste0(vintage, "_corr"))
  
  # Write the dataframe to a CSV
  #write.csv(corr_df, file = "correlations_20.csv", row.names = FALSE)
  
  # get the names of the top Ntopics
  #list_topics_select <- names(top_topics)
  #list_topics_select <- c("T11", "T127", "T27", "T77", "T52", "T179", "T74")
  #list_topics_select <- c("T11", "T127", "T27", "T77", "T52", "T74")
  #list_topics_select <- c("T11", "T127", "T27", "T77", "T52", "T74", "T88", "T119",
  #                        "T133", "T168", "T58", "T63", "T61", "T154", "T163", "T169",
  #                        "T140", "T4", "T8", "T93")
  #list_topics_select <- c("T11", "T127", "T27", "T77", "T52", "T74", "T88", "T119",
  #                                                "T133", "T168", "T58")
  #list_topics_select <- c("T11", "T127", "T27", "T77")
  #list_topics_select <-  paste0("T", 0:199)
  # 20 most correlated and meaningful topics
  #list_topics_select <- c("T27", "T127", "T11", "T138", "T52", "T81", "T74", "T100",
  #                        "T77", "T88", "T179", "T136", "T58", "T168", "T129", "T20",
  #                        "T173", "T175", "T131", "T184")
  # 10 stable topics
  list_topics_select <- c("T11", "T127", "T27", "T52", "T74", "T81", "T138", "T100",
                          "T131", "T77")
  
  df_topics_trafo_M %>%
    pivot_longer(cols = -c(date, year, quarter, month, day), 
                 names_to = "topic", 
                 values_to = "vals") %>%
    filter(topic %in% list_topics_select) %>%
    pivot_wider(id_cols = c(date, year, quarter, month, day), 
                names_from = topic, 
                values_from = vals) -> df_topics_trafo_M
  
  # EXPORT TO CSV ----
  
  # indices for now- and forecasts
  
  df_gdp %>% 
    group_by(year, quarter) %>%
    filter(date == max(date),
           is.na(d_gdp)) %>%
    ungroup() %>%
    select(date) -> dates_fore
  
  diff_q <- floor(as.numeric(dates_fore[, 1, drop = T] - as.Date(vintage)) / 90)
  
  df_ind <- data.frame()
  counter_fore <- 1
  for (i in seq(1, nrow(dates_fore)))
  {
    if (diff_q[i] == -1)
    {
      name <- "ind_backcast"
    } else if (diff_q[i] == 0)
    {
      name <- "ind_nowcast"
    } else {
      name <- paste0("ind_forecast", counter_fore, "Q")
      counter_fore <- counter_fore + 1
    }
    vals <- mat.or.vec(nrow(df_gdp), 1)
    vals[df_gdp$date == dates_fore[i, , drop = T]] <- 1
    
    df_ind <- rbind(df_ind, data.frame(name = name, vals = vals, date = df_gdp$date))
  }
  
  pivot_wider(df_ind, names_from = name, values_from = vals) -> df_ind_wide
  
  df_gdp <- merge(df_gdp, df_ind_wide, by = "date")
  
  # merge data sets
  df_data <- merge(df_topics_trafo_M, economic_data, by = c("date", "year", "quarter","month","day"), all = T)
  df_data <- merge(df_data, df_gdp, by = c("date", "year", "quarter","month","day"), all = T)
  df_data <- df_data[ , !(names(df_data) %in% c("quarter", "day"))]
  
  # export to csv
  write.csv(df_data, file = paste0("./vintages_both_MIDAS/", "vint_", year(vintage), "_", month(vintage), "_", day(vintage), ".csv"),
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

#start_date <- as.Date("1997-01-01")
#end_date <- as.Date("2005-01-01")

# Repeat the start date 4 times
#start_sequence <- rep(start_date, 4)

# Create a sequence from 1998 to 2004 and repeat each date 4 times
#mid_sequence <- rep(seq(as.Date("1998-01-01"), by = "year", length.out = 2005-1998), each = 4)

# Repeat the end date 2 times
#end_sequence <- rep(end_date, 2)

# Combine all sequences together
#sample_starts <- c(start_sequence, mid_sequence, end_sequence)

#for (i in 1:length(vintages)) {
#  v <- vintages[i]
#  dh <- date_hs[i]
#  prepare_vintage(v, dh)
#  
#}

library(parallel)

run_prepare_vintage <- function(i) {
  v <- vintages[i]
  dh <- date_hs[i]
  #ss <- sample_starts[i]
  #prepare_vintage(v, dh, ss)
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
  library(zoo)
  library(purrr)
  
})

clusterEvalQ(cl, load("../DFM/data/vintages_gdp.Rda"))
clusterEvalQ(cl, setwd(getwd()))

#clusterExport(cl, c("vintages", "date_hs", "sample_starts", "prepare_vintage", "f_outl", "rollmean", "bw_filter"))
clusterExport(cl, c("vintages", "date_hs", "prepare_vintage", "f_outl", "rollmean", "bw_filter"))

if(!dir.exists("./vintages_both_MIDAS")) {
  dir.create("./vintages_both_MIDAS")
}

# Measure the time taken
timing <- system.time({
  result <- parLapply(cl, 1:length(vintages), run_prepare_vintage)
})

stopCluster(cl)

print(timing)

