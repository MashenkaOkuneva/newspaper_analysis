# This script calculates the correlations between sign-adjusted topics and annualized quarterly GDP growth 
# for each data vintage from 2010Q1 to 2018Q2. It pre-processes the daily sign-adjusted topics using 
# moving averages and biweight filtering before calculating correlations. The script outputs CSV files 
# containing the top 20 topics with the highest absolute correlations for each vintage. All resulting CSV 
# files are saved in the folder "correlations".

rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(bundesbank)
library(tibble)

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
correlations_vintage <- function(vintage, date_h, sample_start = c("1992-01-01"), Ntopics = 20,
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
  #_____________________________________________________#
  
  
  # LOAD TOPICS ----
  #_____________________________________________________#
  #_select sample,
  #_extend to 7-day week,
  #_____________________________________________________#
  df_raw <- read.csv("./topics/sign_adjusted_daily_topics_format.csv")
  
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
  dat <- df_topics[, ind_topics]
  #dat[ind_outl] <- NA
  
  # store pattern of missings
  ind_NA <- is.na(dat)
  colnames(ind_NA) <- names(df_topics)[grepl("T", names(df_topics))]
  
  # moving average
  dat_ma <- apply(dat, c(2), rollmean, k = K)
  
  # detrend with biweight filter
  dat_bw <- apply(dat_ma, c(2), bw_filter, bw = bw)
  dat_detrend <- dat_ma - dat_bw # de-trended topics
  
  # reimpose NA pattern
  dat_detrend_NA <- dat_detrend
  dat_detrend_NA[ind_NA] <- NA
  
  # store in df_topics_trafo
  df_topics_trafo <- df_topics
  df_topics_trafo[ind_topics] <- dat_detrend_NA
  
  # rm first K rows
  df_topics_trafo <- df_topics_trafo[seq(K+1, nrow(df_topics_trafo)), ]
  
  # DOWNLOAD GDP DATA ----
  # load from file
  # load("vintages_gdp.Rda")
  
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
  
  # convert to "daily" frequency
  dates_tmp <- data.frame(date = seq(min(df_topics_trafo$date), 
                                     max(df_topics_trafo$date), 
                                     by = "days")
  )
  
  dates_tmp %>% 
    mutate(year = year(date),
           month = month(date),
           quarter = ceiling(month / 3),
           day = day(date)) %>%
    merge(df_gdp, by = c("date", "year", "quarter", "month", "day"), all.x = T) -> df_gdp
  
  # get rid of dates_tmp
  rm(dates_tmp)
  
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
  top_topics <- sort(abs(cor_topics_gdp), decreasing = TRUE)
  
  # Topics to be excluded
  #exclude_topics <- c("T16", "T196", "T98", "T179", "T138")
  exclude_topics <- c("T7", "T15", "T16", "T36", "T50", "T90", "T94", "T98", "T101", 
                      "T107", "T109", "T122", "T124", "T132", "T134", "T150",
                      "T158", "T162", "T163", "T165", "T177", "T181", "T196")
  
  # Remove the topics from top_topics
  top_topics <- top_topics[!(names(top_topics) %in% exclude_topics)]
  
  top_topics <- top_topics[1:Ntopics]
  
  # Create a dataframe with the desired columns
  corr_df <- data.frame(
    topic = names(top_topics),
    correlation = as.numeric(top_topics)
  )
  
  # Rename columns based on the vintage
  colnames(corr_df) <- c(paste0(vintage, "_topics"), paste0(vintage, "_corr"))
  
  # export to csv
  write.csv(corr_df, file = paste0("./correlations/", "vint_", year(vintage), "_", month(vintage), "_", day(vintage), "_correlations", ".csv"),
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

#for (i in 1:length(vintages)) {
#  v <- vintages[i]
#  dh <- date_hs[i]
#  correlations_vintage(v, dh)
#  
#}

library(parallel)

run_correlations_vintage <- function(i) {
  v <- vintages[i]
  dh <- date_hs[i]
  correlations_vintage(v, dh)
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

clusterEvalQ(cl, load("vintages_gdp.Rda"))
clusterEvalQ(cl, setwd(getwd()))

clusterExport(cl, c("vintages", "date_hs", "correlations_vintage", "f_outl", "rollmean", "bw_filter"))

if(!dir.exists("./correlations")) {
  dir.create("./correlations")
}

# Measure the time taken
timing <- system.time({
  result <- parLapply(cl, 1:length(vintages), run_correlations_vintage)
})

stopCluster(cl)

print(timing)

