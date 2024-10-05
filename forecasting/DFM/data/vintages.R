rm(list = ls())
#_____________________________________________________#
#_This script processes and prepares a dataset containing annualized GDP growth 
# and daily topic-based indicators for macroeconomic forecasting.  
# It performs several key tasks: 
# 1. Outlier removal from the topic-based indicators.
# 2. Applying a moving average for smoothing.
# 3. Detrending the series using a biweight filter.
# 4. Reimposing the NA pattern in the dataset.
# 5. Downloading GDP data and calculating the annualized quarterly growth rate.
# 6. Preparing for the forecast by appending rows and creating the Xi_q indicator.
# 7. Calculating weights for flow variable.
# 8. Pre-selecting topics based on their correlation with GDP growth.
# 9. Finally, the script generates indicators for backcasting, nowcasting, 
# and forecasting, merges all dataframes, adds flags indicating the estimation sample, 
# and exports the prepared dataset to a CSV file.
# This prepared dataset can then be used for economic forecasting with the dynamic factor model.
#
# The current script performs these steps for 34 real-time data vintages in parallel.
#_____________________________________________________#

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(bundesbank)
library(tibble)
#library(zoo)

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
# sample_start = c("1992-01-01")
prepare_vintage <- function(vintage, date_h, sample_start = c("1992-01-01"), Ntopics = 20,
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
  
  #simple_diff <- function(x) {
  #  x_filled = zoo::na.locf(x, na.rm = FALSE)
  #  x_filled - lag(x_filled)
  #}
  
  #df_topics <- df_topics %>%
  #  mutate(
  #    across(starts_with("T"), ~ ifelse(is.na(.), NA, simple_diff(.)), .names = "{col}")
  #  )
  
  # if no further transformations
  #df_topics %>% filter(date >= as.Date(sample_start)) -> df_topics
  #df_topics_trafo <- df_topics
  
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
  
  # APPEND ROWS FOR FORECASTS ----
  
  dates_tmp <- data.frame(date = seq(max(df_gdp$date), 
                                     as.Date(date_h), 
                                     by = "days")
  )
  
  dates_tmp %>% 
    mutate(year = year(date),
           month = month(date),
           quarter = ceiling(month / 3),
           day = day(date)) %>%
    merge(df_gdp, by = c("date", "year", "quarter", "month", "day"), all = T) -> df_gdp
  
  # CREATE Xi_q INDICATOR ----
  
  # create Xi_q indicators that equals 0 at start of period and 1 elsewhere
  df_gdp %>% 
    mutate(Xi_qd = ifelse(month %in% c(seq(1, 10, by = 3)) & day == 1, 0, 1)) -> df_gdp
  
  df_gdp$Xi_qd[1] <- 0 # first obs is also "start" of quarter
  
  # WEIGHTS FOR FLOW VARIABLE ----
  
  # days per quarter and average number of days per quarter over the entire sample
  df_gdp %>% 
    select(year, quarter, day) %>% 
    filter(!(year == year(sample_start)-1 & quarter == 1)) %>%
    group_by(year, quarter) %>% 
    summarise(n_days_q = n()) %>% 
    ungroup() -> df_n_days_q
  
  df_n_days_q$n_days_avg_q = floor(mean(df_n_days_q$n_days_q))
  
  # loop over quarters to construct W_q_c and W_q_p
  years <- seq(year(df_gdp$date[1]), year(df_gdp$date[nrow(df_gdp)])) 
  quarters <- seq(1, 4) 
  t_prev <- 0
  t <- 0
  k <- df_n_days_q$n_days_avg_q[1]
  df_W_qd <- data.frame()
  for (y in years)
  {
    for (q in quarters)
    {
      t_prev <- t
      k_t <- df_n_days_q[df_n_days_q$year == y & df_n_days_q$quarter == q, "n_days_q", drop = T]
      if (length(k_t) == 0) # quarter not in sample!
        next
      else
      {
        t <- t + k_t
        s <- seq(t_prev + 1, t)
        #W_qd_c = k * (t + 1 - s) / k_t # see Banbura et al. (2011, p. 30, eqn 10)
        W_qd_c = (t + 1 - s) / k_t # quarterly GDP level  is the average of daily GDP in the respective quarter
        df_W_qd <- rbind(df_W_qd, data.frame(year = y,
                                             quarter = q,
                                             day = seq(1, k_t),
                                             W_qd_c = W_qd_c,
                                             W_qd_p = c(0, rev(W_qd_c[2:length(W_qd_c)]))
        )
        )
        
      }
    }
  }
  
  # cbind to df
  df_gdp <- cbind(df_gdp, select(df_W_qd, W_qd_c, W_qd_p))
  
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
  #exclude_topics <- c("T7", "T15", "T16", "T36", "T50", "T90", "T94", "T98", "T101", 
  #                    "T107", "T109", "T122", "T124", "T132", "T134", "T150",
  #                    "T158", "T162", "T163", "T165", "T177", "T181", "T196")
  
  # Remove the topics from top_topics
  #top_topics <- top_topics[!(names(top_topics) %in% exclude_topics)]
  
  #top_topics <- top_topics[1:Ntopics]
  
  # get the names of the top Ntopics
  #list_topics_select <- names(top_topics)
  #list_topics_select <- c("T11", "T127", "T27", "T77")
  # LASSO
  #list_topics_select <- c("T0", "T11", "T16", "T27", "T36", "T41", "T42", "T98", "T125",
  #                        "T127", "T129", "T196", "T198")
  #list_topics_select <- c("T11", "T127", "T27", "T77", "T52", "T74", "T88", "T119",
  #                        "T133", "T168", "T58", "T63", "T61", "T154", "T163", "T169",
  #                       "T140", "T4", "T8", "T93")
  # 20 most correlated and meaningful topics
  #list_topics_select <- c("T27", "T127", "T11", "T138", "T52", "T81", "T74", "T100",
  #                        "T77", "T88", "T179", "T136", "T58", "T168", "T129", "T20",
  #                        "T173", "T175", "T131", "T184")
  # All topics
  #list_topics_select <-  paste0("T", 0:199)
  # 10 most correlated and meaningful topics
  #list_topics_select <- c("T27", "T127", "T11", "T138", "T52", "T81", "T74", "T100",
  #                        "T77", "T88")
  # 10 most correlated and meaningful topics, K=30, first vintage correlation
  #list_topics_select <- c("T27", "T127", "T11", "T179", "T81", "T77", "T119",
  #                        "T74", "T52", "T133")
  #file_path <- paste0("./correlations/vint_", year(vintage), "_", month(vintage), "_", day(vintage), "_correlations.csv")
  #correlations <- read.csv(file_path)
  # Extract the topics from the first column
  #list_topics_select <- as.character(correlations[[1]])
  # 8 most correlated and meaningful topics
  #list_topics_select <- c("T27", "T127", "T11", "T52", "T81", "T74", "T100",
  #                        "T77")
  # 7 most correlated and meaningful topics
  #list_topics_select <- c("T27", "T127", "T11", "T52", "T81", "T74",
  #                        "T77")
  # 10 stable topics
  list_topics_select <- c("T11", "T127", "T27", "T52", "T74", "T81", "T138", "T100",
                          "T131", "T77")
  df_topics_trafo %>%
    pivot_longer(cols = -c(date, year, quarter, month, day), 
                 names_to = "topic", 
                 values_to = "vals") %>%
    filter(topic %in% list_topics_select) %>%
    pivot_wider(id_cols = c(date, year, quarter, month, day), 
                names_from = topic, 
                values_from = vals) -> df_topics_trafo
  
  # EXPORT TO CSV ----
  #_____________________________________________________#
  #_indices for back-, now- and forecasts
  #_merge datasets
  #_add flag indicating estimation sample 1:Nt
  #_change var names
  #_____________________________________________________#
  
  # indices for back-, now- and forecasts
  
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
  df_data <- merge(df_topics_trafo, df_gdp, by = c("date", "year", "quarter", "month", "day"), all = T)
  
  # change var names
  ind_q <- grepl("d_gdp", names(df_data))
  Nq <- sum(ind_q)
  names(df_data)[ind_q] <- paste0("y_q_", seq(1, Nq))
  
  ind_d <- grepl("T.", names(df_data))
  Nd <- sum(ind_d)
  names(df_data)[ind_d] <- paste0("y_d_", seq(1, Nd))
  
  # add flag for estimation sample
  df_data$ind_sample <- ifelse(df_data$date <= vintage, 1, 0)
  
  # export to csv
  write.csv(df_data, file = paste0("./vintages/", "vint_", year(vintage), "_", month(vintage), "_", day(vintage), ".csv"),
            row.names = F,
            quote = F,
            na = "NaN")
}

# Generate vintages
vintages <- seq(as.Date("2010-01-30"), as.Date("2018-04-30"), by = "quarter")
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
  #library(zoo)
  
})

clusterEvalQ(cl, load("vintages_gdp.Rda"))
clusterEvalQ(cl, setwd(getwd()))

#clusterExport(cl, c("vintages", "date_hs", "sample_starts", "prepare_vintage", "f_outl", "rollmean", "bw_filter"))
clusterExport(cl, c("vintages", "date_hs", "prepare_vintage", "f_outl", "rollmean", "bw_filter"))

if(!dir.exists("./vintages")) {
  dir.create("./vintages")
}

# Measure the time taken
timing <- system.time({
  result <- parLapply(cl, 1:length(vintages), run_prepare_vintage)
})

stopCluster(cl)

print(timing)

