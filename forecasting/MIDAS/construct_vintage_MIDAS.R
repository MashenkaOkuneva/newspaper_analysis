rm(list = ls())
#_____________________________________________________#
#_This script processes and prepares a dataset containing annualized GDP growth 
# and monthly topic-based indicators for macroeconomic forecasting.  
# It performs several key tasks: 
# 1. Applying a moving average for smoothing.
# 2. Detrending the series using a biweight filter.
# 3. Reimposing the NA pattern in the dataset.
# 4. Convert transformed topics to monthly frequency.
# 5. Downloading GDP data and calculating the annualized quarterly growth rate.
# 6. If necessary, pre-selecting topics based on their correlation with GDP growth.
# 7. Finally, the script exports the prepared dataset to a CSV file.
# This prepared dataset can then be used for economic forecasting with the MIDAS model.
#_____________________________________________________#

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(bundesbank)
library(tibble)

# FUNCTIONS ----
#_____________________________________________________#
#_f_interpolate (currently not required),
#_ roll_mean 
#_ bw_filter
#_ f_outl
#_____________________________________________________#

f_interpolate <- function(x)
  # replace NA in x with linear interpolation
{
  for (t in seq(1, length(x)))
  {
    if (is.na(x[t]))
    {
      ind_nextobs <- 0
      t_NA = T
      while (t_NA == T)
      {
        
        if (is.na(x[t+ind_nextobs]))
          ind_nextobs <- ind_nextobs + 1
        else
          t_NA = F
        
      }
      x[t] <- 0.5 * (x[t-1] + x[t+ind_nextobs])
    }
  }
  return(x)
}

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

# SET-UP ----
#_____________________________________________________#
#_sample_start,
#_specify vintage, 
#_forecast horizon,
#_number of topics to select
#_window of moving average
#_band width
#_aalpha
#_(these parameters will be function input at a later point)
#_____________________________________________________#

sample_start <- c("1992-01-01")
vintage <- c("2010-01-30") # requires backcast!
date_h <- c("2010-09-30") # 2010Q3
Ntopics <- 6 # select Ntopics topics that have the highest correlation with quarterly GDP growth
K = 30 # window of moving average
bw = 1200 # bandwidth for biweight filter
aalpha <- 10 # number of IQR between median to determine outlier
corr_select <- 0.6 # include topics with an absolute correlation larger than corr_select

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

plot(df_topics$date, df_topics[, 7], 
     type = "l", col = "blue", 
     ylim = c(-0.02, 0.12), 
     main = "Topic T1", sub = "raw and detrended series",
     ylab = "",
     xlab = "")
lines(df_topics_trafo$date, df_topics_trafo[, 7], type = "l", col = "red")

# convert transformed topics to monthly frequency
df_topics_trafo_M <- df_topics_trafo %>%
  group_by(year, month) %>%
  summarise(across(starts_with("T"), ~mean(., na.rm = TRUE))) %>%
  mutate(
    date = as.Date(paste(year, month, "01"), format = "%Y %m %d") + days(days_in_month(month)-1),
    quarter = quarter(date)
  ) %>%
  select(date, everything())  # Reordering columns so 'date' is first


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
         day = day(date)# middle of the quarter, e.g. 15.2. for Q1
  )-> df_gdp

# adjust name of series
names(df_gdp)[2] <- "gdp"

# calculate annualized quarterly growth rate
df_gdp$d_gdp <- c(NA, 400 * diff(log(df_gdp$gdp)))

# select and reorder columns
df_gdp %>% select(date, year, quarter, month, day, d_gdp) -> df_gdp

# convert to "monthly" frequency
dates_tmp <- data.frame(date = df_topics_trafo_M$date)

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
# All topics
#list_topics_select <-  paste0("T", 0:199)
# 20 most correlated and meaningful topics
#list_topics_select <- c("T27", "T127", "T11", "T138", "T52", "T81", "T74", "T100",
#                        "T77", "T88", "T179", "T136", "T58", "T168", "T129", "T20",
#                        "T173", "T175", "T131", "T184")
# 10 stable topics
list_topics_select <- c("T11", "T127", "T27", "T52", "T74", "T81", "T138", "T100",
                        "T131", "T77")

df_topics_trafo_M %>%
  pivot_longer(cols = -c(date, year, month), 
               names_to = "topic", 
               values_to = "vals") %>%
  filter(topic %in% list_topics_select) %>%
  pivot_wider(id_cols = c(date, year, month), 
              names_from = topic, 
              values_from = vals) -> df_topics_trafo_M

# EXPORT TO CSV ----

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
df_data <- merge(df_topics_trafo_M, df_gdp, by = c("date", "year", "month"), all = T)
df_data <- df_data[ , !(names(df_data) %in% c("quarter", "day"))]

# export to csv
write.csv(df_data, file = paste0("vint_", year(vintage), "_", month(vintage), "_", day(vintage), ".csv"),
          row.names = F,
          quote = F,
          na = "NaN")
