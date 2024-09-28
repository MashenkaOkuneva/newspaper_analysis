# This script pre-processes (smooths and de-trends) sign-adjusted daily topics, 
# aggregates them to quarterly frequency by taking the mean, 
# and saves selected topics as separate CSV files for the period 2006Q1-2018Q4.

rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(bundesbank)
library(tibble)
library(readr)

# Moving average function
rollmean <- function(x, k){
  xroll <- array(NA, c(length(x)))
  for (t in seq(k, length(x)))
    xroll[t] <- mean(x[(t-k+1):t], na.rm = TRUE)
  
  return(xroll)
}

# Biweight filter function
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

# Function to detect outliers
f_outl <- function(y, aalpha)
{
  return(abs((y - median(y, na.rm = T))) > aalpha * IQR(y, na.rm = T))
}

# Read in the raw data
df_raw <- read.csv("./sign_adjusted_daily_topics_format.csv")

# Add date and quarter variables
df_raw %>%
  mutate(date = make_date(year = df_raw$year, 
                          month = df_raw$month, 
                          day = df_raw$day),
         quarter = ceiling(month / 3)) %>% 
  select(date, year, quarter, month, day, everything()) -> df_topics 

# Get rid of raw df
rm(df_raw)

# Extend series with NA to 7-day week 
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

# Get rid of dates_tmp
rm(dates_tmp)

# Get column indices corresponding to topics
ind_topics <- which(grepl("T", names(df_topics)))
dat <- df_topics[, ind_topics]

# Store pattern of missings
ind_NA <- is.na(dat)
colnames(ind_NA) <- names(df_topics)[grepl("T", names(df_topics))]

# Apply moving average smoothing
K <- 30
dat_ma <- apply(dat, c(2), rollmean, k = K)

# Detrend using the biweight filter
bw <- 1200
dat_bw <- apply(dat_ma, c(2), bw_filter, bw = bw)
dat_detrend <- dat_ma - dat_bw # de-trended topics

# Reimpose NA pattern
dat_detrend_NA <- dat_detrend
dat_detrend_NA[ind_NA] <- NA

# Store in df_topics_trafo
df_topics_trafo <- df_topics
df_topics_trafo[ind_topics] <- dat_detrend_NA

# Remove initial K rows to account for moving average lag
df_topics_trafo <- df_topics_trafo[seq(K+1, nrow(df_topics_trafo)), ]

# Convert transformed topics to quarterly frequency
df_topics_trafo %>% 
  pivot_longer(cols = -c(date, year, quarter, month, day), 
               names_to = "topic", 
               values_to = "vals") %>%
  group_by(topic, year, quarter) %>%
  summarise(avg_vals = mean(vals, na.rm = T)) %>%
  pivot_wider(id_cols = c(year, quarter), 
              names_from = topic, 
              values_from = avg_vals) -> df_topics_trafo_Q

# Define the selected topics
selected_topics <- c("T11", "T27", "T52", "T127", "T81", "T77", "T74", "T131", "T138", "T100")

# Filter the data for the years 2006 to 2018 and the selected topics
df_filtered <- df_topics_trafo_Q %>%
  filter(year >= 2006 & year <= 2018) %>%
  select(year, quarter, all_of(selected_topics))

# Loop through each selected topic
for (topic in selected_topics) {
  
  # Create a new data frame for each topic
  df_topic <- df_filtered %>%
    ungroup() %>%
    mutate(quarter = case_when(
      quarter == 1 ~ paste0(year, "-01-01"),
      quarter == 2 ~ paste0(year, "-04-01"),
      quarter == 3 ~ paste0(year, "-07-01"),
      quarter == 4 ~ paste0(year, "-10-01")
    )) %>%
    select(quarter, sentiment = all_of(topic))
  
  # Ensure the column is named correctly
  colnames(df_topic)[2] <- "sentiment"
  
  # Create a file name for each topic
  file_name <- paste0("./sentiment/", topic, "_quarter.csv")
  
  # Save the data frame to a CSV file
  write.csv(df_topic, file = file_name, row.names = FALSE)
}