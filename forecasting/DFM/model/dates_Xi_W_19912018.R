rm(list = ls())
#_____________________________________________________#
#_This script creates a dataframe containing the dates
#_from January 1st 1991 to December 31st 2018 along 
#_with variables indicating the year, quarter, month,
#_week, day and weekday of said dates. Furthermore,
#_it also calculates the indicator variables Xi_q, 
#_Xi_m, Xi_w that indicate the start of a new quarter,
#_month or week. In addition, the weights for current 
#_and previous periods for both monthly and quarterly 
#_flow variables -W_m_c, Wm_p and W_q_c and W_q_p, 
#_respectively, are constructed. 
#_These variables are used to generate
#_artificial data from a daily factor model mixing all
#_of the above frequencies. 
#_____________________________________________________#

# load packages
library(lubridate)
library(dplyr)
library(tidyr)

#_____________________________________________________#
#_set up dataframe with year, quarter, ... vars
#_____________________________________________________#

# date variable from 1.1.1991 to 31.12.2018
df <- data.frame(date = seq(
                            as.Date("1991-01-01"), 
                            as.Date("2018-12-31"), 
                            by = "day"
                            )
                  )

# add year, quarter, month, week, day, weekday vars  
df %>% 
  mutate(year = year(date),
         month = month(date),
         quarter = ceiling(month/3),
         week = isoweek(date),
         day = day(date),
         weekday = wday(date, label = T)) %>%
  select(date, weekday, year, quarter, month, week, day) -> df


#_____________________________________________________#
#_construct vars that indicate start of period
#_____________________________________________________#

# create Xi indicators that equal 0 at start of period and 1 elsewhere
df %>% 
  mutate(Xi_qd = ifelse(month %in% c(seq(1, 10, by = 3)) & day == 1, 0, 1),
         Xi_md = ifelse(day == 1, 0, 1),
         Xi_wd = ifelse(weekday == "Mo", 0, 1)) -> df

#_____________________________________________________#
#_map weekly to monthly observations
#_____________________________________________________#

# determine to which month each weekly observation belongs according to where the majority of its constituent days are in
# this is appropriate for a weekly flow, e.g. weekly retail sales
df %>% 
  select(date, year, month, week) %>% 
  group_by(year, week) %>% 
  mutate(month_of_w = median(month)) -> df_tmp_flow

# if the weekly series is a stock and published on a given day or is known to reflect data up until a certain date
# then selecting that date is more appropriate, e.g. initial claims (published Thursday but reflect developments until the previous Saturday)
df %>% 
  select(date, year, month, week, weekday) %>% 
  filter(weekday == "Sa") %>% 
  group_by(year, month) %>%
  summarise(n_week_per_month = n()) -> df_tmp_stock

#_____________________________________________________#
#_calculate weights for monthly flow variables
#_____________________________________________________#

# days per month and average number of days per month over the entire sample
df %>% 
  select(year, month, day) %>% 
  group_by(year, month) %>% 
  summarise(n_days_m = n()) %>% 
  ungroup() -> df_n_days_m

df_n_days_m$n_days_avg_m = floor(mean(df_n_days_m$n_days_m))

# loop over quarters to construct W_q_c and W_q_p
years <- seq(1991, 2018)
months <- seq(1, 12)
t_prev <- 0
t <- 0
k <- df_n_days_m$n_days_avg_m[1]
df_W_md <- data.frame()
for (y in years)
{
  print(y)
  for (m in months)
  {
    print(paste("m =", m))
    t_prev <- t
    print(paste("tprev =", t_prev))
    k_t <- df_n_days_m[df_n_days_m$year == y & df_n_days_m$month == m, "n_days_m", drop = T]
    print(paste("k_t =",k_t))
    t <- t + k_t
    print(paste("t =", t))
    s <- seq(t_prev + 1, t)
    #W_qd_c = k * (t + 1 - s) / k_t # see Banbura et al. (2011, p. 30, eqn 10)
    W_md_c = (t + 1 - s) / k_t # monthly level  is the average of daily level in the respective month
    if (y == years[length(years)] & m == months[length(quarters)]) # W_q_p = 0 => Not necessary but conceptually clearer!
    {
      
      df_W_md <- rbind(df_W_md, data.frame(year = y,
                                           month = m,
                                           day = seq(1, k_t),
                                           W_md_c = W_md_c,
                                           W_md_p = 0
      )
      )
      
    } else
    {
      df_W_md <- rbind(df_W_md, data.frame(year = y,
                                           month = m,
                                           day = seq(1, k_t),
                                           W_md_c = W_md_c,
                                           W_md_p = c(0, rev(W_md_c[2:length(W_md_c)]))
      )
      )
    }
  }
}

# cbind to df
df <- cbind(df, select(df_W_md, W_md_c, W_md_p))

#_____________________________________________________#
#_calculate weights for quarterly flow variables
#_____________________________________________________#

# days per quarter and average number of days per quarter over the entire sample
df %>% 
  select(year, quarter, day) %>% 
  group_by(year, quarter) %>% 
  summarise(n_days_q = n()) %>% 
  ungroup() -> df_n_days_q

df_n_days_q$n_days_avg_q = floor(mean(df_n_days_q$n_days_q))

# loop over quarters to construct W_q_c and W_q_p
years <- seq(1991, 2018)
quarters <- seq(1, 4)
t_prev <- 0
t <- 0
k <- df_n_days_q$n_days_avg_q[1]
df_W_qd <- data.frame()
for (y in years)
{
  print(y)
  for (q in quarters)
  {
    print(paste("q =", q))
    t_prev <- t
    print(paste("tprev =", t_prev))
    k_t <- df_n_days_q[df_n_days_q$year == y & df_n_days_q$quarter == q, "n_days_q", drop = T]
    print(paste("k_t =",k_t))
    t <- t + k_t
    print(paste("t =", t))
    s <- seq(t_prev + 1, t)
    #W_qd_c = k * (t + 1 - s) / k_t # see Banbura et al. (2011, p. 30, eqn 10)
    W_qd_c = (t + 1 - s) / k_t # quarterly GDP level  is the average of daily GDP in the respective quarter
    if (y == years[length(years)] & q == quarters[length(quarters)]) # W_q_p = 0 => Not necessary but conceptually clearer!
    {

      df_W_qd <- rbind(df_W_qd, data.frame(year = y,
                                         quarter = q,
                                         day = seq(1, k_t),
                                         W_qd_c = W_qd_c,
                                         W_qd_p = 0
                                        )
                    )

    } else
    {
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
df <- cbind(df, select(df_W_qd, W_qd_c, W_qd_p))

#_____________________________________________________#
#_dates for MATLAB plots
#_____________________________________________________#

# index and dates vars containing the year on the first day, else NA 
df$ind_plot <- NA
df$dates_plot <- NA
for (t in seq(1, nrow(df)))
{
  if (df$quarter[t] == 1 && df$month[t] == 1 && df$day[t] == 1)
  {
    df$dates_plot[t] = df$year[t]
    df$ind_plot[t] = t
  }
}

#_____________________________________________________#
#_export df to csv
#_____________________________________________________#

write.csv(df, "dates_Xi_W_19912018.csv", row.names = F, na = "")
