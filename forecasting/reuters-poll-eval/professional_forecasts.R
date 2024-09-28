# This script creates CSV files containing the last day of each forecasted quarter, 
# the annualized median forecast of GDP growth from the Reuters poll, and the date when the forecast was made.
# Separate CSV files are created for backcasts, nowcasts, 1-step-ahead, and 2-step-ahead forecasts, 
# focusing on the evaluation period from 2010 to 2018.

library(lubridate)
library(forcats)
library(ggplot2)
library(bundesbank)
library(ggsci)

# clear workspace
rm(list = ls())

# directories
dir_forecasts <- "/poll data/"

# evaluation period
yys <- 2006:2018 # years
qqs <- 1:4 # quarters

# max and min horizons
max_h <- 3 # 3Q-ahead forecast (relative to the quarter in which the forecast is made)
min_h <- -1 # backcast

get_forecasts <- function(yy, qq, dir_in, df_in, max_h, min_h)
{
  # define helper function
  dropNA_first <- function(df)
    # function that removes NA observations and drops first element
    # output is a vector
  {
    tmp <- df[!is.na(df)] # rem NA
    out <- tmp[-1] # drop first element
  }
  
  
  # read in csv
  dat <- read.csv(paste0(getwd(), dir_in, yy, "Q", qq, ".csv"))
  # get row indices 
  ind_median <- grep("median", dat[, 1], ignore.case = TRUE)
  ind_min <- grep("min", dat[, 1], ignore.case = TRUE)
  ind_max <- grep("max", dat[, 1], ignore.case = TRUE)
  ind_date <- grep("consensus", dat[, 1], ignore.case = TRUE)
  
  # extract values
  dates_fore_tmp <- dmy(dropNA_first(dat[ind_date, ]))
  medf_tmp <- as.numeric(dropNA_first(dat[ind_median, ]))
  minf_tmp <- as.numeric(dropNA_first(dat[ind_min, ]))
  maxf_tmp <- as.numeric(dropNA_first(dat[ind_max, ]))
  quarter_date <- make_date(year = yy, month = 3 * qq - 2, day = 1) 
  
  # calculate forecast horizon (in quarters)
  horizon_tmp <- round(as.numeric((quarter_date - dates_fore_tmp) / 90), digits = 0)
  
  # insert NA where no values are present between max_h and min_h
  seq_h <- seq(max(c(max(horizon_tmp), max_h)), min(c(min(horizon_tmp), min_h)))
  ind_h <- seq_h %in% horizon_tmp
  tmp_NA <- rep(NA, length(seq_h))
  medf <- tmp_NA; medf[which(ind_h)] <- medf_tmp
  minf <- tmp_NA; minf[which(ind_h)] <- minf_tmp
  maxf <- tmp_NA; maxf[which(ind_h)] <- maxf_tmp
  dates_fore <- tmp_NA; dates_fore[which(ind_h)] <- dates_fore_tmp
  dates_fore <- as_date(dates_fore)
  horizon <- seq_h
  
  # store in df
  df <- data.frame(dates_fore = as_date(dates_fore), 
                   med = medf,
                   min = minf,
                   max = maxf,
                   quarter = quarter_date,
                   horizon = horizon
  )
  
  df_out <- rbind(df_in, df)
  
  return(df_out)
}

df <- data.frame() # initialize df
for (yy in yys)
{
  for (qq in qqs)
  {
    df <- get_forecasts(yy, qq, dir_forecasts, df, max_h, min_h)
  }
}

df <- df[df$horizon <= 2, ]
# Get the last date of each quarter
df$quarter_end <- ceiling_date(df$quarter, unit = "quarter") - days(1)

# Subset the dataframe to select backcasts only
backcasts <- df[df$horizon == -1 & 
                  df$quarter_end >= as.Date("2009-12-31") & 
                  df$quarter_end <= as.Date("2018-03-31"), 
                c("quarter_end", "med", "dates_fore")]
# Annuaized quarterly growth rate
backcasts$med <- backcasts$med * 4
write.csv(backcasts, "backcasts_professional.csv", row.names=FALSE)


# Subset the dataframe to select nowcasts only
nowcasts <- df[df$horizon == 0 & 
                  df$quarter_end >= as.Date("2010-03-31") & 
                  df$quarter_end <= as.Date("2018-06-30"), 
                c("quarter_end", "med", "dates_fore")]
# Annuaized quarterly growth rate
nowcasts$med <- nowcasts$med * 4
write.csv(nowcasts, "nowcasts_professional.csv", row.names=FALSE)

# Subset the dataframe to select 1-step-ahead forecasts only
forecasts_1step <- df[df$horizon == 1 & 
                 df$quarter_end >= as.Date("2010-06-30") & 
                 df$quarter_end <= as.Date("2018-09-30"), 
               c("quarter_end", "med", "dates_fore")]
# Annuaized quarterly growth rate
forecasts_1step$med <- forecasts_1step$med * 4
write.csv(forecasts_1step, "forecasts_1step_professional.csv", row.names=FALSE)

# Subset the dataframe to select 2-step-ahead forecasts only
forecasts_2step <- df[df$horizon == 2 & 
                        df$quarter_end >= as.Date("2010-09-30") & 
                        df$quarter_end <= as.Date("2018-12-31"), 
                      c("quarter_end", "med", "dates_fore")]
# Annuaized quarterly growth rate
forecasts_2step$med <- forecasts_2step$med * 4
write.csv(forecasts_2step, "forecasts_2step_professional.csv", row.names=FALSE)
