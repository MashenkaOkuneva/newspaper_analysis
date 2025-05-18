rm(list = ls())

# PACKAGES ----
library(lubridate)
library(dplyr)
library(tidyr)
library(readxl)
library(purrr)
library(knitr)
library(kableExtra)
library(stringr)
library(zoo)
library(broom)
library(sandwich)

# FUNCTIONS ----
#_____________________________________________________#
#_ roll_mean 
#_ bw_filter
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

# FUNCTION TO TRANSFORM DAILY TOPICS INTO QUARTERLY & MONTHLY ----
transform_to_freq <- function(file, sample_start = c("1992-01-01"), K = 30, bw = 1200) {

  # LOAD TOPICS ----
  #_____________________________________________________#
  #_select sample,
  #_extend to 7-day week
  #_____________________________________________________#
  
  df_raw <- read.csv(file) %>%
    select(-any_of("X"))
  
  # add date and quarter variable
  df_raw %>%
    mutate(date = make_date(year = df_raw$year, 
                            month = df_raw$month, 
                            day = df_raw$day),
           quarter = ceiling(month / 3)) %>% 
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
  #_moving average,
  #_detrend using biweight filter
  #_reimpose NA pattern
  #_adjust sample
  #_____________________________________________________#
  
  # select only topics
  dat <- df_topics[, ind_topics]
  
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
  
  # Create a "YYYY-MM" date column in df_topics_trafo_Q to match the GDP file
  df_topics_trafo_Q <- df_topics_trafo_Q %>%
    mutate(date = paste(year, 
                        ifelse(quarter == 1, "03",
                               ifelse(quarter == 2, "06",
                                      ifelse(quarter == 3, "09", "12"))),
                        sep = "-"))
  
  # convert transformed topics to monthly frequency
  df_topics_trafo %>% 
    pivot_longer(cols = -c(date, year, quarter, month, day), 
                 names_to = "topic", 
                 values_to = "vals") %>%
    group_by(topic, year, quarter, month) %>%
    summarise(avg_vals = mean(vals, na.rm = T)) %>%
    pivot_wider(id_cols = c(year, month), 
                names_from = topic, 
                values_from = avg_vals) -> df_topics_trafo_M
  
  # Create a date column in df_topics_trafo_M that matches the surveys date format
  # 04/1991, 05/1991 etc.
  df_topics_trafo_M <- df_topics_trafo_M %>%
    mutate(date = paste(sprintf("%02d", month), year, sep = "/"))
  
  list(quarterly = df_topics_trafo_Q, monthly = df_topics_trafo_M)
}

# sign-adjusted topics
topics_sign <- transform_to_freq(
  file = "./topics/sign_adjusted_daily_topics_format.csv"
)
# drop every obs whose year is 2008 or 2009
topics_sign_q_nc <- topics_sign$quarterly %>% filter(! year %in% c(2008, 2009))

# original topics
topics_orig <- transform_to_freq(
  file          = "./topics/daily_topics.csv"
)
# drop every obs whose year is 2008 or 2009
topics_orig_q_nc <- topics_orig$quarterly %>% filter(! year %in% c(2008, 2009))

# BPW-adjusted topics
topics_bpw <- transform_to_freq(
  file = "./topics/BPW_adjusted_daily_topics.csv"
)
# drop every obs whose year is 2008 or 2009
topics_bpw_q_nc  <- topics_bpw$quarterly  %>% filter(! year %in% c(2008, 2009))

# original topics estimated on all articles
topics_all <- transform_to_freq(
  file = "./topics/daily_topics_all_articles.csv"
)
# drop every obs whose year is 2008 or 2009
topics_all_q_nc <- topics_all$quarterly %>% filter(! year %in% c(2008, 2009))

# CORRELATION ANALYSIS ----
# Compute correlations of every topic with quarterly GDP growth (return the top 20 by absolute correlation),
# or subset to only a given set of topics
calc_topic_corr <- function(file, econ_var = "d_gdp", topics_df, selected_topics = NULL) {
  econ <- read.csv(file, stringsAsFactors = FALSE)
  
  # Bring "YYYY-MM-DD" -> "YYYY-MM" for merging
  econ <- econ %>%
    mutate(date = substr(date, 1, 7))
  
  # Merge topics and GDP series on date
  merged <- topics_df %>% inner_join(econ, by = "date")
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic
  corr_df <- lapply(topic_cols, function(topic) {
    corr_val <- cor(merged[[topic]], merged[[econ_var]], use = "complete.obs")
    data.frame(topic = topic, corr = corr_val)
  }) %>% bind_rows()
  
  if (!is.null(selected_topics)) {
    # Only return those topics
    corr_df <- filter(corr_df, topic %in% selected_topics) %>%
      # reorder rows to match "selected_topics"
      slice(match(selected_topics, topic))
  } else {
    # Or sort descending by absolute correlation and take the top 20 topics
    corr_df <- arrange(corr_df, desc(abs(corr))) %>% slice(1:20)
  }
  
  # Rename the correlation column
  corr_df <- corr_df %>% rename(GDP = corr)
  
  return(corr_df)
}

# 1. Top 20 topics by absolute correlation with GDP  
gdp_corr_top20 <- calc_topic_corr("../../AR1/gdp_growth_actual.csv", topics_df = topics_sign$quarterly, econ_var = "d_gdp")

# 2. Only a selected set of topics
selected_topics <- c("T27", "T127", "T11", "T81", "T77", 
         "T74", "T52", "T131", "T138", "T100")

gdp_corr_selected <- calc_topic_corr("../../AR1/gdp_growth_actual.csv", topics_df = topics_sign$quarterly, econ_var = "d_gdp", selected_topics = selected_topics)

# 3. Top 20 topics estimated on all articles by absolute correlation with GDP
gdp_corr_all <- calc_topic_corr("../../AR1/gdp_growth_actual.csv", topics_df = topics_all$quarterly, econ_var = "d_gdp")

# Quarterly correlations with GDP growth + statistical significance, for a selected set of topics
calc_topic_corr_gdp_sig <- function(file, econ_var, topics_df, selected_topics, nw_lag = 4) {
  # Bring "YYYY-MM-DD" -> "YYYY-MM" for merging
  gdp <- read.csv(file, stringsAsFactors = FALSE) %>%
    mutate(date = substr(date, 1, 7))
  
  topics_df <- topics_df %>% ungroup()
  
  # Merge topics and GDP series on date
  merged <- inner_join(topics_df, gdp, by = "date")
  
  # Identify topic columns (topics start with "T")
  topic_cols <- grep("^T", names(merged), value = TRUE)
  
  # Restrict to "selected_topics"
  topic_cols <- intersect(topic_cols, selected_topics)
  
  # Compute correlation for each topic
  map_dfr(topic_cols, function(topic) {
    # Pick non‐missing obs
    df <- merged %>%
      select(y = all_of(econ_var), x = all_of(topic)) %>%
      filter(!is.na(x) & !is.na(y))
    
    # OLS fit
    fit    <- lm(y ~ x, data = df)
    # HAC cov matrix up to lag=4
    vcovNW <- NeweyWest(fit, lag = nw_lag, prewhite = FALSE)
    
    b      <- coef(fit)["x"]
    se_nw  <- sqrt(vcovNW["x","x"])
    tval   <- b / se_nw
    df_res <- df.residual(fit)
    pval   <- 2 * pt(abs(tval), df = df_res, lower.tail = FALSE)
    stars  <- symnum(pval,
                     corr      = FALSE,
                     cutpoints = c(0, .01, .05, .1, 1),
                     symbols   = c("***","**","*",""))
    
    tibble(
      topic   = topic,
      corr    = cor(df$x, df$y, use = "complete.obs"),
      #beta    = b,
      #t_NW    = tval,
      #p_NW    = pval,
      signif  = stars
    )
  }) %>%
    arrange(desc(abs(corr)))
}
  
## SURVEYS ##
# Load surveys data and variable definitions
surveys <- read_excel("Surveys.xlsx") %>%
  select(-pub_date_ESI)

# Use short variable names
surveys <- surveys %>%
  rename(
    ifoIndTradeClimate = `ifo: industry and trade, climate`,
    ifoIndTradeCurrent = `ifo: industry and trade, current situation`,
    ifoIndTradeExp     = `ifo: industry and trade, expectations`,
    GfKBCE             = `GfK: business cycle expectations`,
    GfKIE              = `GfK: income expectations`,
    GfKWtB             = `GfK: willingness-to-buy`,
    GfKCCI             = `GfK: consumer climate indicator`
  )

survey_vars <- setdiff(names(surveys), "date")

surveys <- surveys %>%
  mutate(across(all_of(survey_vars), ~ {
    if (is.character(.x)){
      # replace any "" or "NA" text with actual NA
      x2 <- na_if(.x, "")
      x2 <- na_if(x2, "NA")
      as.numeric(x2)}
    else{.x}
  }))

surveys_q <- surveys %>% 
  mutate(my = as.yearmon(date, format = "%m/%Y"), quarter = as.yearqtr(my)) %>% 
  group_by(quarter) %>% 
  summarise(across(all_of(survey_vars), ~ mean(.x, na.rm = TRUE)), 
            .groups = "drop") %>%
  arrange(quarter) %>%
  # Convert back to a date at quarter-end to join with the quarterly topics
  mutate(
    date = format(as.Date(quarter, frac = 1), "%Y-%m")
  ) %>%
  select(date, all_of(survey_vars))

# Given a variable name, merge a survey indicator with the topics data 
# and compute correlations for each topic column. Then, return the top 20 topics sorted 
# by descending absolute correlation.
calc_topic_corr_monthly <- function(surveys_df, survey_var, topics_df) {
  
  # Merge topics and surveys on date 
  merged <- topics_df %>% inner_join(surveys_df, by = "date")
  
  # Ensure the survey variable column is numeric
  merged[[survey_var]] <- as.numeric(as.character(merged[[survey_var]]))
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic with the given survey variable
  corr_df <- lapply(topic_cols, function(topic) {
    corr_val <- cor(merged[[topic]], merged[[survey_var]], use = "complete.obs")
    data.frame(topic = topic, corr = corr_val)
  }) %>% bind_rows()
  
  # Sort by absolute correlation and select the top 20 topics
  corr_df <- corr_df %>% arrange(desc(abs(corr))) %>% slice(1:20)
  
  return(corr_df)
}

# Create correlation dfs for each of the survey indicators
for(sv in survey_vars) {
  df_name <- paste0(sv, "_corr")
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly(surveys_df = surveys, survey_var = sv, topics_df = topics_sign$monthly)
  assign(df_name, corr_df)
}

# Create an empty list to store survey correlation data frames
survey_corr_list <- list()

for(sv in survey_vars) {
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly(surveys_df = surveys, survey_var = sv, topics_df = topics_sign$monthly)
  # Rename the 'corr' column to the variable name
  corr_df <- corr_df %>% rename(!!sv := corr)
  # Store in the list 
  survey_corr_list[[sv]] <- corr_df
}

# Create an empty list to store survey correlation data frames
survey_corr_list_all <- list()

for(sv in survey_vars) {
  # Calculate the correlation 
  corr_df_all <- calc_topic_corr_monthly(surveys_df = surveys, survey_var = sv, topics_df = topics_all$monthly)
  # Rename the 'corr' column to the variable name
  corr_df_all <- corr_df_all %>% rename(!!sv := corr)
  # Store in the list 
  survey_corr_list_all[[sv]] <- corr_df_all
}

# Combine correlations of GDP and surveys
# 1. Top 20 topics by absolute correlation with GDP
final_corr <- reduce(survey_corr_list, full_join, .init = gdp_corr_top20, by = "topic")

# 2. Only a selected set of topics
final_corr <- reduce(survey_corr_list, full_join, .init = gdp_corr_selected, by = "topic")

# 3. Top 20 topics estimated on all articles by absolute correlation with GDP
final_corr <- reduce(survey_corr_list_all, full_join, .init = gdp_corr_all, by = "topic")

calc_topic_corr_quarterly_sig <- function(surveys_q_df, survey_var, topics_q_df, nw_lag = 4) {
  
  topics_q_df <- topics_q_df %>% ungroup()
  surveys_q_df <- surveys_q_df %>% ungroup()
  
  # Merge topics and surveys on date 
  merged <-  topics_q_df %>% inner_join(surveys_q_df, by = "date")
  
  # Ensure the survey variable column is numeric
  merged[[survey_var]] <- as.numeric(as.character(merged[[survey_var]]))
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic with the given survey variable
  map_dfr(topic_cols, function(topic) {
    # Pick non‐missing obs
    df <- merged %>% select(y = all_of(survey_var), x = all_of(topic)) %>%
      filter(!is.na(x), !is.na(y))
    
    # OLS fit
    fit    <- lm(y ~ x, data = df)
    # HAC cov matrix up to lag=4
    vcovNW <- NeweyWest(fit, lag = nw_lag, prewhite = FALSE)
    
    b      <- coef(fit)["x"]
    se_nw  <- sqrt(vcovNW["x","x"])
    tval   <- b / se_nw
    df_res <- df.residual(fit)
    pval   <- 2 * pt(abs(tval), df = df_res, lower.tail = FALSE)
    stars  <- symnum(pval,
                     corr      = FALSE,
                     cutpoints = c(0, .01, .05, .1, 1),
                     symbols   = c("***","**","*",""))
    
    tibble(
      topic   = topic,
      corr    = cor(df$x, df$y, use = "complete.obs"),
      #beta    = b,
      #t_NW    = tval,
      #p_NW    = pval,
      signif  = stars
    )
  }) %>%
    arrange(desc(abs(corr)))
}

survey_corr_list_q <- list()
for(sv in survey_vars) {
  survey_corr_list_q[[sv]] <-
    calc_topic_corr_quarterly_sig(surveys_q, sv, topics_sign$quarterly, nw_lag = 4) %>%
    rename(
      !!paste0(sv, "_corr")  := corr,
      !!paste0(sv, "_star")  := signif
    )
}

gdp_q <- read.csv("../../AR1/gdp_growth_actual.csv") %>%
  # Turn "YYYY-MM-DD" into "YYYY-MM"
  mutate(date = substr(date, 1, 7))

gdp_corr_sig <- calc_topic_corr_quarterly_sig(
  surveys_q_df  = gdp_q,
  survey_var    = "d_gdp",
  topics_q_df   = topics_sign$quarterly,
  nw_lag        = 4
) %>%
  rename(
    GDP_corr = corr,
    GDP_star = signif
  )

# Keep only the selected topics
gdp_corr_sig <- gdp_corr_sig %>%
  filter(topic %in% selected_topics)

# Combine quarterly correlations of GDP and surveys
all_corr_q <- c(list(GDP = gdp_corr_sig), survey_corr_list_q)

final_corr_q <- reduce(all_corr_q, full_join, by = "topic")

# CORRELATION TABLES FOR THE TEXT ----
make_corr_table <- function(
    econ_var,
    n_top,
    surveys_to_include,
    topics_to_cross,
    topic_type,         # e.g. "topics", "topics_BPW", "topics_uncertainty", "topics_bcc"
    estimation_period,  # e.g. "2009", "2018"
    num_topics,         # e.g. "200", "100"
    source,             # e.g. "all", "dpa", "hb", "sz", "welt"
    output_dir = "correlations_different_approaches"
) {
  # 1) Define topic labels
  topic_labels <- c(
    "T27"  = "\\makecell[tc]{ Economic Crises \\\\ and Recessions}",
    "T127" = "\\makecell[tc]{ Major Banks and \\\\ Investment Banking}",
    "T11"  = "Mergers and Acquisitions",
    "T81"  = "\\makecell[tc]{ Corporate Restructuring and \\\\ Job Cuts in Germany}",
    "T77"  = "Private Investment",
    "T74" = "\\makecell[tc]{ Concerns about Economic\\\\ Bubbles and Recessions}",
    "T52"  = "\\makecell[tc]{ German Automobile Industry \\\\ and Major Manufacturers}",
    "T131" = "\\makecell[tc]{German Investments in \\\\ Emerging Markets}",
    "T138" = "\\makecell[tc]{ Financial and Economic \\\\ Performance}",
    "T100"  = "\\makecell[tc]{ Market Reactions to \\\\News}"
  )
  
  # 2) build the dataframe of top correlations
  df <- final_corr %>%
    select(topic, all_of(econ_var), all_of(surveys_to_include)) %>%
    arrange(desc(abs(.data[[econ_var]]))) %>%
    slice(1:n_top) %>%
    mutate(RawLabel = topic_labels[topic]) %>%
    rowwise() %>%
    mutate(
      Label = if (topic %in% topics_to_cross) {
        # crossed-out topics
        if (str_detect(RawLabel, "\\\\makecell")) {
          # extract the inside of \makecell[tc]{…}
          body <- str_match(RawLabel, "\\\\makecell\\[tc\\]\\{(.*)\\}")[,2]
          parts <- str_split(body, "\\\\\\\\")[[1]]
          # wrap each line in \sout{…}
          crossed <- paste0("\\sout{", parts, "}", collapse=" \\\\ ")
          # reassemble as a single \makecell
          paste0("\\makecell[tl]{", crossed, "}")
        } else {
          paste0("\\sout{", RawLabel, "}")
        }
      } else if (str_detect(RawLabel, "\\\\makecell\\[tc\\]")) {
        # any non-crossed makecell[tc] -> makecell[tl]
        str_replace(RawLabel, "\\\\makecell\\[tc\\]\\{", "\\\\makecell[tl]{")
      } else {
        RawLabel
      }
    ) %>%
    ungroup() %>%
    mutate(across(all_of(c(econ_var, surveys_to_include)), ~ round(.,3))) %>%
    select(
      ID = topic,
      Label = Label,
      all_of(econ_var),
      all_of(surveys_to_include)
    )
  
  # 3) a single lookup for all possible survey‐column renames
  survey_renames <- c(
    ifoIndTradeClimate = "ifo\\_Climate",
    ifoIndTradeCurrent = "ifo\\_Current",
    ifoIndTradeExp     = "ifo\\_Exp",
    ESI                = "ESI",
    GfKBCE             = "GfKBCE",
    GfKIE              = "GfKIE",
    GfKWtB             = "GfKWtB",
    GfKCCI             = "GfKCCI"
  )
  
  # 4) apply those renames to the df's names
  new_names <- names(df)
  for (sv in surveys_to_include) {
    new_names[new_names == sv] <- survey_renames[sv]
  }
  
  # 5) figure out how many right‐aligned 'r' columns
  n_nums <- ncol(df) - 2
  
  # 6) grab raw LaTeX tabular from kable()
  raw_tab <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = c("l","l", rep("r", n_nums)),
      col.names= new_names
    ) %>%
    as.character()
  
  # 7) replace booktabs rules with \hline
  raw_tab <- gsub("\\\\toprule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\midrule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\bottomrule", "\\\\hline", raw_tab)
  
  # 8) build dynamic footnote definitions
  defs <- list(
    ifoIndTradeClimate = "ifo business climate for the industry \\& trade (balances)",
    ifoIndTradeCurrent = "current business situation for the industry \\& trade (balances)",
    ifoIndTradeExp     = "ifo business cycle expectations for the industry \\& trade (balances)",
    ESI                = "Economic Sentiment Indicator",
    GfKBCE             = "GfK: business cycle expectations",
    GfKIE              = "GfK: income expectations",
    GfKWtB             = "GfK: willingness-to-buy",
    GfKCCI             = "GfK: consumer climate indicator"
  )
  
  foot_items <- vapply(
    surveys_to_include,
    function(sv) paste0("‘", survey_renames[sv], "’ = ", defs[[sv]]),
    character(1)
  )
  
  caption_text <- if (topic_type == "topics") {
    sprintf("  \\caption{Topics Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_BPW") {
    sprintf("  \\caption{Sentiment-adjusted Topics (BPW) Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_bcc") {
    sprintf("  \\caption{Sign-adjusted Topics (BCC) Most Correlated with %s (First Release) and Selected Surveys}\n", econ_var)
  } else {
    sprintf("  \\caption{%s Most Correlated with %s and Selected Surveys}\n",
            topic_type, econ_var)
  }
  
  label_text <- sprintf(
    "  \\label{tab:cor_%s_%s_%s_%s_%s}\n",
    tolower(econ_var),
    topic_type,
    estimation_period,
    num_topics,
    source
  )
  
  note_body <- if (topic_type == "topics") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_BPW") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sentiment-adjusted topic (BPW) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A sentiment-adjusted topic (BPW) is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_bcc") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sentiment-adjusted topic (BCC) is among the top 20 in absolute correlation with that survey; otherwise it is NA."
    )
  } else {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic (%s) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      topic_type,
      econ_var
    )
  }
  
  footnote_text <- paste0(
    "Note: ", 
    paste(foot_items, collapse="; "), ". ",
    note_body
  )
  
  # 9) assemble final .tex
  full_tex <- paste0(
    "\\begin{table}[h!]\n",
    "  \\centering\n",
    "  \\begin{threeparttable}\n",
    "    \\footnotesize\n",
    "    \\renewcommand{\\arraystretch}{1.3}\n",
    caption_text,
    label_text, "\n",
    paste(raw_tab, collapse = "\n"), "\n\n",
    "    \\begin{tablenotes}[flushleft]\n",
    "      \\small \\item ", footnote_text, "\n",
    "    \\end{tablenotes}\n",
    "  \\end{threeparttable}\n",
    "\\end{table}\n"
  )
  
  # 10) write it out
  
  # ensure the subfolder exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # build the output path
  output_file <- file.path(
    output_dir,
    paste0(
      "correlation_table_",
      econ_var, "_",
      topic_type, "_",
      estimation_period, "_",
      num_topics, "_",
      source,
      ".tex"
    )
  )
  
  writeLines(full_tex, output_file)
}

make_corr_table_q <- function(
    econ_var,
    n_top,
    surveys_to_include,
    topics_to_cross,
    topic_type,         # e.g. "topics", "topics_BPW", "topics_uncertainty", "topics_bcc"
    estimation_period,  # e.g. "2009", "2018"
    num_topics,         # e.g. "200", "100"
    source,             # e.g. "all", "dpa", "hb", "sz", "welt"
    corr_df, 
    output_dir = "correlations_different_approaches"
) {
  # 1) Define topic labels
  topic_labels <- c(
    "T27"  = "\\makecell[tc]{ Economic Crises \\\\ and Recessions}",
    "T127" = "\\makecell[tc]{ Major Banks and \\\\ Investment Banking}",
    "T11"  = "Mergers and Acquisitions",
    "T81"  = "\\makecell[tc]{ Corporate Restructuring and \\\\ Job Cuts in Germany}",
    "T77"  = "Private Investment",
    "T74" = "\\makecell[tc]{ Concerns about Economic\\\\ Bubbles and Recessions}",
    "T52"  = "\\makecell[tc]{ German Automobile Industry \\\\ and Major Manufacturers}",
    "T131" = "\\makecell[tc]{German Investments in \\\\ Emerging Markets}",
    "T138" = "\\makecell[tc]{ Financial and Economic \\\\ Performance}",
    "T100"  = "\\makecell[tc]{ Market Reactions to \\\\News}"
  )
  
  # 2) build the dataframe of top correlations
  df <- corr_df %>%
    select(
      topic,
      all_of(paste0(econ_var,   "_corr")),
      all_of(paste0(econ_var,   "_star")),
      all_of(paste0(surveys_to_include, "_corr")),
      all_of(paste0(surveys_to_include, "_star"))
    ) %>%
    arrange(desc(abs(.data[[paste0(econ_var, "_corr")]]))) %>%
    slice(1:n_top) %>%
    mutate(RawLabel = topic_labels[topic]) %>%
    rowwise() %>%
    mutate(
      Label = if (topic %in% topics_to_cross) {
        # crossed-out topics
        if (str_detect(RawLabel, "\\\\makecell")) {
          # extract the inside of \makecell[tc]{…}
          body <- str_match(RawLabel, "\\\\makecell\\[tc\\]\\{(.*)\\}")[,2]
          parts <- str_split(body, "\\\\\\\\")[[1]]
          # wrap each line in \sout{…}
          crossed <- paste0("\\sout{", parts, "}", collapse=" \\\\ ")
          # reassemble as a single \makecell
          paste0("\\makecell[tl]{", crossed, "}")
        } else {
          paste0("\\sout{", RawLabel, "}")
        }
      } else if (str_detect(RawLabel, "\\\\makecell\\[tc\\]")) {
        # any non-crossed makecell[tc] -> makecell[tl]
        str_replace(RawLabel, "\\\\makecell\\[tc\\]\\{", "\\\\makecell[tl]{")
      } else {
        RawLabel
      }
    ) %>%
    ungroup() 
  
  df <- df %>%
    mutate(
      # GDP cell
      !!econ_var := sprintf("%0.3f%s",
                            .data[[paste0(econ_var, "_corr")]],
                            .data[[paste0(econ_var, "_star")]])
    )
  
  for (sv in surveys_to_include) {
    df <- df %>%
      mutate(
      # every survey cell
        !!sv := sprintf("%0.3f%s",
                        get(paste0(sv, "_corr")),
                        get(paste0(sv, "_star"))))
  }
  
  df <- df %>%
    select(
      ID    = topic,
      Label,
      all_of(c(econ_var, surveys_to_include))
    )
  
  # 3) a single lookup for all possible survey‐column renames
  survey_renames <- c(
    ifoIndTradeClimate = "ifo\\_Climate",
    ifoIndTradeCurrent = "ifo\\_Current",
    ifoIndTradeExp     = "ifo\\_Exp",
    ESI                = "ESI",
    GfKBCE             = "GfKBCE",
    GfKIE              = "GfKIE",
    GfKWtB             = "GfKWtB",
    GfKCCI             = "GfKCCI"
  )
  
  # 4) apply those renames to the df's names
  new_names <- names(df)
  for (sv in surveys_to_include) {
    new_names[new_names == sv] <- survey_renames[sv]
  }
  
  # 5) figure out how many right‐aligned 'r' columns
  n_nums <- ncol(df) - 2
  
  # 6) grab raw LaTeX tabular from kable()
  raw_tab <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = c("l","l", rep("c", n_nums)),
      col.names= new_names
    ) %>%
    as.character()
  
  # 7) replace booktabs rules with \hline
  raw_tab <- gsub("\\\\toprule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\midrule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\bottomrule", "\\\\hline", raw_tab)
  
  # 8) build dynamic footnote definitions
  defs <- list(
    ifoIndTradeClimate = "ifo business climate for the industry \\& trade (balances)",
    ifoIndTradeCurrent = "current business situation for the industry \\& trade (balances)",
    ifoIndTradeExp     = "ifo business cycle expectations for the industry \\& trade (balances)",
    ESI                = "Economic Sentiment Indicator",
    GfKBCE             = "GfK: business cycle expectations",
    GfKIE              = "GfK: income expectations",
    GfKWtB             = "GfK: willingness-to-buy",
    GfKCCI             = "GfK: consumer climate indicator"
  )
  
  foot_items <- vapply(
    surveys_to_include,
    function(sv) paste0("‘", survey_renames[sv], "’ = ", defs[[sv]]),
    character(1)
  )
  
  caption_text <- if (topic_type == "topics") {
    sprintf("  \\caption{Topics Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_BPW") {
    sprintf("  \\caption{Sentiment-adjusted Topics (BPW) Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_bcc") {
    sprintf("  \\caption{Sign-adjusted Topics (BCC) Most Correlated with %s (First Release, q-o-q Growth) and Selected Surveys (Quarterly Averages)}\n", econ_var)
  } else {
    sprintf("  \\caption{%s Most Correlated with %s and Selected Surveys}\n",
            topic_type, econ_var)
  }
  
  label_text <- sprintf(
    "  \\label{tab:cor_%s_%s_%s_%s_%s_quarterly}\n",
    tolower(econ_var),
    topic_type,
    estimation_period,
    num_topics,
    source
  )
  
  note_body <- if (topic_type == "topics") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_BPW") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sentiment-adjusted topic (BPW) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A sentiment-adjusted topic (BPW) is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_bcc") {
    sprintf(
      "Significance levels: * p<0.10; ** p<0.05; *** p<0.01. Significance levels are based on t-statistics from OLS regression with Newey-West SEs (maximum lag order = 4)."
    )
  } else {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic (%s) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      topic_type,
      econ_var
    )
  }
  
  footnote_text <- paste0(
    "Note: ", 
    paste(foot_items, collapse="; "), ". ",
    note_body
  )
  
  # 9) assemble final .tex
  full_tex <- paste0(
    "\\begin{table}[h!]\n",
    "  \\centering\n",
    "  \\begin{threeparttable}\n",
    "    \\footnotesize\n",
    "    \\renewcommand{\\arraystretch}{1.3}\n",
    caption_text,
    label_text, "\n",
    paste(raw_tab, collapse = "\n"), "\n\n",
    "    \\begin{tablenotes}[flushleft]\n",
    "      \\small \\item ", footnote_text, "\n",
    "    \\end{tablenotes}\n",
    "  \\end{threeparttable}\n",
    "\\end{table}\n"
  )
  
  # 10) write it out
  
  # ensure the subfolder exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  # build the output path
  output_file <- file.path(
    output_dir,
    paste0(
      "correlation_table_",
      econ_var, "_",
      topic_type, "_",
      estimation_period, "_",
      num_topics, "_",
      source, "_",
      "q",
      ".tex"
    )
  )
  
  writeLines(full_tex, output_file)
}

## SENTIMENT-ADJUSTED TOPICS (BCC) ##
# For GDP:
make_corr_table(
  econ_var = "GDP",
  n_top = 10,
  surveys_to_include = c("ifoIndTradeClimate","ifoIndTradeCurrent","ifoIndTradeExp","ESI"),
  topics_to_cross      = c(),
  topic_type          = "topics_bcc",
  estimation_period   = "2009",
  num_topics          = "200",
  source              = "all"
)

# Quarterly correlations
make_corr_table_q(
  econ_var           = "GDP",
  n_top              = 10,
  surveys_to_include = c("ifoIndTradeClimate","ifoIndTradeCurrent","ifoIndTradeExp","ESI"),
  topics_to_cross    = c(),        
  topic_type         = "topics_bcc",
  estimation_period  = "2009",
  num_topics         = "200",
  source             = "all",
  corr_df            = final_corr_q
)

# ECONOMIC INDICATORS ----
# Load economic indicators
econ_indicators <- read_excel("Economic_indicators.xlsx") %>%
  rename(
    Manuf_MV   = `Manufacturing_motor_vehicles`,
    ifo_EB     = `ifo_employment_barometer`,
    ifo_EB_man = `ifo_EB_manufacturing`,
    ifo_auto_exp = `ifo_auto_exp`,
    ifo_auto_current = `ifo_auto_current`,
    ifo_auto_climate = `ifo_auto_climate`
  )

# Build a quarterly 'Manuf_MV' series by averaging the three monthly values
Manuf_MV_q <- econ_indicators %>%
  mutate(my = as.yearmon(date, format = "%m/%Y"), quarter = as.yearqtr(my)) %>%
  group_by(quarter) %>%
  summarise(
    Manuf_MV_avg = mean(Manuf_MV, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(quarter) %>%
  # Compute q-o-q growth on the quarter averages
  mutate(
    qoq_Manuf_MV = (Manuf_MV_avg / lag(Manuf_MV_avg) - 1) * 100
  ) %>%
  filter(!is.na(qoq_Manuf_MV)) %>%
  # Convert back to a date at quarter-end to join with the quarterly topics
  mutate(
    date = format(as.Date(quarter, frac = 1), "%Y-%m")
  ) %>%
  select(date, qoq_Manuf_MV)

indicator_vars <- setdiff(names(econ_indicators), "date")

# Build quarterly averages of economic indicators
econ_indicators_q <- econ_indicators %>%
  mutate(
    # parse "MM/YYYY" strings into yearmon
    my  = as.yearmon(date, format = "%m/%Y"),
    # then into yearqtr
    quarter = as.yearqtr(my)
  ) %>%
  group_by(quarter) %>%
  summarise(
    across(all_of(indicator_vars), ~ mean(.x, na.rm = TRUE)),
    .groups = "drop"
  ) %>%
  mutate(
    date = format(as.Date(quarter, frac = 1), "%Y-%m")
  ) %>%
  select(date, all_of(indicator_vars[2:length(indicator_vars)]))

# Bind "Manuf_MV_q" and other quarterly economic indicators into one data frame
econ_indicators_q <- econ_indicators_q %>%
  full_join(Manuf_MV_q, by = "date")  

econ_indicators_q <- econ_indicators_q %>%
  relocate(qoq_Manuf_MV, .after = date)

# Given a variable name, merge an economic indicator with the topics data 
# and compute correlations for each topic column.
calc_topic_corr_monthly_indicators <- function(econ_indicators_df, econ_indicators_var, topics_df) {
  
  # Merge topics and economic indicator on date 
  merged <- topics_df %>% inner_join(econ_indicators_df, by = "date")
  
  # Ensure the indicator variable column is numeric
  merged[[econ_indicators_var]] <- as.numeric(as.character(merged[[econ_indicators_var]]))
  
  # Identify topic columns (topics start with "T")
  topic_cols <- names(merged)[grepl("^T", names(merged))]
  
  # Compute correlation for each topic with the given economic indicator
  corr_df <- lapply(topic_cols, function(topic) {
    corr_val <- cor(merged[[topic]], merged[[econ_indicators_var]], use = "complete.obs")
    data.frame(topic = topic, corr = corr_val)
  }) %>% bind_rows()
  
  # Sort by absolute correlation
  corr_df <- corr_df %>% arrange(desc(abs(corr)))
  
  return(corr_df)
}

# Compute correlations for each of the three indicators

# Create correlation dfs for each of the economic indicators
for(iv in indicator_vars) {
  df_name <- paste0(iv, "_corr")
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly_indicators(econ_indicators_df = econ_indicators, econ_indicators_var = iv,
                                                topics_df  = topics_sign$monthly
  )
  assign(df_name, corr_df)
}

Manuf_MV_corr <- calc_topic_corr_monthly_indicators(
  econ_indicators_df  = Manuf_MV_q,             
  econ_indicators_var = "qoq_Manuf_MV",       
  topics_df           = topics_sign$quarterly
)

indicators_corr_list <- list()
for(iv in indicator_vars) {
  # Calculate the correlation 
  corr_df <- calc_topic_corr_monthly_indicators(econ_indicators_df = econ_indicators, econ_indicators_var = iv,
    topics_df  = topics_sign$monthly
  ) %>%
    # Rename the 'corr' column to the variable name
    rename(!!iv := corr)
  # Store in the list 
  indicators_corr_list[[iv]] <- corr_df
}

indicators_corr_list[["Manuf_MV"]] <- 
  calc_topic_corr_monthly_indicators(
    econ_indicators_df  = Manuf_MV_q,             
    econ_indicators_var = "qoq_Manuf_MV",        
    topics_df           = topics_sign$quarterly 
  ) %>%
  rename(Manuf_MV = corr) 

# Combine correlations of GDP and economic indicators
# Only a selected set of topics
final_corr_econ_indicators <- reduce(indicators_corr_list, full_join, .init = gdp_corr_selected, by = "topic") 

# Define topic labels
topic_labels <- c(
  "T27"  = "\\makecell[tc]{ Economic Crises \\\\ and Recessions}",
  "T127" = "\\makecell[tc]{ Major Banks and \\\\ Investment Banking}",
  "T11"  = "Mergers and Acquisitions",
  "T81"  = "\\makecell[tc]{ Corporate Restructuring and \\\\ Job Cuts in Germany}",
  "T77"  = "Private Investment",
  "T74" = "\\makecell[tc]{ Concerns about Economic\\\\ Bubbles and Recessions}",
  "T52"  = "\\makecell[tc]{ German Automobile Industry \\\\ and Major Manufacturers}",
  "T131" = "\\makecell[tc]{German Investments in \\\\ Emerging Markets}",
  "T138" = "\\makecell[tc]{ Financial and Economic \\\\ Performance}",
  "T100"  = "\\makecell[tc]{ Market Reactions to \\\\News}"
)

# Build the dataframe of correlations
df_indicators <- final_corr_econ_indicators %>%
  arrange(desc(abs(GDP))) %>%
  slice(1:10) %>%
  mutate(RawLabel = topic_labels[topic]) %>%
  rowwise() %>%
  mutate(
    Label = if (str_detect(RawLabel, "\\\\makecell\\[tc\\]")) {
      # makecell[tc] -> makecell[tl]
      str_replace(RawLabel, "\\\\makecell\\[tc\\]\\{", "\\\\makecell[tl]{")
    } else {
      RawLabel
    }
  ) %>%
  ungroup() %>%
  mutate(across(all_of(c("GDP", indicator_vars)), ~ round(.,3))) %>%
  select(
    ID = topic,
    Label = Label,
    all_of(indicator_vars)
  )

# Figure out how many right‐aligned 'r' columns
n_nums <- ncol(df_indicators) - 2

# Grab raw LaTeX tabular from kable()
raw_tab <- df_indicators %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    align    = c("l","l", rep("r", n_nums)),
    col.names= c("ID", "Label",
                 "Manuf\\_MV", "EB", "EB\\_man",
                 "auto\\_exp", "auto\\_current", "auto\\_climate")
  ) %>%
  as.character()

# Replace booktabs rules with \hline
raw_tab <- gsub("\\\\toprule",    "\\\\hline", raw_tab)
raw_tab <- gsub("\\\\midrule",    "\\\\hline", raw_tab)
raw_tab <- gsub("\\\\bottomrule", "\\\\hline", raw_tab)

# Build dynamic footnote definitions
defs <- list(
  Manuf_MV   = "Manufacturing of motor vehicles, trailers, and semi-trailers (Source: Destatis)",
  ifo_EB     = "ifo Employment Barometer (index)",
  ifo_EB_man = "ifo Employment Barometer for manufacturing (balances)",
  ifo_auto_exp = "ifo Business Expectations for the automotive industry",
  ifo_auto_current = "ifo Assessment of the Current Situation for the automotive industry",
  ifo_auto_climate = "ifo Business Climate for the automotive industry"
)

indicator_renames <- c(
  Manuf_MV    = "Manuf\\_MV",
  ifo_EB      = "EB",
  ifo_EB_man  = "EB\\_man",
  ifo_auto_exp = "auto\\_exp",
  ifo_auto_current = "auto\\_current",
  ifo_auto_climate = "auto\\_climate"
)

foot_items <- vapply(
  indicator_vars,
  function(iv) paste0("‘", indicator_renames[iv], "’ = ", defs[[iv]]),
  character(1)
)

caption_text <- sprintf("  \\caption{Correlations of Sign-adjusted Topics (BCC) with Selected Economic Indicators}\n")

label_text <- sprintf(
  "  \\label{tab:cor_gdp_topics_bcc_2009_200_all_economic_indicators}\n"
)

footnote_text <- paste0(
  "Note: ", 
  paste(foot_items, collapse="; "), "."
)

# Assemble final .tex
full_tex <- paste0(
  "\\begin{table}[h!]\n",
  "  \\centering\n",
  "  \\begin{threeparttable}\n",
  "    \\scriptsize\n",
  "    \\renewcommand{\\arraystretch}{1.3}\n",
  caption_text,
  label_text, "\n",
  paste(raw_tab, collapse = "\n"), "\n\n",
  "    \\begin{tablenotes}[flushleft]\n",
  "      \\small \\item ", footnote_text, "\n",
  "    \\end{tablenotes}\n",
  "  \\end{threeparttable}\n",
  "\\end{table}\n"
)

# Write it out

# Build the output path
output_file <- file.path(
  "correlations_different_approaches",
  "correlations_topics_gdp_and_indicators.tex"
)

writeLines(full_tex, output_file)

calc_topic_corr_quarterly_sig <- function(econ_indicators_df, econ_indicators_var, topics_q_df, nw_lag = 4) {
  topics_q_df        <- topics_q_df %>% ungroup()
  econ_indicators_df <- econ_indicators_df %>% ungroup()
  
  # Merge topics and economic indicators on date 
  merged <- inner_join(topics_q_df, econ_indicators_df, by = "date")
  
  # Identify topic columns (topics start with "T")
  topic_cols <- grep("^T", names(merged), value = TRUE)
  
  # Compute correlation for each topic with the given economic indicator
  map_dfr(topic_cols, function(topic) {
    # Pick non‐missing obs
    df <- merged %>%
      select(y = all_of(econ_indicators_var), x = all_of(topic)) %>%
      filter(!is.na(x), !is.na(y))
    
    # OLS fit
    fit    <- lm(y ~ x, data = df)
    # HAC cov matrix up to lag=4
    vcovNW <- NeweyWest(fit, lag = nw_lag, prewhite = FALSE)
    
    b      <- coef(fit)["x"]
    se_nw  <- sqrt(vcovNW["x","x"])
    tval   <- b / se_nw
    df_res <- df.residual(fit)
    pval   <- 2 * pt(abs(tval), df = df_res, lower.tail = FALSE)
    stars  <- symnum(pval,
                     corr      = FALSE,
                     cutpoints = c(0, .01, .05, .1, 1),
                     symbols   = c("***","**","*",""))
    
    tibble(
      topic   = topic,
      corr    = cor(df$x, df$y, use = "complete.obs"),
      #beta    = b,
      #t_NW    = tval,
      #p_NW    = pval,
      signif  = stars
    )
  }) %>%
    arrange(desc(abs(corr)))
}

indicator_vars_q <- setdiff(names(econ_indicators_q), "date")

indicators_corr_list_q <- list()
for(iv in indicator_vars_q) {
  tmp <- calc_topic_corr_quarterly_sig(econ_indicators_q, iv, topics_sign$quarterly, nw_lag = 4)
  indicators_corr_list_q[[iv]] <-
    tmp %>% 
    rename(
      !!paste0(iv, "_corr") := corr,
      !!paste0(iv, "_star") := signif
    )
}

# Combine correlations of GDP and quarterly economic indicators
# Only a selected set of topics
final_corr_econ_indicators_q <- reduce(indicators_corr_list_q, full_join, .init = gdp_corr_sig, by = "topic") 

# Restrict to the ten selected topics
final_corr_econ_indicators_q <- final_corr_econ_indicators_q %>%
  filter(topic %in% selected_topics)

make_corr_table_q_EI <- function(
    econ_var,
    n_top,
    surveys_to_include,
    topics_to_cross,
    topic_type,         # e.g. "topics", "topics_BPW", "topics_uncertainty", "topics_bcc"
    estimation_period,  # e.g. "2009", "2018"
    num_topics,         # e.g. "200", "100"
    source,             # e.g. "all", "dpa", "hb", "sz", "welt"
    corr_df, 
    output_dir = "correlations_different_approaches"
) {
  # 1) Define topic labels
  topic_labels <- c(
    "T27"  = "\\makecell[tc]{ Economic Crises \\\\ and Recessions}",
    "T127" = "\\makecell[tc]{ Major Banks and \\\\ Investment Banking}",
    "T11"  = "Mergers and Acquisitions",
    "T81"  = "\\makecell[tc]{ Corporate Restructuring and \\\\ Job Cuts in Germany}",
    "T77"  = "Private Investment",
    "T74" = "\\makecell[tc]{ Concerns about Economic\\\\ Bubbles and Recessions}",
    "T52"  = "\\makecell[tc]{ German Automobile Industry \\\\ and Major Manufacturers}",
    "T131" = "\\makecell[tc]{German Investments in \\\\ Emerging Markets}",
    "T138" = "\\makecell[tc]{ Financial and Economic \\\\ Performance}",
    "T100"  = "\\makecell[tc]{ Market Reactions to \\\\News}"
  )
  
  # 2) Build the dataframe of top correlations
  df <- corr_df %>%
    select(
      topic,
      all_of(paste0(econ_var,   "_corr")),
      all_of(paste0(econ_var,   "_star")),
      all_of(paste0(surveys_to_include, "_corr")),
      all_of(paste0(surveys_to_include, "_star"))
    ) %>%
    arrange(desc(abs(.data[[paste0(econ_var, "_corr")]]))) %>%
    slice(1:n_top) %>%
    mutate(RawLabel = topic_labels[topic]) %>%
    rowwise() %>%
    mutate(
      Label = if (topic %in% topics_to_cross) {
        # crossed-out topics
        if (str_detect(RawLabel, "\\\\makecell")) {
          # extract the inside of \makecell[tc]{…}
          body <- str_match(RawLabel, "\\\\makecell\\[tc\\]\\{(.*)\\}")[,2]
          parts <- str_split(body, "\\\\\\\\")[[1]]
          # wrap each line in \sout{…}
          crossed <- paste0("\\sout{", parts, "}", collapse=" \\\\ ")
          # reassemble as a single \makecell
          paste0("\\makecell[tl]{", crossed, "}")
        } else {
          paste0("\\sout{", RawLabel, "}")
        }
      } else if (str_detect(RawLabel, "\\\\makecell\\[tc\\]")) {
        # any non-crossed makecell[tc] -> makecell[tl]
        str_replace(RawLabel, "\\\\makecell\\[tc\\]\\{", "\\\\makecell[tl]{")
      } else {
        RawLabel
      }
    ) %>%
    ungroup() 
  
  df <- df %>%
    mutate(
      # GDP cell
      !!econ_var := sprintf("%0.3f%s",
                            .data[[paste0(econ_var, "_corr")]],
                            .data[[paste0(econ_var, "_star")]])
    )
  
  for (sv in surveys_to_include) {
    df <- df %>%
      mutate(
        # every survey cell
        !!sv := sprintf("%0.3f%s",
                        get(paste0(sv, "_corr")),
                        get(paste0(sv, "_star"))))
  }
  
  df <- df %>%
    select(
      ID    = topic,
      Label,
      qoq_Manuf_MV,
      ifo_auto_exp, ifo_auto_current, ifo_auto_climate,
      ifo_EB, ifo_EB_man
    )
  
  df <- df %>%
    # Bold the ID for T81 and T52
    mutate(
      ID = ifelse(ID %in% c("T81","T52"),
                  paste0("\\textbf{", ID, "}"),
                  ID)
    ) %>%
    # Bold the first four coefficient columns for T52 
    mutate(across(
      c("qoq_Manuf_MV", "ifo_auto_exp", "ifo_auto_current", "ifo_auto_climate"),
      ~ ifelse(ID == "\\textbf{T52}",  
               paste0("\\textbf{", ., "}"),
               .)
    )) %>%
    # Bold the last two coefficient columns for T81 only
    mutate(across(
      c("ifo_EB", "ifo_EB_man"),
      ~ ifelse(ID == "\\textbf{T81}",
               paste0("\\textbf{", ., "}"),
               .)
    ))
  
  # 3) a single lookup for all possible survey‐column renames
  survey_renames <- c(
    qoq_Manuf_MV    = "Manuf\\_MV",
    ifo_EB      = "EB",
    ifo_EB_man  = "EB\\_man",
    ifo_auto_exp = "auto\\_exp",
    ifo_auto_current = "auto\\_current",
    ifo_auto_climate = "auto\\_climate"
  )
  
  # 4) apply those renames to the df's names
  new_names <- names(df)
  for (sv in surveys_to_include) {
    new_names[new_names == sv] <- survey_renames[sv]
  }
  
  # 5) figure out how many right‐aligned 'r' columns
  n_nums <- ncol(df) - 2
  
  # 6) grab raw LaTeX tabular from kable()
  raw_tab <- df %>%
    kable(
      format   = "latex",
      booktabs = TRUE,
      escape   = FALSE,
      align    = c("l","l", rep("c", n_nums)),
      col.names= new_names
    ) %>%
    as.character()
  
  # 7) replace booktabs rules with \hline
  raw_tab <- gsub("\\\\toprule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\midrule",    "\\\\hline", raw_tab)
  raw_tab <- gsub("\\\\bottomrule", "\\\\hline", raw_tab)
  
  # 8) build dynamic footnote definitions
  defs <- list(
    qoq_Manuf_MV   = "Manufacturing of motor vehicles, trailers, and semi-trailers (Source: Destatis)",
    ifo_EB     = "ifo Employment Barometer (index)",
    ifo_EB_man = "ifo Employment Barometer for manufacturing (balances)",
    ifo_auto_exp = "ifo Business Expectations for the automotive industry",
    ifo_auto_current = "ifo Assessment of the Current Situation for the automotive industry",
    ifo_auto_climate = "ifo Business Climate for the automotive industry"
  )
  
  foot_items <- vapply(
    surveys_to_include,
    function(sv) paste0("‘", survey_renames[sv], "’ = ", defs[[sv]]),
    character(1)
  )
  
  caption_text <- if (topic_type == "topics") {
    sprintf("  \\caption{Topics Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_BPW") {
    sprintf("  \\caption{Sentiment-adjusted Topics (BPW) Most Correlated with %s and Selected Surveys}\n", econ_var)
  } else if (topic_type == "topics_bcc") {
    sprintf("  \\caption{Correlations of Sign-adjusted Topics (BCC) with the Manufacturing of Motor Vehicles (q-o-q Growth) and Selected Economic Indicators (Quarterly Averages)}\n")
  } else {
    sprintf("  \\caption{%s Most Correlated with %s and Selected Surveys}\n",
            topic_type, econ_var)
  }
  
  label_text <- sprintf(
    "  \\label{tab:cor_%s_%s_%s_%s_%s_economic_indicators_quarterly}\n",
    tolower(econ_var),
    topic_type,
    estimation_period,
    num_topics,
    source
  )
  
  note_body <- if (topic_type == "topics") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_BPW") {
    sprintf(
      "For survey correlations, the coefficient is shown only if that sentiment-adjusted topic (BPW) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A sentiment-adjusted topic (BPW) is crossed out if its relationship with %s was judged difficult to explain economically.",
      econ_var
    )
  } else if (topic_type == "topics_bcc") {
    sprintf(
      "Significance levels: * p<0.10; ** p<0.05; *** p<0.01. Significance levels are based on t-statistics from OLS regression with Newey-West SEs (maximum lag order = 4)."
    )
  } else {
    sprintf(
      "For survey correlations, the coefficient is shown only if that topic (%s) is among the top 20 in absolute correlation with that survey; otherwise it is NA. A topic is crossed out if its relationship with %s was judged difficult to explain economically.",
      topic_type,
      econ_var
    )
  }
  
  footnote_text <- paste0(
    "Note: ", 
    paste(foot_items, collapse="; "), ". ",
    note_body
  )
  
  # 9) assemble final .tex
  full_tex <- paste0(
    "\\begin{table}[h!]\n",
    "  \\centering\n",
    "  \\begin{threeparttable}\n",
    "    \\scriptsize\n",
    "    \\renewcommand{\\arraystretch}{1.3}\n",
    caption_text,
    label_text, "\n",
    paste(raw_tab, collapse = "\n"), "\n\n",
    "    \\begin{tablenotes}[flushleft]\n",
    "      \\small \\item ", footnote_text, "\n",
    "    \\end{tablenotes}\n",
    "  \\end{threeparttable}\n",
    "\\end{table}\n"
  )
  
  # 10) write it out
  
  # ensure the subfolder exists
  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
  
  output_file <- file.path(
    output_dir,
    "correlations_topics_gdp_and_indicators_q.tex"
  )
  
  writeLines(full_tex, output_file)
}

make_corr_table_q_EI(
  econ_var           = "GDP",
  n_top              = 10,
  surveys_to_include = indicator_vars_q,
  topics_to_cross    = c(),       
  topic_type         = "topics_bcc",
  estimation_period  = "2009",
  num_topics         = "200",
  source             = "all", 
  corr_df            = final_corr_econ_indicators_q
)

# COMPUTE & MERGE CORRELATIONS FOR SELECTED TOPICS ----

# WITH CRISIS #
# (a) sign-adjusted (BCC) correlations
corr_sign <- gdp_corr_selected %>% rename(BCC = GDP) %>%
  arrange(desc(abs(BCC)))

# (b) original-topics correlations:
corr_orig <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_orig$quarterly,
  selected_topics = selected_topics
) %>% rename(Original = GDP)

# (c) sentiment-adjusted (BPW) correlations
corr_bpw <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_bpw$quarterly,
  selected_topics = selected_topics
) %>% rename(BPW = GDP)


# join using corr_sign as the driver (so the order is its order)
df_corr_compare <- corr_sign %>%
  left_join(corr_orig, by = "topic") %>%
  left_join(corr_bpw, by = "topic")

# WITH CRISIS (ADD SIGNIFICANCE) #
# (a) sign-adjusted (BCC) correlations
corr_sign_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_sign$quarterly,
  selected_topics = selected_topics,
  nw_lag          = 4
) %>% rename(BCC_corr = corr, BCC_star = signif)

# (b) original-topics correlations
corr_orig_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_orig$quarterly,
  selected_topics = selected_topics,
  nw_lag          = 4
) %>% rename(Original_corr = corr, Original_star = signif)

# (c) sentiment-adjusted (BPW) correlations
corr_bpw_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_bpw$quarterly,
  selected_topics = selected_topics,
  nw_lag          = 4
) %>% rename(BPW_corr = corr, BPW_star = signif)

topic_labels <- purrr::map_chr(
  topic_labels,
  ~ stringr::str_replace_all(.x, "\\\\makecell\\[tc\\]", "\\\\makecell[tl]")
)

# join using corr_sign_sig as the driver (so the order is its order)
df_corr_compare_sig <- corr_sign_sig %>%
  left_join(corr_orig_sig, by = "topic") %>%
  left_join(corr_bpw_sig,  by = "topic") %>%
  mutate(
    BCC      = paste0(sprintf("%0.3f", BCC_corr),    BCC_star),
    Original = paste0(sprintf("%0.3f", Original_corr), Original_star),
    BPW      = paste0(sprintf("%0.3f", BPW_corr),    BPW_star),
    ID       = topic,
    Label    = topic_labels[topic]
  ) %>%
  select(ID, Label, BCC, Original, BPW)

# WITHOUT CRISIS #
# (a) sign-adjusted (BCC) correlations, no-crisis
corr_sign_nc <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_sign_q_nc,
  selected_topics = selected_topics
) %>% rename(BCC = GDP) %>%
  arrange(desc(abs(BCC)))

# (b) original-topics correlations, no-crisis
corr_orig_nc <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_orig_q_nc,
  selected_topics = selected_topics
) %>% rename(Original = GDP)

# (c) BPW-adjusted correlations, no-crisis
corr_bpw_nc <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_bpw_q_nc,
  selected_topics = selected_topics
) %>% rename(BPW = GDP)

# join using corr_sign_nc as the driver (so the order is its order)
df_corr_compare_nc <- corr_sign_nc %>%
  left_join(corr_orig_nc, by = "topic") %>%
  left_join(corr_bpw_nc,  by = "topic")

# WITHOUT CRISIS (ADD SIGNIFICANCE) #
# (a) sign-adjusted (BCC) correlations, no-crisis
corr_sign_nc_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_sign_q_nc,
  selected_topics = selected_topics,
  nw_lag          = 4
) %>% rename(BCC_corr = corr, BCC_star = signif)

# (b) original-topics correlations, no-crisis
corr_orig_nc_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_orig_q_nc,
  selected_topics = selected_topics
) %>% rename(Original_corr = corr, Original_star = signif)

# (c) BPW-adjusted correlations, no-crisis
corr_bpw_nc_sig <- calc_topic_corr_gdp_sig(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_bpw_q_nc,
  selected_topics = selected_topics
) %>% rename(BPW_corr = corr, BPW_star = signif)

# join using corr_sign_nc_sig as the driver (so the order is its order)
df_corr_compare_nc_sig <- corr_sign_nc_sig %>%
  left_join(corr_orig_nc_sig, by = "topic") %>%
  left_join(corr_bpw_nc_sig,  by = "topic") %>%
  mutate(
    BCC_NC   = paste0(sprintf("%0.3f", BCC_corr),    BCC_star),
    Original_NC = paste0(sprintf("%0.3f", Original_corr), Original_star),
    BPW_NC     = paste0(sprintf("%0.3f", BPW_corr),    BPW_star),
    ID       = topic,
    Label    = topic_labels[topic]
  ) %>%
  select(ID, Label, BCC_NC, Original_NC, BPW_NC)

# WRITE OUT A LaTeX TABLE (WITH CRISIS)

# switch all \makecell[tc] to \makecell[tl]
topic_labels <- purrr::map_chr(
  topic_labels,
  ~ stringr::str_replace_all(.x, "\\\\makecell\\[tc\\]", "\\\\makecell[tl]")
)

df_corr_compare <- df_corr_compare %>%
  # round all correlation coefficients to 3 decimals
  mutate(across(c(BCC, Original, BPW), ~ round(., 3))) %>%
  mutate(ID = topic, Label = topic_labels[topic]) %>%
  select(ID, Label, BCC, Original, BPW)

if (!dir.exists("correlations_different_approaches")) dir.create("correlations_different_approaches")

tex_tab <- df_corr_compare %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape    = FALSE,
    caption = paste0(
      "Correlations of sign-adjusted topics (BCC), topics (Original), and BPW-adjusted topics (BPW) ",
      "with annualized q-o-q GDP growth (first release) ",
      "\\label{tab:cor_gdp_different_approaches}"
    ),
    col.names = c("ID", "Label", "BCC", "Original", "BPW"),
    align     = c("l","l", rep("r", 3))
  ) %>%
  kable_styling(latex_options="hold_position") %>%
  as.character()

# write to disk
writeLines(tex_tab, file.path("correlations_different_approaches", "correlations_different_approaches.tex"))

# WRITE OUT A LaTeX TABLE (WITH CRISIS, ADD SIGNIFICANCE)
tex_tab <- df_corr_compare_sig %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape   = FALSE,
    col.names= c("ID","Label","BCC","Original","BPW"),
    align    = c("l","l","c","c","c")
  ) %>%
  as.character()

full_tex <- paste0(
  "\\begin{table}[h!]\n",
  "  \\centering\n",
  "  \\caption{Correlations of sign-adjusted topics (BCC), topics (Original), and BPW-adjusted topics (BPW) with annualized q-o-q GDP growth (first release)}\n",
  "  \\label{tab:cor_gdp_different_approaches_sig}\n",
  "  \\begin{threeparttable}\n",
  "    \\footnotesize\n",
  "    \\renewcommand{\\arraystretch}{1.3}\n",
  tex_tab, "\n\n",
  "    \\begin{tablenotes}[flushleft]\n",
  "      \\small\n",
  "      \\item Significance levels: * p<0.10; ** p<0.05; *** p<0.01. ",
  "Significance levels are based on t-statistics from OLS regression with Newey-West SEs (maximum lag order = 4).\n",
  "    \\end{tablenotes}\n",
  "  \\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(full_tex,
           file.path("correlations_different_approaches",
                     "correlations_different_approaches_with_sig.tex"))

# WRITE OUT A LaTeX TABLE (WITH AND WITHOUT CRISIS)
# full-sample
df_full <- corr_sign %>% rename(BCC_with = BCC) %>%
  left_join(corr_orig   %>% rename(Original_with = Original), by="topic") %>%
  left_join(corr_bpw    %>% rename(BPW_with      = BPW),      by="topic")

# no-crisis
df_nc   <- corr_sign_nc %>% rename(BCC_no = BCC) %>%
  left_join(corr_orig_nc %>% rename(Original_no = Original), by="topic") %>%
  left_join(corr_bpw_nc  %>% rename(BPW_no      = BPW),      by="topic")

df_combined <- df_full %>%
  left_join(df_nc, by="topic") %>%
  mutate(across(
    c(BCC_with, BCC_no,
      Original_with, Original_no,
      BPW_with, BPW_no),
    ~ round(., 3)
  )) %>%
  mutate(ID    = topic, Label = topic_labels[topic]
  ) %>%
  select(ID, Label,
         BCC_with,  BCC_no,
         Original_with, Original_no,
         BPW_with,      BPW_no)

if (!dir.exists("correlations_different_approaches")) dir.create("correlations_different_approaches")

tex_tab <- df_combined %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape    = FALSE,
    caption = paste0(
      "Correlations of sign-adjusted topics (BCC), topics (Original), and BPW-adjusted topics (BPW) ",
      "with annualized q-o-q GDP growth (first release): with and without (NC) Financial Crisis (2008-2009)} ",
      "\\label{cor_gdp_different_approaches_crisis} ",
      "\\renewcommand{\\arraystretch}{1.3"
    ),
    col.names = c(
      "ID", "Label",
      "BCC",    "BCC\\_NC",
      "Original", "Original\\_NC",
      "BPW",    "BPW\\_NC"
    ),
    align = c("l","l", rep("r", 6))
  ) %>%
  kable_styling(latex_options = c("hold_position", "scale_down")) %>%
  as.character()

# write to disk
writeLines(tex_tab, file.path("correlations_different_approaches", "correlations_different_approaches_with_and_without_crisis.tex"))

# WRITE OUT A LaTeX TABLE (WITH AND WITHOUT CRISIS, ADD SIGNIFICANCE)
df_combined_sig <- df_corr_compare_sig %>%
  left_join(df_corr_compare_nc_sig, by = c("ID","Label")) %>%
  select(
    ID, Label,
    BCC, BCC_NC,
    Original, Original_NC,
    BPW, BPW_NC
  )

if (!dir.exists("correlations_different_approaches")) dir.create("correlations_different_approaches")

tex_tab <- df_combined_sig %>%
    kable(
      format    = "latex",
      booktabs  = TRUE,
      escape    = FALSE,
      col.names = c(
        "ID", "Label",
        "BCC", "BCC\\_NC",
        "Original", "Original\\_NC",
        "BPW", "BPW\\_NC"
      ),
      align     = c("l","l", rep("c", 6))
    ) %>%
    as.character()
  
full_tex <- paste0(
  "\\begin{table}[h!]\n",
  "  \\centering\n",
  "  \\label{tab:cor_gdp_different_approaches_crisis_sig}\n",
  "  \\begin{threeparttable}\n",
  "    \\scriptsize\n",
  "    \\renewcommand{\\arraystretch}{1.3}\n",
  "  \\caption{Correlations of sign-adjusted topics (BCC), topics (Original), and BPW-adjusted topics (BPW) with annualized q-o-q GDP growth (first release): with and without (NC) Financial Crisis (2008-2009)}\n",
  tex_tab, "\n\n",
  "    \\begin{tablenotes}[flushleft]\n",
  "      \\small\n",
  "      \\item Significance levels: * p<0.10; ** p<0.05; *** p<0.01. ",
  "Significance levels are based on t-statistics from OLS regression with Newey-West SEs (maximum lag order = 4).\n",
  "    \\end{tablenotes}\n",
  "  \\end{threeparttable}\n",
  "\\end{table}\n"
)

writeLines(
  full_tex,
  file.path("correlations_different_approaches",
            "correlations_different_approaches_with_and_without_crisis_sig.tex")
)
  
# CORRELATIONS FOR TOPICS ESTIMATED ON ALL ARTICLES ----

# topic correlations (estimated on all articles)
corr_all <- gdp_corr_all %>% rename(Original_all = GDP) %>%
  arrange(desc(abs(Original_all))) %>%
  slice(1:10)

# Without crisis
selected_topics_all <- c("T13", "T197", "T12", "T9", "T43", 
                     "T2", "T20", "T44", "T109", "T62")
corr_all_nc <- calc_topic_corr(
  file            = "../../AR1/gdp_growth_actual.csv",
  econ_var        = "d_gdp",
  topics_df       = topics_all_q_nc,
  selected_topics = selected_topics_all
) %>%
  rename(Original_all_nc = GDP)

corr_all_combined <- corr_all %>%
  left_join(corr_all_nc, by = "topic")

# WRITE OUT A LaTeX TABLE

topic_labels <- c(
  T13   = "Economic Crisis",
  T197  = "Growth and Expansion",
  T12   = "Interviews and Opinions",
  T9   = "Financial Aid and Funding",
  T43   = "Market Competition",
  T2   = "German Banking Sector",
  T20   = "Media Reports",
  T44  = "\\makecell[tc]{International Financial \\\\ Support}",
  T109  = "\\makecell[tc]{Major Banks and \\\\ Investment Banking}",
  T62  = "Economic Outlook"
)

# switch all \makecell[tc] to \makecell[tl]
topic_labels <- purrr::map_chr(
  topic_labels,
  ~ stringr::str_replace_all(.x, "\\\\makecell\\[tc\\]", "\\\\makecell[tl]")
)

corr_all_combined <- corr_all_combined %>%
  mutate(
    ID = topic,
    Label = topic_labels[topic],
    Original_all    = round(Original_all, 3),
    Original_all_nc = round(Original_all_nc, 3)
  ) %>%
  select(ID, Label, Original_all, Original_all_nc)

if (!dir.exists("correlations_different_approaches")) dir.create("correlations_different_approaches")

tex_tab <- corr_all_combined %>%
  kable(
    format   = "latex",
    booktabs = TRUE,
    escape    = FALSE,
    caption = paste0(
      "Correlations of topics (Original) estimated on all articles with annualized q-o-q GDP growth ",
      "(first release): full sample and without (NC) Financial Crisis (2008–2009) ",
      "\\label{cor_gdp_topics_all_articles}"
    ),
    col.names = c(
      "ID", "Label",
      "Original (all articles)",
      "Original\\_NC"
    )
  ) %>%
  kable_styling(latex_options="hold_position") %>%
  as.character()

# write to disk
writeLines(tex_tab, file.path("correlations_different_approaches", "correlations_original_topics_all_articles.tex"))




