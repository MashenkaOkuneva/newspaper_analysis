# This codes generates a dummy series of sentiment
# to include in the nb_reuterspoll_sentiment.Rmd
# notebook, purely for illustrative purposes
rm(list = ls())

dates <- seq(as.Date("2006-01-01"), as.Date("2018-10-01"), by = "quarters")

sentiment <- rnorm(length(dates))

df_sentiment <- data.frame(quarter = dates, sentiment = sentiment)

write.csv(df_sentiment, file = "./sentiment/dummy_sentiment.csv", row.names = F)
