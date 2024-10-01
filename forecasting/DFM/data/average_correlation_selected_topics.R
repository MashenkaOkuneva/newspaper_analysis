# This script calculates the average correlation across data vintages for a set of selected sign-adjusted 
# topics used in the out-of-sample forecasting experiment. It reads in the correlation CSV files from the 
# "correlations" folder, sums the correlations for each selected topic across all vintages, and then 
# computes the average correlation for each topic. The resulting average correlations are saved in the 
# "average_correlation_selected_topics.csv" file.

# Load necessary libraries
library(readr)
library(dplyr)

# Set the path to the 'correlations' folder
correlations_folder <- "./correlations"

# List all CSV files in the folder
file_list <- list.files(path = correlations_folder, pattern = "\\.csv$", full.names = TRUE)

# Define the selected topics with the "T" prefix
selected_topics <- c(27, 127, 11, 81, 77, 74, 52, 131, 138, 100)
selected_topics <- paste0("T", selected_topics)  # Add the "T" prefix to each topic

# Initialize a data frame to store sum of correlations and counts for each topic
topic_correlations <- data.frame(
  topic = selected_topics,
  correlation_sum = rep(0, length(selected_topics)),
  count = rep(0, length(selected_topics))
)

# Loop through each file and extract the correlations for selected topics
for (file in file_list) {
  data <- read_csv(file)
  
  # Loop through each selected topic
  for (topic in selected_topics) {
    if (topic %in% data[[1]]) {  # topics are in the first column
      correlation <- data %>% filter(data[[1]] == topic) %>% pull(2)  # correlations are in the second column
      topic_correlations$correlation_sum[topic_correlations$topic == topic] <- 
        topic_correlations$correlation_sum[topic_correlations$topic == topic] + correlation
      topic_correlations$count[topic_correlations$topic == topic] <- 
        topic_correlations$count[topic_correlations$topic == topic] + 1
    }
  }
}

# Calculate the average correlation for each selected topic
topic_correlations <- topic_correlations %>%
  mutate(average_correlation = ifelse(count > 0, correlation_sum / count, NA))

# Save the results to a CSV file
write_csv(topic_correlations %>% select(topic, average_correlation), "average_correlation_selected_topics.csv")

# Print the final results
print(topic_correlations %>% select(topic, average_correlation))
