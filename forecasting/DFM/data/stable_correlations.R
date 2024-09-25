# Load necessary libraries
library(readr)
library(dplyr)

# Set the path to the 'correlations' folder
correlations_folder <- "./correlations"

# List all CSV files in the folder
file_list <- list.files(path = correlations_folder, pattern = "\\.csv$", full.names = TRUE)

# Initialize an empty vector to store the topics
all_topics <- vector("list", length = length(file_list))

# Loop through files and extract top 10 topics
for (i in seq_along(file_list)) {
  data <- read_csv(file_list[i])
  top_topics <- head(data[[1]], 10)  # topics are in the first column
  all_topics[[i]] <- top_topics
}

# Combine all topics into a single vector and count their frequency
all_topics <- unlist(all_topics)
topic_counts <- table(all_topics)

# Sort the table to see the most common topics
sorted_topic_counts <- sort(topic_counts, decreasing = TRUE)

# Print the sorted counts
print(sorted_topic_counts)
