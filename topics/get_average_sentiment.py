# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 18:07:44 2024

@author: mokuneva
"""

import pandas as pd

def get_average_sentiment(group, n_articles=11):
    """
    Function to calculate the average sentiment for each topic on a particular day.
    It selects 'n_articles' articles with the highest proportion of each topic and
    calculates the average sentiment among these articles.
    If the sentiment score is >= 0.5, it is treated as +1, otherwise as -1.
    Returns a series with topic names as the index and the average sentiment as values.
    """
    # Prepare the list of topic column names
    topic_columns = [col for col in group.columns if col.startswith('T')]
    average_sentiments = []

    # Iterate through each topic
    for topic in topic_columns:
        # Sort the group by topic proportion and select top 'n_articles' articles
        top_articles = group.sort_values(by=topic, ascending=False).head(n_articles)
        # Convert sentiment scores to +1 or -1
        sentiments = top_articles['scores'].apply(lambda x: 1 if x >= 0.5 else -1)
        # Calculate the average sentiment
        average_sentiment = sentiments.mean()
        average_sentiments.append(average_sentiment)

    # Return a series with topic names as the index and the average sentiment as values
    return pd.Series(average_sentiments, index=topic_columns)

