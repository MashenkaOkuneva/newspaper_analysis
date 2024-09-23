# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 13:24:37 2024

@author: mokuneva
"""
import pandas as pd

def get_daily_sentiment_sign(group, n_articles=11):
    """
    Function to calculate the sign of sentiment for each topic on a particular day.
    It selects 'n_articles' articles with the highest proportion of each topic and
    calculates whether there are more positive/no clear tone or negative articles among these.
    Returns a series with topic names as the index and sign of sentiment as values.
    """
    # Prepare the list of topic column names
    topic_columns = [col for col in group.columns if col.startswith('T')]
    sentiment_signs = []

    # Iterate through each topic
    for topic in topic_columns:
        # Sort the group by topic proportion and select top 'n_articles' articles
        top_articles = group.sort_values(by=topic, ascending=False).head(n_articles)
        # Count the number of positive/no clear tone and negative articles
        num_positive = (top_articles['scores'] >= 0.5).sum()
        num_negative = (top_articles['scores'] < 0.5).sum()
        # If there are more positive/no clear tone articles, the sign is +1, otherwise -1
        sign = 1 if num_positive > num_negative else -1
        sentiment_signs.append(sign)

    # Return a series with topic names as the index and sign of sentiment as values
    return pd.Series(sentiment_signs, index=topic_columns)