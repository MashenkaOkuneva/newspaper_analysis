# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 18:09:23 2024

@author: mokuneva
"""

from get_average_sentiment import get_average_sentiment

def process_chunk_average_sentiment(data_chunk, n_articles=11):
    """
    Process each chunk by applying the get_average_sentiment function.
    This calculates the average sentiment for each topic based on the top 'n_articles'.
    """
    return data_chunk.groupby('date').apply(get_average_sentiment, n_articles=n_articles)
