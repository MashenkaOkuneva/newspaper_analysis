# -*- coding: utf-8 -*-
"""
Created on Sat Sep 21 13:25:37 2024

@author: mokuneva
"""

from get_daily_sentiment_sign import get_daily_sentiment_sign

def process_chunk(data_chunk, n_articles=11):
    """
    Process each chunk by applying the get_daily_sentiment_sign function.
    """
    return data_chunk.groupby('date').apply(get_daily_sentiment_sign, n_articles=n_articles)