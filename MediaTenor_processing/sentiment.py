# -*- coding: utf-8 -*-
"""
Created on Mon Jun 10 11:08:31 2024

@author: mokuneva
"""

def sentiment(row):
    '''
    Determine the sentiment of an article based on the majority vote of annotators.
    '''
    # Check if 'negative' has the majority vote
    if (row['negative'] > row['no_clear_tone']) & (row['negative'] > row['positive']):
        return -1
    # Check if 'positive' has the majority vote
    if (row['positive'] > row['negative']) & (row['positive'] > row['no_clear_tone']):
        return 1
    # Check if 'no_clear_tone' has the majority vote
    if (row['no_clear_tone'] > row['negative']) & (row['no_clear_tone'] > row['positive']):
        return 0
    # If no agreement between annotators, return NaN
    return float('NaN')