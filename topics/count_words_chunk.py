# -*- coding: utf-8 -*-
"""
@author: mokuneva
"""

from sklearn.feature_extraction.text import CountVectorizer
import numpy as np

def count_words_chunk(text_chunk, vocabulary):
    
    """Calculate the count of specified words using CountVectorizer"""
    
    vectorizer = CountVectorizer(vocabulary=vocabulary)
    X_chunk = vectorizer.fit_transform(text_chunk)
    return np.asarray(X_chunk.sum(axis=1)).flatten()