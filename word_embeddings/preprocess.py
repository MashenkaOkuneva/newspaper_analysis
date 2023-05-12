# -*- coding: utf-8 -*-
"""
Created on Fri Jan  8 14:52:00 2021

@author: mokuneva
"""

import re
from string import punctuation
punctuation = punctuation + '»«'


def preprocess(text):
    
    '''This function takes a string of text as input and returns a list 
    of preprocessed tokens. The preprocessing steps include transforming the 
    text to lowercase, removing punctuation, removing non-alphabetic characters, 
    and reducing multiple whitespaces to a single space.'''
    
    # Transform the text to lowercase
    text = text.lower()
    
    # Remove punctuation from the text
    text = ''.join([c for c in text if c not in punctuation])
    
   # Remove non-alphabetic characters from the text
    text = ''.join([c for c in text if c.isalpha() == 1 or c == ' '])
    
    # Replace multiple whitespaces with a single space
    text = re.sub(r'\s{2,}', ' ', text)
    
    # Split the text into individual words (tokens)
    words = text.split()

    return words
