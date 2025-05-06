# -*- coding: utf-8 -*-
"""
@author: mokuneva
"""
import re


def count_words_mp(text):
    
    '''This function calculates the number of words in a text.'''
    
    # remove each occurence of the quotation marks ' and `
    text = re.sub(r"['`]", r'', text)
    # remove each occurence of the hyphen to count a multiple-word noun as one word (e.g., Experten-Gruppe)
    text = text.replace('-', '')
    # replace each non-alphabetic character by a single space character
    # \u00C0-\u017F is the unicode range, important to match Latin letters with any diacritics (e.g., umlauts)
    text = re.sub(r'[^a-zA-Z\u00C0-\u017F]+', r' ', text)
    # split the text into words whenever a sequence of at least one whitespace character is encountered
    text = re.split('\s+', text)
    # remove any remaining whitespace before or after the resulting words
    text = [w for w in text if w != '']
    return len(text)