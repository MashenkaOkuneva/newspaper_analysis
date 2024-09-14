# -*- coding: utf-8 -*-
"""
Created on Mon Feb  8 12:28:05 2021

@author: mokuneva
"""
import os
import nltk

def load_words(file_path):
    """Load words from a file"""
    with open(file_path, 'r', encoding='utf-8', newline='\n', errors='ignore') as f:
        words = [line.rstrip() for line in f]
    return words

def keep_economy_related_sentences(text, economy_related_words):
    """
    This function filters sentences from a given text, keeping only those 
    sentences that contain words present in economy_related_words.

    Parameters:
    text (str): Text to be filtered.
    economy_related_words (list): List of words that are being checked for.

    Returns:
    str: Filtered text.
    """
    
    # Convert economy_related_words to a set for more efficient lookup
    economy_related_set = set(economy_related_words)

    # Tokenize the text into sentences
    sentences = nltk.sent_tokenize(text.replace(u'\ufeff', ''))

    # Keep only those sentences that contain any word from economy_related_words
    sentences_keep = [s for s in sentences if set(nltk.word_tokenize(s.lower())).intersection(economy_related_set)]

    # Join the kept sentences back into a text
    text_keep = ' '.join(sentences_keep)

    return text_keep
