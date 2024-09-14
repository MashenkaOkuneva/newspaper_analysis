# -*- coding: utf-8 -*-
"""
Created on Wed Feb  3 14:25:23 2021

@author: mokuneva
"""

def remove_words_without_embeddings(text, words_without_embeddings):
    """
    This function removes words from a text that do not have embeddings.
    
    Parameters:
    text (str): The input text from which words will be removed.
    words_without_embeddings (list): A list of words that do not have embeddings.
    
    Returns:
    str: The text with words removed that do not have embeddings.
    """
    words_in_text = text.split()
    words_with_embeddings = [word for word in words_in_text if word not in words_without_embeddings]
    
    return ' '.join(words_with_embeddings)
