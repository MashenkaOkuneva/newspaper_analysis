# -*- coding: utf-8 -*-
"""
Created on Sat Jun 10 16:50:21 2023

@author: mokuneva
"""

from string import punctuation
import re
import nltk

class TokenizeTestDataMulti():
    def __init__(self, text, economy_related_words, words_without_embeddings):
        """
        Initialize the TokenizeTestDataMulti object with the given text, economy-related words,
        and the list of words that do not have embeddings.
        """
        self.doc = text
        self.economy_related_words = economy_related_words
        self.words_without_embeddings = words_without_embeddings

    def keep_relevant_sentences(self):
        
        """Filter out sentences from the text that do not contain any economy-related words.
        
        Returns the filtered text as a string.
        """
        
        # Convert economy_related_words to a set for more efficient lookup
        economy_related_set = set(self.economy_related_words)
        
        # Tokenize the text into sentences
        sentences = nltk.sent_tokenize(self.doc.replace(u'\ufeff', ''))

        # Keep only those sentences that contain any word from economy_related_words
        sentences_keep = [s for s in sentences if set(nltk.word_tokenize(s.lower())).intersection(economy_related_set)]
        
        # Join the kept sentences back into a text
        self.doc = ' '.join(sentences_keep)

        return self.doc

    def clean_test_data(self):
        
        """Perform pre-processing on the text to prepare it for analysis.
        
        This includes converting to lowercase, removing URLs and punctuation, 
        removing non-alphabetic characters, removing multiple spaces and single-letter tokens,
        and removing words that do not have embeddings.

        Returns the cleaned text as a list of words.
        """
        
        # Convert the article to lowercase
        test_article = self.doc.lower()
        
        # Remove URLs
        test_article = re.sub(r'https\S+|http\S+|www.\S+', '', test_article)
        
        # Remove punctuation
        test_article = test_article.replace('.', ' ').replace('-', ' ').replace('/', ' ')
        test_article = ''.join([c for c in test_article if c not in punctuation and c not in ['»', '«']])

        # Remove non-alphabetic characters from the text
        test_article = ''.join([c for c in test_article if (c.isalpha() or c in [' ', '\n'])])

        # Remove multiple spaces and single-letter tokens
        test_article = re.sub(r'\s{2,}', ' ', test_article.strip())
        test_article = ' '.join([word for word in test_article.split() if len(word) > 1])
        
        # Remove words that do not have embeddings
        test_article = ' '.join([word for word in test_article.split() if word not in self.words_without_embeddings])
        
        # Tokenize the article
        test_article = test_article.split()
               
        self.doc = test_article
        
        return self.doc