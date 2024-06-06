# -*- coding: utf-8 -*-
"""
Created on Wed Jun  5 14:12:21 2024

@author: mokuneva
"""

import string

# Define a set of punctuation characters to exclude
exclude = set(string.punctuation)

class Normalize:
    """
    A class used to normalize the titles in the dataset provided by Media Tenor and the titles of articles from
    Factiva and LexisNexis.
    This is necessary because the titles in the Media Tenor dataset were manually entered and may contain inconsistencies 
    in punctuation, spacing, and other formatting issues.
    """
    
    def __init__(self, doc_data):
        """
        Initialize the Normalize class with a list of document titles.
        """
        self.docs = [s for s in doc_data]
        
    def normalized(self):
        """
        Normalize each document title by removing punctuation, converting to lowercase, and standardizing spaces.
        
        Returns:
        list: A list of normalized document titles.
        """
        def one_doc(docs):
            # Remove specific punctuation, replace hyphens with spaces, convert to lowercase, and strip leading/trailing spaces
            stage1 =  ''.join(ch for ch in docs.replace('-', ' ').lower() if (ch not in exclude) and (ch not in ['"', '„', '“', '»', '«'])).strip()
            # Standardize spaces to a single space
            stage2 =  " ".join(stage1.split())
            return stage2
        
        # Apply normalization function to each document title
        self.docs = list(map(one_doc, self.docs))
        
         # Return the normalized titles
        return self.docs