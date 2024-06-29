# -*- coding: utf-8 -*-
"""
Created on Fri Jun 28 14:36:33 2024

@author: mokuneva
"""

import os
import codecs

def extract_article_data_capital_factiva(capital_txt_path):
    """
    Read and extract relevant information from Capital articles downloaded from Factiva.
    
    Parameters:
    capital_txt_path (str): Path to Capital TXT files.

    Returns:
    dict: A dictionary containing lists of extracted information (journal, day, month, 
    year, title, text, file name).
    """
    
    # Initialize lists to store extracted data
    journal = []
    month = []
    texts = []
    day = []
    year = []
    titles = []
    files = []
    
    # Iterate through all files in the directory
    for f in os.listdir(capital_txt_path):
        # Ignore .gitkeep files
        if f == '.gitkeep':
            continue
        # Open each TXT file with UTF-8 encoding
        with codecs.open(os.path.join(capital_txt_path, f), "r", encoding='utf-8') as input_data:
            files.append(f)
            text = []
            author = 'null author'
            
            # Extract the title
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()
                # Check if the line is not a general topic identifier
                if line and line not in ["Welt der Wirtschaft", "Titel", "Invest", "Start", "Editorial", "Leben"]:
                    titles.append(line)
                    break
                    
            # Skip a line containing '\r\n' or '\n' 
            for line in input_data:
                if (line.find('\r\n') != -1) or (line.find('\n') != -1):
                    break 
                    
            # Extract the author's name
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()
                if 'Wörter' not in line:
                    author = line
                break
                    
            # Extract the date (day, month, year)     
            for line in input_data:
                if len(line.split()) == 3 and len(line.split()[2]) == 4:
                    day.append(line.split()[0])
                    month.append(line.split()[1])
                    year.append(line.split()[2])
                    break
                        
            # Extract the journal's name
            for line in input_data:
                if any(keyword in line for keyword in ['Capital']):
                    journal.append('Capital')
                    break
            
            # Skip all lines starting after the journal's name until and including the copyright line
            for line in input_data:
                if any(keyword in line for keyword in ['Copyright']):
                    break
            
            # Skip a line containing '\r\n' or '\n' 
            for line in input_data:
                if (line.find('\r\n') != -1) or (line.find('\n') != -1):
                    break 
                    
            # Extract the main artilcle's text
            for line in input_data:
                # Exit the loop if we have reached the end of the main text
                if any(keyword in line for keyword in ['Document', 'Bildunterschrift:',  # Check if line contains one of the keywords
                                                       'Dokument CAPITL', 'MITARBEIT:']):        
                    break
                    
                # If the line does not contain certain exclusions, is not empty, and
                # does not start with the author's name, add it to the text list
                if (all(exclusion not in line for exclusion in ['TEXT:', 'ILLUSTRATION:', 'INTERVIEW:', 'ILLUSTRATIONEN:', 'FOTO:', 
                                                                       'FOTOS:']) and
                    len(line.split()) > 0 and
                    line.lower().split()[0] != author.lower().split()[0]):
                    
                    # Ensure the line ends with a proper punctuation mark
                    if line.replace("\n", '').strip()[-1] not in ['.', '!', ':', ';', '?', '"']: 
                        line += '.'
    
                    # Append the cleaned line to the text list
                    text.append(line.replace("\n", ''))
                    
            # Join the text lines into a single string
            text = " ".join(text)
            if text:
                texts.append(text)

    return {
        'journal': journal,
        'day': day,
        'month': month,
        'year': year,
        'title': titles,
        'text': texts,
        'file': files
    }