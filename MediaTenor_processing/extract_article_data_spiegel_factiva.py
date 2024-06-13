# -*- coding: utf-8 -*-
"""
Created on Tue Jun 11 16:27:16 2024

@author: mokuneva
"""

import os
import codecs

def extract_article_data_spiegel_factiva(spiegel_txt_path):
    """
    Read and extract relevant information from Spiegel articles downloaded from Factiva.
    
    Parameters:
    spiegel_txt_path (str): Path to Spiegel TXT files.

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
    for f in os.listdir(spiegel_txt_path):
        # Ignore .gitkeep files
        if f == '.gitkeep':
            continue
        # Open each TXT file with UTF-8 encoding
        with codecs.open(os.path.join(spiegel_txt_path, f), "r", encoding='utf-8') as input_data:
            files.append(f)
            text = []
            author = 'null, author'
            
            # Extract the title
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()
                # Check if the line is not a general topic identifier
                if line and line not in ["Ausland", "Wirtschaft", "Deutschland", "Titel", "Reporter", "Handelskrieg", 
                                         "Personalien", "Verfassung", "Politik", "Leitartikel", "Coronakrise", "Meinung",
                                         "Wirtschaft investigativ", "Deutschland investigativ", "Inhalt", "Gesellschaft", 
                                         "Die Woche"]:
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
                if any(keyword in line for keyword in ['Der Spiegel', 'Spiegel Online', 'Spiegel Plus']):
                    journal.append('Spiegel')
                    break
            
            # Skip all lines starting after the journal's name until and including the copyright line
            for line in input_data:
                if any(keyword in line for keyword in ['(c)', '©']):
                    break
            
            # Skip a line containing '\r\n' or '\n' 
            for line in input_data:
                if (line.find('\r\n') != -1) or (line.find('\n') != -1):
                    break 
            
            # Extract the main artilcle's text
            j=0
            for line in input_data:
                if len(line.split()) > 0:
                    j += 1
                # Exit the loop if we have reached the end of the main text
                if ((author.split(',')[0] in line and j > 5) or   # Check if author's name is in the line and j > 5
                    any(keyword in line for keyword in ['/ DER SPIEGEL', '/ Der Spiegel',  # Check if line contains one of the keywords
                                                       'Dokument SPGL', '/ Rex', 'Mail:', '/ laif']) or 
                    line.isupper()):  # Check if the entire line is uppercase        
                    break
                    
                # Check if the line is not empty
                if len(line.split()) > 0:
                    # Ensure the line ends with a proper punctuation mark
                    if line.replace("\n", '').strip()[-1] not in ['.', '!', ':', ';', '?', '"']: 
                        line += '.'
    
                    # Replace specific characters in the line
                    replacements = {
                        '\u202f': '',
                        '•': '',
                        '&#039;': "'",
                        '‣': '',
                        '■': ''
                    }
    
                    for old, new in replacements.items():
                        line = line.replace(old, new)
    
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