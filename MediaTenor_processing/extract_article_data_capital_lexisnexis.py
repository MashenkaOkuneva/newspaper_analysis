# -*- coding: utf-8 -*-
"""
Created on Fri Jun 28 19:22:45 2024

@author: mokuneva
"""

import os
import codecs

def extract_article_data_capital_lexisnexis(capital_lexisnexis_txt_path):
    """
    Read and extract relevant information from Capital articles downloaded from LexisNexis.
    
    Parameters:
    capital_lexisnexis_txt_path (str): Path to Capital TXT files.

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
    
    # Iterate through all TXT files in the directory
    for f in os.listdir(capital_lexisnexis_txt_path):
        # Ignore .gitkeep files
        if f == '.gitkeep':
            continue
        # Open each TXT file with UTF-8 encoding
        with codecs.open(os.path.join(capital_lexisnexis_txt_path, f), "r", encoding='utf-8') as input_data:
            files.append(f)
            text = []
            author = 'null author'
            highlight_done = False
            body_done = False
            
            # Extract the title from 'Search Terms'
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()
                if 'Search Terms:' in line:
                    titles.append(line.replace('Search Terms: ', ''))
                    break
                    
            # Extract the journal name          
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()
                if 'Capital' not in line:
                    journal.append('Capital')
                    break
            
            # Extract the author or the lead-in (if present) 
            for line in input_data:
                line = line.replace("\n", '').replace("\r", '').replace("\ufeff", '')
                line = line.strip()   
                # Extract the author's name (if present)
                if 'Byline:' in line:
                    author = line.replace('Byline:\xa0', '')
                    break
                # Extract the lead-in (if present)
                elif 'Highlight:' in line and author == 'null author':
                    highlight_done = True
                    line = line.replace('Highlight:\xa0', '')
                    if line.replace("\n", '').strip()[-1] not in ['.', '!', ':', ';','?', '"']:
                        line  = line  + '.'
                    text.append(line.replace("\n", ''))
                    break
                # No author name or lead-in found; reached the main text of the article
                # The main text of the article always starts with the 'Body' identifier
                elif 'Body' in line:
                    body_done = True
                    break
            
            # Extract the lead-in if not already done
            for line in input_data:
                # Extract the lead-in (if present)
                if 'Highlight:' in line and not highlight_done:
                    line = line.replace('Highlight:\xa0', '')
                    if line.replace("\n", '').strip()[-1] not in ['.', '!', ':', ';','?', '"']:
                        line  = line  + '.'
                    text.append(line.replace("\n", ''))
                    break
                # No lead-in found; reached the main text of the article
                # The main text of the article always starts with the 'Body' identifier
                elif 'Body' in line:
                    body_done = True
                    break
                else:
                    break
            
            # Skip the line with 'Body', which is the identifier that the main text of the article starts
            for line in input_data:
                if 'Body' in line and not body_done:
                    break
                else:
                    break 
                    
            # Extract the main artilcle's text
            for line in input_data:
                # Exit the loop if we have reached the end of the main text
                if any(keyword in line for keyword in ['Bildunterschrift:', 'Classification', 'Kasten:']):
                    break
                
                # Check if the line is not empty, does not contain the 'Body' or 'Graphic:' identifiers, and
                # does not start with the author's name
                if len(line.split()) > 0 and 'Body' not in line and 'Grafik:' not in line and \
                line.lower().split()[0] != author.lower().split()[0]:
                    
                    # Ensure the line ends with a proper punctuation mark
                    if line.replace("\n", '').strip()[-1] not in ['.', '!', ':', ';','?', '"']: 
                        line = line + '.'
                        
                    # Append the cleaned line to the text list
                    text.append(line.replace("\n", ''))
            
            # Extract the date (day, month, year)        
            for line in input_data:
                if 'Load-Date:' in line:
                    line = line.replace('Load-Date:\xa0', '').replace(',', '')
                    day.append(line.split()[1])
                    month.append(line.split()[0])
                    year.append(line.split()[2])
                    break  
                    
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