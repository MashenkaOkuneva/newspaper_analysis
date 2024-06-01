# -*- coding: utf-8 -*-
"""
Created on Thu May 30 11:47:57 2024

@author: mokuneva
"""

import os
import codecs

def extract_article_data(bams_txt_path, bild_txt_path):
    """
    Read and extract relevant information from TXT files in BamS and BILD directories.
    
    Parameters:
    bams_txt_path (str): Path to BamS TXT files.
    bild_txt_path (str): Path to BILD TXT files.

    Returns:
    dict: A dictionary containing lists of extracted information (journal, day, month, 
    year, titles, texts, sentiment, file name).
    """
    # Initialize lists to store article information
    journal = []
    day = []
    month = []
    year = []
    titles = []
    texts = []
    sentiment = []
    file = []

    # Process each path (BamS and BILD)
    for path in [bams_txt_path, bild_txt_path]:
        for filename in os.listdir(path):
            file.append(filename)
            with codecs.open(os.path.join(path, filename), "r", encoding='utf-8') as input_data:
                # Initialize text list to store lead-in (if present) and the main text
                text = []
                s_done = False
                
                # Extract the title
                for line in input_data:
                    line = line.strip().replace("\ufeff", '')
                    # Check if the line is not a general topic identifier
                    if line and line not in ["Wirtschaft", "Deutschland & die Welt", "Politik & Gesellschaft", 
                                             "Politik & Wirtschaft", "Politik", "Sonntagsfrage", "Debatte", "Geld", 
                                             "Mein Leben & Ich", "CORONA-KRISE", "Nachrichten"]:
                        line = line.replace("STANDPUNKT;", '').replace("Geld & Leben;", '')
                        titles.append(line)
                        break
                
                # Extract the date (day, month, year)
                for line in input_data:
                    parts = line.split()
                    if len(parts) == 3 and len(parts[2]) == 4:
                        day.append(parts[0])
                        month.append(parts[1])
                        year.append(parts[2])
                        break
                
                # Extract the newspaper's name
                for line in input_data:
                    # In some cases, the article from BamS was also available in BILD Plus or on bild.de.
                    # We might have downloaded the version from BILD Plus or bild.de, but the same article was published in BamS.
                    if any(keyword in line for keyword in ["BILD am Sonntag", "BILD Plus", "bild.de"]):
                        journal.append('BamS')
                        break
                    if any(keyword in line for keyword in ["BILD", "B.Z."]):
                        journal.append('BILD')
                        break
                
                # Skip to the end of the metadata section
                for line in input_data:
                    if 'Copyright' in line:
                        break
                        
                # Skip empty lines
                for line in input_data:
                    if line in ['\r\n', '\n']:
                        break
                
                # Extract the lead-in (if present) and sentiment
                for line in input_data:
                    # Extract sentiment if it comes directly after Copyright
                    if 'Sentiment' in line:
                        sentiment.append(line.split()[1])
                        s_done = True
                        break
                    # Exit the loop if there is no lead-in between Copyright and metadata
                    if any(keyword in line for keyword in ["VON", "Wirtschaftskolumne von"]) or \
                    line.isupper() or len(line.strip()) <= 4:
                        break
                    # If there is a lead-in, add it to the text list with a period at the end
                    text.append(line.strip() + '.')
                
                # Extract sentiment if it does not come directly after Copyright
                if not s_done:
                    for line in input_data:
                        if 'Sentiment' in line:
                            sentiment.append(line.split()[1])
                            break
                
                # Extract the main artilcle's text
                for line in input_data:
                    # Exit the loop if we have reached the end of the main text
                    if any(keyword in line for keyword in ['Document', 'Ihre Meinung', 'Dokument BIL', 
                                                           'Dokument ZBIL', 'Dokument BZDE', 'Lesen Sie morgen:']) or \
                    (line == 'Autor'):
                        break
                    # If the line does not contain certain exclusions and is not entirely uppercase, add it to the text list
                    if all(exclusion not in line for exclusion in ['QUELLE', 'FOTOS', 'FOTO', 'Fotos:', 'Foto:', 
                                                                   'Diskutieren Sie']) and not line.isupper():
                        text.append(line.strip())
                
                # Join the text list into a single string and add to texts list
                text = " ".join(text)
                if text:
                    texts.append(text.strip())

    return {
        'journal': journal,
        'day': day,
        'month': month,
        'year': year,
        'titles': titles,
        'texts': texts,
        'sentiment': sentiment,
        'file': file
    }