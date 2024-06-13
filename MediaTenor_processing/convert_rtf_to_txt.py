# -*- coding: utf-8 -*-
"""
Created on Tue Jun 11 10:44:18 2024

@author: mokuneva
"""

import os
import codecs
from striprtf.striprtf import rtf_to_text

def convert_rtf_to_txt(input_directory, output_directory):
    """
    Convert RTF files in the input directory to TXT files in the output directory.
    Remove any temporary files starting with ~$ in the output directory.
    """
    # Ensure the output directory exists
    os.makedirs(output_directory, exist_ok=True)

    # Iterate through all RTF files in the input directory
    for filename in os.listdir(input_directory):
        if filename.lower().endswith(".rtf"):
            # Read the RTF file and convert it to plain text
            with codecs.open(os.path.join(input_directory, filename), "r") as file:
                text_rtf = rtf_to_text(file.read())
            
            # Replace the .rtf or .RTF extension with .txt for the new file
            new_filename = filename[:-4] + ".txt"
            
            # Write the converted text to the new TXT file
            with codecs.open(os.path.join(output_directory, new_filename), 'w', encoding='utf-8') as new_file:
                new_file.write(text_rtf)
                
    # Remove any temporary files starting with ~$
    for filename in os.listdir(output_directory):
        if filename.startswith("~$"):
            os.remove(os.path.join(output_directory, filename))