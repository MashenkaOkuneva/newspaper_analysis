import codecs
import os
import pandas as pd


def read_dictionary(file):
    """This function reads in data from .txt file."""
    with codecs.open(file, 'r', 'utf-8-sig') as f:
        # spitlines() splits a string into a list, where each line is a list item.
        dictionary = f.read().splitlines()
        return set([d.lower().strip() for d in dictionary])

# A list of stopwords
stp = read_dictionary(os.getcwd() + '\\topicmodels\\stopwords.txt')
# A list of names   
names = read_dictionary(os.getcwd() + '\\topicmodels\\names.txt')

# Load the bigrams from a csv file to the Dataframe.    
bigrams = pd.read_csv(os.getcwd().replace('\\newspaper_analysis\\topics', '') + '\\newspaper_data_processing' + '\\Collocations\\bigrams.csv', encoding = 'utf-8-sig', header = None)
# Take only the first 2000 bigrams
bigrams = bigrams.head(2000)
# Create a list of bigrams
bigrams = list(bigrams[0])
# Create a list with bigrams as one word, e.g. 'IG_Metall'
bigrams_one_word = ['_'.join(bi.split(' ')) for bi in bigrams]

# Load the trigrams from a csv file to the Dataframe.
trigrams = pd.read_csv(os.getcwd().replace('\\newspaper_analysis\\topics', '') + '\\newspaper_data_processing' + '\\Collocations\\trigrams.csv', encoding = 'utf-8-sig', header = None)
# Take only the first 1000 trigrams
trigrams = trigrams.head(1000)
# Create a list of trigrams
trigrams = list(trigrams[0])
# Create a list with trigrams as one word
trigrams_one_word = ['_'.join(tri.split(' ')) for tri in trigrams]

# Create a dictionary where the keys are bigrams, and the values are bigrams as one word
dic_bigrams = dict(zip(bigrams, bigrams_one_word))
# Create a dictionary where the keys are trigrams, and the values are trigrams as one word
dic_trigrams = dict(zip(trigrams, trigrams_one_word))
