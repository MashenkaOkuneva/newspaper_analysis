# -*- coding: utf-8 -*-
"""
Created on Sat Sep  3 15:17:17 2022

@author: mOkuneva
"""

import topicmodels

def perplexity_fold(data):
    """
    Output perplexity for a particular fold.
    """
    
    # Initialize an LDA object using the LDA class. 
    # A list of stems from the train set (data[0]) is an input.
    # The number of topics is data[2].
    ldaobj = topicmodels.LDA.LDAGibbs(data[0],data[2])
        
    # Initial Sampling: 500 is the number of burn-in iterations; 50 is a thinning interval; 
    # 10 is the number of samples to take.
    ldaobj.sample(500,50,10)
    
    # Keep the last sample to save memory
    ldaobj.samples_keep(1)
    
    # Iteratively sample and keep last sample to save memory
    # Repeat 6 times: iterations 1000-4000
    for _ in range(6):
        # Sample more
        ldaobj.sample(0,50,10) 
        # Keep the last sample to save memory
        ldaobj.samples_keep(1)
        
    # Iterations: 4000-4500
    ldaobj.sample(0,50,10)
    
    # Keep the last 10 samples corresponding to 500 iterations where the chain has converged.
    ldaobj.samples_keep(10)
       
    # Re-sample document-specific topic proportions for the test data (data[1])
    # using 100 iterations of Gibbs sampler.
    queryobj = topicmodels.LDA.QueryGibbs(data[1],ldaobj.token_key,ldaobj.tt)
    
    queryobj.query(100)
    
    # The final perplexity is the average over 10 samples.
    perplexity = queryobj.perplexity().mean()
    
    return(perplexity)    
    