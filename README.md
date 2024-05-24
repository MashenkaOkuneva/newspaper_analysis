# Nowcasting German GDP with text data

## Overview

This repository contains the necessary resources to replicate the main results from the paper "Nowcasting German GDP with Text Data" by Mariia Okuneva, Philipp Hauber, Kai Carstensen, and Jasper Bär. It includes the code for sentiment extraction, topic modeling, and out-of-sample forecasting exercise.

## Repository Structure

The repository is organized into several folders, each dedicated to a specific part of the paper:

- **[data](https://github.com/MashenkaOkuneva/newspaper_analysis/tree/main/data)**: This folder contains notebooks that calculate descriptive statistics for the dataset, both before and after pre-processing. It also includes visualizations of daily publications across all news media.
  - **[Descriptive Statistics.ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics.ipynb)**: Calculates descriptive statistics for the full pre-processed dataset (all news media) and generates a figure showing daily publications across all media sources.
  - **[Descriptive Statistics (no pre-processing).ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics%20(no%20pre-processing).ipynb)**: Performs similar calculations and generates figures for the unprocessed version of the full dataset.
  - **[Descriptive Statistics (Handelsblatt).ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics%20(Handelsblatt).ipynb)**: Analyzes the effect of pre-processing on Handelsblatt alone, with figures showing daily publications before and after pre-processing.
  - **[Descriptive Statistics (SZ).ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics%20(SZ).ipynb)**: Focuses on SZ, presenting statistics and daily publication figures pre- and post-processing.
  - **[Descriptive Statistics (Welt).ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics%20(Welt).ipynb)**: Examines Welt, detailing statistics and visualizing daily publications before and after pre-processing.
  - **[Descriptive Statistics (dpa).ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/data/Descriptive%20Statistics%20(dpa).ipynb)**: Dedicated to dpa, providing statistics and publication figures highlighting the impact of pre-processing.

- **[MediaTenor_processing](https://github.com/MashenkaOkuneva/newspaper_analysis/tree/main/MediaTenor_processing)**:
  - **[Metadata Corrections for Sentiment Analysis.ipynb](https://github.com/MashenkaOkuneva/newspaper_analysis/blob/main/MediaTenor_processing/Metadata%20Corrections%20for%20Sentiment%20Analysis.ipynb)**: Demonstrates the modifications made to metadata in the Excel file acquired from Media Tenor International. The goal was to ensure accurate matching of articles downloaded from LexisNexis and Factiva with their metadata from the file.