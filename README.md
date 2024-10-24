
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Opening the black box of retractions: Exploring the causes and consequences of data management errors

<!-- badges: start -->
<!-- badges: end -->

This repository contains the analysis, the data files, the figures, and
the manuscript for the project titled “Opening the black box of
retractions: Exploring the causes and consequences of data management
errors”. The projects OSF repository can be found on the following
[link](https://osf.io/v7fc2/).

## Folder structure

The `data/` folder contains all the datafiles that are needed to
reproduce the results of the project. The datafiles that contain
personal information or not allowed to openly share are not present in
this folder (e.g. sample database with email addresses, source files
with comments that contain information that makes it possible to
identify the respondent, and the RetractionWacth database).

The `analysis/` folder contains all the analyses files in rmarkdowns
separately for the study. Within this folder you can find the following
files:

- `retractionwatch_simulating-data.Rmd` file contains the script for
  simulating an example dataset (see
  `data/source/retractionwatch_source_simulated_data.csv`) in order to
  write and preregister the data processing and analysis script before
  the data collection begins. The simulation script is based on test
  responses that can be found in the
  `data/simulation/retractionwatch_survey_January+8,+2023_04.56.csv`
  datafile.

- `retractionwatch_source-to-raw.Rmd` file contains the code necessary
  for the transformation of the source data (the datafile downloaded
  directly from Qualtrics as is) to the raw datafile. We made sure that
  the raw datafile do not contain any information that can be used to
  identify any of the respondents.

- `retractionwatch_raw-to-processed.Rmd` file contains the code that
  cleans the dataset and transforms is into a tidy format ready for the
  analysis. We used thematic analysis to group free-text responses.
  While this script contains the preparatory steps of the thematic
  analysis the grouping was carried out manually outside of the scope of
  this script.

- `retractionwacth_analysis` contains the preregistered analysis script.
