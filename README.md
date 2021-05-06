# Data and code for article "Is this a click towards diversity? Explaining when andwhy news users make diverse choice?"
### Authors: Felicia Loecherbach, Kasper Welbers, Judith Moeller, Damian Trilling, Wouter van Atteveldt
### Published in Proceedings of WebSci'21 (DOI will follow)

This repository contains the data and code needed for the analyses presented in the paper. It contains two main folders with subfolders: 
- data:
  - raw: All raw data files - except for two files (*elasticsearch_info.csv* and *news.csv*) that were too large to be uploaded to GitHub. They are available upon request.
  - intermediate: Files that were produced with processing steps and that are used as input for the analyses. 
- src: 
  - processing: Scripts for reading, cleaning, merging the data and preparing it for the analysis. Results in the analysis_backup.rds file used for analysis. 
  - analysis: Randomization checks and the main analysis (using the analysis_backup.rds file). 

Thus, to recreate the analyses that are reported on in the paper, run the src/analysis/analysis.Rmd script. 
If you are interested in using different processing steps for the raw data look at the src/processing/ folder. For some parts you might need to request the two missing data files (see comment above). 

