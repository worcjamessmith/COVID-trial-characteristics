# Overview
This repo is for work done for the registered report: Association between investigating COVID-19 and design characteristics in global clinical trial registrations (the stage 1 in principal acceptance, i.e. protocol and planned analysis code, is [here](https://osf.io/f6d2v/)).

# Workflow description 
Here the script processing order and any manual steps taken are explained.

## Data download 
The raw data file from ICTRP is too large to push to GitHub. It is instead stored on OSF.

1. `download_raw_data.R`
    
    * This downloads the large raw data file from OSF. You could also use it to download the zipped file if the csv is too large. The other necessary raw data files are already in this repo (see description of raw data files below). 

## Data processing
To run the data processing script with the full dataset, the main raw data file (`ICTRPFullExport-672212-23-11-20.csv`) needs to be added to the `data/initial_import/raw` folder. See info on raw data files below. There is a 1% sample of the raw data provided in this repo to demonstrate computational reproducibility, but the full file is too large to push to github. The scripts will create the necessary folders and subfolders.

1. `clean_data.R`

    * Takes raw csv files and produces .rData and csv files of both ICTRP (for control arms) and covid data. Fixes dates, removes quotations, and adds source registry column. All original columns are kept in the export but not necessarily cleaned if we don't need them.

2. `filter_data.R`

    * Takes output of `clean_data.R` and removes withdrawn trials, limits to interventional trials, limits dates, filters conditions, randomly orders rows and creates csvs for manual eligibility screen. 
    
## Manual eligibility screen

The output produced from `filter_data.R` was copied into a new intermediate subfolder: `data/manual_processing/eligibility_screen/intermediate/for_screening` where the files were manually converted to .xlsx format. Each file was opened and all data converted to a table (Insert > Table). The include column was then edited so that the only inputs allowed were: "1, 0, Unsure" using the data validation tool (Data > Data validation. Allow: List. Source: "1,0,Unsure"). Copies of these files were distributed for eligibility screening. 
    
Eligibility screens were done in duplicate by two reviewers and the results of the screen are in `...intermediate/screened`
    
3. `compare_eligibility.R` 

    * Takes the datasets screened for eligibility and compares the decisions, producing a dataset for each of the arms of the dataset with any disagreements. The new datasets contain a column for the final consensus decision and a column any notes about the decisions. The produced csv files are located in  `...intermediate/for_screening`. Copies of these were converted to excel files and screened to examine sources of disagreements and decide on inclusion/exclusion.
    
The files with consensus decisions are in `...intermediate/screened` (suffix `_consolidated`.
  
4. `list_included_trials.R`

    * Combines consensus decisions from disagreements with agreements from the first screen, and produces the stratified random samples of 15% data used for manual extraction for quality control. Also produces csv files of all eligible trials identified and the 847 trials specified in the protocol to be used as the final dataset. All eligible trials are stored in case we need to add more trials at a later date.
    
The random samples for manual extraction are in: `data/manual_processing/manual_extraction/random_sample/`.
The lists of eligible trials, including the subsets of 847 trials, are in `data/manual_processing/eligibility_screen/eligible_trials/`.


  
  
# Dependency management
The project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html). Use `renv::restore()` to download the correct package versions and ensure computational reproducibility. 

# Raw Data
ICTRP data for control samples were downloaded from ICTRP on 23rd November 2020 from the 'full data download' here: https://www.who.int/ictrp/data/en/ and were up to date on 23rd November. Checking the database on 8th December 2020 there was no further update to the data, but the data fields file was better formatted than the version I had, so that was downloaded on 8th Dec. COVID-19 trial data were downloaded on 8th December 2020 from the `Download COVID-19 trials csv format [7022 rows, updated on: 4 December 2020]` link here: https://www.who.int/clinical-trials-registry-platform.

There will be four files in `data/initial_import/raw` if you run the `download_raw_data.R` script to get the ICTRP data. 

1. `ICTRP_export_datafields.txt` - these are the datafields that corresponds to the column names in the ICTRP data (items 2 and 3).
2. `ICTRPFullExport-672212-23-11-20.csv`. This is the raw ICTRP data file containing the whole database, which is too large to put on github so is on OSF (see Data download section). The zipped file is also on OSF (`ICTRPFullExport-672212-23-11-20.zip`), which is the format in which it was originally downloaded from ICTRP.
3. `SAMPLE0.01ICTRPFullExport-672212-23-11-20.csv`. A sample of 1% of `ICTRPFullExport-672212-23-11-20.csv` which can be used to check code/reproducibility if you don't want to run the scripts with the full data. 
5. `COVID19-web.csv`. The COVID-19 trials including column names.

In the `data/initial_import/supplementary` folder there is `registry_prefixes_date_formats.csv` which has the registry prefixes and date formatting. The date formatting is the formatting used for the Date_registration column in the ICTRP export. This file used in `clean_data.R`. It mostly matches the formatting in the Date_enrollment column of the ICTRP export too, except for ct.gov and JPRN. The same enrollment date formats are used in ICTRP and COVID-19 export.