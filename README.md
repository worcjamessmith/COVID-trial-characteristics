# Overview
This repo is for work done for the registered report: Association between investigating COVID-19 and design characteristics in global clinical trial registrations (the stage 1 in principal acceptance, i.e. protocol and planned analysis code, is [here](https://osf.io/f6d2v/)).

# Script processing order
## Data download 
The raw data file from ICTRP is too large to push to GitHub. It is instead stored on OSf.

1. `download_raw_data.R`
    
    * This downloads the large raw data file from OSF. You could also use it to download the zipped file if the csv is too large. The other necessary raw data files are already in this repo. 

## Data processing
To run the data processing script with the full dataset, the main raw data file (`ICTRPFullExport-672212-23-11-20.csv`) needs to be added to the `data/initial_import/raw` folder. See info on raw data files below. There is a 1% sample of the raw data provided in this repo to demonstrate computational reproducibility, but the full file is too large to push to github. The scripts will create the necessary folders and subfolders.

1. `clean_data.R`

    * Takes raw csv files and produces .rData and csv files of both ICTRP (for control arms) and covid data. Fixes dates, removes quotations, and adds source registry column. All original columns are kept in the export but not necessarily cleaned if we don't need them.

2. `filter_data.R`

    * Takes output of `clean_data.R` and removes withdrawn trials, limits to interventional trials, limits dates, filters conditions, randomly orders rows and creates csvs for manual eligibility screen.
  
# Dependency management
The project uses [`renv`](https://rstudio.github.io/renv/articles/renv.html). Use `renv::restore()` to download the correct package versions. 

# Raw Data
ICTRP data for control samples were downloaded from ICTRP on 23rd November 2020 from the 'full data download' here: https://www.who.int/ictrp/data/en/ and were up to date on 23rd November. Checking the database on 8th December 2020 there was no further update to the data, but the data fields file was better formatted than the version I had, so that was downloaded on 8th Dec. COVID-19 trial data were downloaded on 8th December 2020 from `Download COVID-19 trials csv format [7022 rows, updated on: 4 December 2020]` link here: https://www.who.int/clinical-trials-registry-platform.

There are five files in `data/initial_import/raw`:

1. `ICTRP_export_datafields.txt` - these are the datafields that corresponds to the column names 
2. `ICTRPFullExport-672212-23-11-20.zip`- this is the raw data folder It is too large to put on github so it will be made available on OSF
3. `ICTRPFullExport-672212-23-11-20.csv` unzipped version of file 2. Also too large for github.
4. `SAMPLE0.01ICTRPFullExport-672212-23-11-20.csv`. A sample of 1% of `ICTRPFullExport-672212-23-11-20.csv` which can be used to check computations and can be pushed to github
5. `COVID19-web.csv`. The COVID-19 trials including column names 

In the `data/initial_import/supplementary` folder there is 
`registry_prefixes_date_formats.csv`
which has the registry prefixes and date formatting. The date formatting is the formatting used for the Date_registration column in the ICTRP export. This file used in `clean_data.R`. It mostly matches the formatting in the Date_enrollment column of the ICTRP export too, except for ct.gov and JPRN. The same enrollment date formats are used in ICTRP and COVID export.




