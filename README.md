
# Raw Data
ICTRP data for control samples were downloaded from ICTRP on 23rd November 2020 from the 'full data download' here: https://www.who.int/ictrp/data/en/ and were up to date on 23rd November. Checking the database on 8th December 2020 there was no further update to the data, but the data fields file was better formatted than the version I had, so I downloaded that on 8th Dec. Three main files:

1. `ICTRP_export_datafields.txt` - these are the datafields that corresponds to the column names 
2. `ICTRPFullExport-672212-23-11-20.zip`- this is the raw data folder It is too large to put on github so it will be made available on OSF
3. `ICTRPFullExport-672212-23-11-20.csv` unzipped version of file 2.

A sample of 1% of the dataset is also provided for demonstrating computational reproducibility more quickly (and with a dataset size that can be pushed to github): `SAMPLE0.01ICTRPFullExport-672212-23-11-20.csv`. A 10% sample is in `SAMPLE0.1ICTRPFullExport-672212-23-11-20.csv`.

COVID-19 trial data were downloaded on 8th December 2020 from `Download COVID-19 trials csv format [7022 rows, updated on: 4 December 2020]` link here: https://www.who.int/clinical-trials-registry-platform. There is one file: `COVID19-web.csv` which contains the data and fields.

In the `initial_import/supplementary` folder there is 
`registry_prefixes_date_formats.csv`
which has the registry prefixes and date formatting. The date formatting is the formatting used for the Date_registration column in the ICTRP export. It mostly matches the formatting in the Date_enrollment column of the ICTRP export, except for ct.gov and JPRN. The same Date_enrollment formats are used in ICTRP and COVID export.




