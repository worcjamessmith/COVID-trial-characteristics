# Clean data

# This script imports the raw data, removes strange formatting, formats
# dates, and adds a source register column to the main ICTRP data

# Load -----
library(tidyverse)

# Functions -----
# removes quotation marks from data (for ICTRP)
removeQuotes <- function(x) gsub("\"", "", x)

# matches the first entry of the df with the correct date format in ct_ids
# and then converts the date according to that format
convertDates <- function(df){
  id <- match(df[[1, "Source_registry"]], ct_ids$Abbreviation)
  df <- df %>% 
    mutate(Date_registration_format = as.Date(Date_registration, 
                                              format = paste(ct_ids[id, "Format"])))
  df
}

# Inputs -----
# location of files and output
file_path <- "data/initial_import/"

# NOTE USING JUST A SAMPLE CURRENTLY FOR EASE
name_ictrp <- "SAMPLEICTRPExport-672212-23-11-20.csv"
name_ictrp_fields <- "ICTRP_export_datafields.txt"
name_covid <- "COVID19-web.csv"
# Clinical trial ID prefixes and date formatting
name_ct_ids <- "supplementary/registry_prefixes_date_formats.csv"
raw_folder <- "raw/"
intermediate_folder <- "intermediate/"
output_folder <- "clean/"

# Import data -----

# Create file structure
if(!dir.exists(paste0(file_path, intermediate_folder))){
  dir.create(paste0(file_path, intermediate_folder))
} 
if(!dir.exists(paste0(file_path, output_folder))){
  dir.create(paste0(file_path, output_folder))
} 

# ICTRP data is strangely formatted - all except first column have extra
# quotations. My computer can't deal with removing all the quotation marks in
# one go so I split the import and quotation removal into two steps. Note that
# it is very slow - expect ~5 mins. These are combined to make an intermediate
# file which is saved as .Rdata. If the intermediate file has already been
# generated then that is loaded rather than the original data.

if(file.exists(paste0(file_path, intermediate_folder, "ictrp.RData"))){
  load(paste0(file_path, intermediate_folder, "ictrp.RData"))
} else {
  ictrp1 <- read_csv(paste0(file_path, raw_folder, name_ictrp),
                     n_max = 3500,
                     col_names = F)
  ictrp1 <- ictrp1 %>%
    mutate_if(is.character, removeQuotes)
  save(ictrp1, file = paste0(file_path, intermediate_folder, "ictrp1.RData"))
  rm(ictrp1)
  
  ictrp2 <- read_csv(paste0(file_path, raw_folder, name_ictrp),
                     skip = 3500,
                     col_names = F)
  ictrp2 <- ictrp2 %>%
    mutate_if(is.character, removeQuotes)
  save(ictrp2, file = paste0(file_path, intermediate_folder, "ictrp2.RData"))
  
  load(paste0(file_path, intermediate_folder, "ictrp1.RData"))
  
  ictrp <- bind_rows(ictrp1, ictrp2)
  rm(ictrp1, ictrp2)
  
  # save a copy for future use
  save(ictrp, file = paste0(file_path, intermediate_folder, "ictrp.RData"))
  
  # remove intermediate files
  file.remove(paste0(file_path, intermediate_folder, "ictrp1.RData"))
  file.remove(paste0(file_path, intermediate_folder, "ictrp2.RData"))
}

# Column names for ictrp dataset
fields <- read_lines(paste0(file_path, raw_folder, name_ictrp_fields))

# COVID data from ICTRP 
covid <- read_csv(paste0(file_path, raw_folder, name_covid), 
                  guess_max = 7022) # use full data to prevent failures 

# Clinical trial ID prefixes and date formatting
ct_ids <- read_csv(paste0(file_path, name_ct_ids))

# Clean data -----
# first 8 are metadata, last 2 are blank
fields <- fields[9:70]
# names appear to match apart from column 2
colnames(ictrp)[1] <- fields[1]
colnames(ictrp)[2] <-"Unknown"
colnames(ictrp)[3:63] <- fields[2:62]

# Registries -----

# Add registries to ICTRP
ictrp$Source_registry <- NA

# extract TrialID prefix (first 6 characters will do)
# need to do this rather than use full ID as in some IDs the trial acronyms later appear
ictrp <- ictrp %>% 
  mutate(Trial_prefix = str_sub(TrialID, 1, 6)) %>% 
  select(TrialID, Trial_prefix, everything()) 

# look up each prefix for trial registry and populate new col with registry
for (i in 1:nrow(ct_ids)){
  y <- grep(ct_ids[i, 2], ictrp$Trial_prefix)
  ictrp$Source_registry[y] <- ct_ids[[i,3]]
}

# any unassigned?
if (anyNA(ictrp$Source_registry)){
  warning("Not all trials assigned registries")
}

# make sure that the ids are mutually exclusive in terms of rows returned
ids <- sapply(ct_ids$Prefix, USE.NAMES = F, function (x){
  grep(x, ictrp$Trial_prefix)
})
if (length(unlist(ids))!= nrow(ictrp)) {
  warning("Prefixes for source registry were not unique to each entry")
}

# Make same registry acronyms in covid data 
reg <- covid$`Source Register`
reg[reg == "ClinicalTrials.gov"] <- "CT.gov"
reg[reg == "EU Clinical Trials Register"] <- "EUCTR"
reg[reg == "CRIS"] <- "KCT"
reg[reg == "REBEC"] <- "RBR"
reg[reg == "Netherlands Trial Register"] <- "NTR"
reg[reg == "German Clinical Trials Register"] <- "DRKS"
reg[reg == "REPEC"] <- "PER"
reg <- as_tibble(reg)
colnames(reg) <- "Source_registry"
covid <- bind_cols(covid, reg)

# check source registries are now the same
if (!setequal(unique(ictrp$Source_registry), unique(covid$Source_registry))){
  warning("Source registries aren't the same across the two datasets: they should be with the full ICTRP dataset used")
}

# Dates ----

# ICTRP registration date
# split to do date formatting by registry
s <- split(ictrp, f = ictrp$Source_registry)
ictrp <- lapply(s, convertDates)
ictrp <- do.call("rbind", ictrp)
rownames(ictrp) <- NULL
rm(s)

if (sum(is.na(ictrp$Date_registration_format)) != sum(ictrp$Date_registration == "")){
  warning("Number of NAs don't match blank date cells. Review Date_registration_format NA values")
}

# reorder for ease of checking
ictrp <- as_tibble(ictrp) %>% 
  select(Source_registry, 
         Date_registration, Date_registration_format, Date_enrollement,
         TrialID, 
         everything())

# working ----- 

# convertDates <- function(df){
df <- ictrp
input_col <- "Date_registration"

select(df, !!input_col)

  id <- match(df[[1, "Source_registry"]], ct_ids$Abbreviation)
  ct_ids[id, 5]
  df <- df %>% 
    mutate(Date_registration_format = as.Date(!!Date_registration, 
                                              format = paste(ct_ids[id, "Format"])))
  df
# }


ictrp %>%
  group_by(Source_registry) %>%
  sample_n(5, replace = T) %>%
  View()

covid %>%
  group_by(Source_registry) %>%
  sample_n(5, replace = T) %>%
  select(`Source Register`, Source_registry, 
         `Date registration`, `Date registration3`,
         `Date enrollement`, ) %>%
  View()

unique(covid$Source_registry)

#then decide if need to do the same for the enrollment date (do need this eventually)

# Other to do 
# should include a check that the data are the same in both datasets (e.g. start
# date is same for same trial number)

# on full data be sure to change the skip number at the beginning 
# decide on which columns to keep

# covid %>% 
#   group_by(`Source Register`) %>% 
#   sample_n(5, replace = T) %>% 
#   select(`Date registration`, `Date registration3`,
#          `Date enrollement`, `Source Register`) %>% 
#   View()
# 
# ictrp %>% 
#   group_by(`Source Register`) %>% 
#   sample_n(5, replace = T) %>% 
#   select(`Date registration`, `Date registration3`,
#          `Date enrollement`, `Source Register`) %>% 
#   View()

# data cleaning ICTRP all has the work fixing the dates from before

# code to make random sample in terminal:
# perl -ne 'print if (rand() < .01)' biglist.txt > subset.txt
# Specifically used: perl -ne 'print if (rand() < .01)' Data/Original/ICTRPFullExport-672212-23-11-20.csv > Data/Original/SAMPLEICTRPExport-672212-23-11-20.csv
# 
# also need to add output file names 

