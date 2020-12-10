# Clean data

# This script imports the raw data, removes strange formatting, formats dates,
# and adds a source register column to the main ICTRP data. Only columns that we
# later use are focussed on

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
  df <- df %>% 
    mutate(Date_enrollment_format = as.Date(Date_enrollment, 
                                            format = paste(ct_ids[id, "Format"])))
  df
} 

# Inputs -----
# location of files and output
file_path <- "data/initial_import/"
# name_ictrp <- "SAMPLEICTRPExport-672212-23-11-20.csv"
name_ictrp <- "ICTRPFullExport-672212-23-11-20.csv"
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
# Add colnames to ictrp 
# first 8 are metadata, last 2 are blank
fields <- fields[9:70]
# names appear to match apart from column 2
colnames(ictrp)[1] <- fields[1]
colnames(ictrp)[2] <-"Unknown"
colnames(ictrp)[3:63] <- fields[2:62]

# rename col in covid to make same as ictrp
covid <- rename(covid, Date_registration = `Date registration`)
# correct spelling and standardise date_enrollment
covid <- rename(covid, Date_enrollment = `Date enrollement`)
ictrp <- rename(ictrp, Date_enrollment = Date_enrollement)

# format date col as character not number
covid$`Date registration3` <- as.character(covid$`Date registration3`)

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
rm(ids)
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
rm(reg)

# check source registries are now the same
if (!setequal(unique(ictrp$Source_registry), unique(covid$Source_registry))){
  warning("Source registries aren't the same across the two datasets: they should be with the full ICTRP dataset used")
}

# ICTRP Dates ----

# add a column to the dataset with date inferred. This is by default false. For
# some enrollment dates, only the month and year are given, so we have to assume
# the day. This column allows us to record this
ictrp$Day_inferred <- F
covid$Day_inferred <- F

# ICTRP registration date
# split to do date formatting by registry
s <- split(ictrp, f = ictrp$Source_registry)
ictrp <- lapply(s, convertDates)
# ctgov and JPRN have different formatting for enrollment and registration
# ctgov first. it has a mix of dates and month/years. 
ctgov <- ictrp$CT.gov
ctgov <- ctgov %>% 
  mutate(Date_enrollment_format = (as.Date(Date_enrollment, 
                                           format = "%B %d, %Y"))) %>% 
  select(Date_enrollment, Date_enrollment_format, Day_inferred, 
         everything())

# add a flag to data for those that we are about to infer day for
ctgov <- ctgov %>% 
  mutate(Day_inferred = if_else(!is.na(Date_enrollment_format),  
                                true = F,
                                false = T))

# assume that the date is the first of the month to allow as.Date                      
ctgov <- ctgov %>% 
  mutate(Date_enrollment_format = if_else(is.na(Date_enrollment_format), 
                                          as.Date(paste(Date_enrollment, 
                                                        " 01", sep=""), 
                                                  format = "%B %Y %d"),
                                          Date_enrollment_format))
ictrp$CT.gov <- ctgov
rm(ctgov)
# JPRN: some of the JPRN ones are in the format we already tried, so only now do
# those that failed. The formats could not be misinterpreted as they are year
# last or year first, so this approach works
jprn <- ictrp$JPRN
jprn <- jprn %>%
  mutate(Date_enrollment_format = if_else(is.na(Date_enrollment_format),
                                          as.Date(Date_enrollment,
                                                  format = "%Y/%m/%d"),
                                          Date_enrollment_format))
ictrp$JPRN <- jprn
rm(jprn)
ictrp <- do.call(bind_rows, ictrp)
rownames(ictrp) <- NULL
rm(s)

if (sum(is.na(ictrp$Date_registration_format)) != sum(ictrp$Date_registration == "")){
  warning("Number of NAs don't match blank date cells. Review Date_registration_format NA values")
}

# reorder for ease of checking
ictrp <- as_tibble(ictrp) %>% 
  select(Source_registry, 
         Date_registration, Date_registration_format, Date_enrollment,
         Date_enrollment_format, Day_inferred,
         TrialID, 
         everything())

# COVID dates -----

# Largely the same as above for ICTRP, though the covid export has one nicely
# formatted date column which we can check our approach against

# split to do date formatting by registry
s <- split(covid, f = covid$Source_registry)
covid <- lapply(s, convertDates)
# ctgov and JPRN have different formatting for enrollment and registration
# ctgov first. it has a mix of dates and month/years. 
ctgov <- covid$CT.gov
ctgov <- ctgov %>% 
  mutate(Date_enrollment_format = (as.Date(Date_enrollment, 
                                           format = "%B %d, %Y"))) %>% 
  select(Date_enrollment, Date_enrollment_format, Day_inferred, 
         everything())

# add a flag to data for those that we are about to infer day for
ctgov <- ctgov %>% 
  mutate(Day_inferred = if_else(!is.na(Date_enrollment_format),  
                                true = F,
                                false = T))

# assume that the date is the first of the month to allow as.Date                      
ctgov <- ctgov %>% 
  mutate(Date_enrollment_format = if_else(is.na(Date_enrollment_format), 
                                          as.Date(paste(Date_enrollment, 
                                                        " 01", sep=""), 
                                                  format = "%B %Y %d"),
                                          Date_enrollment_format))
covid$CT.gov <- ctgov
rm(ctgov)
# JPRN: some of the JPRN ones are in the format we already tried, so only now do
# those that failed. The formats are not ambiguous as they are year
# last or year first, so this approach works
jprn <- covid$JPRN
jprn <- jprn %>%
  mutate(Date_enrollment_format = if_else(is.na(Date_enrollment_format),
                                          as.Date(Date_enrollment,
                                                  format = "%Y/%m/%d"),
                                          Date_enrollment_format))
covid$JPRN <- jprn
rm(jprn)
covid <- do.call(bind_rows, covid)
rownames(covid) <- NULL
rm(s)

if (sum(is.na(covid$Date_registration_format)) != sum(covid$Date_registration == "")){
  warning("Number of NAs don't match blank date cells. Review Date_registration_format NA values")
}

# reorder for ease of checking
covid <- as_tibble(covid) %>% 
  select(Source_registry, 
         Date_registration, Date_registration_format, Date_enrollment,
         Date_enrollment_format, Day_inferred,
         TrialID, 
         everything())

# convert the nicely formatted date column and check against my conversions
covid <- covid %>% 
  mutate(Date_registration3_format = as.Date(`Date registration3`, format = "%Y%m%d"))

if (!setequal(covid$Date_registration_format, covid$Date_registration3_format))
  warning("Date registration differs between my conversion and ICTRP Date registration3 column")

# check that registration date is consistent across datasets for the same trials
x <- inner_join(covid, ictrp, by = "TrialID") %>% 
  select(Date_registration_format.x, Date_registration_format.y,)

if(!identical(x$Date_registration_format.x, x$Date_registration_format.y))
  warning("Registration dates don't match for the same trials across the two data sources")
rm(x)

# Reorder columns 

ictrp <- ictrp %>% 
  select(Source_registry, TrialID, public_title, Scientific_title, url, 
         Date_registration_format, Date_enrollment_format, Day_inferred, 
         study_type, study_design, phase, Target_size, 
         Primary_sponsor, Countries, Interventions, Conditions, Bridging_flag, 
         Retrospective_flag, everything())

covid <- covid %>% 
  select(Source_registry, TrialID, `Public title`, `Scientific title`, `web address`,
         Date_registration_format, Date_enrollment_format, Day_inferred, 
         `Study type`, `Study design`, Phase, `Target size`, 
         `Primary sponsor`, Countries, Intervention, Condition, `Bridging flag truefalse`, 
         `Retrospective flag`, everything())

# save files
save(ictrp, file = paste0(file_path, output_folder, "ictrp.R"))
save(covid, file = paste0(file_path, output_folder, "covid.R"))
write_csv(ictrp, file = paste0(file_path, output_folder, "ictrp.csv"))
write_csv(covid, file = paste0(file_path, output_folder, "covid.csv"))



