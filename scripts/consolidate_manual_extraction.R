# consolidate_manual_extraction.R

# Load -----
library(tidyverse)
library(readxl)

# Inputs -----
# file path for all excels to import
file_path <- "data/manual_processing/manual_extraction/completed/"

# Import data -----
# automated extraction 
d_all <- read_csv("data/automated_extraction/automated_extraction.csv") 
# 15% sample extracted previously
d_man <- read_xlsx("data/manual_processing/manual_extraction/Manual_extraction_all.xlsx") %>% 
  arrange(TrialID)

files <- list.files(file_path)
paths <- paste0(file_path, files)
dfs <- sapply(paths, read_xlsx, na = c("", "NA"))
# remove file type from names 
files <- substr(files, 1, nchar(files)-5)
names(dfs) <- files
names(dfs)[1] <- "Interventions2"

# Function -----
make_datasets <- function(main, intervention, sponsor){
  
  # join with interventions
  main <- main %>% 
    left_join(intervention, 
              by = c("study_arm", "TrialID", "url"))
  
  # join with sponsors   
  sponsors_all <- d_all %>% 
    select(TrialID, url, sponsor_type, Primary_sponsor) %>% 
    filter(complete.cases(.))
  
  sponsor <- sponsor %>% 
    select(TrialID, url, Primary_sponsor, sponsor_type)
  
  sponsors_all <- sponsors_all %>% 
    bind_rows(sponsor) %>% 
    arrange(TrialID)
  
  # some of the url/TrialID pairs are duplicated across im and main arms, so
  # remove duplicates to allow join
  sponsors_all <- sponsors_all %>% 
    unique() %>% 
    select(-Primary_sponsor)
  
  main <- left_join(main, sponsors_all, 
                    by = c("TrialID", "url")) 
  
  # join with blinding of specific parties
  main <- left_join(main, blind_all,
                    by = c("TrialID", "url", "study_arm"))
  
  main <- main %>%
    mutate(Exclude = coalesce(Exclude.x, Exclude.y),
           Notes = coalesce(Notes.x, Notes.y)) %>%
    select(-c(Exclude.x, Exclude.y, Notes.x, Notes.y))
  
  main
}

# Wrangle -----
d_man <- d_man %>% 
  filter(Exclude != "Yes")

# subset of automated data that was already manually extracted
d_sub <- d_all[(d_all$TrialID %in% d_man$TrialID),] %>% 
  arrange(TrialID)

# these later need to be numeric. Note that 'unknown' is made NA
stopifnot(!anyNA(dfs$Manual_extraction_all_2$sample_size))
stopifnot(!anyNA(dfs$Main2$sample_size))

dfs$Manual_extraction_all_2$sample_size <- 
  as.numeric(dfs$Manual_extraction_all_2$sample_size)
dfs$Main2$sample_size <- 
  as.numeric(dfs$Main2$sample_size)

# these are the ones that we didn't manually do as they were complete for these
# variables
d <- d_all %>% 
  select(-(vaccine:traditional), 
         -(subject_blind:analyst_blind),
         -sponsor_type, -Interventions) %>% 
  filter(complete.cases(.)) %>% 
  select(study_arm:Scientific_title, primary_purpose, multicentre, Study_design,
         control_arm:region_Oceania, Date_enrollment_format, sample_size,
         everything())

d_all <- d_all %>% 
  arrange(TrialID)

# blinding cols
blind_all <- d_all %>% 
  select(TrialID, url,  study_arm,
         subject_blind, caregiver_blind, investigator_blind,
         outcome_blind, analyst_blind)

sponsors_all <- d_all %>% 
  select(TrialID, url, sponsor_type, Primary_sponsor) %>% 
  filter(complete.cases(.))

# Make datasets -----

d1a <- make_datasets(bind_rows(dfs$Main2, d), 
                   bind_rows(dfs$Interventions2, 
                             dfs$Manual_extraction_all_interventions_2), 
                   dfs$Sponsor_type_manual2) %>% 
  arrange(TrialID)

d1b <- make_datasets(dfs$Manual_extraction_all_2, 
                   dfs$Manual_extraction_all_interventions_2, 
                   dfs$Sponsor_type_manual2)


d1 <- bind_rows(d1a, d1b) %>% 
  arrange(TrialID)

stopifnot(all.equal(d_all$TrialID, d1$TrialID))

# can't do the second one yet because don't have it.....
d2a <- make_datasets(bind_rows(dfs$Main1, d), 
                     dfs$Interventions2, # don't yet have interventions1
                     dfs$Sponsor_type_manual1) %>% 
  arrange(TrialID)
# need to rename the columns in cat's manual one

stopifnot(all.equal(d1a$TrialID, d2a$TrialID))
rm(d1a, d1b)

summary(d1)

write_csv(d1, "d1.csv")







