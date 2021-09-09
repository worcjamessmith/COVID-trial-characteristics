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

sponsor <- sponsor %>%
  select(-c(Scientific_title, Notes, Extractor))

sponsors_all <- d_all %>% 
  select(TrialID, url, sponsor_type, Primary_sponsor) %>% 
  filter(complete.cases(.))

# Make datasets -----

# start with the subset that were already manually done once
# not sure this makes sense - better to combine
# Let's use everything ending in 2 for this

# join the complete cases from automated with the manually extracted.

d2 <- bind_rows(d, dfs$Manual_extraction_all_2) %>% 
  bind_rows(dfs$Main2) %>% 
  arrange(TrialID)

# join with interventions
dfs$Inverventions2 <- bind_rows(dfs$Manual_extraction_all_interventions_2,
                                dfs$Inverventions2)
                                
d2 <- d2 %>% 
  left_join(dfs$Inverventions2,
            by = c("study_arm", "TrialID", "url"))

# join with sponsors
dfs$Sponsor_type_manual2 <- dfs$Sponsor_type_manual2 %>% 
  select(-c(Scientific_title, Notes, Extractor))

# sponsors_all <- sponsors_all %>% 
#   bind_rows(dfs$Sponsor_type_manual2) %>% 
#   arrange(TrialID)
# stopifnot(all(sponsors_all$TrialID == d_all$TrialID))

# some of the url/TrialID pairs are duplicated across im and main arms, so
# remove duplicates to allow join
sponsors_all <- sponsors_all %>% 
  unique() %>% 
  select(-Primary_sponsor)

d2 <- left_join(d2, sponsors_all, 
          by = c("TrialID", "url")) 

d2 <- left_join(d2, blind_all,
                    by = c("TrialID", "url", "study_arm")) %>% 
  arrange(TrialID)


make_datasets <- function(main, intervention, sponsor){
  # join with interventions
  main <- main %>% 
    left_join(intervention, 
              by = c("study_arm", "TrialID", "url"))
  
  # join with sponsors          

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
  
  main
}
x <- make_datasets(bind_rows(dfs$Main2, d), 
                   dfs$Inverventions2, dfs$Sponsor_type_manual2)
x2 <- make_datasets(dfs$Manual_extraction_all_2, 
                   dfs$Manual_extraction_all_interventions_2, 
                   dfs$Sponsor_type_manual2)

x <- bind_rows(x, x2) %>% 
  arrange(TrialID)

all_equal(d2, x)

x <- make_datasets(bind_rows(dfs$Main1, d), 
                   dfs$Inverventions1, dfs$Sponsor_type_manual1)


# main <- dfs$Main2
# intervention <- dfs$Inverventions2
# sponsor <- dfs$Sponsor_type_manual2



all.equal(colnames(dfs$Main2), colnames(dfs$Manual_extraction_all_2))


x <- which(!colnames(d_all) %in% colnames(d2))
colnames(d_all)[x]
x <- which(!colnames(d2) %in% colnames(d_all))
colnames(d2)[x]


# CONTINUE FROM HERE -----
# double check the above, particularly the intervention bit which was rushed. 
# check a few entries against the excel to be safes
# should really do as a function 
# I need to add back in subject blind:analyst blind from d_all (do at end)
# this approach will work for that.
# blind_all <- d_all %>% 
#   select(TrialID, url,  study_arm, 
#          subject_blind, caregiver_blind, investigator_blind,
#          outcome_blind, analyst_blind)
#  d2_man <- left_join(d2_man, blind_all, 
#                     by = c("TrialID", "url", "study_arm")) 

x <- which(!colnames(d_all) %in% colnames(d2_man))
colnames(d_all)[x]
x <- which(!colnames(d2_man) %in% colnames(d_all))
colnames(d2_man)[x]

# perhaps try this coalesce to replace the NA values in d_sub with the manually
# extracted values from d_man, thus keeping the automated values in d_sub
# in fact this rqdatatable solution looks perfect
# https://community.rstudio.com/t/merging-2-dataframes-and-replacing-na-values/32123/2

# #this works but maybe save this until data are combined
# d2_man %>% 
#   mutate(Exclude = coalesce(Exclude.x, Exclude.y),
#          Notes = coalesce(Notes.x, Notes.y)) %>% 
#   select(-c(Exclude.x, Exclude.y, Notes.x, Notes.y))

# also something to check against i.e. the original

colnames(dfs$Sponsor_type_manual2)
head(dfs$Sponsor_type_manual2)

sapply(d1_man, anyNA)

dfs$Manual_extraction_all_interventions_2 %>% 
  View()

all.equal(d_sub$TrialID, x$TrialID)


which(!colnames(dfs$Manual_extraction_all_2) %in% colnames(d))
colnames(dfs$Manual_extraction_all_2)[30:31]



# d1_sub <- left_join(xxx, dfs$Manual_extraction_all_interventions_2) 



all(d_man$TrialID == d_sub$TrialID)

x <- which(!d_sub$TrialID %in% d1i$TrialID)
d_sub[x,] %>% 
  View()








