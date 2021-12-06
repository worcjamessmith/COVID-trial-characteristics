# Takes the manual extraction efforts and combines them into two datasets. Those
# two datasets are then compared and any discrepancies (or fields that were left
# as NA in at least one dataset) are recorded in a new dataset with "NA".
# That dataset is saved for manual review and consolidation.

# Load -----
library(rqdatatable) # for natural_join
library(tidyverse)
library(readxl)

# Inputs -----
# file path for all excels to import
file_path <- "data/manual_processing/manual_extraction/completed/"
# output path
output_path <- "data/manual_processing/manual_extraction/Compared.csv"

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
names(dfs)[1] <- "Interventions1" # correct typo
names(dfs)[2] <- "Interventions2"

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

d_man$`Start Date` <- 
  as.Date(d_man$`Start Date`, 
        format = "%d/%m/%Y")

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

# rename columns to match
d_man <- d_man %>% 
  rename(url = URL,
         Scientific_title = Title,
         Conditions = Condition,
         study_arm = COVID,
         Date_enrollment_format = `Start Date`,
         control_arm = UseOfControlArm,
         randomisation = Randomisation,
         blinding = Blinding,
         subject_blind = SubjectBlind,
         caregiver_blind = CaregiverBlind,
         investigator_blind = InvestigatorBlind,
         outcome_blind = OutcomeBlind, 
         analyst_blind = AnalystBlind, 
         prospective = ProspectiveRegistration,
         Source_registry = SourceRegistry,
         phase_clean = Phase,
         region_Africa = Africa, 
         region_N_America = NorthernAmerica,
         region_L_America = LatinAmericaCarribbean,
         region_Asia = Asia,          
         region_Europe = Europe,
         region_Oceania = Oceania,
         multicentre = MultiCentre,
         primary_purpose = PrimaryPurpose,
         sponsor_type = SponsorType,
         sample_size = SampleSize,
         conventional = Conventional,
         vaccine = Vaccine,
         traditional = Traditional)

# make data same in study_arm
d_man[d_man$study_arm == "Yes", ]$study_arm <- "covid"
d_man[d_man$study_arm == "No", ]$study_arm <- "main"
d_man[d_man$study_arm == "IM", ]$study_arm <- "im"

# Fix original manual -----

# join in this way because we want to rely on the automated extraction when it
# was done in this case. The automated and manual differ a little because we
# didn't correct some entries, particularly in prospective, as there was
# ambiguity about what was correct. This is discussed in the paper.
d_man2 <- natural_join(d_sub, d_man, 
                      by = c("TrialID", "url"),
                      jointype = "FULL")

# use the original dates, as the join messes them up. use d_man rather than
# d_sub as it has 2 NA filled in
d_man2$Date_enrollment_format <- d_man$Date_enrollment_format
d_man <- d_man2
rm(d_man2)

# Make datasets -----

# Dataset 1
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

# Dataset 2
# there are some in d which were also in d_man. Those are removed
d <- d[!(d$TrialID %in% d_man$TrialID),] %>% 
  arrange(TrialID)

d2a <- make_datasets(bind_rows(dfs$Main1, d), 
                     dfs$Interventions1, 
                     dfs$Sponsor_type_manual1) %>% 
  arrange(TrialID)

# join with manual data
d2 <- bind_rows(d2a, d_man) %>% 
  arrange(TrialID)

stopifnot(all.equal(d1$TrialID, d2$TrialID))

# Compare datasets -----

# remove extra cols from 15% original manual extraction
d2 <- d2 %>% 
  select(-c(...40, ContradictionBlinding, ContradictionControlArm,
            ContradictionProspectiveRegistration, 
            ContradictionRandomisation))

# correct some "NA" s
d2$Exclude[d2$Exclude == "NA"] <- NA

stopifnot(all.equal(colnames(d1), colnames(d2)))

d_com <- d1 # combined dataset
# compare every col and make NA where there are any discrepancies
v_names <- colnames(d1)
for (v in v_names){
  x <- which(d1[v] != d2[v] | is.na(d1[v]) | is.na(d2[v]))
  d_com[x, v] <- NA
}

# for exclude and notes, we want to keep all of both to review.
d_com$Exclude1 <- d1$Exclude
d_com$Exclude2 <- d2$Exclude
d_com$Notes1 <- d1$Notes
d_com$Notes2 <- d2$Notes

d_com <- select(d_com, -Notes)

# Write data -----
write.csv(d_com, output_path)










