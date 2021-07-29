# Compare automated extraction to manual data extraction and create and
# consolidate datasets for remaining manual extraction 

# Any differences between manual and automated are stored in
# "data/automated_extraction/compare_to_manual/" for reference.

# Inputs -----
output_path <- "data/manual_processing/manual_extraction/"
output_path_difs <- "data/automated_extraction/compare_to_manual/"

# Load -----
library(tidyverse)
library(readxl)

# Create folder -----
if(!dir.exists(output_path_difs)){
  dir.create(output_path_difs)
} 

# Read data -----
d_man <- read_xlsx("data/manual_processing/manual_extraction/Manual_extraction_all.xlsx") %>% 
  arrange(TrialID)
d_all <- read_csv("data/automated_extraction/automated_extraction.csv")

# Wrangle -----

d_man <- d_man %>% 
  filter(Exclude != "Yes")

# subset of full dataset that were manually extracted
d_sub <- d_all[(d_all$TrialID %in% d_man$TrialID),] %>% 
  arrange(TrialID)

stopifnot(all.equal(d_man$TrialID, d_sub$TrialID))

# fix dates
d_man$`Start Date` <- as.Date(d_man$`Start Date`, format = "%d/%m/%Y")

# Control arm -----
# only one discrepancy where I have assigned the control arm

x <- which(d_man$UseOfControlArm != d_sub$control_arm)

difs <- 
  bind_cols(d_sub, d_man$UseOfControlArm) %>% 
  rename(control_manual = "...40") %>% 
  select(TrialID:control_arm, control_manual, everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "control_arm.csv"))

# Randomisation -----
# only one discrepancy and it is in NCT so >95% agreement

d_man[d_man$Randomisation == "NA",]$Randomisation <- "Not applicable"

x <- which(d_man$Randomisation != d_sub$randomisation)

difs <- 
  bind_cols(d_sub, d_man$Randomisation) %>% 
  rename(manual = "...40") %>% 
  select(TrialID:randomisation, manual, everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "randomisation.csv"))

sum(d_man$Randomisation != d_sub$randomisation, na.rm = T)

# Blinding -----
# matches exactly after correcting errors in manual data

x <- which(d_man$Blinding != d_sub$blinding)
         
difs <- 
  bind_cols(d_sub, d_man$Blinding) %>% 
  rename(manual = "...40") %>% 
  select(TrialID:blinding, manual, everything()) %>% 
  .[x,]

# need to label the unreported ones as NA 
write_csv(difs, paste0(output_path_difs, "blinding.csv"))

# Prospective reg-----

# some disagreement, unsure what's going on 

x <- which(d_man$ProspectiveRegistration != d_sub$prospective)

difs <- 
  bind_cols(d_sub, d_man$ProspectiveRegistration) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Date_registration_format, Date_enrollment_format, 
         prospective, manual, everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "prospective.csv"))

# Start date -----

# one disagreement with PACTR which is because we have the anticipated start
# date rather than actual
x <- which(d_man$`Start Date` != d_sub$Date_enrollment_format)

difs <- 
  bind_cols(d_sub, d_man$`Start Date`, d_man$ProspectiveRegistration) %>% 
  rename(manual = "...40", 
         Prospective_manual = "...41") %>% 
  select(TrialID, Date_registration_format, Date_enrollment_format, 
         manual, prospective, Prospective_manual, everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "start_date.csv"))

# Sponsor type ----
d_man[d_man$SponsorType == "Industry",]$SponsorType <-  "industry"
d_man[d_man$SponsorType == "Investigator",]$SponsorType <-  "investigator"
d_man[d_man$SponsorType == "Non-industry",]$SponsorType <-  "non_industry"

x <- which(d_sub$sponsor_type != d_man$SponsorType)

difs <- 
  bind_cols(d_sub, d_man$SponsorType) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, sponsor_type, manual, Primary_sponsor,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "sponsor.csv"))

# sponsor performance is acceptable 

table(d_man$SponsorType)
table(d_sub$sponsor_type, useNA = "a")

# Vaccine, traditional, conventional -----

# performance of these is good

table(d_man$Vaccine, useNA = "a")
table(d_sub$vaccine, useNA = "a")
# vaccine good 
x <- which(d_man$Vaccine != d_sub$vaccine)
x <- which(d_man$Traditional != d_sub$traditional)

paste(d_sub[x,]$TrialID, d_sub[x,]$Interventions)
d_man[x,]$Traditional
d_sub[x,]$traditional

x <- which(d_man$Conventional != d_sub$conventional)
paste(d_sub[x,]$TrialID, d_sub[x,]$Interventions)
d_man[x,]$Conventional
d_sub[x,]$conventional

# Phase ----- 
# Only one difference which is in EUCTR 
table(d_sub$Source_registry)

d_man <- d_man %>% 
  mutate(Phase = case_when(
    Phase == "1" ~ "Phase 1",
    Phase == "2" ~ "Phase 2", 
    Phase == "3" ~ "Phase 3",
    Phase == "4" ~ "Phase 4",
    TRUE ~ Phase
  ))

x <- which(d_man$Phase != d_sub$phase_clean)
table(d_sub$phase_clean)
table(d_man$Phase)

difs <- 
  bind_cols(d_sub, d_man$Phase) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Phase, phase_clean, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "phase.csv"))

# Sample size -----
# no differences 
table(d_man$SampleSize, useNA = "a")
table(d_sub$sample_size, useNA = "a")
x <- which(d_man$SampleSize != d_man$SampleSize)

# Primary purpose ----- 

# One error in IRCT, two in NCT. There was one difference for TCTR20190109001
# which in manual was originally "other". Reclassified as "treatment" but it
# could be argued either way

table(d_man$PrimaryPurpose, useNA = "a")
table(d_sub$primary_purpose, useNA = "a")
x <- which(d_man$PrimaryPurpose != d_sub$primary_purpose)

difs <- 
  bind_cols(d_sub, d_man$PrimaryPurpose) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Study_design, primary_purpose, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "purpose.csv"))

# Geographic regions ----- 

# Africa - acceptable
x <- which(d_man$Africa != d_sub$region_Africa)

difs <- 
  bind_cols(d_sub, d_man$Africa) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_Africa, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_africa.csv"))

# North america - acceptable 
x <- which(d_man$NorthernAmerica != d_sub$region_N_America)

difs <- 
  bind_cols(d_sub, d_man$NorthernAmerica) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_N_America, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_n_america.csv"))

# Latin America acceptable (one difference)

x <- which(d_man$LatinAmericaCarribbean != d_sub$region_L_America)

difs <- 
  bind_cols(d_sub, d_man$LatinAmericaCarribbean) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_L_America, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_l_america.csv"))

# Asia- Acceptable 

x <- which(d_man$Asia != d_sub$region_Asia)
difs <- 
  bind_cols(d_sub, d_man$Asia) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_Asia, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_Asia.csv"))

# Europe - acceptable 
x <- which(d_man$Europe != d_sub$region_Europe)
difs <- 
  bind_cols(d_sub, d_man$Europe) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_Europe, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_Europe.csv"))

# Oceania - acceptable 
x <- which(d_man$Oceania != d_sub$region_Oceania)
difs <- 
  bind_cols(d_sub, d_man$Oceania) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, region_Oceania, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "reg_Oceania.csv"))

# Multicentre -----

# acceptable 

x <- which(d_man$MultiCentre != d_sub$multicentre)
difs <- 
  bind_cols(d_sub, d_man$MultiCentre) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Countries, multicentre, manual,
         everything()) %>% 
  .[x,]

write_csv(difs, paste0(output_path_difs, "multicentre.csv"))

# Sponsors for manual extraction -----
x <- is.na(d_all$sponsor_type)

sponsor <- d_all %>% 
  select(TrialID, url, Scientific_title, Primary_sponsor, sponsor_type) %>% 
  .[x,]

write_csv(sponsor, paste0(output_path, "sponsor.csv"))

table(d_all$sponsor_type, useNA = "a")

# Summarise data -----
d <- d_all %>% 
  select(control_arm:traditional) %>% 
  select(-sample_size)
sapply(d, table, useNA = "a")

# Bridging flag -----
table(d_all$Bridging_flag, useNA = "a")

sum(d_all$Bridging_flag != "FALSE" & d_all$prospective == "No")
# only 59 to review 


d_all %>% 
  sample_n(100) %>% 
  arrange(study_arm) %>% 
  select(Bridging_flag, everything()) 


d_man %>% 
  select(UseOfControlArm:ContradictionProspectiveRegistration) %>% 
  sapply(function(x) table(x, useNA = "a"))

d_sub %>% 
  select(control_arm:traditional) %>% 
  sapply(function(x) table(x, useNA = "a"))


unique(d_all$Primary_sponsor[is.na(d_all$sponsor_type)])



