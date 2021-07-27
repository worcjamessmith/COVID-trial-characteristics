# compare to manual data extraction and create and consolidate manual extraction
# efforts

# Load -----
library(tidyverse)
library(readxl)

# Inputs -----
output_path <- "data/manual_processing/manual_extraction/"

# Read data -----
d_man <- read_xlsx("data/manual_processing/manual_extraction/Manual_extraction_all.xlsx") %>% 
  arrange(TrialID)
d_all <- read_csv("2021.07.01_check_JS.csv")

d_man <- d_man %>% 
  filter(Exclude != "Yes")

d_sub <- d_all[(d_all$TrialID %in% d_man$TrialID),] %>% 
  arrange(TrialID)

stopifnot(all.equal(d_man$TrialID, d_sub$TrialID))

# Sponsors for manual extraction -----

x <- is.na(d_all$sponsor_type)

sponsor <- d_all %>% 
  select(TrialID, url, Scientific_title, Primary_sponsor, sponsor_type) %>% 
  .[x,]

write_csv(sponsor, paste0(output_path, "sponsor.csv"))


table(d_all$sponsor_type, useNA = "a")

unique(d_all$Primary_sponsor[is.na(d_all$sponsor_type)])



# control arm
x <- which(d_man$UseOfControlArm != d_sub$control_arm | 
        is.na(d_man$UseOfControlArm) |
        is.na(d_sub$control_arm))

difs <- 
  bind_cols(d_sub, d_man$UseOfControlArm) %>% 
  rename(control_manual = "...40") %>% 
  select(TrialID:control_arm, control_manual, everything()) %>% 
  .[x,]

write_csv(difs, "difs_for_nick/control_arm_manual.csv")

# randomisation -----

x <- which(d_man$Randomisation != d_sub$randomisation | 
             is.na(d_man$Randomisation) |
             is.na(d_sub$randomisation))

difs <- 
  bind_cols(d_sub, d_man$Randomisation) %>% 
  rename(manual = "...40") %>% 
  select(TrialID:randomisation, manual, everything()) %>% 
  .[x,]

write_csv(difs, "difs_for_nick/randomisation_manual.csv")

# looks pretty good

# blinding -----
# extremely good 
x <- which(d_man$Blinding != d_sub$blinding | 
             is.na(d_man$Blinding) |
             is.na(d_sub$blinding))

difs <- 
  bind_cols(d_sub, d_man$Blinding) %>% 
  rename(manual = "...40") %>% 
  select(TrialID:blinding, manual, everything()) %>% 
  .[x,]

write_csv(difs, "difs_for_nick/blinding_manual.csv")

# prospective reg-----

# some disagreement, unsure what's going on 

x <- which(d_man$ProspectiveRegistration != d_sub$prospective | 
             is.na(d_man$ProspectiveRegistration) |
             is.na(d_sub$prospective))

difs <- 
  bind_cols(d_sub, d_man$ProspectiveRegistration) %>% 
  rename(manual = "...40") %>% 
  select(TrialID, Date_registration_format, Date_enrollment_format, 
         prospective, manual, everything()) %>% 
  .[x,]

write_csv(difs, "difs_for_nick/prospective_manual.csv")

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

write_csv(difs, "difs_for_nick/sponsor_manual.csv")

# Vaccine, traditional, conventional -----

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


table(d_man$SponsorType)
table(d_sub$conventional)


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




