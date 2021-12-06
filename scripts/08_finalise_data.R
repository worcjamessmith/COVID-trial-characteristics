# Read in consolidated data and do some final checks/ corrections

# Load -----
library(tidyverse)
library(readxl)

# Inputs -----
# file path for all excels to import
file_compared <- "data/manual_processing/manual_extraction/completed/Compared.xlsx"
file_check <- "data/automated_extraction/automated_extraction.csv"
output_path <- "data/final_dataset.csv"

# Import data -----
d <- read_xlsx(file_compared, guess_max = 2541, na = "NA") %>% 
  arrange(TrialID)
d_check <- read_csv(file_check, guess_max = 2541) %>% 
  arrange(TrialID)

# Correct entries -----
d$randomisation[which(d$control_arm == "No" & d$randomisation == "No")] <- 
  "Not applicable"

# "trials with a single group will be considered...unblinded"
d$blinding[which(d$control_arm == "No" & d$blinding == "Yes")] <- "No"

# manually reviewed those with unreported control arm. This has placebo
# suggesting it it not single arm
d$control_arm[1075] <- "Yes"

# there are more NA in the compared data, probably because a few cols ended up
# differing between the manual extractions. Replace with earlier version that is
# more complete. (not used in analysis)
d$Scientific_title <- d_check$Scientific_title

# Some checks -----
stopifnot(!any(d$control_arm == "No" & d$randomisation == "Yes"))
stopifnot(all.equal(d$TrialID, d_check$TrialID))
stopifnot(all.equal(d$url, d_check$url))

# there are no NAs in cols used for modelling, except for sample size. I have
# manually checked those entry and no sample size is given

d <- d %>% 
  filter(Exclude_decision != "Yes" |
           is.na(Exclude_decision))

d <- d %>% 
  select(-c(Column1, Exclude, Exclude1, Exclude2, Notes1, 
            Notes2, Exclude_decision, Reason))

d <- d %>% 
  select(
    TrialID, url, Scientific_title, Conditions, study_arm, # general info
    Date_enrollment_format,
    control_arm, randomisation, blinding, prospective, # outcomes
    Source_registry, phase_clean, 
    region_Africa: region_Oceania, 
    multicentre, primary_purpose, sponsor_type, sample_size,
    vaccine, conventional, traditional,
    subject_blind:analyst_blind,
    everything()
    )

d <- d %>% 
  rename(start_date = Date_enrollment_format,
         source_registry = Source_registry)

colnames(d)

# Write data -----
write_csv(d, output_path)

