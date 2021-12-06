# Generate final list of included trials and sample them to make dataset for
# manual data extraction quality control step, and datasets for automated
# extraction.

# Load -----
library(tidyverse)
library(readxl)

# Inputs -----
# root for most files
root_path <- "data/manual_processing/eligibility_screen/"

# screened file location
screened_path <- "intermediate/screened/"
# files with list of trials with agreed inclusion previously
covid_agree <- "covid_agree_include.csv"
ictrp_main_agree <- "ictrp_main_agree_include.csv"
ictrp_im_agree <- "ictrp_im_agree_include.csv"
# consensus decision files from previous disagreements
covid_consolidated <- "covid_disagreements_consolidated.xlsx"
ictrp_main_consolidated <- "ictrp_main_disagreements_consolidated.xlsx"
ictrp_im_consolidated <- "ictrp_im_disagreements_consolidated.xlsx"

# files originally generated to screen. These are needed so we know the order
# the data was originally in (as we include them by including and excluding in
# that order) and as it's easier to use these than reformat the imported excels
covid_orig <- "covid.csv"
ictrp_main_orig <- "ictrp_main.csv"
ictrp_im_orig <- "ictrp_im.csv"

# output for eligible trials
eligible_output_path <- "eligible_trials/"

# output for subset of trials that will be manual extracted (not in ref to root)
manual_output_path <- "data/manual_processing/manual_extraction/"
random_sample_folder <- "random_sample/"

# Create folders -----
if(!dir.exists(manual_output_path)) {
  dir.create(manual_output_path)
}
if(!dir.exists(paste0(manual_output_path, random_sample_folder))) {
  dir.create(paste0(manual_output_path, random_sample_folder))
}

if(!dir.exists(paste0(root_path, eligible_output_path))) {
  dir.create(paste0(root_path, eligible_output_path))
}

# Import data ----
# COVID
# read the dfs in as characters to make binding easy later
covid_agree <- read_csv(paste0(root_path, screened_path, covid_agree),
                        col_types = cols(.default = "c"))
covid_consolidated <- read_xlsx(paste0(root_path, screened_path, 
                                       covid_consolidated), 
                                col_types = "text")
covid_orig <- read_csv(paste0(root_path, covid_orig)) %>% 
  select(-Include)

# ICTRP main 
ictrp_main_agree <- read_csv(paste0(root_path, screened_path, ictrp_main_agree),
                             col_types = cols(.default = "c"))
ictrp_main_consolidated <- read_xlsx(paste0(root_path, screened_path, 
                                            ictrp_main_consolidated),
                                     col_types = "text")
ictrp_main_orig <- read_csv(paste0(root_path, ictrp_main_orig)) %>% 
  select(-Include)

# ICTRP indication matched
ictrp_im_agree <- read_csv(paste0(root_path, screened_path, ictrp_im_agree),
                             col_types = cols(.default = "c"))
ictrp_im_consolidated <- read_xlsx(paste0(root_path, screened_path, 
                                            ictrp_im_consolidated),
                                     col_types = "text")
ictrp_im_orig <- read_csv(paste0(root_path, ictrp_im_orig)) %>% 
  select(-Include)

# Combine datasets -----
# COVID
# select those we agreed to include
covid_consolidated <- covid_consolidated %>% 
  filter(Decision == 1) %>% 
  select(-c(Include_ct, Include_js, Decision, Consensus_notes))

covid_include <- bind_rows(covid_agree, covid_consolidated)

# some of the EU trials have different trialIDs but the same URL. They are
# basically each the same trial. We keep he first one of each (they are already
# randomly ordered). Duplicated ones can be seen using:

# covid_include %>% 
#   group_by(url) %>% 
#   filter(n() != 1) %>% 
#   View()
covid_include <- covid_include %>%
  group_by(url) %>%
  filter(row_number()==1)

# keep only those trials that we want to include, and keep the order of the
# original df
keep <- covid_orig$TrialID %in% covid_include$TrialID
covid_include <- covid_orig[keep, ]

# limit sample size as per registration 
covid847 <- covid_include[1:847, ]

# ICTRP main
ictrp_main_consolidated <- ictrp_main_consolidated %>% 
  filter(Decision == 1) %>% 
  select(-c(Include_ct, Include_js, Decision, Consensus_notes))

ictrp_main_include <- bind_rows(ictrp_main_agree, ictrp_main_consolidated)

# as with covid arm, there are effectively some duplicates
ictrp_main_include <- ictrp_main_include %>% 
  group_by(url) %>%
  filter(row_number()==1)

# keep only those trials that we want to include, and keep the order of the
# original df
keep <- ictrp_main_orig$TrialID %in% ictrp_main_include$TrialID
ictrp_main_include <- ictrp_main_orig[keep, ]

# limit sample size as per registration 
ictrp_main847 <- ictrp_main_include[1:847, ]

# ICTRP indication matched
ictrp_im_consolidated <- ictrp_im_consolidated %>% 
  filter(Decision == 1) %>% 
  select(-c(Include_ct_ra, Include_js, Decision, Consensus_notes))

ictrp_im_include <- bind_rows(ictrp_im_agree, ictrp_im_consolidated)

# as with other arms, there are effectively some duplicates
ictrp_im_include <- ictrp_im_include %>% 
  group_by(url) %>%
  filter(row_number()==1)

# keep only those trials that we want to include, and keep the order of the
# original df
keep <- ictrp_im_orig$TrialID %in% ictrp_im_include$TrialID
ictrp_im_include <- ictrp_im_orig[keep, ]

# limit sample size as per registration 
ictrp_im847 <- ictrp_im_include[1:847, ]

rm(covid_agree, covid_consolidated, covid_orig, 
   ictrp_main_agree, ictrp_main_consolidated, ictrp_main_orig,
   ictrp_im_agree, ictrp_im_consolidated, ictrp_im_orig)
  
# Random sample -----
# random sample stratified by source registry for manual extraction 
# Ordered by Source_registry for convenience when extracting
set.seed(1234)
covid_sample <- covid847 %>% 
  group_by(Source_registry) %>% 
  sample_frac(0.15) %>% 
  arrange(Source_registry) %>% 
  select(TrialID, url, Scientific_title, Conditions, everything())

ictrp_main_sample <- ictrp_main847 %>% 
  group_by(Source_registry) %>% 
  sample_frac(0.15) %>% 
  arrange(Source_registry) %>% 
  select(TrialID, url, Scientific_title, Conditions, everything())

ictrp_im_sample <- ictrp_im847 %>% 
  group_by(Source_registry) %>% 
  sample_frac(0.15) %>% 
  arrange(Source_registry) %>% 
  select(TrialID, url, Scientific_title, Conditions, everything())

# Write output files-----
# main output files
write_csv(covid847, 
          paste0(root_path, eligible_output_path, "covid847.csv"))
write_csv(ictrp_main847,
          paste0(root_path, eligible_output_path, "ictrp_main847.csv"))
write_csv(ictrp_im847,
          paste0(root_path, eligible_output_path, "ictrp_im847.csv"))

# keep all of those thatwe agreed met criteria (not just 847, in case we later
# need to add more)
write_csv(covid_include, 
          paste0(root_path, eligible_output_path, "covid_all.csv"))
write_csv(ictrp_main_include, 
          paste0(root_path, eligible_output_path, "ictrp_main_all.csv"))
write_csv(ictrp_im_include, 
          paste0(root_path, eligible_output_path, "ictrp_im_all.csv"))

# random samples for manual extraction
write_csv(covid_sample, 
          paste0(manual_output_path, random_sample_folder, "covid_sample.csv"))
write_csv(ictrp_main_sample, 
          paste0(manual_output_path, random_sample_folder, "ictrp_main_sample.csv"))
write_csv(ictrp_im_sample, 
          paste0(manual_output_path, random_sample_folder, "ictrp_im_sample.csv"))
