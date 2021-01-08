
# Load -----
library(tidyverse)
library(readxl)
library(writexl)

# Inputs -----
# path to screened files which are compared
root_path <- "data/manual_processing/eligibility_screen/"
screened_path <- "intermediate/screened/"
covid_js <- "covid_JS.xlsx"
covid_ct <- "covid_CT.xlsx"
ictrp_main_js <- "ictrp_main_JS.xlsx"
ictrp_main_ct <- "ictrp_main_CT.xlsx"

# output goes here
screening_path <- "intermediate/for_screening/"

# file names that were created by the previous script. Used for checking that
# there have been no row deletions and and reordering data. They are in the
# root_path
covid_orig <- "covid.csv"
ictrp_main_orig <- "ictrp_main.csv"

# Import data -----
covid_js <- read_xlsx(paste0(root_path, screened_path, covid_js), 
                      guess_max = 21474836,
                      na = "NA")
covid_ct <- read_xlsx(paste0(root_path, screened_path, covid_ct), 
                      guess_max = 21474836,
                      na = "NA")
covid_orig <- read_csv(paste0(root_path, covid_orig))

ictrp_main_js <- read_xlsx(paste0(root_path, screened_path, ictrp_main_js), 
                           guess_max = 21474836,
                           na = "NA")
ictrp_main_ct <- read_xlsx(paste0(root_path, screened_path, ictrp_main_ct), 
                           guess_max = 21474836,
                           na = "NA")
ictrp_main_orig <- read_csv(paste0(root_path, ictrp_main_orig))

# Clean -----
# Two extra rows in covid_ct to remove (checked manually that are just
# NA and random value)
covid_ct <- covid_ct[1:3528, ]

# Four extra rows in ictrp_main_ct to remove (checked manually)
ictrp_main_ct <- ictrp_main_ct[1:3501, ]

# both ictrp_main_js and ictrp_main_ct have an extra row which must have been
# introduced in error in an earlier step. It is just part of another
# registration that has been split across two lines. It does not include any
# important information so we remove it from both 
text <- setdiff(ictrp_main_ct$TrialID, ictrp_main_orig$TrialID)
ictrp_main_js <- filter(ictrp_main_js, TrialID != text)
ictrp_main_ct <- filter(ictrp_main_ct, TrialID != text)

# Checks and wrangling -----
# Check key variables are the same as original data and in correct order
all.equal(covid_js$TrialID, covid_orig$TrialID)
all.equal(covid_js$url, covid_orig$url)

all.equal(ictrp_main_js$TrialID, ictrp_main_orig$TrialID)
all.equal(ictrp_main_js$url, ictrp_main_orig$url)

# order is not as it was in covid_ct or ictrp_main_ct - reorder to the same as
# original
re_order <- match(covid_orig$TrialID, covid_ct$TrialID)
covid_ct <- covid_ct[re_order, ]
re_order <- match(ictrp_main_orig$TrialID, ictrp_main_ct$TrialID)
ictrp_main_ct <- ictrp_main_ct[re_order, ]

# check that they are now equal to original and js
all.equal(covid_orig$TrialID, covid_ct$TrialID)
all.equal(covid_orig$url, covid_ct$url)
all.equal(covid_js$TrialID, covid_ct$TrialID)

all.equal(ictrp_main_orig$TrialID, ictrp_main_ct$TrialID)
all.equal(ictrp_main_orig$url, ictrp_main_ct$url)
all.equal(ictrp_main_js$TrialID, ictrp_main_ct$TrialID)

# Compare eligibility screen ----- 

# COVID trials
# 1270 rows have been screened by both extractors, so we only look at these
x <- covid_js$Include[1:1270]
y <- covid_ct$Include[1:1270]

# Keep any that don't match or that have NA (e.g. F:NA , NA:T, NA:NA pairs).
# There shouldn't be any NAs at this stage 
keep <- (x != y) | (is.na(x)) | (is.na(y))
# Get the row indices
keep <- which(keep)

# proportion disagreement out of those assessed by both extractors
length(keep)/1270 # 0.05984252

# create df of disagreements and original assessor decisions
covid_disagree <- bind_cols(covid_ct[keep, 1], covid_js[keep, 1], covid_orig[keep, ]) %>% 
  rename(Include_ct = Include...1, 
         Include_js = Include...2, 
         Decision = Include...3) %>% 
  mutate(Consensus_notes = NA) %>% 
  select(Include_ct, Include_js, Decision, Consensus_notes, everything())

# Main trials
# 1969 rows have been screened by both extractors, so we only look at these
x <- ictrp_main_js$Include[1:1969]
y <- ictrp_main_ct$Include[1:1969]

# Keep any that don't match or that have NA (e.g. F:NA , NA:T, NA:NA pairs).
# There shouldn't be any NAs at this stage 
keep <- (x != y) | (is.na(x)) | (is.na(y))
# Get the row indices
keep <- which(keep)

# proportion disagreement out of those assessed by both extractors
length(keep)/1969 # 0.07211783

ictrp_main_disagree <- bind_cols(ictrp_main_ct[keep, 1], ictrp_main_js[keep, 1], ictrp_main_orig[keep, ]) %>% 
  rename(Include_ct = Include...1, 
         Include_js = Include...2, 
         Decision = Include...3) %>% 
  mutate(Consensus_notes = NA) %>% 
  select(Include_ct, Include_js, Decision, Consensus_notes, everything())

# Write output files -----
write_csv(covid_disagree, 
          paste0(root_path, screening_path, "covid_disagreements.csv"))
write_csv(ictrp_main_disagree, 
          paste0(root_path, screening_path, "ictrp_main_disagreements.csv"))

