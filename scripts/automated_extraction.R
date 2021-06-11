require(tidyverse)

# Ictrp_im847 ----- 
# Repeat for new data

d <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/covid847.csv")

ct <- d %>% 
  filter(Source_registry == "CT.gov")

design <- ct %>% 
  select(TrialID, Study_design)

# these are the outputs we need:
# Use of control arm # if randomised, we assume there was a control arm (not implmented yet)
# Randomisation  # if single arm, we assume not randomised
# Blinding
# Subject blind*
# Caregiver blind*
# Investigator blind*
# Outcome blind*
# Analyst blind*
# Primary purpose

# this works and doesn't require all the messing around separating. Much easier
design <- design %>% 
  mutate(
    # "Sequential Assignment" is variable in meaning so we don't assign that
    # here
    control_arm = case_when(grepl(
      "Single Group", Study_design) ~ "No",
      grepl("Allocation: Randomized|Parallel|Crossover|Factorial", Study_design) ~ "Yes"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Allocation: Non-Randomized", Study_design) ~ "No",
      grepl("Allocation: Randomized", Study_design) ~ "Yes",
      grepl("Allocation: N/A", Study_design) ~ "Unreported"),
    # if randomisation missing, state unreported
    randomisation = case_when(
      is.na(randomisation) ~ "Unreported",
      TRUE ~ randomisation),
    subject_blind = ifelse(grepl("Participant", Study_design), 
                         "Yes", "No"),
    caregiver_blind = ifelse(grepl("Care Provider", Study_design), 
                           "Yes", "No"), 
    investigator_blind = ifelse(grepl("Investigator", Study_design), 
                              "Yes", "No"),
    outcome_blind = ifelse(grepl("Outcomes Assessor", Study_design), 
                         "Yes", "No"),
    # blinding is reported for all of the trials so according to our definition
    # we can assume analyst not blinded. Best to do this at end though with all
    # trials
    analyst_blind = NA,
    # all have blinding reported so we know blinded if not stated that open label
    blinding = ifelse(grepl("None \\(Open Label\\)", Study_design),
                      "No", "Yes"),
    primary_purpose = case_when(
      grepl("Primary purpose: Treatment", Study_design) ~ "Treatment",
      grepl("Primary purpose: Prevention", Study_design) ~ "Prevention",
      TRUE ~ "Other")
    )

filter(ct, TrialID == "NCT02723812") %>% 
  View()


# Notes -----

unique(design$allocation)
anyNA(design$allocation)
unique(design$intervention_model)
anyNA(design$intervention_model)
# sequential - is this controlled? 
  # NCT03801915 = Yes
  # NCT04162340 = No 
  # NCT04061590 = Yes
  # NCT03895879 = Yes
  # NCT03844217 = No
  # NCT04082325 = Yes
  # NCT04104672 = Yes
  # NCT04185090 = Yes
  # NCT04187404 = No
# looks like this needs to be manual
unique(design$masking)
anyNA(design$masking)
unique(design$primary_purpose_ct)
anyNA(design$primary_purpose_ct)



