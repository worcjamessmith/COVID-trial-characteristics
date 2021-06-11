require(tidyverse)

# Combine datasets -----
main <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_main847.csv")
im <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_im847.csv")
d <- bind_rows(main, im, .id = "id")
d$id[d$id == 1] <- "main"
d$id[d$id == 2] <- "im"

# TRIAL DESIGN -----

# these are the outputs we need:
# Use of control arm # if randomised, there was a control arm
# Randomisation  # if single arm, not randomised
# Blinding
# Subject blind*
# Caregiver blind*
# Investigator blind*
# Outcome blind*
# Analyst blind*
# Primary purpose

# * if at least one of the specific parties blinded is stated, the others will be
# recorded as “No”, unless it is clear that at least one other party has been
# blinded (e.g. if the trial is described as double-blind but only one specific
# party is described as blinded).In the latter case, the other parties will be
# reported as unrecorded.If none are stated, they will all be recorded as
# unreported

# Clinicaltrials.gov ----- 

# works for all three datasets

# d <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_main847.csv")

ct <- d %>% 
  filter(Source_registry == "CT.gov")

design <- ct %>% 
  select(TrialID, Study_design)

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

# Euctr -----

eu <- d %>% 
  filter(Source_registry == "EUCTR")

design <- eu %>% 
  select(TrialID, Study_design)

design %>%
  mutate(
    # all in main or im are either controlled:yes or no
    control_arm = case_when(
      grepl("Controlled: yes", Study_design) ~ "Yes",
      grepl("Controlled: no", Study_design) ~ "No"),
    # all in main or im randomised:yes or no apart from one which is not controlled
    # may need to add an unreported for other arms
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Randomised: no", Study_design) ~ "No",
      grepl("Randomised: yes", Study_design) ~ "Yes",
      grepl("Randomised:<br>", Study_design) ~ "Unreported"),
    # all in main or im meet these
    blinding = case_when(
      grepl("blind: yes", Study_design) ~ "Yes", 
      grepl("blind: no", Study_design) ~ "No", 
      grepl("blind:<br>", Study_design) ~ "Unreported"),
    # who specifically is blinded is not stated
    # many say double or single but not who 
    subject_blind = "Unreported", 
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = "Unreported",
    analyst_blind = "Unreported",
    primary_purpose = "Unreported"
    )

# check each item captures all trials. Done for im and main
# all contain "blind"
i <- grep("Randomised: yes|Randomised: no", 
            design$Study_design)

design$Study_design[!i]


# Phase -----
# data exploration 
main <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_main847.csv")
im <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_im847.csv")
covid <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/covid847.csv")

table(main$Source_registry)
table(im$Source_registry)
table(covid$Source_registry)

p <- c(unique(main$Phase), unique(im$phase), unique(covid$Phase))
p <- unique(p)
which(is.na(main$Phase))
which(is.na(im$Phase))
which(is.na(covid$Phase))


# christiana to do countries

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



