

# Load -----
require(tidyverse)

# Inputs -----
main <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_main847.csv")
im <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_im847.csv")
covid <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/covid847.csv")

# Combine datasets -----
# bridging flags are in different formats (ictrp lists the ID, covid is T|F)
covid$Bridging_flag <- as.character(covid$Bridging_flag)

d <- bind_rows(main, im, covid, .id = "id")
d$id[d$id == 1] <- "main"
d$id[d$id == 2] <- "im"
d$id[d$id == 3] <- "covid"
d <- rename(d, study_arm = id)

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
# party is described as blinded). In the latter case, the other parties will be
# reported as unrecorded.If none are stated, they will all be recorded as
# unreported

# clinicaltrials.gov ----- 

# works for all three arms 

ct <- d %>% 
  filter(Source_registry == "CT.gov")

design <- ct %>% 
  select(study_arm, TrialID, Study_design)

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
    # we can assume analyst not blinded
    analyst_blind = "No",
    # all have blinding reported so we know blinded if not stated that open label
    blinding = ifelse(grepl("None \\(Open Label\\)", Study_design),
                      "No", "Yes"),
    primary_purpose = case_when(
      grepl("Primary purpose: Treatment", Study_design, 
            ignore.case = T) ~ "Treatment",
      grepl("Primary purpose: Prevention", Study_design,
            ignore.case = T) ~ "Prevention",
      # if it says primary purpose but isn't one of above it is other
      grepl("Primary purpose", Study_design, 
            ignore.case = T) ~ "Other")
    )

# euctr -----

# checked for all study arms

eu <- d %>% 
  filter(Source_registry == "EUCTR")

design <- eu %>% 
  select(study_arm, TrialID, Study_design)

design <- design %>%
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
    primary_purpose = NA
    )

sum(grepl("blind: yes|blind: no|blind:<br>", design$Study_design))
anyNA(design$blinding)

# irct -----

# checked for all study_arms

irct <- d %>% 
  filter(Source_registry == "IRCT")

design <- irct %>% 
  select(study_arm, TrialID, Study_design) 

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl("Assignment: Parallel|Assignment: Crossover", Study_design) ~ "Yes",
      grepl("Assignment: Single", Study_design) ~ "No",
      # there are some that are randomized so must be controlled
      grepl("Randomization: Randomized", Study_design) ~ "Yes"),
    randomisation = case_when(
      # control_arm == "No" ~ "No", # remember to add this, though don't think needed
      grepl("Randomization: Not randomized", Study_design) ~ "No",
      grepl("Randomization: Randomized", Study_design) ~ "Yes",
      grepl("Randomization: N/A", Study_design) ~ "Unreported"),
    blinding = case_when(
      grepl("Blinding: Single blinded|Blinding: Double blinded|Blinding: Triple blinded",
            Study_design) ~ "Yes",
      grepl("Blinding: Not blinded", Study_design) ~ "No"),
    subject_blind = "Unreported", 
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = "Unreported",
    analyst_blind = "Unreported",
    primary_purpose = case_when(
      grepl("Purpose: Treatment", Study_design) ~ "Treatment",
      grepl("Purpose: Prevention", Study_design) ~ "Prevention",
      grepl("Purpose: Health service research|Purpose: Supportive|Purpose: Other",
            Study_design) ~ "Other"))

# jprn -----

# works for all 
# double check this based on Nick conversation 

jprn <- d %>% 
  filter(Source_registry == "JPRN")

design <- jprn %>% 
  select(TrialID, Scientific_title, Study_design)

# define terms used to describe trials
control <- "parallel|Cross-over|Crossover|placebo-controlled|randomized controlled trial"
no_control <- "single arm|non-controlled|Uncontrolled|No control group"
blind <- "double-blind|double-masked|double blind|single blind|observer-blind|single-blind|evaluator-blind"

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design, ignore.case = T) ~ "Yes",
      grepl(no_control, Study_design, ignore.case = T) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("non-randomized", 
            design$Study_design, ignore.case = T) ~ "No",
      grepl("randomized|Random allocation", 
            design$Study_design, ignore.case = T) ~ "Yes",
      # this trial is randomized based on title
      TrialID == "JPRN-JapicCTI-194635" ~ "Yes"),
    blinding = case_when(
      grepl(blind, design$Study_design, ignore.case = T) ~ "Yes",
      grepl("open", design$Study_design, ignore.case = T) ~ "No"),
    subject_blind = "Unreported", 
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = case_when(
      grepl("observer-blind|evaluator-blind", 
            design$Study_design, ignore.case = T) ~ "Yes",
      TRUE ~ "Unreported"),
    analyst_blind = "Unreported")


# chictr -----

# many details missing, will need to try to get info from titles

chictr <- d %>% 
  filter(Source_registry == "ChiCTR")

design <- chictr %>% 
  select(TrialID, Scientific_title, Study_design)

# ADD case control here once decided
control <- "Parallel|Cross-over|Quasi-randomized controlled|Randomized parallel controlled trial|Factorial|Non randomized control"

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design) ~ "Yes",
      grepl("Single arm", Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Non randomized", Study_design) ~ "No",
      grepl("Randomized", 
            Study_design, ignore.case = T) ~ "Yes"),
    blinding = "Unreported",
    subject_blind = "Unreported", 
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = "Unreported",
    analyst_blind = "Unreported") 

unique(design$Study_design)

i <- grepl("Non randomized|randomized",
           design$Study_design, ignore.case = T)

design[!i,] 

# ctri -----

# covid ones look different but they just have \n which is why in csv you can't see it
# so this is feasible 
ctri <- d %>% 
  filter(Source_registry == "CTRI")

design <- ctri %>% 
  select(study_arm, TrialID, Scientific_title, Study_design)

control <- "parallel|Crossover|Factorial Trial|Cluster Randomized Trial|Multiple Arm Trial|Controlled Trial"
random <- "randomized|Computer generated randomization|Method of generating randomization sequence:Other|Stratified randomization|Stratified block randomization|Random Number Table"
# we consider not applicable stated to mean no blinding
no_blind <- "Blinding and masking:Open Label|Blinding and masking:Not Applicable"

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design, ignore.case = T) ~ "Yes",
      grepl("Single Arm Trial", Study_design, 
            ignore.case = T) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Non-randomized", Study_design,
            ignore.case = T) ~ "No",
      grepl(random, Study_design, 
            ignore.case = T) ~ "Yes"),
    # they all have a blinding field, and if not open label or 
    # not applicable, they have some blinding
    blinding = case_when(
      grepl(no_blind, Study_design,
            ignore.case = T) ~ "No",
      TRUE ~ "Yes"),
    subject_blind = ifelse(
      grepl("masking:Participant", design$Study_design, ignore.case = T), 
      "Yes", "No"),
    # we assume caregiver is no because at least one other 
    # party is specified for all trials with blinding
    caregiver_blind = "No",
    investigator_blind = ifelse(
      grepl("Investigator", design$Study_design, ignore.case = T), 
      "Yes", "No"),
    outcome_blind = ifelse(
      grepl("Outcome Assessor", design$Study_design, ignore.case = T), 
      "Yes", "No"),
    # we consider the data entry operator to be the analyst
    analyst_blind = ifelse(
      grepl("Date-entry Operator", design$Study_design, ignore.case = T), 
      "Yes", "No"))

# anzctr -----

anzctr <- d %>% 
  filter(Source_registry == "ANZCTR")

design <- anzctr %>% 
  select(study_arm, TrialID, Scientific_title, Study_design)

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl("Assignment: Parallel|controlled trial", 
            Study_design) ~ "Yes",
      grepl("Assignment: Single group", 
            Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Non-randomised", 
            Study_design) ~ "No",
      grepl("Randomised controlled trial", 
            Study_design) ~ "Yes"),
    blinding = case_when(
      grepl("Masking: Open", 
            Study_design) ~ "No",
      grepl("Masking: Blinded", 
            Study_design) ~ "Yes"),
    subject_blind = "Unreported",
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = "Unreported",
    analyst_blind = "Unreported",
    primary_purpose = case_when(
      grepl("Purpose: Treatment", 
            Study_design) ~ "Treatment",
      grepl("Purpose: Prevention", 
            Study_design) ~ "Prevention",
      grepl("Purpose: Diagnosis", 
            Study_design) ~ "Other")) 

# isrctn -----

isrctn <- d %>% 
  filter(Source_registry == "ISRCTN")

# not really standardise apart from purpose

design <- isrctn %>% 
  select(study_arm, TrialID, Scientific_title, Study_design)

design <- design %>% 
  mutate(
    randomisation = case_when(
      grepl("non-random", Study_design,
            ignore.case = T) ~ "No",
      grepl("random", Study_design,
            ignore.case = T) ~ "Yes"),
    control_arm = case_when(
      randomisation == "Yes" ~ "Yes",
      grepl("factorial-design", Study_design, 
            ignore.case = T) ~ "Yes",
      grepl("single-arm|historical control", Study_design) ~ "No"),
    blinding = case_when(
      grepl("blind", Study_design,
            ignore.case = T) ~ "Yes",
      grepl("open", Study_design, 
            ignore.case = T) ~ "No"),
    subject_blind = "Unreported",
    caregiver_blind = "Unreported",
    investigator_blind = "Unreported",
    outcome_blind = "Unreported",
    analyst_blind = "Unreported",
    primary_purpose = case_when(
      grepl("Treatment", Study_design) ~ "Treatment",
      grepl("Prevention", Study_design) ~ "Prevention",
      grepl("\\(Other", Study_design) ~ "Other"))

# kct -----
kct <- d %>% 
  filter(Source_registry == "KCT")

design <- kct %>% 
  select(study_arm, TrialID, Scientific_title, Study_design)

design <- design %>% 
  mutate(
    control_arm = case_when(
      grepl("Intervention Model : Parallel", 
            Study_design) ~ "Yes", 
      grepl("Intervention Model : Single Group", 
            Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No", 
      grepl("RCT", Study_design) ~ "Yes"),
    blinding = case_when(
      grepl("Masking : Open", Study_design) ~ "No",
      grepl("Masking : Double", Study_design) ~ "Yes"),
    subject_blind = case_when(
      grepl("Subject", Study_design) ~ "Yes"),
    caregiver_blind = "Unreported",
    investigator_blind = case_when(
      grepl("Investigator", Study_design) ~ "Yes"),
    outcome_blind = case_when(
      grepl("Outcome Accessor", Study_design) ~ "Yes"), 
    primary_purpose = case_when(
      grepl("Primary Purpose : Treatment", Study_design) ~ "Treatment",
      grepl("Primary Purpose : Prevention", Study_design) ~ "Prevention",
      grepl("Primary Purpose : Supportive Care", Study_design) ~ "Other")) 



# PHASE -----

# then assign each of these to an option and use == to say which is which
p <- unique(p);p1 <- unique(d$Phase)

# to develop this list I took all the unique phases across all datasets and
# assigned each of them to a category. NA values are addressed later

phase1 = c("0", "Phase 1", "Early Phase 1",
           "Human pharmacology (Phase I): yes                Therapeutic exploratory (Phase II): no                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): no",
           "1", "Phase I", "0 (exploratory trials)", 
           "Phase1", 
           "Human pharmacology (Phase I): yesTherapeutic exploratory (Phase II): noTherapeutic confirmatory - (Phase III): noTherapeutic use (Phase IV): no",
           "I (Phase I study)",
           "Phase-1")

phase2 = c("Human pharmacology (Phase I): noTherapeutic exploratory (Phase II): yesTherapeutic confirmatory - (Phase III): noTherapeutic use (Phase IV): no",
           "2", "Phase 2",
           "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): no",
           "Phase2", "Phase 1/Phase 2",
           "Human pharmacology (Phase I): yes                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): no",
           "1-2",
           "Human pharmacology (Phase I): yesTherapeutic exploratory (Phase II): yesTherapeutic confirmatory - (Phase III): noTherapeutic use (Phase IV): no",
           "Phase II", "Phase-2", "II",
           "Phase 1/ Phase 2",
           "II (Phase II study)",
           "Human pharmacology (Phase I): no\nTherapeutic exploratory (Phase II): yes\nTherapeutic confirmatory - (Phase III): no\nTherapeutic use (Phase IV): no\n",
           "Phase 1 / Phase 2",
           "Human pharmacology (Phase I): yes\nTherapeutic exploratory (Phase II): yes\nTherapeutic confirmatory - (Phase III): no\nTherapeutic use (Phase IV): no\n")

phase3 = c("Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): no                Therapeutic confirmatory - (Phase III): yes                Therapeutic use (Phase IV): no",
           "Human pharmacology (Phase I): noTherapeutic exploratory (Phase II): noTherapeutic confirmatory - (Phase III): yesTherapeutic use (Phase IV): no",
           "2-3", "Phase 3", "3", "Phase 2/ Phase 3", 
           "Phase 2/Phase 3",
           "Human pharmacology (Phase I): noTherapeutic exploratory (Phase II): yesTherapeutic confirmatory - (Phase III): yesTherapeutic use (Phase IV): no",
           "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): yes                Therapeutic use (Phase IV): no",
           "Phase III", "IIIb", "III", "III (Phase III study)",
           "Phase 2 / Phase 3",
           "Phase-3",
           "Human pharmacology (Phase I): no\nTherapeutic exploratory (Phase II): no\nTherapeutic confirmatory - (Phase III): yes\nTherapeutic use (Phase IV): no\n",
           "Phase II/III",
           "Human pharmacology (Phase I): no\nTherapeutic exploratory (Phase II): yes\nTherapeutic confirmatory - (Phase III): yes\nTherapeutic use (Phase IV): no\n")

phase4 = c("4", "Post Marketing Surveillance", "Phase 4",
           "Human pharmacology (Phase I): noTherapeutic exploratory (Phase II): noTherapeutic confirmatory - (Phase III): noTherapeutic use (Phase IV): yes",
           "Phase4",
           "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): no                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): yes",
           "Phase-4",
           "Phase 3/ Phase 4",
           "Human pharmacology (Phase I): noTherapeutic exploratory (Phase II): noTherapeutic confirmatory - (Phase III): yesTherapeutic use (Phase IV): yes",
           "Post-market",
           "Phase IV",
           "Phase 3 / Phase 4",
           "IV (Phase IV study)",
           "Human pharmacology (Phase I): no\nTherapeutic exploratory (Phase II): no\nTherapeutic confirmatory - (Phase III): no\nTherapeutic use (Phase IV): yes\n")

undefined = c("N/A", "Not Applicable", "Bioequivalence", 
              "Not applicable", "Other", "New Treatment Measure Clinical Study",
              "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): yes",
              "Basic Science", "Pilot study" )
unreported = c("Not selected", "NULL", "Not Specified")

# number 33 of p is NA, need to deal with that separately 
# consider anything post-market etc to be phase 4

x <- c(phase1, phase2, phase3, phase4, undefined, unreported)
length(x)
which (!p %in% x)

ph <- c(main$Phase, im$Phase, covid$Phase) %>% 
  as_tibble() %>% 
  rename(Phase = value)

ph <- ph %>%
  mutate(
    phase_clean = case_when(
      Phase %in% phase1 ~ "1",
      Phase %in% phase2 ~ "2",
      Phase %in% phase3 ~ "3",
      Phase %in% phase4 ~ "4", 
      Phase %in% undefined ~ "Undefined", 
      Phase %in% unreported ~ "Unreported", 
      is.na(Phase) ~ "Unreported"))

which(is.na(ph$Phase))
which(is.na(ph$phase_clean))
table(ph$phase_clean)


# Notes -----

ictrp <- bind_rows(main, im)

ictrp <- ictrp %>% 
  filter(Source_registry != "CT.gov", 
         Source_registry != "EUCTR", 
         Source_registry != "IRCT")

unique(ictrp$Study_design)
d <- d %>% 
  group_by(Source_registry) %>% 
  filter(n() >=50) 
table(d$Source_registry)

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



