# Extract data

# This script extracts the relevant data from the ICTRP exports where possible.
# It was compared against an effort to do the same in Python and discrepancies
# were resolved until the scripts agreed. 

# It is worth noting that the script may not be robust if used for other
# datasets, as much of it was developed interactively by checking how suitable
# each approach was for the particular data at hand. If reusing it would be
# essential to check the reliability of the code.

# One output is produced: automated_extraction.csv.

# Aspects of this script were written by Christiana Kartsonaki: geographic
# region, multi-centre, and sample size.

# Inputs -----
input_path <- "data/manual_processing/eligibility_screen/eligible_trials/"
output_path <- "data/automated_extraction/"

# Create folder -----
if(!dir.exists(output_path)){
  dir.create(output_path)
} 

# Load -----
require(tidyverse)

# Read data -----
main <- read_csv(paste0(input_path, "ictrp_main847.csv"))
im <- read_csv(paste0(input_path, "ictrp_im847.csv"))
covid <- read_csv(paste0(input_path, "covid847.csv"))
continents <- read.csv("data/initial_import/supplementary/UNSD — Methodology.csv")

# Combine datasets -----
# bridging flags are in different formats (ictrp lists the ID, covid is T|F)
covid$Bridging_flag <- as.character(covid$Bridging_flag)

d <- bind_rows(main, im, covid, .id = "id")
d$id[d$id == 1] <- "main"
d$id[d$id == 2] <- "im"
d$id[d$id == 3] <- "covid"
d <- rename(d, study_arm = id)

# 1. TRIAL DESIGN -----

# this section splits the data into the different registries and then tries to
# extract info from the "Study_design" column from the ICTRP export. Then the
# data are combined and we try to get any extra data from the titles and
# remaining Study_design that weren't yet captured

# Note that not all source registries have a split effort to extract the data.
# We didn't do it for ones with very few entries.

# these are the outputs we aim for:
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

# Randomisations:
# Patients are randomised to one of at least two study arms, by any method. If
# not stated or single group we will consider the study non-randomised, but will
# record single-group as NA and not stated as unreported.

d_subset <- d %>% 
  select(study_arm, Source_registry, TrialID, Study_design, )

# clinicaltrials.gov ----- 

ct <- d_subset %>% 
  filter(Source_registry == "CT.gov") 

ct <- ct %>% 
  mutate(
    # "Sequential Assignment" is variable in meaning so we don't assign that
    # here
    control_arm = case_when(
      grepl("Allocation: Randomized|Parallel|Crossover|Factorial", Study_design) ~ "Yes",
      grepl("Single Group", Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Allocation: Non-Randomized", Study_design) ~ "No",
      grepl("Allocation: Randomized", Study_design) ~ "Yes",
      grepl("Allocation: N/A", Study_design) ~ NA_character_,
      # manually reviewed some of these and they are non randomised
      grepl("Sequential Assignment", Study_design) ~ "No"),
    # if randomisation missing, state unreported
    randomisation = case_when(
      is.na(randomisation) ~ NA_character_,
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
    blinding = ifelse(grepl("None \\(Open Label\\)|Masking: Open Label", Study_design),
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

eu <- d_subset %>% 
  filter(Source_registry == "EUCTR") 

eu <- eu %>%
  mutate(
    # all meet these conditions
    control_arm = case_when(
      grepl("Controlled: yes|Parallel group: yes", Study_design) ~ "Yes",
      grepl("Controlled: no", Study_design) ~ "No"),
    # all meet these
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Randomised: no", Study_design) ~ "No",
      grepl("Randomised: yes", Study_design) ~ "Yes",
      grepl("Randomised:<br>", Study_design) ~ NA_character_),
    # all meet these
    blinding = case_when(
      grepl("blind: yes", Study_design) ~ "Yes", 
      grepl("Open: yes", Study_design) ~ "No", 
      grepl("blind:<br>", Study_design) ~ NA_character_),
    # who specifically is blinded is not stated
    # many say double or single but not who 
    subject_blind = NA, 
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = NA,
    analyst_blind = NA,
    primary_purpose = NA
    )

# irct -----

irct <- d_subset %>% 
  filter(Source_registry == "IRCT")

irct <- irct %>% 
  mutate(
    randomisation = case_when(
      grepl("Randomization: Not randomized", Study_design) ~ "No",
      grepl("Randomization: Randomized", Study_design) ~ "Yes",
      grepl("Randomization: N/A", Study_design) ~ NA_character_),
    control_arm = case_when(
      grepl("Assignment: Parallel|Assignment: Crossover", 
            Study_design) ~ "Yes",
      grepl("Assignment: Single", Study_design) ~ "No"),
    blinding = case_when(
      grepl("Blinding: Single blinded|Blinding: Double blinded|Blinding: Triple blinded",
            Study_design) ~ "Yes",
      grepl("Blinding: Not blinded", Study_design) ~ "No"),
    subject_blind = NA, 
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = NA,
    analyst_blind = NA,
    primary_purpose = case_when(
      grepl("Purpose: Treatment", Study_design) ~ "Treatment",
      grepl("Purpose: Prevention", Study_design) ~ "Prevention",
      grepl("Purpose: Health service research|Purpose: Supportive|Purpose: Other",
            Study_design) ~ "Other"))

# jprn -----

jprn <- d_subset %>% 
  filter(Source_registry == "JPRN") 

# define terms used to describe trials
control <- "parallel|Cross-over|Crossover|placebo-controlled|randomized controlled trial"
no_control <- "single arm|non-controlled|Uncontrolled|No control group"
blind <- "double-blind|double-masked|double blind|single blind|observer-blind|single-blind|evaluator-blind"

jprn <- jprn %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design, ignore.case = T) ~ "Yes",
      grepl(no_control, Study_design, ignore.case = T) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("non-randomized", 
            Study_design, ignore.case = T) ~ "No",
      grepl("randomized|Random allocation", 
            Study_design, ignore.case = T) ~ "Yes"),
    blinding = case_when(
      grepl(blind, Study_design, ignore.case = T) ~ "Yes",
      grepl("open", Study_design, ignore.case = T) ~ "No"),
    subject_blind = NA, 
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = case_when(
      grepl("observer-blind|evaluator-blind", 
            Study_design, ignore.case = T) ~ "Yes"),
    analyst_blind = NA,
    primary_purpose = case_when(
      grepl("prevention purpose", Study_design, 
            ignore.case = T) ~ "Prevention",
      grepl("treatment purpose", Study_design, 
            ignore.case = T) ~ "Treatment"))

# chictr -----

chictr <- d_subset %>% 
  filter(Source_registry == "ChiCTR") 

control <- "Parallel|Cross-over|Quasi-randomized controlled|Randomized parallel controlled trial|Factorial|Non randomized control|Dose comparison|Case-Control study"

chictr <- chictr %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design) ~ "Yes",
      grepl("Single arm|Sequential", Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Non randomized|Quasi-randomized", Study_design) ~ "No",
      grepl("Randomized", 
            Study_design, ignore.case = T) ~ "Yes"),
    blinding = NA,
    subject_blind = NA, 
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = NA,
    analyst_blind = NA,
    primary_purpose = NA) 

# ctri -----

# covid ones look different but they just have \n in the cells and have same
# content otherwise

ctri <- d_subset %>% 
  filter(Source_registry == "CTRI") 

control <- "parallel|Crossover|Factorial Trial|Cluster Randomized Trial|Multiple Arm Trial|Controlled Trial"
random <- "randomized|Computer generated randomization|Method of generating randomization sequence:Other|Stratified randomization|Stratified block randomization|Random Number Table"
# we consider not applicable stated to mean no blinding
no_blind <- "Blinding and masking:Open Label|Blinding and masking:Not Applicable"

ctri <- ctri %>% 
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
      grepl("masking:Participant", Study_design, ignore.case = T), 
      "Yes", "No"),
    # we assume caregiver is no because at least one other 
    # party is specified for all trials with blinding
    caregiver_blind = "No",
    investigator_blind = ifelse(
      grepl("Investigator", Study_design, ignore.case = T), 
      "Yes", "No"),
    outcome_blind = ifelse(
      grepl("Outcome Assessor", Study_design, ignore.case = T), 
      "Yes", "No"),
    # we consider the data entry operator to be the analyst
    analyst_blind = ifelse(
      grepl("Date-entry Operator", Study_design, ignore.case = T), 
      "Yes", "No"))

# anzctr -----

anzctr <- d_subset %>% 
  filter(Source_registry == "ANZCTR") 

anzctr <- anzctr %>% 
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
    subject_blind = NA,
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = NA,
    analyst_blind = NA,
    primary_purpose = case_when(
      grepl("Purpose: Treatment", 
            Study_design) ~ "Treatment",
      grepl("Purpose: Prevention", 
            Study_design) ~ "Prevention",
      grepl("Purpose: Diagnosis", 
            Study_design) ~ "Other")) 

# isrctn -----

# not really standardised apart from purpose

isrctn <- d_subset %>% 
  filter(Source_registry == "ISRCTN") 

isrctn <- isrctn %>% 
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
    subject_blind = NA,
    caregiver_blind = NA,
    investigator_blind = NA,
    outcome_blind = NA,
    analyst_blind = NA,
    primary_purpose = case_when(
      grepl("Treatment", Study_design) ~ "Treatment",
      grepl("Prevention", Study_design) ~ "Prevention",
      grepl("\\(Other", Study_design) ~ "Other"))

# kct -----
kct <- d_subset %>% 
  filter(Source_registry == "KCT") 

kct <- kct %>% 
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
    caregiver_blind = NA,
    investigator_blind = case_when(
      grepl("Investigator", Study_design) ~ "Yes"),
    outcome_blind = case_when(
      grepl("Outcome Accessor", Study_design) ~ "Yes"), 
    primary_purpose = case_when(
      grepl("Primary Purpose : Treatment", Study_design) ~ "Treatment",
      grepl("Primary Purpose : Prevention", Study_design) ~ "Prevention",
      grepl("Primary Purpose : Supportive Care", Study_design) ~ "Other")) 

# Join registries and search others -----

d_2 <- bind_rows(ct, eu, irct, jprn, chictr, 
                 ctri, anzctr, isrctn, kct) %>% # 2453
  full_join(d) 

rm(ct, eu, irct, jprn, chictr, ctri, anzctr, isrctn, kct,
   blind, control, no_blind, no_control, random, d_subset)

d_2 <- d_2 %>% 
  mutate(
    randomisation = case_when(
      control_arm == "No" ~ "No", 
      TRUE ~ randomisation),
    control_arm = case_when(
      randomisation == "Yes" ~ "Yes", 
      TRUE ~ control_arm)) 

# Key words in titles -----

# Search for any keywords in titles and study design that we dont' already have.
# note that these are only checked in the context of the above processing
# already being done

# Randomisation 
non_random <- c("Non-randomized", "Randomized: No", "nonrandomized",
                "Quasi-randomized")

# terms that do mean random (don't use ignore case)
random <- c("controlled-randomized",
            "Randomised,Permuted block randomization",
            "(?i)A randomised",
            "Simple randomization",
            "randomized, double-blind",
            "Randomised,Simple randomization",
            "Randomized clinical trial",
            "Randomized controlled trial",
            "a randomized, active-controlled",
            "participants randomly allocated",
            "randomized-controlled trial",
            "Randomization will be stratified",
            "randomized-controlled",
            "(?i), Randomized",
            "Randomized trial",
            "^Randomized",
            "Simple randomization",
            "randomized-controlled",
            ",Randomised",
            "randomised- controlled",
            "Phase 1 Randomized",
            "(?i)a randomized",
            "stratified-randomized",
            "A random controlled",
            "pilot randomized",
            "Prospective, Randomised"
            )

d_2 <- d_2 %>%
  mutate(randomisation = case_when(
    !is.na(randomisation) ~ randomisation,
    grepl(paste0(non_random, collapse = "|"),
          Study_design) ~ "No",
    grepl(paste0(non_random, collapse = "|"),
          Public_title) ~ "No",
    grepl(paste0(non_random, collapse = "|"),
          Scientific_title) ~ "No",
    grepl(paste0(random, collapse = "|"),
          Study_design) ~ "Yes",
    grepl(paste0(random, collapse = "|"),
          Public_title) ~ "Yes",
    grepl(paste0(random, collapse = "|"),
          Scientific_title) ~ "Yes"))

# Blinding 
# not blind  ignore.case = T
not_blind <- c("unmasked",
               "Masking: None",
               "Masking: Open",
               "open label",
               "open-label",
               ", open,",
               " open,",
               ", open",
               "open clinical trial"
               )
    
# blind           ignore.case = T
blind <- c("Masking: Blinded",
           "Masking: Double",
           "double-blind", 
           # note that double blind and single blind are mentioned in all the
           # EUCTR study_designs. Addressed below
           "single blind", 
           "single-blind",
           "double blind",
           "assessor-blind",
           "A blinded nurse",
           "triple blind",
           ", blinded,",
           ", Blind,",
           "Masking: Single",
           "partially blind",
           "Observer-blind",
           "Observer Blind",
           "partially-blinded",
           "partly blinded",
           "opaque envelopes",
           "double-bind" # one mispelt
           )

d_2 <- d_2 %>% 
  mutate(blinding = case_when(
    !is.na(blinding) ~ blinding, 
    grepl(paste0(not_blind, collapse = "|"), 
          Study_design, ignore.case = T) ~ "No",
    grepl(paste0(not_blind, collapse = "|"), 
          Public_title, ignore.case = T) ~ "No",
    grepl(paste0(not_blind, collapse = "|"), 
          Scientific_title, ignore.case = T) ~ "No",
    grepl(paste0(blind, collapse = "|"), 
          Public_title, ignore.case = T) ~ "Yes",
    grepl(paste0(blind, collapse = "|"), 
          Scientific_title, ignore.case = T) ~ "Yes"))

# because all EUCTR trials state double blind and single blind we don't do the
# study design search for them
d_2 <- d_2 %>% 
  mutate(blinding = case_when(
    Source_registry == "EUCTR" ~ blinding, 
    !is.na(blinding) ~ blinding,
    grepl(paste0(blind, collapse = "|"), 
          Study_design, ignore.case = T) ~ "Yes")) 

# fix a specific trial that says both open and blinded (seems more blinded)
d_2[d_2$TrialID == "JPRN-JapicCTI-194971",]$blinding <- "Yes"

d_2 <- d_2 %>% 
  mutate(
    subject_blind = ifelse(
      blinding == "No", "No", subject_blind),
    caregiver_blind = ifelse(
      blinding == "No", "No", caregiver_blind),
    investigator_blind = ifelse(
      blinding == "No", "No", investigator_blind),
    outcome_blind = ifelse(
      blinding == "No", "No", outcome_blind),
    analyst_blind = ifelse(
      blinding == "No", "No", analyst_blind)) 

# d_2 <- d_2 %>% 
#   mutate(
#     blinding = ifelse(
#       is.na(blinding), "Unreported", blinding),
#     subject_blind = ifelse(
#       is.na(subject_blind), "Unreported", subject_blind),
#     investigator_blind = ifelse(
#       is.na(investigator_blind), "Unreported", investigator_blind),
#     outcome_blind = ifelse(
#       is.na(outcome_blind), "Unreported", outcome_blind),
#     analyst_blind = ifelse(
#       is.na(analyst_blind), "Unreported", analyst_blind)
#     ) 

# Control arm
control <- c("Parallel",
             "\\bcontrolled",
             "multi-arm",
             "multi arm",
             "two arm", 
             "two-arm",
             "three arm",
             "three-arm",
             "Factorial",
             "Control: Placebo",
             "Control: Active",
             "randomized control trial"
             )

non_control <- c("single-arm",
                 "single arm",
                 "uncontrolled", 
                 "single group",
                 "Control: Historical"
                 )

d_2 <- d_2 %>% 
  mutate(control_arm = case_when(
    !is.na(control_arm) ~ control_arm, 
    grepl(paste0(non_control, collapse ="|"), Study_design,
          ignore.case = T) ~ "No",
    grepl(paste0(non_control, collapse ="|"), Public_title,
          ignore.case = T) ~ "No",
    grepl(paste0(non_control, collapse ="|"), Scientific_title,
          ignore.case = T) ~ "No",
    grepl(paste0(control, collapse ="|"), Study_design,
          ignore.case = T) ~ "Yes",
    grepl(paste0(control, collapse ="|"), Public_title,
          ignore.case = T) ~ "Yes",
    grepl(paste0(control, collapse ="|"), Scientific_title,
          ignore.case = T) ~ "Yes")) 

# Primary purpose 

# identify those matching the terms across the columns which have missing
# entries for primary purpose (don't want to override them as they shoudl be
# more reliable)
prev <- which(is.na(d_2$primary_purpose) &
                (grepl("prevent|prophyla", d_2$Study_design, ignore.case = T)|
                grepl("prevent|prophyla", d_2$Public_title, ignore.case = T)|
                grepl("prevent|prophyla", d_2$Scientific_title, ignore.case = T)))

# all of the EUCTR trials say treatment in study_design so we can't search them
treat <- which(is.na(d_2$primary_purpose) & d_2$Source_registry != "EUCTR" &
                 (grepl("treatment", d_2$Study_design, ignore.case = T)|
                grepl("treatment", d_2$Public_title, ignore.case = T)|
                grepl("treatment", d_2$Scientific_title, ignore.case = T)))

treat2 <- which(is.na(d_2$primary_purpose) & d_2$Source_registry == "EUCTR" &
        (grepl("treatment", d_2$Public_title, ignore.case = T)|
           grepl("treatment", d_2$Scientific_title, ignore.case = T)))

treat <- c(treat, treat2)

# find those returned by both searches so they will be manually reviewed later
x <- c(prev, treat)
duplicates <- x[duplicated(x) | duplicated(x, fromLast = TRUE)]

d_2$primary_purpose[prev] <- "Prevention"
d_2$primary_purpose[treat] <- "Treatment"
d_2$primary_purpose[duplicates] <- NA

# get a few specific ones that are clearly labelled but also said one of the
# other terms (this happens because we didn't do DRKS as it's own group above)
prev <- grep("Study design purpose: Prevention", d_2$Study_design)
d_2$primary_purpose[prev] <- "Prevention"
d_2$primary_purpose[grep("Study design purpose: Diagnostic", d_2$Study_design)] <- 
  "Other"

rm(blind, control, non_control, non_random, not_blind, random, prev,
   treat, x, duplicates, treat2)

d_2 <- d_2 %>% 
  mutate(
    randomisation = case_when(
      control_arm == "No" ~ "Not applicable", 
      TRUE ~ randomisation),
    control_arm = case_when(
      randomisation == "Yes" ~ "Yes", 
      TRUE ~ control_arm)) 

# Individual fixes -----

# e.g. some trials with e.g. contradictory info

d_2[d_2$TrialID == "ChiCTR-IIC-16008366", ]$control_arm <- "No"
d_2[d_2$TrialID == "ChiCTR2000030810", ]$control_arm <- "Yes"
d_2[d_2$TrialID == "EUCTR2016-003433-20-FR", ]$control_arm <- "Yes"
d_2[d_2$TrialID == "NCT04273321", ]$control_arm <- "No"
d_2[d_2$TrialID == "NCT04419623", ]$control_arm <- "No"
d_2[d_2$TrialID == "IRCT20200907048644N1", ]$control_arm <- "Yes"
d_2[d_2$TrialID == "NCT04273321", ]$control_arm <- "Yes"
d_2[d_2$TrialID == "NCT03884465", ]$control_arm <- "Yes"

d_2[d_2$TrialID == "JPRN-jRCTs041190009", ]$blinding <- "Yes"

d_2[d_2$TrialID == "EUCTR2020-001747-21-PT", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "NL8633", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "NL8547", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "IRCT20180921041079N1", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "IRCT20200907048644N1", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "ChiCTR-IIC-16008366", ]$randomisation <- "No"
d_2[d_2$TrialID == "NCT03839134", ]$randomisation <- "Yes"
d_2[d_2$TrialID == "NCT04419623", ]$randomisation <- "No"

d_2[d_2$TrialID == "PACTR201905466349317", ]$primary_purpose <- "Prevention"
d_2[d_2$TrialID == "PACTR202007720062393", ]$primary_purpose <- NA_character_
d_2[d_2$TrialID == "JPRN-jRCTs031190015", ]$primary_purpose <- "Other"

# 2. PHASE -----

# then assign each of these to an option and use == to say which is which
# p1 <- unique(d$Phase)

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

# consider anything post-market etc to be phase 4
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

undefined = c("Bioequivalence", 
              "Other", "New Treatment Measure Clinical Study",
              "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): yes",
              "Basic Science", "Pilot study", 
              "N/A", 
              "Not Applicable", "Not applicable")

unreported = c("Not selected", "NULL", "Not Specified")

d_2 <- d_2 %>%
  mutate(
    phase_clean = case_when(
      Phase %in% phase1 ~ "Phase 1",
      Phase %in% phase2 ~ "Phase 2",
      Phase %in% phase3 ~ "Phase 3",
      Phase %in% phase4 ~ "Phase 4", 
      Phase %in% undefined ~ "Undefined", 
      Phase %in% unreported ~ "Unreported",
      is.na(Phase) ~ "Unreported"))

rm(phase1, phase2, phase3, phase4, undefined, unreported)

# manual comparison indicated that JPRN N/A can be unreported rather than
# undefined
d_2[which(d_2$Source_registry == "JPRN" & d_2$Phase == "N/A"),]$phase_clean <- 
  "Unreported"

# correct one described as P2/P4 which based on manual check is phase 2
d_2[d_2$TrialID=="EUCTR2018-004305-11-GB", "phase_clean"] <- "Phase 2"

# 3. SPONSOR TYPE -----

not_ind <- c("universi",  # all ignore.case = T
              "pharmacy",
              "department",
              "institut", "insitute", # second due to typo in one entry
              "hospital",
              "centre", "center",
              "council",
              "college",
              "academy",
              "facul",
              "Hôpitaux",
              "Hopitaux",
              "Foundation",
              "school",
             "Mayo Clinic",
             "World Health Organization",
             "NYU Langone Health",
             "The Cleveland Clinic",
             "Radboudumc",
             "Dr. Negrin University Hospital",
             "NHS",
             "fundaci"
              )  

industry <- c("pharma", # all ignore.case = T
              "\\bInc",
              "\\bLtd",
              "\\bCo\\b", 
              "S\\.A\\.",
              "Company",
              "Limited",
              "Corporation",
              "\\bplc",
              "\\bGmbH",
              "\\bAG$",
              "\\bKGaA",
              "\\bCorp\\.",
              "\\bLP",
              "\\bLLC",
              "\\bNV",
              "\\bAB$",
              "\\bKK",
              "Therapeutics",
              "labs",
              "\\bSAS",
              "glaxo",
              "zeneca",
              "bioscience",
              "\\bSA\\b",
              "Novartis",
              "ingelheim",
              "\\bSAS\\b",
              "Bayer",
              "Merck",
              "Pfizer",
              "sanofi",
              "biopharm",
              "\\bS\\.p\\.A",
              "Farmaceutic",
              "Takeda",
              "La Roche",
              "La-Roche",
              "Seqirus",
              "AbbVie",
              "Novavax",
              "Novo Nordisk",
              "shionogi",
              "Dr. Abidi pharmacutical Co.",
              "Dr. Reddy’s Laboratories S.A."
              )

investigator <- c("\\bMD\\b", # all ignore.case = T
                  "\\bphd",
                  "^dr\\b",
                  "prof"
                  )

non_ind <- grep(
  paste0(not_ind, collapse = "|"),
  d_2$Primary_sponsor, ignore.case = T)

ind <- grep(
  paste0(industry, collapse = "|"),
  d_2$Primary_sponsor, 
  ignore.case = T)

inv <- grep(
  paste0(investigator, collapse = "|"),
  d_2$Primary_sponsor, ignore.case = T)

# Those that were returned by more than one grep are removed
x <- c(non_ind, ind, inv)
x <- x[!(duplicated(x) | duplicated(x, fromLast = TRUE))]

non_ind <- non_ind[non_ind %in% x]
ind <- ind[ind %in% x]
inv <- inv[inv %in% x] 

d_2$sponsor_type <- NA
d_2$sponsor_type[non_ind] <- "non_industry"
d_2$sponsor_type[ind] <- "industry"
d_2$sponsor_type[inv] <- "investigator"

rm(ind, industry, inv, investigator, non_ind, not_ind, x)

# now assign some specifics that were removed as duplicates or that 
# we know should be in one category 
industry <- c("Dr. Reddy", "Dr. Abidi pharmacutical Co.",
         "EMD Serono Research & Development Institute, Inc.",
         "Perseverance Research Center, LLC",
         "Instituto Grifols",
         "Laboratorios Sophia S.A de C.V.")

ind <- grep(paste0(industry, collapse = "|"), 
     d_2$Primary_sponsor, ignore.case = T)

not_ind <- c("The First Affiliated Hospital of Guangdong Pharmaceutical University",
             "Islamic Azad University",
             "University of Saskatchewan",
             "Dr. Negrin University Hospital",
             "KK Women's and Children's Hospital",
             "Chiang Mai University",
             "Riyadh Colleges of Dentistry and Pharmacy",
             "parul ayurved pharmacy",
             "Almaa Siddha Multispeciality Hospital Pvt Ltd",
             "Ottawa Heart Institute Research Corporation",
             "degli Studi di Sassari",
             "del Hospital Universitario de Bellvitge")

non_ind <- grep(paste0(not_ind, collapse = "|"), 
                d_2$Primary_sponsor, ignore.case = T)

d_2$sponsor_type[non_ind] <- "non_industry"
d_2$sponsor_type[ind] <- "industry"

rm(industry, ind, not_ind, non_ind)

# x <- grep("^dr\\b", d_2$Primary_sponsor, ignore.case = T)
# d_2$Primary_sponsor[x]

# d_2 %>%
#   filter(is.na(sponsor_type)) %>%
#   select(Primary_sponsor) %>%
#   distinct() %>% 
#   View()
# 
# d_2 %>%
#   filter(is.na(sponsor_type)) %>%
#   select(Primary_sponsor) %>%
#   group_by_all() %>%
#   filter(n() >2) %>%
#   distinct() %>% View()

# JPRN ones appear to be the ones providing all the named individuals. Need to
# check these are accurate. Although they are not all individuals, about half
# have already been assigned

# 4. PROSPECTIVE REGISTRATION -----

# We  will  manually  review  all  trials  with  a  bridging  flag  to identify
# the other trial registrations and will consider a trial prospectively
# registered if it was prospectively registered in at least one registry. If
# there is no statement regarding prospective registration  and  no  start  date
# or  registration  date  is found  in  all  registry  entries,  we  will
# consider the study retrospectively registered

# fix incorrect date
d_2[which(d_2$Date_enrollment_format == "2640-06-22"), "Date_enrollment_format"] <- 
  as.Date("2019-04-01")

# Date_registration_format, Date_enrollment_format are the relevant cols
d_2 <- d_2 %>% 
  mutate(
    reg_enrol_difference = Date_enrollment_format - Date_registration_format,
    prospective = case_when(
      sign(reg_enrol_difference) == -1 ~ "No",
      sign(reg_enrol_difference) == 0 ~ "Yes",
      sign(reg_enrol_difference) == 1 ~ "Yes",
      # missing means retrospective
      is.na(Date_enrollment_format) ~ "No"))

# for those with the date inferred, override the values we just created because
# the number of days is innacurate and the prospective registration needs to be
# based on month year not day
dates_inf <- d_2[d_2$Day_inferred == T, ]
d_2[d_2$Day_inferred == T, ]$reg_enrol_difference <- NA

# make the registration date first of the month to be comparable to enrollment
reg_m_y <- format(dates_inf$Date_registration_format, "%Y-%m")
reg_m_y <- as.Date(paste(reg_m_y, "-01", sep=""), 
                   format = "%Y-%m-%d")

pros <- sign(dates_inf$Date_enrollment_format - reg_m_y)
pros[pros == -1] <- "No"
pros[pros == 0] <- "No"
pros[pros == 1] <- "Yes"
d_2[d_2$Day_inferred == T, ]$prospective <- as.character(pros)

# where the trial was registered on the first day of the month, we know that
# enrollment date was equal to or after it, so they are prospective even if
# enrollment day was inferred
x <- d_2$Day_inferred == T &
  d_2$Date_registration_format == d_2$Date_enrollment_format
d_2[x, ]$prospective <- "Yes"

rm(pros, dates_inf, reg_m_y, x)

# some individual fixes
d_2[d_2$TrialID == "PACTR201905466349317", ]$Date_enrollment_format <- 
  as.Date("2019/03/18")
d_2[d_2$TrialID == "PACTR201902478249291", ]$Date_enrollment_format <- 
  as.Date("2017/10/15")
d_2[d_2$TrialID == "PACTR202006760881890", ]$Date_enrollment_format <- 
  as.Date("2020/09/14")
d_2[d_2$TrialID == "PACTR202009786901147", ]$Date_enrollment_format <- 
  as.Date("2021/02/04")

# 5. GEOGRAPHIC REGION -----

# we want to have NA and LA as separate regions:
continents[continents$Sub.region.Name == "Northern America", "Region.Name"] <-
  "Northern America"
continents[continents$Sub.region.Name == "Latin America and the Caribbean",
           "Region.Name"] <- "Latin America and the Caribbean"

continents <- continents %>% 
  select(Region.Name, Country.or.Area) %>%
  distinct()

countries_list <- base::strsplit(d_2$Countries, split = ";")
# there are still some countries separated by "," but can't separate on that due
# to some having comma in name

countries_list <- lapply(countries_list, unique)

# change alternative spellings 
countries_list[which(countries_list %in% c("China?", "CHINA"))] <- "China"
countries_list[which(countries_list == "Japan,Asia(except Japan)")] <- "Japan"
countries_list[which(countries_list == "Tanzania")] <- "United Republic of Tanzania"
countries_list[which(countries_list == "Hat Yai")] <- "Thailand"

countries_list[which(countries_list == "NULL")] <- NA

# merge with continents
regions_list <-
  lapply(countries_list, function(x)
    if (!is.na(x))
      merge(
        data.frame(country = x),
        continents,
        by.x = "country",
        by.y = "Country.or.Area",
        all.x = TRUE,
        all.y = FALSE
      ))

# check those not matched and fix

# unmatched <-
#   lapply(regions_list, function(x)
#     x[which(!is.na(x$country) & is.na(x$Region.Name)), ])
# 
# unique(do.call(rbind, unmatched[which(unlist(lapply(unmatched, function(x)
#   dim(x)[1])) > 0)]))

countries_list <-
  lapply(countries_list, function(x)
    gsub(
      "United Kingdom",
      "United Kingdom of Great Britain and Northern Ireland",
      x
    ))

countries_list <-
  lapply(countries_list, function(x)
    gsub("United States", "United States of America", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("United States of America of America", "United States of America", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Moldova, Republic of", "Republic of Moldova", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Chinese", "China", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Iran, Islamic Republic of", "Iran (Islamic Republic of)", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Korea, Republic of", "Republic of Korea", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Czech Republic", "Czechia", x))

regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("Hong Kong", x$country)] <-
      "Asia"
    return(data.frame(x))
  })

countries_list <-
  lapply(countries_list, function(x)
    gsub("Vietnam", "Viet Nam", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub(
      "Korea, Democratic People's Republic of",
      "Democratic People's Republic of Korea",
      x
    ))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Korea South", "Republic of Korea", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("thailand", "Thailand", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("Macedonia, The Former Yugoslav Republic of",
         "North Macedonia", x))

countries_list <-
  lapply(countries_list, function(x)
    gsub("The Netherlands", "Netherlands", x))

continents[grep("voire", continents[, 2], value = TRUE), 2] <-
  "Côte d’Ivoire"

countries_list <-
  lapply(countries_list, function(x)
    gsub("Cote Divoire", "Côte d’Ivoire", x))
countries_list <-
  lapply(countries_list, function(x)
    gsub("CÃ´te D'Ivoire", "Côte d’Ivoire", x))

# merge with continents again
regions_list <-
  lapply(countries_list, function(x)
    if (!is.na(x))
      merge(
        data.frame(country = x),
        continents,
        by.x = "country",
        by.y = "Country.or.Area",
        all.x = TRUE,
        all.y = FALSE
      ))

# Assign some values manually
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("mone", x$country)] <-
      NA
    return(data.frame(x))
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("European Union", x$country)] <-
      "Europe"
    return(x)
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("No Country", x$country)] <-
      NA
    return(data.frame(x))
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("Europe", x$country)] <-
      "Europe"
    return(data.frame(x))
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("North America", x$country)] <-
      "Northern America"
    return(data.frame(x))
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("Taiwan", x$country)] <-
      "Asia"
    return(data.frame(x))
  })
regions_list <-
  lapply(regions_list, function(x) {
    x$Region.Name[grep("Hong Kong", x$country)] <-
      "Asia"
    return(data.frame(x))
  })

# unmatched <- lapply(regions_list, function(x) x[which(is.na(x$Region.Name)),])
# unique(do.call(rbind, unmatched[which(unlist(lapply(unmatched, function(x) dim(x)[1])) > 0)]))

lapply(regions_list[which(unlist(lapply(regions_list, function(x)
  dim(x)[1])) > 1)], function(x)
    x$Region.Name)

regions <- lapply(regions_list, function(x) unique(x$Region.Name))

d_2$region <- unlist(lapply(regions, function(x) paste(x, collapse = ", ")))

d_2$region_Africa <-
  ifelse(grepl("Africa", d_2$region), "Yes", "No")
d_2$region_N_America <-
  ifelse(grepl("Northern America", d_2$region), "Yes", "No")
d_2$region_L_America <-
  ifelse(grepl("Latin America and the Caribbean", d_2$region),
         "Yes",
         "No")
d_2$region_Asia <- 
  ifelse(grepl("Asia", d_2$region), "Yes", "No")
d_2$region_Europe <-
  ifelse(grepl("Europe", d_2$region), "Yes", "No")
d_2$region_Oceania <-
  ifelse(grepl("Oceania", d_2$region), "Yes", "No")

d_2[d_2$Countries == "Japan, South America, Europe" &
      !is.na(d_2$Countries), ]$region_Asia <- "Yes"  
d_2[d_2$Countries == "Japan, South America, Europe" &
      !is.na(d_2$Countries), ]$region_L_America <- "Yes"  
d_2[d_2$Countries == "Japan, Asia except Japan, North America, South America, Europe" &
      !is.na(d_2$Countries), ]$region_Asia <- "Yes"  
d_2[d_2$Countries == "Japan, Asia except Japan, North America, South America, Europe" &
      !is.na(d_2$Countries), ]$region_L_America <- "Yes"  
d_2[d_2$Countries == "Japan, Asia except Japan, North America, South America, Europe" &
      !is.na(d_2$Countries), ]$region_Europe <- "Yes"  

unreported <-
  which(
    d_2$region_Africa == "No" &
      d_2$region_N_America == "No" &
      d_2$region_L_America == "No" &
      d_2$region_Asia == "No" &
      d_2$region_Europe == "No" & 
      d_2$region_Oceania == "No"
  )
d_2[unreported,]$region_Africa <- "Unreported"
d_2[unreported,]$region_N_America <- "Unreported"
d_2[unreported,]$region_L_America <- "Unreported"
d_2[unreported,]$region_Asia <- "Unreported"
d_2[unreported,]$region_Europe <- "Unreported"
d_2[unreported,]$region_Oceania <- "Unreported"

# comparison to manual extraction showed that ct.gov trials with NULL country do
# report country elsewhere
missing <- which(d_2$Countries == "NULL" & d_2$Source_registry == "CT.gov")
d_2[missing,]$region_Africa <- NA_character_
d_2[missing,]$region_N_America <- NA_character_
d_2[missing,]$region_L_America <- NA_character_
d_2[missing,]$region_Asia <- NA_character_
d_2[missing,]$region_Europe <- NA_character_
d_2[missing,]$region_Oceania <- NA_character_
                 
rm(continents, regions, regions_list, missing, unreported)

# 6. MULTI-CENTRE -----
d_2$multicentre <- NA_character_

# multicentre trials
d_2[which(unlist(lapply(countries_list, length)) > 1),]$multicentre <- "Yes"

d_2[grep("multicent", d_2$Scientific_title, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"
d_2[grep("multi-cent", d_2$Scientific_title, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"

d_2[grep("multicent", d_2$Public_title, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"
d_2[grep("multi-cent", d_2$Public_title, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"

d_2[grep("multicent", d_2$Study_design, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"
d_2[grep("multi-cent", d_2$Study_design, names(d_2), ignore.case = TRUE),]$multicentre <- "Yes"

# single centre trials
d_2[grep("singlecent", d_2$Scientific_title, names(d_2), ignore.case = TRUE),]$multicentre <- "No"
d_2[grep("single-cent", d_2$Scientific_title, names(d_2), ignore.case = TRUE),]$multicentre <- "No"

d_2[grep("singlecent", d_2$Public_title, names(d_2), ignore.case = TRUE),]$multicentre <- "No"
d_2[grep("single-cent", d_2$Public_title, names(d_2), ignore.case = TRUE),]$multicentre <- "No"

d_2[grep("singlecent", d_2$Study_design, names(d_2), ignore.case = TRUE),]$multicentre <- "No"
d_2[grep("single-cent", d_2$Study_design, names(d_2), ignore.case = TRUE),]$multicentre <- "No"
# table(d_2$multicentre, useNA = "ifany")

rm(countries_list)

# 7. SAMPLE SIZE -----

d_2$Target_size <- as.character(d_2$Target_size)
d_2$sample_size <- NA_character_
d_2[which(!grepl("\\D", d_2$Target_size)), ]$sample_size <-
  d_2[which(!grepl("\\D", d_2$Target_size)), ]$Target_size
d_2$sample_size <- as.numeric(d_2$sample_size)
d_2[which(grepl("\\D", d_2$Target_size)), ]$sample_size <-
  sapply(d_2[which(grepl("\\D", d_2$Target_size)), ]$Target_size, function(x) {
    s <-
      strsplit(x, ";")
    ss <-
      sapply(s[[1]], function(y)
        strsplit(y, ":")[[1]][2])
    total <- sum(as.numeric(ss), na.rm = TRUE)
    return(total)
  })

# one is incorrect as shown by comparison to ND. Fix
d_2[d_2$TrialID == "ChiCTR2000032135", "sample_size"] <- 50


# 8. INTERVENTION TYPE -----

# At least one of the arms of the trial includes the intervention type.
# Conventional therapies include small molecules, antibodies, proteins,
# blood-derived products(e.g. plasma), biologicals and ATMPS. Traditional
# medicines include herbal therapies and vitamin therapies.

# all ignore case = T
conventional <- c("plasma",
                  "cin\\b",
                  "vir\\b", # ZingiVir problematic for vir\\b
                  "mab\\b",
                  "quine\\b",
                  "stem cell",
                  "heparin",
                  "methasone\\b",
                  "interferon",
                  "caine\\b",
                  "Cannabidiol",
                  "antibiotic",
                  "magnesium",
                  "iron",
                  "Caffeine"
                  )

traditional <- c("vitamin",
                 "herb",
                 "leaf extract",
                 "aqueous extract",
                 "seed extract",
                 "natural product", 
                 "ayush",
                 "Ayurv",
                 "tradition",
                 "Siddha",
                 "Curcumin"
                 )

vaccine <- c("Vaccine",
             "Afluria",
             "Fluarix",
             "Flublok",
             "Fluzone",
             "BNT162b2",
             "AZD1222",
             "nanoflu",
             "ChAdOx1"
             )

d_2 <- d_2 %>%
  mutate(
    vaccine = ifelse(grepl(
      paste0(vaccine, collapse = "|"), Interventions, ignore.case = T
    ), "Yes", NA),
    conventional = ifelse(grepl(
      paste0(conventional, collapse = "|"),
      Interventions,
      ignore.case = T
    ), "Yes", NA),
    traditional = ifelse(grepl(
      paste0(traditional, collapse = "|"),
      Interventions,
      ignore.case = T
    ), "Yes", NA),
    traditional = ifelse(grepl(
      "traditional hepatoprotective", 
      Interventions), 
      NA, traditional)
  ) 
  
rm(conventional, traditional, vaccine)

# Make dataset -----

d_2[is.na(d_2$Bridging_flag), ]$Bridging_flag <- "FALSE"

d_save <- d_2 %>% 
  select(study_arm, TrialID, url, Scientific_title, Conditions, Date_enrollment_format,
         control_arm, randomisation, blinding, subject_blind, 
         caregiver_blind, investigator_blind, outcome_blind, analyst_blind,
         prospective, Source_registry, phase_clean, 
         region_Africa:region_Oceania, 
         multicentre, primary_purpose, sponsor_type, sample_size,
         vaccine, conventional, traditional, Bridging_flag,
         # extras for checks
         Study_design, Day_inferred, Date_registration_format,
         Phase, Public_title, Countries, Primary_sponsor, Interventions)

write_csv(d_save, paste0(output_path, "automated_extraction.csv"))

# source("scripts/working.R")



