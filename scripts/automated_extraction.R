# Aspects of this script were written by Christiana Kartsonaki


# Inputs -----
# add

# Load -----
require(tidyverse)

# Read data -----
main <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_main847.csv")
im <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/ictrp_im847.csv")
covid <- read_csv("data/manual_processing/eligibility_screen/eligible_trials/covid847.csv")
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
    control_arm = case_when(grepl(
      "Single Group", Study_design) ~ "No",
      grepl("Allocation: Randomized|Parallel|Crossover|Factorial", Study_design) ~ "Yes"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Allocation: Non-Randomized", Study_design) ~ "No",
      grepl("Allocation: Randomized", Study_design) ~ "Yes",
      grepl("Allocation: N/A", Study_design) ~ NA_character_),
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

eu <- d_subset %>% 
  filter(Source_registry == "EUCTR") 

eu <- eu %>%
  mutate(
    # all meet these conditions
    control_arm = case_when(
      grepl("Controlled: yes", Study_design) ~ "Yes",
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
      grepl("blind: no", Study_design) ~ "No", 
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
    control_arm = case_when(
      grepl("Assignment: Parallel|Assignment: Crossover", 
            Study_design) ~ "Yes",
      grepl("Assignment: Single", Study_design) ~ "No"),
    randomisation = case_when(
      grepl("Randomization: Not randomized", Study_design) ~ "No",
      grepl("Randomization: Randomized", Study_design) ~ "Yes",
      grepl("Randomization: N/A", Study_design) ~ NA_character_),
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
    primary_purpose = NA)


# chictr -----

chictr <- d_subset %>% 
  filter(Source_registry == "ChiCTR") 

control <- "Parallel|Cross-over|Quasi-randomized controlled|Randomized parallel controlled trial|Factorial|Non randomized control"

chictr <- chictr %>% 
  mutate(
    control_arm = case_when(
      grepl(control, Study_design) ~ "Yes",
      grepl("Single arm", Study_design) ~ "No"),
    randomisation = case_when(
      control_arm == "No" ~ "No",
      grepl("Non randomized", Study_design) ~ "No",
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

# count(d_2, analyst_blind)

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

# NEXT STEP HERE IS TO DO THE SEARCHING OF OTHER COLS WITH KEY WORDS
# Only really want to do it on those that haven't already been assigned so use something like 
# d %>% 
#   mutate(
#     randomisation = case_when(
#       !is.na(randomisation) ~ randomisation,
#       TRUE ~ "CHECK")) %>% 
#   View

# IRCT20200907048644N1 has contradictory information (it says randomised but single arm)
# need to define a heirarchy. Single arm should take precedent. This is the only one

# this trial is randomized based on title
# TrialID == "JPRN-JapicCTI-194635" ~ "Yes"),
# Look at how christiana did it with grep several columns

# 2. PHASE -----

# then assign each of these to an option and use == to say which is which
p1 <- unique(d$Phase)

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

undefined = c("N/A", "Not Applicable", "Bioequivalence", 
              "Not applicable", "Other", "New Treatment Measure Clinical Study",
              "Human pharmacology (Phase I): no                Therapeutic exploratory (Phase II): yes                Therapeutic confirmatory - (Phase III): no                Therapeutic use (Phase IV): yes",
              "Basic Science", "Pilot study" )

unreported = c("Not selected", "NULL", "Not Specified")

# number 33 of p is NA, need to deal with that separately 

d_2 <- d_2 %>%
  mutate(
    phase_clean = case_when(
      Phase %in% phase1 ~ "1",
      Phase %in% phase2 ~ "2",
      Phase %in% phase3 ~ "3",
      Phase %in% phase4 ~ "4", 
      Phase %in% undefined ~ "Undefined", 
      Phase %in% unreported ~ NA_character_))

rm(p1, phase1, phase2, phase3, phase4, undefined, unreported)

# 3. SPONSOR TYPE -----
sponsor <- d_2 %>% 
  select(Primary_sponsor) %>% 
  unique()

not_ind <- c("universi",  # all ignore.case = T
              "pharmacy",
              "department",
              "institut", "insitute", # second due to typo in one entry
              "hospital",
              "centre|center",
              "council",
              "college",
              "academy",
              "facul",
              "Hôpitaux",
              "Hopitaux",
              "Foundation",
              "school"
              )  

industry <- c("pharma", # all ignore.case = T
              " Inc",
              " Ltd",
              "Co\\.", # deliberate no space
              " S\\.A\\.",
              "Company",
              "Limited",
              "Corporation",
              " plc",
              " GmbH",
              " AG$",
              " KGaA",
              " SA$",
              " Corp\\.",
              " LP",
              " LLC",
              " NV",
              " AB$",
              " KK",
              "Therapeutics",
              "labs"
              )

investigator <- c(" MD",
                  " phd",
                  "^dr ", "^dr\\.",
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

d_2$sponsor_type <- NA
d_2$sponsor_type[non_ind] <- "non_industry"
d_2$sponsor_type[ind] <- "industry"
d_2$sponsor_type[inv] <- "investigator"

table(d_2$sponsor_type, useNA = "always")

# find those that are returned by multiple searches (e.g. ind and non_ind)
# assign all of these NA and be done manually later
x <- unlist(list(non_ind, ind, inv))
d_2$Primary_sponsor[unique(x[duplicated(x)])] <- NA

rm(ind, industry, inv, investigator, non_ind, not_ind, x)

# done <- unique(c(ind, non_ind, inv))

# d_2 %>% 
#   filter(is.na(sponsor_type)) %>% 
#   select(Source_registry, Primary_sponsor) %>% 
#   View()

# also check that all the individuals aren't coming from one source registry or something
# check coverage of ct.gov # 287 missing (23%). 4/5 coverage in general
# so would be nice to get the ct.gov classifications if poss

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

# Date_registration_format, Date_enrollment_format are the relevant cols

d_2 <- d_2 %>% 
  mutate(
    reg_enrol_difference = Date_enrollment_format - Date_registration_format,
    prospective = case_when(
      sign(reg_enrol_difference) == -1 ~ "No",
      sign(reg_enrol_difference) == 0 ~ "No",
      sign(reg_enrol_difference) == 1 ~ "Yes"))

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

rm(pros, dates_inf, reg_m_y)

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
  "Cote D'Ivoire"

countries_list <-
  lapply(countries_list, function(x)
    gsub("Cote Divoire", "Cote D'Ivoire", x))
countries_list <-
  lapply(countries_list, function(x)
    gsub("CÃ´te D'Ivoire", "Cote D'Ivoire", x))

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

unmatched <- lapply(regions_list, function(x) x[which(is.na(x$Region.Name)),])
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
table(d_2$multicentre, useNA = "ifany")

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
    ), "Yes", NA)
  ) 
    

# Make dataset -----

x <- d_2 %>% 
  select(TrialID, url, Scientific_title, Conditions, Date_enrollment_format,
         control_arm, randomisation, blinding, subject_blind, 
         caregiver_blind, investigator_blind, outcome_blind, analyst_blind,
         prospective, Source_registry, phase_clean, 
         region_Africa:region_Oceania, 
         multicentre, primary_purpose, sponsor_type, sample_size,
         vaccine, conventional, traditional)

x %>% 
  select(control_arm:traditional) %>% 
  lapply(function(x) table(x, useNA = "a"))


# Bridging flag -----
# table(d$Bridging_flag, useNA = "a")
# count(d, Bridging_flag) %>% 
#   View() # 326 need review

# Notes -----

# count NAs
# x <- lapply(d_2, is.na)
# 
# sapply(x, sum)


# d_2 %>%
#   filter(study_arm == "main") %>%
#   count(prospective)

# single centre less common term than multi
# 
# 234 yes for multi-centre


# ictrp <- bind_rows(main, im)
# 
# ictrp <- ictrp %>% 
#   filter(Source_registry != "CT.gov", 
#          Source_registry != "EUCTR", 
#          Source_registry != "IRCT")
# 
# unique(ictrp$Study_design)
# d <- d %>% 
#   group_by(Source_registry) %>% 
#   filter(n() >=50) 
# table(d$Source_registry)
# 
# unique(design$allocation)
# anyNA(design$allocation)
# unique(design$intervention_model)
# anyNA(design$intervention_model)
# # sequential - is this controlled? 
#   # NCT03801915 = Yes
#   # NCT04162340 = No 
#   # NCT04061590 = Yes
#   # NCT03895879 = Yes
#   # NCT03844217 = No
#   # NCT04082325 = Yes
#   # NCT04104672 = Yes
#   # NCT04185090 = Yes
#   # NCT04187404 = No
# # looks like this needs to be manual
# unique(design$masking)
# anyNA(design$masking)
# unique(design$primary_purpose_ct)
# anyNA(design$primary_purpose_ct)
# 
# x <- lapply(all, is.na)
# # seems that the problem is coming from the NAs?
# lapply(x, sum)
# 
# # check correct length
# d %>% 
#   filter(Source_registry != "DRKS",
#          Source_registry != "LBCTR",
#          Source_registry != "NTR",
#          Source_registry != "PACTR",
#          Source_registry != "PER",
#          Source_registry != "RBR",
#          Source_registry != "RPCEC",
#          Source_registry != "TCTR") %>% 
#   nrow()

