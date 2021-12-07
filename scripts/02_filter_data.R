# Filter cleaned data prior to manual review

# Removes withdrawn trials, limits to interventional trials, limits dates,
# filters conditions, randomly orders rows and creates csvs for manual
# eligibility screen

# Load -----
library(tidyverse)

# Inputs -----
file_path <- "data/"
input_subfolder <- "initial_import/clean/"
name_ictrp <- "ictrp.R"
name_covid <- "covid.R"
output_folder <- "manual_processing/"
output_subfolder <- "eligibility_screen/"

# Create folders -----
if(!dir.exists(paste0(file_path, output_folder))) {
  dir.create(paste0(file_path, output_folder))
  dir.create(paste0(file_path, output_folder, output_subfolder))
}

if(!dir.exists(paste0(file_path, output_folder, output_subfolder))) {
  dir.create(paste0(file_path, output_folder, output_subfolder))
}

# Import data -----
load(paste0(file_path, input_subfolder, name_covid))
load(paste0(file_path, input_subfolder, name_ictrp))

# standardise some colnames
ictrp <- rename(ictrp, Public_title = public_title, 
       Study_type = study_type,
       Study_design = study_design, 
       Phase = phase)
# colnames when writing the if statement. Ensures they don't get wrongly labelled
cols <- c('Source_registry','TrialID','Public title','Scientific title',
  'web address','Date_registration_format','Date_enrollment_format',
  'Day_inferred','Study type','Study design','Phase','Target size',
  'Primary sponsor','Countries','Intervention','Condition',
  'Bridging flag truefalse','Retrospective flag')
if(identical(cols, colnames(covid[1:18]))){
  colnames(covid)[1:18] <- colnames(ictrp)[1:18]
} else {
  stop("Colnames differ from what the should be. Was the order of columns changed?")
}
rm(cols)

covid <- rename(covid, Recruitment_status = `Recruitment Status`)

# Remove withdrawn ----
# NA values are removed by default so need to keep them when excluding withdrawn
ictrp <- ictrp %>% 
  filter(is.na(Recruitment_status) | Recruitment_status != "Withdrawn")
# removes 672212 - 662604 = 9608

covid <- covid %>% 
  filter(is.na(Recruitment_status) | Recruitment_status != "Withdrawn")
# removes none 

# Interventional filter -----
# get the interventional study labels
types <- unique(ictrp$Study_type)
types <- types[grep("intervention", unique(ictrp$Study_type), ignore.case = T)]
# removing this only removes trials from 2017 or before so has no impact on the
# results, but is done for completeness
types <- types[types != "Interventional,observational"]
# review indicates treatment study are interventional. All from ChiCTR
types <- c(types, "Treatment study") 

ictrp <- filter(ictrp, Study_type %in% types)
covid <- filter(covid, Study_type %in% types)
rm(types)

# Limit dates -----
# im = indication-matched control
ictrp_im <- ictrp %>% 
  filter(Date_registration_format >= "2018-01-01" & 
         Date_registration_format <= "2019-12-31")

# main control  
ictrp_main <- ictrp %>% 
  filter(Date_registration_format >= "2019-01-01" & 
           Date_registration_format <= "2019-12-08")
# rm(ictrp)

# covid arm 
covid <- covid %>% 
  filter(Date_registration_format >= "2020-01-01")

# Filter COVID conditions -----

# covid synonyms directly from supp table 4 of S1-IPA (+COVID-19)
cov_conds <- c("COVID-19", "COVID", "coronavirus", "SARS-CoV-2", 
               "severe acute respiratory syndrome coronavirus 2", 
               "2019-nCoV", "2019 novel coronavirus",
               "Wuhan coronavirus")

# # I used the following code to check whether any relevant conditions were missed
# # with our search terms by manually reviewing the conditions:
# rows <- grep(paste(cov_conds,collapse="|"), covid$Conditions, ignore.case= T)
# conds <- covid[-rows,]$Conditions %>% 
#   unique()
# 
# # manual review revealed extra ones of relevance:
cov_conds <- c(cov_conds, "2019 nCoV", "Sars-CoV2", "SARS CoV-2", 
              "SARS-CoV 2", "CoV2 SARS", "Sars Cov 2", "SARSCo-V-2",
              "New Corona virus disease 2019", "COVD-19",
              "Corona Virus Disease 2019", "SARS-COV-19")

rows <- grep(paste(cov_conds,collapse="|"), 
             covid$Conditions, ignore.case= T)
covid <- covid[rows, ]

# remove any containing these terms from ictrp data
rows <- grep(paste(cov_conds,collapse="|"), 
             ictrp_main$Conditions, ignore.case= T)
ictrp_main <- ictrp_main[-rows, ]

rows <- grep(paste(cov_conds,collapse="|"), 
             ictrp_im$Conditions, ignore.case= T)
ictrp_im <- ictrp_im[-rows, ]

# Filter indication-matched conditions -----

# conditions copied directly from inclusion criteria is S1-IPA
# IMPORTANT: there is a mistake in the im_conds list which is corrected below. 
# See section "correct indication matched filtering" for information and correction
im_conds <- c("septic shock",
              "multi organ failure", "multiple organ failure", "multiple organ dysfunction",
              "syndrome", "multiple systems organ failure", "multisystem organ failure",
              "cardiogenic shock",
              "myocarditis", "myocardial inflammation",
              "myocardial ischaemia",
              "respiratory failure", "respiratory insufficiency",
              "ARDS", "respiratory distress syndrome",
              "Pneumonia",
              "influenza", "flu",
              "respiratory arrest", "apnea", "breathing cessation", "breathing stops", "pulmonary arrest",
              "cardiac arrest", "heart attack", "asystole", "asystolia", "asystolic")

rows <- grep(paste(im_conds,collapse="|"), 
             ictrp_im$Conditions, ignore.case= T)

ictrp_im <- ictrp_im[rows, ] 

# Randomly order rows -----
# Randomly order rows, make column order convenient for manual review of
# inclusion, and limit to a reasonable data size for manual review. 

# use the seed and other code we prespecifed, which was:
# rows <- sample(nrow(ictrp_data))
# ictrp_random <- ictrp_data[rows, ]
# we simply change the dataset name for each dataset below
set.seed(1234)

rows <- sample(nrow(ictrp_main)) 
ictrp_main <- ictrp_main[rows, ]

rows <- sample(nrow(ictrp_im)) 
ictrp_im <- ictrp_im[rows, ]

rows <- sample(nrow(covid)) 
covid <- covid[rows, ]

# add include column for manual extraction and reorder cols for ease of review
ictrp_main <- ictrp_main %>% 
  mutate(Include = NA) %>% 
  select(Include, Scientific_title, Interventions, url, Public_title, everything()) 
# limit size as very unlikely to need more to reach sample size
ictrp_main <- ictrp_main[1:3500, ]

ictrp_im <- ictrp_im %>% 
  mutate(Include = NA) %>% 
  select(Include, Scientific_title, Interventions, url, Public_title, everything()) 

covid <- covid %>% 
  mutate(Include = NA) %>% 
  select(Include, Scientific_title, Interventions, url, Public_title, everything())

# Write files ----- 
write_csv(ictrp_main, paste0(file_path, output_folder, output_subfolder, "ictrp_main.csv"))
# write_csv(ictrp_im, paste0(file_path, output_folder, output_subfolder, "ictrp_im.csv"))
write_csv(covid, paste0(file_path, output_folder, output_subfolder, "covid.csv"))

# Correct indication matched filtering -----

# There is a mistake in the filtering of indications in the indication matched
# dataset above: "syndrome" is used alone rather than as part of the full
# expression: "multiple organ dysfunction syndrome". We also noticed that the
# terms "flu" and "ards" were returning irrelevant results. Here this is
# corrected. Correcting the filtering results in many fewer potentially eligible
# trials, so we also extend the time frame over which we sample.

# This is done here rather than correcting above as that would change the
# specific random sample drawn for the other arms, which had already been
# reviewed when this mistake was noticed.

# filter the dates. 2016 should give sufficient sample size
ictrp <- ictrp %>% 
  filter(Date_registration_format >= "2016-01-01" & 
           Date_registration_format <= "2019-12-31")

im_conds <- c("septic shock",
              "multi organ failure", "multiple organ failure", 
              "multiple organ dysfunction syndrome",
              "multiple systems organ failure", "multisystem organ failure",
              "cardiogenic shock",
              "myocarditis", "myocardial inflammation",
              "myocardial ischaemia",
              "respiratory failure", "respiratory insufficiency",
              "ARDS", "respiratory distress syndrome",
              "Pneumonia",
              "influenza", "flu",
              "respiratory arrest", "apnea", "breathing cessation", "breathing stops", "pulmonary arrest",
              "cardiac arrest", "heart attack", "asystole", "asystolia", "asystolic")

rows <- grep(paste(im_conds,collapse="|"), 
             ictrp$Conditions, ignore.case= T)

ictrp_im <- ictrp[rows, ]

# irrelevant "conditions" that come up when "flu" searched
flu_false_conds <- c("fluor", "influenc", "fluid", "flush", "effluvium", 
                     "reflux", "flutter", "flurane", "Flurpiridaz", "fluent", 
                     "leflunomide", "Fluconazole", "fludarabine", 
                     "fluctuat", "fluoxetine", "Scheimpflug","TRIFLURIDINE",
                     "flucytosine", "fluoxetine")

rows <- grep(paste(flu_false_conds, collapse="|"),
             ictrp_im$Conditions, ignore.case= T)

ictrp_im <- ictrp_im[-rows, ]

# irrelevant "conditions" when searching "ards"
ards_false_conds <- c("Stewards", "hazards", "towards", "wards",
                      "regards", "retardSystem", "Richardson")

rows <- grep(paste(ards_false_conds, collapse="|"),
             ictrp_im$Conditions, ignore.case= T)

ictrp_im <- ictrp_im[-rows, ]

# remove any covid trials
rows <- grep(paste(cov_conds, collapse="|"),
             ictrp_im$Conditions, ignore.case= T)

ictrp_im <- ictrp_im[-rows, ]

# randomly order the rows according to the pre-specified code
set.seed(1234)
rows <- sample(nrow(ictrp_im)) 
ictrp_im <- ictrp_im[rows, ]

ictrp_im <- ictrp_im %>% 
  mutate(Include = NA) %>% 
  select(Include, Scientific_title, Interventions, url, Public_title, everything()) 

# Write corrected file -----
write_csv(ictrp_im, paste0(file_path, output_folder, output_subfolder, "ictrp_im.csv"))


