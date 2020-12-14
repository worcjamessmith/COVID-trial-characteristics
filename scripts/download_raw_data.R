# Download raw data

# One of the raw data files is too large to push to github. You can download it
# from the Open Science framework using this script and it will automatically go
# into the correct folder

# Load ----
library(osfr)
library(tidyverse)

# Download data -----
# used osf_auth while the project was private
osf_auth("") # add auth code here
project <- osf_retrieve_node("https://osf.io/pjc9s/")

osf_ls_files(project, path = "Large-raw-data") %>% 
  filter(name == "ICTRPFullExport-672212-23-11-20.csv") %>% 
  osf_download(path = "data/initial_import/raw")

rm(project)
