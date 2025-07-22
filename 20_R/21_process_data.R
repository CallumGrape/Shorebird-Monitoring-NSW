
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Process shorebirds data, preliminary analysis
## Created:  2025 July 

# 1 - Packages ----

# install.packages("motus", 
#                  repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                            CRAN = 'https://cloud.r-project.org'))
library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 


# 2 - Settings ----

# Global
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #setwd where the file is

# 3 - Load data ----
data <- readRDS(here::here("10_data", "data.rds"))











# # Remove NA & undesired receivers
# df.alltags <- df.alltags %>% 
#   filter(!is.na(recvDeployName)) %>% 
#   filter(!(recvDeployName %in% c("Throsby Creek Test Site"))) %>% 
#   mutate(recvDeployName = recode(recvDeployName, 
#                                  !!!station_rename_map))
# 
# # Rename receivers
# station_rename_map <- list(
#   "Barry_Fullerton_cove"  = "Fullerton Entrance",
#   "North Swann Pond"      = "Swan Pond" ,
#   "Ramsar Road Floodgate" = "Ramsar Road",
#   "Milham's Pond"         = "Milhams Pond")
# df.alltags <- df.alltags %>% 
#   mutate(stationName = recode(stationName,
#                               !!!station_rename_map))


