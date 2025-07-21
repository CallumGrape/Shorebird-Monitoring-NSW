
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Download and process MOTUS data, alltags
## Created:  2025 July 

# MOTUS Username: c3541851@uon.edu.au
# MOTUS Password: C**********2*![****]


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

setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #setwd where the file is
Sys.setenv(TZ="UTC") 
motusLogout()
proj.num <- 294  # Motus project ID (all data) OR receiver code (receiver specific data)        


# 3 - Download all data per Project ----

sql.motus <- tagme(projRecv = proj.num, 
                   new = FALSE, # TRUE overwrites existing (large data takes a while)
                   update = TRUE, 
                   dir = here("10_data"))

# 4 - Process data ----

# Rename receivers
station_rename_map <- list(
  "Barry_Fullerton_cove"  = "Fullerton Entrance",
  "North Swann Pond"      = "Swan Pond" ,
  "Ramsar Road Floodgate" = "Ramsar Road",
  "Milham's Pond"         = "Milhams Pond")
df.recvDeps <- df.recvDeps %>% 
  mutate(stationName = recode(stationName,
                              !!!station_rename_map))

# Extract a manageable data frame
df.alltags <- tbl(sql.motus, "alltags") %>% 
  collect() %>% 
  as.data.frame()

tagIDs <- (sql.motus %>% 
  tbl("tagdeps") %>% 
  as.data.frame() %>% 
  filter(!is.na(speciesID)) %>% 
  select(tagID))$tagID %>%
  unique() # BECAREFUL if ever you get same ID for multiple individuals

# Remove NA & undesired receivers
df.alltags <- df.alltags %>% 
  filter(!is.na(recvDeployName)) %>% 
  filter(!(recvDeployName %in% c("Throsby Creek Test Site"))) %>% 
  mutate(recvDeployName = recode(recvDeployName, 
                                 !!!station_rename_map))
# Filtering data for tags
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1) %>% 
  filter(is.na(speciesEN)==FALSE)
  
# Adding variable
df.alltags <- df.alltags %>% 
  # Time
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus)) %>%
  # Sunrise/set
  sunRiseSet(df.alltags, 
             lat = "recvDeployLat", 
             lon = "recvDeployLon", 
             ts = "ts") %>% 
  mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
  # Positive signal strength (min. = 0) for plotting
  mutate(sigPositive = sig + abs(min(sig))) %>%
  # As Factor
  mutate(motusTagID = as.factor())







