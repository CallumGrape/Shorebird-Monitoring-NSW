
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Download MOTUS data, all tags recorded through the worldwide MOTUS network (get my tags by anyone’s receivers)
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

# Global
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
Sys.setenv(TZ="UTC") 
motusLogout()

# Project Number
proj.num <- 294       

# 3 - Download all data and metadata per Project ----

# Load data from online network (either 1st time or update)
# sql.motus <- tagme(projRecv = proj.num, 
#                    new = FALSE, # TRUE overwrites existing (large data takes a while)
#                    update = TRUE, 
#                    dir = here("1_data", "motus.sql"))
# metadata(sql.motus, proj.num)

# Load local data
sql.motus <- dbConnect(SQLite(), here::here("1_data", "alltags", "project-294.motus"))

# Load tide (Callum work 01_import_tide_data.R)
tidalCurve <- readRDS(here::here("1_data", "tides", "tidalCurve.rds"))
tideData <- readRDS(here::here("1_data", "tides", "tideData.rds"))
tidalCurveFunc <- splinefun(tideData$tideDateTimeAus, tideData$tideHeight, method = "natural")
get.tideIndex <- function(time){ return(which.min(abs(tideData$tideDateTimeAus-time)))}

# 4 - Extract data ----

# All tags MOTUS recorded for the project
df.alltags <- tbl(sql.motus, "alltags") %>%
  dplyr::collect() %>%
  as.data.frame() %>%
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time), 
         doy = yday(time)) 

# Specs for all tags MOTUS recorded for the project
df.tags <- tbl(sql.motus, "tags") %>%
  filter(projectID == proj.num) %>% # Has to be specified!
  dplyr::collect() %>% 
  as.data.frame()

# Specs for tags MOTUS recorded for the project & deployed
df.tagdeps <- tbl(sql.motus, "tagdeps") %>%
  dplyr::collect() %>%
  as.data.frame()

# 5 - Filtering tag data ----

# Cleaning and correcting tags metadata
df.alltags <- df.alltags %>% 
   filter(
    # test tags
     motusTagID != c("43291"),
    # pending, unconfirmed or undeployed tags
    !motusTagID %in% c("43288", "43291", "43297", "43299",
                       "43307", "43424", "43425", "60470", 
                       "60579", "81123", "81136", "81137"),
    # used for test/validation before tagging bird (remove time before the tagging)
    !(motusTagID == "81134" & time < dmy("23-11-2024")),
    !(motusTagID == "60575" & time < dmy("25-10-2023")) ) %>% 
    # NA species
     mutate(speciesEN = case_when(
       is.na(speciesEN) & motusTagID %in% c("60470", "81121") ~ "Red-necked Avocet",
       is.na(speciesEN) & motusTagID %in% c("81118") ~ "Red-necked Avocet",
       TRUE ~ speciesEN))
  
# Cleaning and correcting receiver metadata
df.alltags <- df.alltags %>% 
  filter(
    # NA
     !is.na(recvDeployLat),
    # site not any longer used
      recvDeployName != c("Throsby Creek Test Site"),
    # test sensor gnome
      recv != c("SG-C621RPI3E17F",       
                "SG-62A5RPI36710") ) %>% 
  mutate(recvDeployName = ifelse(is.na(recvDeployName) & recv == "SG-D5BBRPI3E2F7", "Windeyers", recvDeployName))
    
     
# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data
         runLen >= 3) # value to be further thought

# Ambiguous (if != 0 then refer to https://motuswts.github.io/motus/articles/05-data-cleaning.html)
clarify(sql.motus)

# 6 - Adding variables ----

df.alltags <- df.alltags %>% 
  
# Sunrise/set
  sunRiseSet(lat = "recvDeployLat", 
             lon = "recvDeployLon", 
             ts = "ts") %>% 
  mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
  
# Positive signal strength (min. = 0) for plotting
  mutate(sigPositive = sig + abs(min(sig))) %>%
  
# As Factor
  mutate(motusTagID = as.factor(motusTagID))

# Tide
df.alltags <- df.alltags %>% 
  mutate(tideHeight = tidalCurveFunc(timeAus),
         tideIndex = map_dbl(timeAus, get.tideIndex))

tide_values <- tideData[df.alltags$tideIndex, 
                        c("tideDateTimeAus",
                          "high_low",
                          "day_night",
                          "tideCategory",
                          "tideID",
                          "tideHeight")]

df.alltags <- df.alltags %>%
  mutate(tideDateTimeAus = tide_values$tideDateTimeAus,
         tideHighLow = as_factor(tide_values$high_low),
         tideDiel = as_factor(tide_values$day_night),
         tideCategory = as_factor(tide_values$tideCategory),
         tideCategoryHeight = tide_values$tideHeight,
         tideID = as_factor(tide_values$tideID),
         tideTimeDiff = abs(difftime(timeAus, tideDateTimeAus, units = "hours")) ) # Time diff btw detect. & nearest tide pts

# 7 - Filtering receivers data ----

# Get summary
df.recvDeps <- tbl(sql.motus, "recvDeps") %>% 
  collect() %>% 
  as.data.frame() %>%
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"),
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney"))

# Rename stations
station_rename <- list(
   "Barry_Fullerton_cove"  = "Fullerton Entrance",
   "North Swann Pond"      = "Swan Pond" ,
   "Ramsar Road Floodgate" = "Ramsar Road",
   "Milham's Pond"         = "Milhams Pond")
df.recvDeps <- df.recvDeps %>% 
  mutate(name = recode(name, !!!station_rename)) %>%
  rename(recvDeployName = "name")

df.alltags <- df.alltags %>% 
  mutate(recvDeployName = recode(recvDeployName,
                                 !!!station_rename)) 
  
# Filter not used stations as out of the local array
df.recvDeps <- df.recvDeps %>% 
  filter(!is.na(latitude),
         recvDeployName != "Throsby Creek Test Site", # not used any longer
         !serno %in% c("SG-C621RPI3E17F",             # test_station
                       "SG-62A5RPI36710"))            # test_station

# 9 - Match starting date for survey effort from the antennas to the first day a bird has been tagged
df.recvDeps <- df.recvDeps %>%
  filter(timeStartAus > "2023-01-31 00:00:00 AEDT")  # BASED ON SHAREPOINT 1 MOTNH BEFORE THE 1ST TAGGED BIRD


# 10 - Save

saveRDS(df.alltags, here::here("1_data", "alltags", "motus.rds", paste0(Sys.Date(), "-data", ".rds" )))
saveRDS(df.recvDeps, here::here("1_data", "alltags", "motus.rds", paste0(Sys.Date(), "-recv-info", ".rds" )))










