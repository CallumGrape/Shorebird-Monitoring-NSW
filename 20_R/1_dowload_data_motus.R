
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Download and process MOTUS data
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

Sys.setenv(TZ="UTC") 
motusLogout()
proj.num <- 294  # Motus project number        


# 3 - TAG: Download data ----

# Tag data (tags & receivers)
sql.motus <- tagme(projRecv = proj.num, 
                   new = FALSE, # TRUE overwrites existing (large data takes a while)
                   update = TRUE, 
                   dir = here("10_data"))

# Tag meta-data (a lot!)
metadata(sql.motus, 
         proj.num)
start_time <- Sys.time()
metadata(sql.motus)
end_time <- Sys.time()
cat("\nMetadata update took ",end_time-start_time," seconds.\n")

# 4 - Receivers: Download data ----

# Receiver array info
df.recvDeps <- tbl(sql.motus, "recvDeps") %>% 
  collect() %>% 
  as.data.frame()
df.serno <- tbl(sql.motus, "recvDeps") %>%
  filter(projectID == 294) %>%
  select(serno) %>%
  distinct() %>%
  collect() %>% as.data.frame()

# Receivers data & meta-data
for(row in 1:nrow(df.serno)) {
  sql_motus <- tagme(df.serno[row, "serno"],
                     new = FALSE, # TRUE overwrites existing (large data takes a while)
                     update = TRUE, 
                     dir = here("10_data"))
  metadata(sql_motus)
}

# Rename receivers
station_rename_map <- list(
  "Barry_Fullerton_cove"  = "Fullerton Entrance",
  "North Swann Pond"      = "Swan Pond" ,
  "Ramsar Road Floodgate" = "Ramsar Road",
  "Milham's Pond"         = "Milhams Pond")

df.recvDeps <- df.recvDeps %>% 
  mutate(stationName = recode(stationName,
                              !!!station_rename_map))
# Correct receivers time
df.recvDeps <- df.recvDeps %>% 
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"),
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney"))


# 5 - TAG: Process data ----

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
  filter(!(recvDeployName %in% c("Throsby Creek Test Site")))
df.alltags <- df.alltags %>% 
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







