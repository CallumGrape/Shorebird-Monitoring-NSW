
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Download and process MOTUS data, going separately by each receivers
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

# 4 - Download data per Receivers ----

# Transform sql to tble
df.recvDeps <- tbl(sql.motus, "recvDeps") %>% 
  collect() %>% 
  as.data.frame()

# Download and add meta-data to df.recvDeps (a lot!)
metadata(sql.motus, proj.num)

# Extract receiver code
df.serno <- tbl(sql.motus, "recvDeps") %>% 
  filter(projectID == 294) %>%
  select(serno) %>% # serno = the receiver code
  distinct() %>%
  collect() %>% 
  as.data.frame()

# Download data/receivers
for(row in 1:nrow(df.serno)) {
  sql_motus <- tagme(df.serno[row, "serno"],
                     new = FALSE, # TRUE overwrites existing (large data takes a while)
                     update = TRUE, 
                     dir = here("10_data", "receivers"))
  metadata(sql_motus)
}

# 5 - Process data ----

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






