
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Check the history of recevers, SG and sites/stations
## Created:  2025 September 

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
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #setwd where the file is
Sys.setenv(TZ="UTC") 
motusLogout()

# Project Number
proj.num <- 294         


# 3 - Download all data per Project ----

# sql.motus <- tagme(projRecv = proj.num, 
#                    new = FALSE, # TRUE overwrites existing (large data takes a while)
#                    update = TRUE, 
#                    dir = here("1_data", "receivers", "motus.sql"))

sql.motus <- DBI::dbConnect(RSQLite::SQLite(), here::here("1_data", "alltags", "project-294.motus"))

# 4 - Download data per Receivers ----

# Transform sql to tble
recv <- tbl(sql.motus, "recvDeps") %>% 
  collect() %>% 
  as.data.frame()

# Download and add meta-data to recv (a lot!)
# metadata(sql.motus, proj.num)

# Correct receivers time
recv <- recv %>% 
  mutate(timeStart = as_datetime(tsStart),
         timeStartAus = as_datetime(tsStart, tz = "Australia/Sydney"),
         timeEnd = as_datetime(tsEnd),
         timeEndAus = as_datetime(tsEnd, tz = "Australia/Sydney"))


# 5 - Check history of Receivers ----

# CHECK CONFLICT BETWEEN BOX ID AND STATION NAME
table(recv$serno, recv$name)

recv %>%
  group_by(name) %>%
  filter(n_distinct(serno) >= 2) %>%
  ungroup() %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>%
  select(name, serno, timeStartAus, timeEndAus) %>%
  arrange(name)

# Filter which station has been not continuously ON
recv_off_chk <- recv %>%
  arrange(name, timeStartAus) %>% # sort by site + time
  group_by(name) %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>% 
  filter(!is.na(offline_start) & offline_end > offline_start) %>%
  mutate(timeOff = round(as.numeric(difftime(offline_end, offline_start, units = "days")), digits = 2)) %>%
  select(name, serno, timeStartAus, timeEndAus, offline_start, offline_end, timeOff) 
recv_off_chk

# List the meant stations
list_recv_off <- unique(recv_off_chk$name)

# Check whether this makes sense
recv_off_chk <- recv %>% 
  filter(name %in% list_recv_off) %>%
  select(name, serno, timeStartAus, timeEndAus) %>%
  arrange(name, timeStartAus)  %>%
  left_join(recv_off_chk %>% select(name, timeOff),
            by = "name")
recv_off_chk

# Filter out gaps under 24h
recv1 <- recv %>% 
  filter(timeStartAus > min(data_all$timeAus
                            # %>% filter(motuTagID = c("")) # TEST TAG TO REMOVE FIRST!!
  )) 

