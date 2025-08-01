
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Access the relative site usage from birds in regard of the survey period for each MOTUS station 
## Created:  2025 July 


# 1 - Packages ----

library(motus)
library(dplyr)
library(here)
library(forcats) 
library(ggplot2)

# 2 - Settings ----

# Set WD
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# 3 - Load data ----

# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) 

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 

# 3 - Extracting receiver with offline periods ----

# Filter which station has been not continuously ON
recv_off_chk <- recv %>%
  arrange(recvDeployName, timeStartAus) %>% # sort by site + time
  group_by(recvDeployName) %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>% 
  filter(!is.na(offline_start) & offline_end > offline_start) %>%
  mutate(timeOff = round(as.numeric(difftime(offline_end, offline_start, units = "days")), digits = 2)) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus, offline_start, offline_end, timeOff) 
recv_off_chk

# List the meant stations
list_recv_off <- unique(recv_off_chk$recvDeployName)

# Check whether this makes sense
recv_off_chk <- recv %>% 
  filter(recvDeployName %in% list_recv_off) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus) %>%
  arrange(recvDeployName, timeStartAus)  %>%
  left_join(recv_off_chk %>% select(recvDeployName, timeOff),
            by = "recvDeployName")
recv_off_chk

# Filter out gaps under 24h
recv1 <- recv %>% 
  filter(timeStartAus > min(data_all$timeAus
                            # %>% filter(motuTagID = c("")) # TEST TAG TO REMOVE FIRST!!
                                       )) 

  
# 4 - Filter out gaps under 24h & date before 1st tag deployment ----








