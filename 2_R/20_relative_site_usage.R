
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

# 3 - Extracting receiver with off-line periods ----

# Filter which station has been not continously ON
recv_off_per <- recv %>%
  arrange(recvDeployName, timeStartAus) %>%
  group_by(recvDeployName) %>%
  mutate(
    offline_start = lag(timeStartAus),
    offline_end = timeStartAus) %>%
  filter(!is.na(offline_start) & 
         offline_end > offline_start) %>%
  mutate(timeOff = offline_end - offline_start) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus, offline_start, offline_end, timeOff) 

# List the meant stations
list_recv_off <- unique(recv_off_per$recvDeployName)

# Compute the amount of time for the meant stations beeing OFF
recv %>% 
  filter(recvDeployName %in% list_recv_off) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus) %>%
  arrange(recvDeployName, timeStartAus)  %>%
  
# Add this as a variable to the main dataset for further substraction
  left_join(
    recv_off_per %>%
      select(recvDeployName, timeOff), by = "recvDeployName")
  
# 3 - Extracting receiver with off-line periods ----


