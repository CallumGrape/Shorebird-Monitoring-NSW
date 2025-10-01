
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Relative behavior analysis with the use of signal strength
## Created:  2025 July 

# 1 - Packages ----

library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 
library(sf)
library(lubridate)
library(rnaturalearth)
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
  )), 1)) # pick up the most recent .rds file

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 


# REMINDER = We just moved up the sig values as following:
# Positive signal strength (min. = 0) for plotting
# mutate(sigPositive = sig + abs(min(sig)))

data <- data_all %>%
  select(Band.ID, recvDeployName, recv, speciesEN, sigPositive, tideCategory, timeAus)

# Color codes
species_colors <- c(
  "Bar-tailed Godwit"      = "#1b9e77",  
  "Far Eastern Curlew"     = "#d95f02",  
  "Masked Lapwing"         = "#7570b3", 
  "Pacific Golden-Plover"  = "#e7298a", 
  "Pied Stilt"             = "#66a61e", 
  "Red-necked Avocet"      = "#e6ab02"   
)

ggplot(data, aes(x = tideCategory, y = sigPositive, fill = speciesEN)) +
  geom_boxplot() +
  scale_fill_manual(values = species_colors) +
  facet_wrap(~ recvDeployName) +
  theme_bw() +
  labs(
    title = "Signal Strength by Tide Category and Species per Receiver Deployment",
    x = "Tide Category",
    y = "Signal Strength",
    fill = "Species"
  )


