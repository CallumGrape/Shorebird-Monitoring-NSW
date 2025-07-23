
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Process shorebirds data, preliminary analysis
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
  here::here("10_data", "alltags", "motus.rds"),
  pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) # pick up the most recent .rds file

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("10_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 

# Data summary (To be edited still...)
tagSummary1 <- data_all %>%
  group_by(motusTagID, recvDeployName) %>% 
  summarize(nDet = n(),
            nRecv = n_distinct(recvDeployName),
            timeMin = min(time),
            timeMax = max(time),
            totDay = length(unique(doy)), 
            species = first(speciesEN),
            .groups = "drop") %>%
  relocate(motusTagID, species)

tag_summary2 <- data_all %>%
  group_by(recvDeployName, tideCategory, speciesEN) %>%
  summarise(nTags = n_distinct(motusTagID), .groups = "drop")

# Selecting variables
data <- data_all %>%
  select(
    # Project ID
    recvProjID,
    tagProjID,
    # Tag
    motusTagID,
    # Time
    time,
    timeAus,
    year,
    # Receiver
    recv,
    recvDeployName,
    # Tide cycle
    tideHeight,
    tideCategory,
    # Circadian cycle
    sunriseNewc,
    sunsetNewc,
    # Signal strengh
    sigPositive,
    # Bird info
    speciesID, speciesEN, speciesFR, speciesSci,
    speciesGroup,
    # Comment
    tagDepComments
    )

# 4 - Summarizing plots ----

ggplot() +
  facet_grid(~ recvDeployName) +
  geom_bar(aes(data, x = tideCategory, y = n_distinct(motusTagID), fill = speciesEN)) +
  theme_bw() + 
  labs(x = "Tide category", y = "Detections", fill = "Species") 

ggplot(tag_summary2, aes(x = tideCategory, y = nTags, fill = speciesEN)) +
  geom_col(position = "stack") +   
  facet_wrap(~ recvDeployName) +   
  theme_bw() +
  labs(x = "Tide Category", y = "Number of detection", fill = "Species")













# 5 - Summarizing maps ---- 

# To be continued ...
# Go to:  https://motuswts.github.io/motus/articles/06-exploring-data.html










