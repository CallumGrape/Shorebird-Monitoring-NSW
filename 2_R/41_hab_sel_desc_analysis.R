
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
  here::here("1_data", "alltags", "motus.rds"),
  pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) # pick up the most recent .rds file

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 

# 3 - Arranging the data ----

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

# 4 - Site usage depending tide and circadian cycle (from birds) ----

# Color codes
species_colors <- c(
  "Bar-tailed Godwit"      = "#1b9e77",  
  "Far Eastern Curlew"     = "#d95f02",  
  "Masked Lapwing"         = "#7570b3", 
  "Pacific Golden-Plover"  = "#e7298a", 
  "Pied Stilt"             = "#66a61e", 
  "Red-necked Avocet"      = "#e6ab02"   
)

tag_summary2 %>%
  ggplot(aes(x = tideCategory, y = nTags, fill = speciesEN)) +
  geom_col(position = "stack") +   
  facet_wrap(~ recvDeployName) +   
  theme_bw() +
  labs(x = "Tide Category", y = "Number of individual detected", fill = "Species") +
  scale_fill_manual(values = species_colors)

tag_summary2 %>%
  split(.$recvDeployName) %>%
  walk2(names(.), ~{
    p <- ggplot(.x, aes(x = tideCategory, y = nTags, fill = speciesEN)) +
      geom_col(position = "stack") +
      facet_wrap(~ speciesEN) +
      scale_fill_manual(values = species_colors) +
      theme_bw() +
      labs(
        title = .y,
        x = "Tide Category", 
        y = "Number of individual detected", 
        fill = "Species")
    print(p)
  })

# 5 - Birds movement direction ----

# Would be cool to map the recv across estuaries and plot the x = hour and y = signal strengh with directionnal arrow
# BY SPECIES
# So a facet_wrap, generating a map by species, where at each rcv you get directional arrow 
# + period (and amount) of hours flew by 

# https://motuswts.github.io/motus/articles/signal-strength.html
# ggplot(data = filter(df_tags, motusTagID == 16039), 
#        aes(x = time, y = sig, colour = runLen_cat, shape = antBearing_cat)) + 
#   geom_point(size = 8) + 
#   theme_bw() +
#   theme(legend.position = "top") +
#   labs(x = "Time", y = "Signal strength") +
#   scale_colour_viridis_d(end = 0.7) +
#   scale_shape_manual(values = c("N" = "\u2191", "S" = "\u2193",
#                                 "E" = "\u2192", "W" = "\u2190"), 
#                      na.value = "\u25AA") +
#   facet_wrap(~ date, scales = "free", ncol = 3)



