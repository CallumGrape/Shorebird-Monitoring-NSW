
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Figure 2 - Access the efficiency of the local MOTUS array depending the monitored bird species
## Created:  2025 August



# 1 - Packages ----

library(dplyr)
library(tidyr)
library(here)
library(ggplot2)
library(lubridate)

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

# 4 - Process variables ----

# Get deployment duration for each tags (in days)
tag_dep_duration <- data_all %>%
  group_by(Band.ID) %>%
  summarise(start_date = min(DateAUS.Trap),
            end_date = max(dateAus),
            period_tag_dep_d = 1 + time_length(interval(start = start_date, end = end_date), unit = "day"), # +1 to include the trapping day
            .groups = "drop") %>%
  select(Band.ID, period_tag_dep_d)

# Get number days each tag has been detected at least once in any station
tag_dep_nb_d <- data_all %>%
  mutate(dateAus = as.Date(dateAus)) %>%
  group_by(Band.ID) %>%
  summarise(days_detect = n_distinct(dateAus), .groups = "drop")

# Get number of days one tag has been recorded in >1 station (for each tags)
multiple_site_detect <- data_all %>%
  group_by(Band.ID, dateAus) %>%
  # Counts per day the number of station one tag has been recorded
  summarise(distinct_sites = n_distinct(recvDeployName),
            .groups = "drop_last") %>%
  # Tells if one day one tag has been recorded several sites
  mutate(multiple_detect = distinct_sites > 1) %>%
  group_by(Band.ID) %>%
  summarise(multiple_site_detect = sum(multiple_detect),
            .groups = "drop")

# 5 - Final table ----

# Get number of day each tag has been recorded /species and /station
tag_detection <- data_all %>%
  group_by(Band.ID, recvDeployName, speciesEN) %>%
  # to count sp*ID per recv
  reframe(days_recorded = n_distinct(dateAus)) %>%
  # flip table to get one column per station
  pivot_wider(names_from = recvDeployName, 
              values_from = days_recorded,
              values_fill = 0) %>% # = 0 if NA
  left_join(multiple_site_detect, by = "Band.ID") %>%
  left_join(tag_dep_nb_d, by = "Band.ID") %>%
  left_join(tag_dep_duration, by = "Band.ID") %>%
  mutate(perc = round((days_detect / period_tag_dep_d) * 100, 1)) # nb of day one tag is recorded once / nb of day the tag is deployed

# list per species
list_species_tables <- tag_detection %>%
  group_by(speciesEN, .add = TRUE) %>%
  group_split()

# 6 - Plot results ----

# Plot per birds
ggplot(tag_detection %>%
         group_by(speciesEN) %>%
         arrange(desc(perc), .by_group = TRUE) %>%
         mutate(Band.ID = factor(Band.ID, levels = unique(Band.ID))), 
       aes(x = Band.ID, y = perc, fill = speciesEN)) +
  geom_col(width = 0.8, alpha = 0.7, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom") +
  labs(x = "Band ID",
       y = "Detection (%)",
       title = "Bird detection across MOTUS array (% of days)")

# Plot per species
ggplot(tag_detection %>%
         group_by(speciesEN) %>%
         summarise(n = n_distinct(Band.ID),            
                   mean_perc = mean(perc, na.rm = TRUE)) %>%
         mutate(species_label = paste0(speciesEN, " (n = ", n, ")")) %>%
         arrange(desc(mean_perc)) %>%
         mutate(species_label = factor(species_label, levels = species_label)), 
       
       aes(x = species_label, y = mean_perc, fill = species_label)) +
  
  geom_col(width = 0.8, alpha = 0.7, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(x = "Species (number of individuals)",
       y = "Detection (%)",
       title = "Species detectability across the MOTUS array (%)")

# Box plot
ggplot(tag_detection %>%
         add_count(speciesEN) %>%
         mutate(species_label = paste0(speciesEN, " (n = ", n, ")")),
       aes(x = reorder(species_label, -perc, FUN = median),
           y = perc, fill = species_label)) +
  
  geom_boxplot(width = 0.8, alpha = 0.7, color = "black") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "none") +
  labs(x = "Species (number of individuals)",
       y = "Detection (%)",
       title = "Species detectability across the MOTUS array")




