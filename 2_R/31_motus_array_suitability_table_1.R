
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Table 1 - Access the efficiency of the local MOTUS array depending the monitored bird species
## Created:  2025 August


# 1 - Packages ----

library(motus)
library(dplyr)
library(here)
library(forcats) 
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)

# 2 - Settings ----

# Set WD
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# 3 - Load data ----

# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1))  %>%
  mutate(DateAUS.Trap = as.Date(DateAUS.Trap),
         dateAus = as.Date(dateAus))

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 

# Tagging/Trapping info
tryCatch({
  write.csv(
    readxl::read_excel("C:/Users/c3541851/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/SHOREBIRD NUMBER TRACKING.xlsx"),
    file.path(here::here("1_data", "spreadsheets"), paste0(Sys.Date(), "-teams.sheet", ".csv")),
    row.names = FALSE
  )
}, error = function(e) {
  write.csv(
    readxl::read_excel("C:/Users/marin/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/SHOREBIRD NUMBER TRACKING.xlsx"),
    file.path(here::here("1_data", "spreadsheets"), paste0(Sys.Date(), "-teams.sheet", ".csv")),
    row.names = FALSE
  )
})
spreadsheet <- read.csv(here::here("1_data", "spreadsheets", paste0(Sys.Date(), "-teams.sheet.csv"))) %>%
  filter(Radio.tag. == "Y") %>%     
  rename(DateAUS.Trap = "Date", motusTagID = "Motus.tag.ID", speciesEN = "Species") %>% 
  mutate(motusTagID = as.factor(motusTagID)) %>%
  select(Band.ID, motusTagID, speciesEN, DateAUS.Trap, everything())  %>%
  mutate(speciesEN = case_when(
    speciesEN == "Eastern Curlew" ~ "Far Eastern Curlew",
    speciesEN == "Black-winged Stilt" ~ "Pied Stilt",
    speciesEN == "Pacific Golden Plover" ~ "Pacific Golden-Plover",
    TRUE ~ speciesEN 
  ))


# 4 - Set variables grouped per species ----

# Nb of individual equipped
nb_tagged <- spreadsheet %>% 
  group_by(speciesEN) %>% # per species
  summarise(nb_tagged = n())

# Nb of individual never detected
nb_undetect <- data_all %>%
  distinct(speciesEN) %>% # to force including all values of species                        
  left_join(spreadsheet %>%
              filter(is.na(Euthanised.),
                     !Band.ID %in% unique(data_all$Band.ID)) %>%
              group_by(speciesEN) %>%
              summarise(nb_undetect = n(), .groups = "drop"),
            by = "speciesEN") %>%
  mutate(nb_undetect = ifelse(is.na(nb_undetect), 0, nb_undetect))

# Nb of individual re-tagged
nb_retagged <- data_all %>%
  distinct(speciesEN) %>% # to force including all values of species                        
  left_join(spreadsheet %>%
              filter(is.na(Euthanised.),
                     !is.na(Retagged.)) %>%
              select(Band.ID, speciesEN, DateAUS.Trap, Location) %>%
              group_by(speciesEN) %>% # per species
              summarise(nb_retagged = n()),
            by = "speciesEN") %>%
  mutate(nb_retagged = ifelse(is.na(nb_retagged), 0, nb_retagged))

# First day monitored (trapping/tagging) 
first_d <- spreadsheet %>% 
  group_by(speciesEN) %>% # per species
  mutate(first_d = min(DateAUS.Trap)) %>%
  select(speciesEN, first_d) %>%
  unique()

# Last day recorded (detection)
last_d <- data_all %>% 
  group_by(speciesEN) %>% # per species
  mutate(last_d = max(dateAus)) %>%
  select(speciesEN, last_d) %>%
  unique()

# Nb of days monitored, total period of detection (mean + SE)
monit_d <- data_all %>%
  
  group_by(Band.ID) %>% # per individual
  summarise(DateAUS.Trap = first(DateAUS.Trap),         
            last_dateAus = max(dateAus),
            monit_d = as.numeric(last_dateAus - DateAUS.Trap) + 1) %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_monit_d = round(mean(monit_d), 0),
            se = sd(monit_d)/sqrt(n_indiv),
            se_lower = mean_monit_d - 1.96*se,
            se_upper = mean_monit_d + 1.96*se,
            mean_monit_d_se = ifelse(is.na(se), 
                                     NA,
                                     paste0("[", ifelse(se_lower < 0, 0, round(se_lower, 0)), "-", round(se_upper, 0), "]"))) %>%
  ungroup() %>%
  select(speciesEN, mean_monit_d, mean_monit_d_se)

# Nb of days actually detected (mean + SE)
detect_d <- data_all %>%
  
  group_by(Band.ID) %>% # per individual
  summarise(detect_d = n_distinct(dateAus)) %>%
  ungroup()  %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_detect_d = round(mean(detect_d), 0),
            se = sd(detect_d)/sqrt(n_indiv),
            se_lower = mean_detect_d - 1.96*se,
            se_upper = mean_detect_d + 1.96*se,
            mean_detect_d_se = ifelse(is.na(se), 
                                      NA, 
                                      paste0("[", ifelse(se_lower < 0, 0, round(se_lower, 0)), "-", round(se_upper, 0), "]"))) %>%
  ungroup() %>%
  select(speciesEN, mean_detect_d, mean_detect_d_se)

# Nb of sites visited per day (mean + SE)
sites_d <- data_all %>%

  group_by(Band.ID, dateAus) %>% # per individual and day
  summarise(nb_sites = n_distinct(recvDeployName), .groups = "drop") %>%  
  group_by(Band.ID) %>%  
  summarise(nb_sites_d = round(mean(nb_sites), 0)) %>%
  ungroup()  %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_sites_d = round(mean(nb_sites_d), 0),
            se = sd(nb_sites_d)/sqrt(n_indiv),
            se_lower = mean_sites_d - 1.96*se,
            se_upper = mean_sites_d + 1.96*se,
            sites_d_se = ifelse(is.na(se), 
                                NA, 
                                paste0("[", ifelse(se_lower < 0, 0, round(se_lower, 1)), "-", round(se_upper, 1), "]"))) %>%
  ungroup() %>%
  select(speciesEN, mean_sites_d, sites_d_se)

# Nb of sites visited in total (mean + SE)
sites_tot <- data_all %>%
  
  group_by(Band.ID) %>% # per individual
  summarise(sites_tot = n_distinct(recvDeployName), .groups = "drop") %>%  
  ungroup()  %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_sites_tot = round(mean(sites_tot), 0),
            se = sd(sites_tot)/sqrt(n_indiv),
            se_lower = mean_sites_tot - 1.96*se,
            se_upper = mean_sites_tot + 1.96*se,
            sites_tot_se = ifelse(is.na(se), 
                                  NA, 
                                  paste0("[", ifelse(se_lower < 0, 0, round(se_lower, 1)), "-", round(se_upper, 1), "]"))) %>%
  ungroup() %>%
  select(speciesEN, mean_sites_tot, sites_tot_se)


# 5 - Gather variables ----

table_1 <- list(nb_tagged,     # nb of individual tagged
                nb_undetect,   # nb of individual undetected
                nb_retagged,   # nb of individual re-tagged
                first_d,       # first day of the first individual tagged
                last_d,        # last day of the last individual detected
                monit_d,       # nb of days between first_d and last_d for each individual
                detect_d,      # nb of days each individual have been detected at least once
                sites_d,       # mean for the nb of sites each individual visited per day
                sites_tot) %>% # total nb of sites each individual visited
  
  reduce(left_join, by = "speciesEN") # all the variables are accessed at species level


# 6 - Display table 1 ----

# Last modifications
table_1_pub <- table_1 %>%
  mutate(monit_d   = ifelse(is.na(mean_monit_d_se), mean_monit_d,paste0(mean_monit_d, "  ", mean_monit_d_se)),
         detect_d  = ifelse(is.na(mean_detect_d_se), mean_detect_d,paste0(mean_detect_d, "  ", mean_detect_d_se)),
         sites_d   = ifelse(is.na(sites_d_se), mean_sites_d,paste0(mean_sites_d, "  ", sites_d_se)),
         sites_tot = ifelse(is.na(sites_tot_se), mean_sites_tot, paste0(mean_sites_tot, "  ", sites_tot_se))) %>%
  left_join(data_all %>% distinct(speciesEN, speciesSci), by = "speciesEN") %>%
  filter(!is.na(speciesSci)) %>%
  select(speciesEN, speciesSci, nb_tagged, nb_undetect, nb_retagged, first_d, last_d, monit_d, detect_d, sites_d, sites_tot) %>%
  rename(species_eng = "speciesEN",
         species_sci = "speciesSci")

# Final
DT::datatable(
  table_1_pub,
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    fixedColumns = list(leftColumns = 2)),
  extensions = c('FixedColumns'),
  caption = 'Table 1: Shorebirds monitoring with local MOTUS automated telemetry array.') %>%
  DT::formatStyle('species_eng',
                  fontWeight = 'bold')



# table_grob <- gridExtra::tableGrob(table_1_pub)
# jpeg(filename = "tabl1.jpeg", width = 1000, height = 200)
# grid::grid.draw(table_grob)
# dev.off()
