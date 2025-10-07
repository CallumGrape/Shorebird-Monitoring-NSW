
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
  )), 1)) 

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
nb_indiv <- spreadsheet %>% 
  group_by(speciesEN) %>%
  distinct(Band.ID) %>%
  count()

# First day monitored (trapping/tagging) 
first_d <- spreadsheet %>% 
  group_by(speciesEN) %>%
  mutate(first_d = min(DateAUS.Trap)) %>%
  select(speciesEN, first_d) %>%
  unique()

# Last day recorded (detection)
last_d <- data_all %>% 
  group_by(speciesEN) %>%
  mutate(last_d = max(dateAus)) %>%
  select(speciesEN, last_d) %>%
  unique()

# Nb of days monitored, total period of detection (mean + SE)
monit_d <- data_all %>%
  
  group_by(Band.ID) %>%
  mutate(DateAUS.Trap = as.Date(DateAUS.Trap),
         monit_d = max(dateAus) - DateAUS.Trap) %>%
  ungroup() %>%
  
  group_by(speciesEN) %>%
  unique(data_all$Band.ID) %>%
  mutate(n_indiv = n_distinct(Band.ID),
         mean_monit_d = round(mean(monit_d), 0),
         se = sd(monit_d) / sqrt(n_indiv),
         se_lower = mean_monit_d - 1.96 * se,
         se_upper = mean_monit_d + 1.96 * se,
         mean_monit_se = paste0("[", round(se_lower, 0), "-", round(se_upper, 0), "]")) %>%
  #select(speciesEN, Band.ID, dateAus, DateAUS.Trap, mean_monit, se_lower, se_upper, mean_monit_se)
  select(speciesEN, mean_monit_d, mean_monit_se) %>%
  unique()

# Nb of days actually detected (mean + SE)
detect_d <- 
  
# Nb of individual never detected

# Nb of sites visited per day (mean + SE)

# Nb of sites visited in total (mean + SE)









nb_undetect <- spreadsheet %>% 
  filter(is.na(Euthanised.)) %>%
  distinct(Band.ID) %>%
  filter(!Band.ID %in% unique(data_all$Band.ID))

nb_detect <- spreadsheet %>% 
  filter(is.na(Euthanised.)) %>%
  distinct(Band.ID) %>%
  filter(Band.ID %in% unique(data_all$Band.ID))

nb_release <- spreadsheet %>% 
  filter(is.na(Euthanised.),
         is.na(Retagged.))

# 5 - Gather variables ----

# 6 - Display table 1 grouped per species ----
moni <- bind_rows(
  tibble(variable = "nb_tagged",
         value  = length(spreadsheet$Band.ID)),
  tibble(variable = "nb_euthanised",
         value  = sum(spreadsheet$Euthanised. == "Y", na.rm = TRUE)),
  tibble(variable = "nb_retagged",
         value  = sum(!is.na(spreadsheet$Retagged.))),
  tibble(variable = "nb_released",
         value  = nrow(nb_release)),
  tibble(variable = "detect_0",
         value  = nrow(nb_undetect)),
  tibble(variable = "detect_inf_150",
         value  = data_all %>%
           count(Band.ID) %>%
           filter(n < 150) %>% 
           nrow() ),
  tibble(variable = "detect_sup_150",
         value  = data_all %>%
           count(Band.ID) %>%
           filter(n > 149) %>% 
           nrow() )
)

detect_dates <- data_all %>%
  group_by(Band.ID) %>%
  summarise(
    first_detect = min(dateAus, na.rm = FALSE),
    last_detect = max(dateAus, na.rm = FALSE),
    .groups = "drop"
  )

undetect <-  spreadsheet %>% 
  filter(Band.ID %in% nb_undetect$Band.ID)%>%
  left_join(detect_dates, by = "Band.ID") %>%
  select(Band.ID, motusTagID, Species, DateAUS.Trap, first_detect, last_detect, everything())  
detect <-  spreadsheet %>% 
  filter(Band.ID %in% nb_detect$Band.ID) %>%
  left_join(data_all %>%
              group_by(motusTagID) %>%
              summarise(Detections = n(), 
                        .groups = "drop"), 
            by = "motusTagID") %>%
  mutate(Detections = ifelse(is.na(Detections), 0, Detections)) %>%
  left_join(detect_dates, by = "Band.ID") %>%
  select(Detections, Band.ID, motusTagID, Species, DateAUS.Trap, first_detect, last_detect, everything())
eutha <-  spreadsheet %>% 
  filter(Euthanised.== "Y") %>%
  left_join(detect_dates, by = "Band.ID") %>%
  select(Band.ID, motusTagID, Species, DateAUS.Trap, first_detect, last_detect, everything())

DT::datatable(
  moni,
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    fixedColumns = list(leftColumns = 2)
  ),
  extensions = c('FixedColumns'),
  caption = 'Overview on our bird data'
) %>%
  DT::formatStyle(
    'variable',
    fontWeight = 'bold'
  ) %>%
  DT::formatStyle(
    c('value'),
    fontWeight = 'bold',
    color = '#808080'  
  )