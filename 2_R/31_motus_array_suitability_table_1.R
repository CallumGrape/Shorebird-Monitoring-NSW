
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
  unique() %>%
  mutate(first_d = format(as.Date(first_d), format = "%d-%m-%Y"))

# Last day recorded (detection)
last_d <- data_all %>% 
  group_by(speciesEN) %>% # per species
  mutate(last_d = max(dateAus)) %>%
  select(speciesEN, last_d) %>%
  unique()%>%
  mutate(last_d = format(last_d, format = "%d-%m-%Y"))

# Nb of days monitored, total period of detection (mean + SE)
monit_d <- data_all %>%
  
  group_by(Band.ID) %>% # per individual
  summarise(DateAUS.Trap = first(DateAUS.Trap),         
            last_dateAus = max(dateAus),
            monit_d = as.numeric(last_dateAus - DateAUS.Trap) + 1) %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_monit_d = round(mean(monit_d), 1),
            se = sd(monit_d)/sqrt(n_indiv),
            se_lower = mean_monit_d - 1.96*se,
            se_upper = mean_monit_d + 1.96*se,
            mean_monit_d_se = ifelse(is.na(se), 
                                     paste0(" \u00B1 0") , 
                                     paste0(" \u00B1 ", round(se, 1)))) %>%
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
            mean_detect_d = round(mean(detect_d), 1),
            se = sd(detect_d)/sqrt(n_indiv),
            se_lower = mean_detect_d - 1.96*se,
            se_upper = mean_detect_d + 1.96*se,
            mean_detect_d_se = ifelse(is.na(se), 
                                      paste0(" \u00B1 0") , 
                                      paste0(" \u00B1 ", round(se, 1)))) %>%
  ungroup() %>%
  select(speciesEN, mean_detect_d, mean_detect_d_se)

# Nb of sites visited per day (mean + SE)
sites_d <- data_all %>%

  group_by(Band.ID, dateAus) %>% # per individual and day
  summarise(nb_sites = n_distinct(recvDeployName), .groups = "drop") %>%  
  group_by(Band.ID) %>%  
  summarise(nb_sites_d = round(mean(nb_sites), 1)) %>%
  ungroup()  %>% 
  left_join(data_all %>% select(Band.ID, speciesEN) %>% distinct(), by = "Band.ID") %>%
  
  group_by(speciesEN) %>% # per species
  summarise(n_indiv = n(),
            mean_sites_d = round(mean(nb_sites_d), 1),
            se = sd(nb_sites_d)/sqrt(n_indiv),
            se_lower = mean_sites_d - 1.96*se,
            se_upper = mean_sites_d + 1.96*se,
            sites_d_se = ifelse(is.na(se), 
                                paste0(" \u00B1 0") , 
                                paste0(" \u00B1 ", round(se, 1)))) %>%
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
            mean_sites_tot = round(mean(sites_tot), 1),
            se = sd(sites_tot)/sqrt(n_indiv),
            se_lower = mean_sites_tot - 1.96*se,
            se_upper = mean_sites_tot + 1.96*se,
            sites_tot_se = ifelse(is.na(se), 
                                  paste0(" \u00B1 0") , 
                                  paste0(" \u00B1 ", round(se, 1)))) %>%
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
  
  select(speciesEN, 
         speciesSci, 
         nb_tagged, 
         nb_undetect, 
         #nb_retagged, 
         first_d, 
         last_d, 
         monit_d, 
         detect_d, 
         sites_d, 
         sites_tot) %>%
  
  rename(species_eng = "speciesEN",
         species_sci = "speciesSci") %>%
  arrange(first_d)

# Final

# Markdown format
DT::datatable(
  table_1_pub,
  options = list(
    scrollX = TRUE,
    pageLength = 10,
    fixedColumns = list(leftColumns = 2)),
  extensions = c('FixedColumns'),
  caption = 'Table 1. Overview on the shorebird species VHF tracked and monitored over the local automated MOTUS array located in the Hunter estuary.') %>%
  DT::formatStyle('species_eng',
                  fontWeight = 'bold')


# table_grob <- gridExtra::tableGrob(table_1_pub)
# jpeg(filename = "tabl1.jpeg", width = 1000, height = 200)
# grid::grid.draw(table_grob)
# dev.off()

# Publication format
library(gt)
library(gtExtras)

table_1_pub %>%
  gt() %>%
  
  tab_header(
    title = md("**Table 1.** Overview of the shorebird species VHF tracked and monitored over the local automated MOTUS array located in the Hunter estuary.")) %>%
  opt_align_table_header(align = "left") %>%
  
  tab_footnote(
    footnote = md("**Legend.** Grouped by species, this table summarises the number of shorebird individuals tagged and tracked using MOTUS-VHF technology in the Hunter estuary near Newcastle (NSW, Australia). Birds have been surveyed from the day they have been tagged (First day) to their last detection (Last day), which gives the Total of days. However, detections occurred only on certain days (Detected days). We also looked at the number of sites each species might visit per day (Nb of sites/days) and the Total number of sites the species visited during its whole survey. Dates are in dd-mm-yyyy format. Mean and standard error values are provided(x̄ \u00B1 SE, with SE = SD/\u221An)"))  %>%

  opt_table_font(font = "Times New Roman") %>%
  
  cols_label(
    species_eng = "Species (En.)",
    species_sci = "Species (Sci.)",
    nb_tagged = "Tagged",
    nb_undetect = "Undetected",
    #nb_retagged = "Re-tagged",
    first_d = "First day",
    last_d = "Last day",
    monit_d = "Total of days",
    detect_d = "Detected days",
    sites_d = "Nb of sites/days",
    sites_tot = "Total of sites" ) %>%
  
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels() ) %>%
  
  tab_style(
    style = cell_text(style = "italic"),
    locations = cells_body(columns = c(species_sci))) %>%
  
  tab_options(
    table.font.size = pct(90),
    heading.title.font.size = px(16)) 
