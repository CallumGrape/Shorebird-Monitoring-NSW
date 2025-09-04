

library(dplyr)

# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1))

# Call and extract last up to date Spreadsheet record 
write.csv(readxl::read_excel("C:/Users/marin/The University of Newcastle/StudentGroupPhD - Louise Williams and Mattea Taylor - General/SHOREBIRD NUMBER TRACKING.xlsx"),
          file.path(here::here("1_data", "spreadsheets"), paste0(Sys.Date(), "-teams.sheet", ".csv")), 
          row.names = FALSE)

# Load df with date at the beginning
spreadsheet <- read.csv(here::here("1_data", "spreadsheets", paste0(Sys.Date(), "-teams.sheet.csv"))) %>%
   filter(Radio.tag. == "Y") %>%     # Keep only the tagged ones
   rename(DateAUS.Trap = "Date", motusTagID = "Motus.tag.ID") %>% 
   mutate(motusTagID = as.factor(motusTagID))

# Join unique Band IDs for inconsistent motusTag (same bird re-tagged, etc)
data_all <- left_join(data_all, 
                      spreadsheet %>% 
                        filter(is.na(Euthanised.)) %>%
                      select(motusTagID, DateAUS.Trap, Band.ID, Bander),
                      by = "motusTagID")

# Band.IDs in spreadsheet but not in data_all (tagged + released but not detected)
nb_undetect <- spreadsheet %>% 
  filter(is.na(Euthanised.)) %>%
  distinct(Band.ID) %>%
  filter(!Band.ID %in% unique(data_all$Band.ID))

# Band.IDs in spreadsheet and  in data_all (tagged + released and detected)
nb_detect <- spreadsheet %>% 
  filter(is.na(Euthanised.)) %>%
  distinct(Band.ID) %>%
  filter(Band.ID %in% unique(data_all$Band.ID))

# Bird released (total tagged and released birds, supposed to be detectable) 
nb_release <- spreadsheet %>% 
  filter(is.na(Euthanised.),
         is.na(Retagged.))

# Combine all into Monitoring table table
moni <- bind_rows(
  
  # Nb of birds trapped & tagged
  tibble(metric = "nb_tagged",
         value  = length(spreadsheet$Band.ID)),
  # Nb of birds euthanised (tag re-used)
  tibble(metric = "nb_euthanised",
         value  = sum(spreadsheet$Euthanised. == "Y", na.rm = TRUE)),
  # Nb of birds re-trapped & re-tagged (initial tag lost)
  tibble(metric = "nb_retagged",
         value  = sum(!is.na(spreadsheet$Retagged.))),
  # Nb of birds trapped, tagged & released (supposed to be detectable)
  tibble(metric = "nb_released",
         value  = nrow(nb_release)),
  # Nb of birds released but never detected
  tibble(metric = "detect_0",
         value  = nrow(nb_undetect)),
  # Nb of birds released with low detection (less than 30 times)
  tibble(metric = "detect_inf_150",
         value  = data_all %>%
           count(Band.ID) %>%
           filter(n < 150) %>% #1*: we can change this treshold value depending our appreciation
           nrow() ),
  # Nb of birds released with good detection (more than 30 times)
  tibble(metric = "detect_sup_150",
         value  = data_all %>%
           count(Band.ID) %>%
           filter(n > 149) %>% #1*
           nrow() )
  )

# Quick checks

# data_all %>% count(Band.ID) #1*
# data_all %>%
#       distinct(Band.ID, motusTagID) %>%
#       count(is_na = is.na(Band.ID), motusTagID)
# data_all %>%
#   distinct(Band.ID, motusTagID) %>%
#   count(is_na = is.na(motusTagID), Band.ID)
# table(data_all$motusTagID[data_all$Band.ID == "6318621"])


# Undetected & Euthanaised tables
detect <-  spreadsheet %>% 
  filter(Band.ID %in% nb_detect$Band.ID) %>%
  left_join(data_all %>%
              group_by(motusTagID) %>%
              summarise(Detections = n(), 
                        .groups = "drop"), 
            by = "motusTagID") %>%
  mutate(Detections = ifelse(is.na(Detections), 0, Detections)) %>%
  select(Detections, Band.ID, motusTagID, Species, DateAUS.Trap, everything())
undetect <-  spreadsheet %>% 
  filter(Band.ID %in% nb_undetect$Band.ID)
eutha <-  spreadsheet %>% 
  filter(Euthanised.== "Y")

