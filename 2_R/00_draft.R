
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Draft to save things that might be useful
## Created:  2025 August 









##################################################################################################

# - Look for MASKED LAPWING ----


# Global
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# Load local data
sql.motus <- dbConnect(SQLite(), here::here("1_data", "alltags", "project-294.motus"))

df.alltags <- tbl(sql.motus, "alltags") %>%
  dplyr::collect() %>%
  as.data.frame() %>%
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time), 
         doy = yday(time)) 

table(df.alltags$motusTagID, df.alltags$speciesEN)

masklap <- df.alltags %>% filter(speciesEN == "Masked Lapwing")
table(masklap$motusTagID)

df.alltagsMASK <- masklap %>% 
  select(tagDepComments, speciesEN, dateAus, motusTagID)


table(masklap$motusFilter, masklap$motusTagID) ### and I filter at = 1 in 11_script SEE BELOW
# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data # MASKED LAPWING 43298 is only INVALID data !!!
         runLen >= 3) # value to be further thought



redneck <- data_all %>% filter(speciesEN == "Red-necked Avocet") %>%
  group_by(Band.ID) %>%
  mutate(DateAUS.Trap = as.Date(DateAUS.Trap),
         monit_d = max(dateAus) - DateAUS.Trap) # YES THE 3 RED NECKED AVO HAVE BEEN MONITORED EXACTLY 28 DAYS
table(redneck$Band.ID, redneck$monit_d)



















##################################################################################################

# - Filtering tag data ----

# STATIONS ?
df.alltags %>%
  mutate(ts = as_date(as_datetime(ts, tz = "Australia/Sydney")),
         date = format(ts, "%Y-%m-%d") ) %>%
  #filter(year(date) == 2025, month(date) == 2) %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(date, motusTagID, recvDeployName, recvDeployID, recv, recvProjID, speciesEN, recvSiteName, tagDepComments) %>%
  dplyr::count(date, motusTagID, recv, recvDeployName, recvDeployID, speciesEN, recvSiteName, tagDepComments) %>%
  filter(recv == "SG-D5BBRPI3E2F7") %>%
  arrange(date) %>%
  distinct() # What are those stations?

df.alltags %>%
  filter(!is.na(recvDeployName),
         recv == "SG-D5BBRPI3E2F7") %>%
  mutate(ts = as_date(as_datetime(ts, tz = "Australia/Sydney")),
         date = format(ts, "%Y-%m-%d") ) %>%
  filter(year(date) == 2025, month(date) == 2) %>%
  summarise(max_date = max(as_date(with_tz(as_datetime(ts, tz = "Australia/Sydney"))), na.rm = TRUE) ) %>%
  pull(max_date) %>%
  format("%Y-%m-%d %H:%M:%S")

df.alltags %>%
  filter(is.na(recvDeployName),
         recv == "SG-D5BBRPI3E2F7") %>%
  mutate(ts = as_date(as_datetime(ts, tz = "Australia/Sydney")),
         date = format(ts, "%Y-%m-%d") ) %>%
  filter(year(date) == 2025, month(date) == 2) %>%
  summarise(min_date = min(as_date(with_tz(as_datetime(ts, tz = "Australia/Sydney"))), na.rm = TRUE) ) %>%
  pull(min_date) %>%
  format("%Y-%m-%d %H:%M:%S")

# RUN LENGTH VALUE ?
df.alltags %>%
  dplyr::count(runLen) # What value to choose?

# DEPLOYED/UNDEPLOYED TAG ?
full_join(as.data.frame(table(df.tags$tagID)), 
          as.data.frame(table(df.tagdeps$tagID)), 
          by = "Var1") %>%
  mutate(Freq.x = ifelse(is.na(Freq.x), 0, 1),
         Freq.y = ifelse(is.na(Freq.y), 0, 1)) %>%
  filter(Freq.x != Freq.y) %>%
  dplyr::rename(tagID = Var1, df.tags = Freq.x, df.tagdeps = Freq.y) # Those tags are not referenced into deployed tags BUT...

df.alltags$motusTagID[is.na(df.alltags$tagDeployID)] #... different to those ones (from all tags)  #### NOT DEPLOYED CHECKED MOTUS (without info)

spreadsheet <- read.csv( # load the most recent file
  tail(sort(list.files(
    here::here("1_data", "spreadsheets"),
    pattern = "-teams.sheet\\.csv$", 
    full.names = TRUE
  )), 1))

table(df.tagdeps$tagID) # DEPLOYED MOTUS ID
table(unique(df.alltags$tagDeployID)) # DEVICE NB
table(df.alltags$motusTagID)
table(spreadsheet$Motus.tag.ID)

table(
  (df.tagdeps %>% rename(ID = "tagID") %>%
     semi_join(df.alltags %>% rename(ID = "motusTagID"), by = "ID") %>%
     semi_join(spreadsheet %>% rename(ID = "Motus.tag.ID"), by = "ID"))$ID
)

# SPECIES NA ?
table(is.na(df.alltags$speciesEN), df.alltags$motusTagID)

df.alltags.corr <- df.alltags %>% 
  group_by(motusTagID) %>%
  filter(any(is.na(speciesEN)) & any(!is.na(speciesEN))) %>%
  ungroup() %>%
  select(motusTagID, speciesEN, ts, tagDeployID, recv, recvDeployName) %>%
  mutate(ts = as_date(as_datetime(ts, tz = "Australia/Sydney")),
         year = year(ts) )
table(df.alltags.corr$motusTagID,  df.alltags.corr$year, df.alltags.corr$speciesEN)  
table(df.alltags$motusTagID[is.na(df.alltags$speciesEN)]) # Tag with NA for speciesEN

check <- df.alltags %>%
  filter(is.na(ID)) %>%
  select(motusTagID, recvDeployName, recvDeployID, recv, speciesEN, recvSiteName, tagDepComments) %>%
  dplyr::count(motusTagID, recv, recvDeployName, recvDeployID, speciesEN, recvSiteName, tagDepComments) %>%
  distinct()
check

table(check$motusTagID)

df.alltags %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(motusTagID, recvDeployName, recvDeployID, recv, recvProjID, speciesEN, recvSiteName, tagDepComments) %>%
  dplyr::count(motusTagID, recv, recvDeployName, recvDeployID, speciesEN, recvSiteName, tagDepComments) %>%
  distinct() # What are those stations?
full_join(as.data.frame(table(df.tags$tagID)), 
          as.data.frame(table(df.tagdeps$tagID)), 
          by = "Var1") %>%
  mutate(Freq.x = ifelse(is.na(Freq.x), 0, 1),
         Freq.y = ifelse(is.na(Freq.y), 0, 1)) %>%
  filter(Freq.x != Freq.y) %>%
  dplyr::rename(tagID = Var1, df.tags = Freq.x, df.tagdeps = Freq.y) # Those tags are not referenced into deployed tags BUT...
df.alltags$motusTagID[is.na(df.alltags$tagDeployID)] #... different to those ones (from all tags)  #### NOT DEPLOYED CHECKED MOTUS (without info)
spreadsheet <- read.csv( # load the most recent file
  tail(sort(list.files(
    here::here("1_data", "spreadsheets"),
    pattern = "-teams.sheet\\.csv$", 
    full.names = TRUE
  )), 1))
table(df.tagdeps$tagID) # DEPLOYED MOTUS ID
table(unique(df.alltags$tagDeployID)) # DEVICE NB
table(df.alltags$motusTagID)
table(spreadsheet$Motus.tag.ID)
table((df.tagdeps %>% rename(ID = "tagID") %>%semi_join(df.alltags %>% rename(ID = "motusTagID"), by = "ID") %>%semi_join(spreadsheet %>% rename(ID = "Motus.tag.ID"), by = "ID"))$ID)

# SPECIES NA ?
table(is.na(df.alltags$speciesEN), df.alltags$motusTagID)
df.alltags.corr <- df.alltags %>% # 6 tags might be just a lack of information but the same bird and then the same specie
  group_by(motusTagID) %>%
  filter(any(is.na(speciesEN)) & any(!is.na(speciesEN))) %>%
  ungroup() %>%
  select(motusTagID, speciesEN, ts, tagDeployID, recv, recvDeployName) %>%
  mutate(ts = as_date(as_datetime(ts, tz = "Australia/Sydney")),
         year = year(ts) )
table(df.alltags.corr$motusTagID,  df.alltags.corr$year, df.alltags.corr$speciesEN)  
#81118 + MASKED lapwing
#81134 going to be filtered in 2024 anyway, PGP
#60470 Red necked avocet
#81121 Red necked avocet
table(df.alltags$motusTagID[is.na(df.alltags$speciesEN)]) # Tag with NA for speciesEN
# 43288  43297 43299 43307 60470  81123  81136  = unconfirmed
# 43291 = test tag
# 60579 = pending
# 81118 = Masked lapwing
# 81121 = rednecked avocet
# 81134= pgp should be filtered before 23/11/2024
# 81136 = undep and filtered before

table(df.alltags$recv, df.alltags$recvDeployName)
df.alltags %>%
  filter(recvDeployName == "Fullerton Entrance") %>%
  group_by(recv)  %>%
  filter(time %in% range(time, na.rm = TRUE)) %>%
  select(recvDeployName, recv, time) %>%
  arrange(recv, time)
df.alltags %>%
  filter(recvDeployName == "Hexham Swamp") %>%
  group_by(recv)  %>%
  filter(time %in% range(time, na.rm = TRUE)) %>%
  select(recvDeployName, recv, time) %>%
  arrange(recv, time)
df.alltags %>%
  filter(recvDeployName == "Windeyers") %>%
  group_by(recv)  %>%
  filter(time %in% range(time, na.rm = TRUE)) %>%
  select(recvDeployName, recv, time) %>%
  arrange(recv, time)

##################################################################################################

# - Create unique ID ----

spreadsheet <- spreadsheet %>%
  filter(Radio.tag. == "Y") %>%
  dplyr::rename(motusTagID = "Motus.tag.ID") %>%
  dplyr::mutate(ID = Band.ID)

df.alltags <- df.alltags %>% 
  left_join(spreadsheet %>% select(motusTagID, ID), by = "motusTagID")

check <- df.alltags %>%
  filter(is.na(ID)) %>%
  select(motusTagID, recvDeployName, recvDeployID, recv, speciesEN, recvSiteName, tagDepComments) %>%
  dplyr::count(motusTagID, recv, recvDeployName, recvDeployID, speciesEN, recvSiteName, tagDepComments) %>%
  distinct()
check

table(check$motusTagID)

##################################################################################################

### CHECK CONFLICT BETWEEN BOX ID AND STATION NAME ###
table(recv$serno, recv$name)

recv %>%
  group_by(name) %>%
  filter(n_distinct(serno) >= 2) %>%
  ungroup() %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>%
  select(name, serno, timeStartAus, timeEndAus) %>%
  arrange(name)



# Filter which station has been not continuously ON
recv_off_chk <- recv %>%
  arrange(name, timeStartAus) %>% # sort by site + time
  group_by(name) %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>% 
  filter(!is.na(offline_start) & offline_end > offline_start) %>%
  mutate(timeOff = round(as.numeric(difftime(offline_end, offline_start, units = "days")), digits = 2)) %>%
  select(name, serno, timeStartAus, timeEndAus, offline_start, offline_end, timeOff) 
recv_off_chk

# List the meant stations
list_recv_off <- unique(recv_off_chk$name)

# Check whether this makes sense
recv_off_chk <- recv %>% 
  filter(name %in% list_recv_off) %>%
  select(name, serno, timeStartAus, timeEndAus) %>%
  arrange(name, timeStartAus)  %>%
  left_join(recv_off_chk %>% select(name, timeOff),
            by = "name")
recv_off_chk

# Filter out gaps under 24h
recv1 <- recv %>% 
  filter(timeStartAus > min(data_all$timeAus
                            # %>% filter(motuTagID = c("")) # TEST TAG TO REMOVE FIRST!!
  )) 


##################################################################################################

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
undetect <-  spreadsheet %>% 
  filter(Band.ID %in% nb_undetect$Band.ID)
eutha <-  spreadsheet %>% 
  filter(Euthanised.== "Y")

