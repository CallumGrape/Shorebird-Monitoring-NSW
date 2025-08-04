
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Draft to save things that might be useful
## Created:  2025 August 


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




# Filter which station has been not continuously ON
recv_off_chk <- recv %>%
  arrange(recvDeployName, timeStartAus) %>% # sort by site + time
  group_by(recvDeployName) %>% # work through the group of the same site's name (and not the serno)
  mutate(offline_start = lag(timeEndAus), # iteratively take the previous row
         offline_end = timeStartAus) %>% 
  filter(!is.na(offline_start) & offline_end > offline_start) %>%
  mutate(timeOff = round(as.numeric(difftime(offline_end, offline_start, units = "days")), digits = 2)) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus, offline_start, offline_end, timeOff) 
recv_off_chk

# List the meant stations
list_recv_off <- unique(recv_off_chk$recvDeployName)

# Check whether this makes sense
recv_off_chk <- recv %>% 
  filter(recvDeployName %in% list_recv_off) %>%
  select(recvDeployName, serno, timeStartAus, timeEndAus) %>%
  arrange(recvDeployName, timeStartAus)  %>%
  left_join(recv_off_chk %>% select(recvDeployName, timeOff),
            by = "recvDeployName")
recv_off_chk

# Filter out gaps under 24h
recv1 <- recv %>% 
  filter(timeStartAus > min(data_all$timeAus
                            # %>% filter(motuTagID = c("")) # TEST TAG TO REMOVE FIRST!!
  )) 




