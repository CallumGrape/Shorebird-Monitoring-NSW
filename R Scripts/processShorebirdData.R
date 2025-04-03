library(DBI)
library(dplyr)
library(lubridate)
library(bioRad) # sunrise / sunset for coordinates
library(purrr) # For map_dbl
library(motus)
library(forcats) # Factor conversion and ordering

source("./R Scripts/shorebirdFunctions.R")

## Store start time to record how long processing takes
start_time <- Sys.time()

## Import Motus Data From SQL File ##
project294.motus <- dbConnect(SQLite(), "Data/project-294.motus")
print("Motus Data Imported Succesfully")

# Extract "alltags" table, which contains the detection data
tbl.alltags <- tbl(project294.motus, "alltags")

# Convert to data frame for easier data exploration
# Note: From my understanding of the Motus documentation, the downside of 
# converting to a data frame (rather than leaving it in # tbl form) is increased
# processing time. The benefit is that it's easier to understand a data frame. 
df.alltags <- tbl.alltags %>% collect() %>% as.data.frame()
rm(tbl.alltags)

## Filter to relevant tags ----
# Extract tag deployment table from motus database
df.tagdeps <- project294.motus %>% tbl("tagdeps") %>% as.data.frame
# Get list of unique tags, filtered to those that have a species (i.e. are deployed)
## IMPORTANT NOTE: This code will not make complete sense if a tag is re-used on multiple individuals.
tagIDs <- (df.tagdeps %>% filter(!is.na(speciesID)) %>% select(tagID))$tagID %>% unique()
rm(df.tagdeps)

# Filter detections by list of tags deployed with a species
#tbl.alltags <- df.alltags %>% filter(motusTagID %in% tagIDs)

## Filter to relevant stations / receivers, and rename them ----
# Filter out detections by NA stations
df.alltags <- df.alltags %>% filter(!is.na(recvDeployName))

# Filter out specific stations
#receivers.remove <- c("Wanggong, Changhua")
#df.alltags <- df.alltags %>% filter(!(recvDeployName %in% receivers.remove))

# Rename specific stations
df.alltags <- df.alltags %>% 
  mutate(recvDeployName = recode(recvDeployName,
                                 "Barry_Fullerton_cove" = "Fullerton Entrance",
                                 "North Swann Pond" = "Swan Pond",
                                 "Ramsar Road Floodgate" = "Ramsar Road",
                                 "Milham's Pond" = "Milhams Pond"
                                 ))

## Filtering / data cleaning ----
# Filter using basic Motus filter (removing 'dubious' detections)
print("Filtering dubious detections (motusFilter = 1)")
df.alltags <- df.alltags %>% filter(motusFilter == 1)

# Remove detections without a species (presumably test tags / before deployment)
print("Removing detections without a species")
df.alltags <- df.alltags %>% filter(is.na(speciesEN)==FALSE)

## Modify Timestamps and Import Sunrise/Set + Tide Data ----
print("Updating timestamps")
# Add datetime time stamp column - one in UTC (default) and the other in Australia (Sydney) time 
df.alltags <- df.alltags %>% mutate(time = as_datetime(ts))
df.alltags <- df.alltags %>% mutate(timeAus = as_datetime(ts, tz = "Australia/Sydney"))

# Add column for date (without time)
df.alltags$dateAus <- df.alltags$timeAus %>% as_date()

## Add sunrise/set times to df.alltags using Motus library function
print("Adding sunrise and sunset times to detections")
df.alltags <- sunRiseSet(df.alltags, lat = "recvDeployLat", lon = "recvDeployLon", ts = "ts")

# Add Newcastle sunrise/set time (based on location of tide data)
df.alltags <- df.alltags %>% mutate(
  sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
  sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)
)

## Misc Processing ----
# Scale signal strength to positive (where minimum = 0) to make plotting easier
df.alltags <- df.alltags %>% mutate(sigPositive = sig + abs(min(sig)))

# Convert categorical variables to factors
df.alltags$motusTagID <- df.alltags$motusTagID %>% as.factor()
df.alltags <- df.alltags %>% mutate(
  motusTagID = as.factor(motusTagID),
  
)

## Import tide data and add to data frame
#Import tide data, tidal curve, and add tidal categorisations to df.alltags 
### NOTE: Tidal classification step will take ~ 10 minutes 
source("R Scripts/importTide.R")

## Summarise detections for each receiver and each tag ----
# NOTE: Must run shorebirdFunctions.R for this to work (to intialise the
# custom functions)
receiverSummary <- generateReceiverSummary()
tagSummary <- generateTagSummary()
speciesSummary <- generateSpeciesSummary()

## Save data frames to file and clean up environment ----
saveRDS(df.alltags, "Data/df.alltags.rds")
saveRDS(receiverSummary, "Data/receiverSummary.rds")
saveRDS(tagSummary, "Data/tagSummary.rds")
saveRDS(speciesSummary, "Data/speciesSummary.rds")

## Remove unnecessary objects
dbDisconnect(project294.motus)

## Print how long processing took and remove time variables
end_time <- Sys.time()
print(end_time-start_time)
rm(start_time, end_time)

## Deprecated - Attempt to only process new data rather than entire data frame ----
# The script used to only process the new data (i.e. new detections), rather than the entire data frame. It ended up 
# making things extra complicated / lots of errors so the script just processes the entire
# data frame each time.

# First section
# Add empty additional columns (necessary for the use of anti_join)
#emptyCols <- c(setdiff(colnames(df.alltags),colnames(df.alltags.new)))
#df.alltags.new[, emptyCols] <- NA
#rm(emptyCols)

# Load existing dataframe
#load(file = "Data/NSW_Shorebird_Data.Rda")

# Remove detections already in main dataframe
#df.alltags.new <- df.alltags.new %>% 
#  anti_join(df.alltags, by = "hitID")

#if (nrow(df.alltags.new) == 0){
#  print("No new detections to add!")
#} else{

# Second section
### Add new detections to df.alltags 
#df.alltags <- df.alltags %>% 
#  bind_rows(df.alltags.new)
#df.alltags <- df.alltags %>% ungroup()

#rm(df.alltags.new)
