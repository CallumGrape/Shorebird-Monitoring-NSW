
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Download MOTUS data, all tags recorded through the worldwide MOTUS network (get my tags by anyone’s receivers)
## Created:  2025 July 

# MOTUS Username: c3541851@uon.edu.au
# MOTUS Password: C**********2*![****]

# 1 - Packages ----

# install.packages("motus", 
#                  repos = c(birdscanada = 'https://birdscanada.r-universe.dev',
#                            CRAN = 'https://cloud.r-project.org'))
library(motus)
library(dplyr)
library(here)
library(DBI)
library(RSQLite)
library(forcats) 
library(lubridate)
library(bioRad) 
library(purrr) 


# 2 - Settings ----

# Global
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
Sys.setenv(TZ="UTC") 
motusLogout()

# Project Number
proj.num <- 294       

# 3 - Download all data and metadata per Project ----

# Load data from online network (either 1st time or update)
# sql.motus <- tagme(projRecv = proj.num, 
#                    new = FALSE, # TRUE overwrites existing (large data takes a while)
#                    update = TRUE, 
#                    dir = here("1_data", "motus.sql"))
# metadata(sql.motus, proj.num)

# Load local data
sql.motus <- dbConnect(SQLite(), here::here("1_data", "alltags", "project-294.motus"))

# Load tide (Callum work 01_import_tide_data.R)
tidalCurve <- readRDS(here::here("1_data", "tides", "tidalCurve.rds"))
tideData <- readRDS(here::here("1_data", "tides", "tideData.rds"))
tidalCurveFunc <- splinefun(tideData$tideDateTimeAus, tideData$tideHeight, method = "natural")
get.tideIndex <- function(time){ return(which.min(abs(tideData$tideDateTimeAus-time)))}

# 4 - Extract data ----

# All tags MOTUS recorded for the project
df.alltags <- tbl(sql.motus, "alltags") %>%
  dplyr::collect() %>%
  as.data.frame()

# Specs for all tags MOTUS recorded for the project
df.tags <- tbl(sql.motus, "tags") %>%
  filter(projectID == proj.num) %>% # Has to be specified!
  dplyr::collect() %>% 
  as.data.frame()

# Specs for tags MOTUS recorded for the project & deployed
df.tagdeps <- tbl(sql.motus, "tagdeps") %>%
  dplyr::collect() %>%
  as.data.frame()

# 5 - Filtering tag data ----

############################################ ??????????? #########################################

# STATIONS ?

df.alltags %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(motusTagID, recvDeployName, recvDeployID, recv, recvProjID, speciesEN, recvSiteName, tagDepComments) %>%
  dplyr::count(motusTagID, recv, recvDeployName, recvDeployID, speciesEN, recvSiteName, tagDepComments) %>%
  distinct() # What are those stations?

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

df.alltags$motusTagID[is.na(df.alltags$tagDeployID)] #... different to those ones (from all tags)

spreadsheet <- read.csv( here::here("1_data", "spreadsheets", "teams.sheet.30.07.25.csv")) 

table(df.tagdeps$tagID)
table(unique(df.alltags$tagDeployID))
table(df.alltags$motusTagID)
table(spreadsheet$Motus.tag.ID)

table(
(df.tagdeps %>% rename(ID = "tagID") %>%
  semi_join(df.alltags %>% rename(ID = "motusTagID"), by = "ID") %>%
  semi_join(spreadsheet %>% rename(ID = "Motus.tag.ID"), by = "ID"))$ID
)

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

table(df.alltags$motusTagID[is.na(df.alltags$speciesEN)]) # Tag with NA for speciesEN


##################################################################################################

# Create a unique individual ID
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

####################################################################################################################################################################################################

# Wrong tags
df.alltags <- df.alltags %>% 
   filter(motusTagID == c("43291")) %>% # test_tag
  
# Wrong receivers
   filter(!is.na(recvDeployLat),
          recvDeployName != c("Throsby Creek Test Site"),
          recv != c("SG-62A5RPI36710") ) # test_station
  
# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data
         runLen >= 3) # value to be further thought

# Ambiguous (if != 0 then refer to https://motuswts.github.io/motus/articles/05-data-cleaning.html)
clarify(sql.motus)

# 6 - Adding variables ----

df.alltags <- df.alltags %>% 
  
# Time
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time), # extract year from time
         doy = yday(time)) %>%
  
# Sunrise/set
  sunRiseSet(lat = "recvDeployLat", 
             lon = "recvDeployLon", 
             ts = "ts") %>% 
  mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
  
# Positive signal strength (min. = 0) for plotting
  mutate(sigPositive = sig + abs(min(sig))) %>%
  
# As Factor
  mutate(motusTagID = as.factor(motusTagID))

# Tide
df.alltags <- df.alltags %>% 
  mutate(tideHeight = tidalCurveFunc(timeAus),
         tideIndex = map_dbl(timeAus, get.tideIndex))

tide_values <- tideData[df.alltags$tideIndex, 
                        c("tideDateTimeAus",
                          "high_low",
                          "day_night",
                          "tideCategory",
                          "tideID",
                          "tideHeight")]

df.alltags <- df.alltags %>%
  mutate(tideDateTimeAus = tide_values$tideDateTimeAus,
         tideHighLow = as_factor(tide_values$high_low),
         tideDiel = as_factor(tide_values$day_night),
         tideCategory = as_factor(tide_values$tideCategory),
         tideCategoryHeight = tide_values$tideHeight,
         tideID = as_factor(tide_values$tideID),
         tideTimeDiff = abs(difftime(timeAus, tideDateTimeAus, units = "hours")) ) # Time diff btw detect. & nearest tide pts

# 7 - Filtering receivers data ----

# Get summary
df.recvDeps <- tbl(sql.motus, "recvDeps") %>% 
  collect() %>% 
  as.data.frame()

# Rename stations
station_rename <- list(
   "Barry_Fullerton_cove"  = "Fullerton Entrance",
   "North Swann Pond"      = "Swan Pond" ,
   "Ramsar Road Floodgate" = "Ramsar Road",
   "Milham's Pond"         = "Milhams Pond")
df.recvDeps <- df.recvDeps %>% 
   mutate(recvDeployName = recode(stationName,
                            !!!station_rename))
df.alltags <- df.alltags %>% 
  mutate(recvDeployName = recode(recvDeployName,
                                 !!!station_rename))
  
# 8 - Save

saveRDS(df.alltags, here::here("1_data", "alltags", "motus.rds", paste0(Sys.Date(), "-data", ".rds" )))
saveRDS(df.recvDeps, here::here("1_data", "alltags", "motus.rds", paste0(Sys.Date(), "-recv-info", ".rds" )))










