
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
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) #setwd where the file is
Sys.setenv(TZ="UTC") 
motusLogout()

# Project Number
proj.num <- 294       

# 3 - Download all data and metadata per Project ----

# Load data from online network (either 1st time or update)
# sql.motus <- tagme(projRecv = proj.num, 
#                    new = FALSE, # TRUE overwrites existing (large data takes a while)
#                    update = TRUE, 
#                    dir = here("10_data", "motus.sql"))
# metadata(sql.motus, proj.num)

# Load local data
sql.motus <- dbConnect(SQLite(), here::here("10_data", "motus.sql", "project-294.motus"))

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

# 5 - Filtering data ----

############################################ ??????????? #########################################
df.alltags %>%
  filter(is.na(recvDeployLat) | is.na(recvDeployName)) %>%
  select(motusTagID, motusFilter, recvDeployLat, recvDeployLon, recvDeployName, recvDeployID, recv, recvProjID, recvProjName, speciesEN, recvSiteName, tagDepComments) %>%
  count(motusTagID, recv, recvDeployLat, recvDeployLon, recvDeployName, recvDeployID, motusFilter, speciesEN, recvSiteName, tagDepComments) %>%
  distinct() # What are those stations?

df.alltags %>%
  count(runLen) # What value to choose?

full_join(as.data.frame(table(df.tags$tagID)), 
          as.data.frame(table(df.tagdeps$tagID)), 
          by = "Var1") %>%
  mutate(Freq.x = ifelse(is.na(Freq.x), 0, 1),
         Freq.y = ifelse(is.na(Freq.y), 0, 1)) %>%
  filter(Freq.x != Freq.y) %>%
  rename(tagID = Var1, df.tags = Freq.x, df.tagdeps = Freq.y) # Those tags are not referenced into deployed tags BUT...

df.alltags$motusTagID[is.na(df.alltags$tagDeployID)] #... different to those ones (from all tags)
##################################################################################################

# Wrong tags
# df.alltags <- df.alltags %>% 
#   filter(motusTagID == c("81121", "60470"))
  
# False positive
df.alltags <- df.alltags %>% 
  filter(motusFilter == 1, # 0 is invalid data
         runLen >= 3, # value to be further thought
         recv != c("SG-62A5RPI36710") ) # test_station

# Ambiguous (if != 0 then refer to https://motuswts.github.io/motus/articles/05-data-cleaning.html)
clarify(sql.motus)

# 6 - Adding variables ----

df.alltags <- df.alltags %>% 
  
# Time
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus)) %>%
  
# Sunrise/set
  sunRiseSet(df.alltags, 
             lat = "recvDeployLat", 
             lon = "recvDeployLon", 
             ts = "ts") %>% 
  mutate(sunriseNewc = sunrise(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(dateAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE)) %>%
  
# Positive signal strength (min. = 0) for plotting
  mutate(sigPositive = sig + abs(min(sig))) %>%
  
# As Factor
  mutate(motusTagID = as.factor())

# 7 - Save

saveRDS(df.alltags, here::here("10_data", "data.rds"))










