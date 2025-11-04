
## Type   :  PhD Project
## Auteur :  Callum Gapes, Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Check shorebirds data, MOTUS filtering
## Created:  2025 July 

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
library(ggplot2)


# 2 - Settings ----

# Global
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 
Sys.setenv(TZ="UTC") 
motusLogout()

# Project Number
proj.num <- 294       

# 3 - Download all data and metadata per Project ----

# Load data from online network (either 1st time or update)
# sql.motus <- tagme(projRecv = 294,
#                    new = TRUE, # TRUE overwrites existing (large data takes a while)
#                    dir = "../1_data/alltags/")
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
  as.data.frame() %>%
  mutate(time = as_datetime(ts),
         timeAus = as_datetime(ts, tz = "Australia/Sydney"),
         dateAus = as_date(timeAus),
         year = year(time), 
         doy = yday(time)) 

# 5 - Cleaning tag data ----

# Cleaning and correcting tags metadata
df.alltags <- df.alltags %>% 
  filter(
    # test tags
    motusTagID != c("43291"),
    # pending, unconfirmed or undeployed tags
    !motusTagID %in% c("43288", "43291", "43297", "43299",
                       "43307", "43424", "43425", "60470", 
                       "60579", "81123", "81136", "81137"),
    # used for test/validation before tagging bird (remove time before the tagging)
    !(motusTagID == "81134" & time < dmy("23-11-2024")),
    !(motusTagID == "60575" & time < dmy("25-10-2023")) ) %>% 
  # NA species
  mutate(speciesEN = case_when(
    is.na(speciesEN) & motusTagID %in% c("60470", "81121") ~ "Red-necked Avocet",
    is.na(speciesEN) & motusTagID %in% c("81118") ~ "Red-necked Avocet",
    TRUE ~ speciesEN))

# Cleaning and correcting receiver metadata
df.alltags <- df.alltags %>% 
  filter(
    # NA
    !is.na(recvDeployLat),
    # site not any longer used
    recvDeployName != c("Throsby Creek Test Site", "Corrie Island"),
    # test sensor gnome
    recv != c("SG-C621RPI3E17F",       
              "SG-62A5RPI36710") ) %>% 
  mutate(recvDeployName = ifelse(is.na(recvDeployName) & recv == "SG-D5BBRPI3E2F7", "Windeyers", recvDeployName))


# 5 - Checking 'MOTUS filtering' tag data ----

plot1 <- ggplot(df.alltags %>%
         filter(motusFilter == 1),
       aes(x = recvDeployName)) +
  geom_bar(fill = "steelblue") +
  theme_minimal() +
  labs(x = "Motus Station", y = "Nb of MOTUS filter = 1 (good)")

plot0 <- ggplot(df.alltags %>%
         filter(motusFilter == 0),
       aes(x = recvDeployName)) +
  geom_bar(fill = "orange") +
  theme_minimal() +
  labs(x = "Motus Station", y = "Nb of MOTUS filter = 0 (filtered out)")

plot10 <- ggpubr::ggarrange(plot1, 
          plot0, 
          ncol = 1, nrow = 2, 
          common.legend = FALSE)

ggpubr::ggexport(plot10,
                 filename = here("3_figures", "motus_filter_count.jpg"),
                 width = 800, height = 1000)

perc <- ggplot(df.alltags %>% 
                 filter(motusFilter %in% c(0, 1)),
       aes(x = recvDeployName, fill = factor(motusFilter))) +
  geom_bar(position = "fill") + 
  scale_fill_manual(values = c("0" = "orange",
                               "1" = "steelblue"),
                    labels = c("0 (filtered out)", 
                               "1 (good)"),
                    name = "motusFilter") +
  theme_minimal() +
  labs(x = "Motus Station",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggpubr::ggexport(perc,
                 filename = here("3_figures", "motus_filter_perc.jpg"),
                 width = 800, height = 1000)


