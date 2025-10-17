
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Available time vs Used time at night/day during high/low tide periods, from shorebirds 
## Created:  2025 October 


# 1 - Packages ----

library(motus)
library(dplyr)
library(here)
library(forcats) 
library(ggplot2)
library(lubridate)
library(tidyr)
library(purrr)
library(readr)
library(bioRad)
library(hms)
library(dplyr)
library(ggplot2)
library(scales)


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

# Tide
tide_data <- read_csv(here::here("1_data", "tides", "TideDataNewcastle.csv"))


# 4 - Process data ----

# Provide the 4 categories to the Tide table
tide_data <- tide_data %>%
  arrange(tideDateTimeAus) %>%
  mutate(prev_time = lag(tideDateTimeAus),
         next_time = lead(tideDateTimeAus) ) %>%
  filter(!is.na(prev_time) & !is.na(next_time)) %>%
  mutate(duration_h = as.numeric(difftime(tideDateTimeAus, prev_time, units = "hours")/2 + 
                                   difftime(next_time, tideDateTimeAus, units = "hours")/2)) %>%
  
  mutate(sunriseNewc = sunrise(tideDateTimeAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunsetNewc = sunset(tideDateTimeAus, 151.7833, -32.9167, elev = -0.268, tz = "Australia/Sydney", force_tz = TRUE),
         sunriseNewcTime = strftime(sunriseNewc, format = "%H:%M:%S", tz = "Australia/Sydney"),
         sunsetNewcTime = strftime(sunsetNewc, format = "%H:%M:%S", tz = "Australia/Sydney")) %>%
  
  mutate(tideDiel = case_when(tideDateTimeAus < sunriseNewc ~ "Nocturnal",
                              tideDateTimeAus > sunriseNewc & tideDateTimeAus < sunsetNewc ~ "Diurnal",
                              tideDateTimeAus > sunsetNewc ~ "Nocturnal")) %>%
  
  mutate(tideCategory = case_when(high_low == "Low" & tideDiel == "Diurnal" ~ "Diurnal_Low",
                                  high_low == "Low" & tideDiel == "Nocturnal" ~ "Nocturnal_Low",
                                  high_low == "High" & tideDiel == "Diurnal" ~ "Diurnal_High",
                                  high_low == "High" & tideDiel == "Nocturnal" ~ "Nocturnal_High") %>% 
           as_factor()) %>%
  rename(tideHighLow = high_low,
         timeAus = tideDateTimeAus) %>%
  select(timeAus, tideCategory, tideHighLow, tideDiel, duration_h, sunriseNewc, sunsetNewc)

# Provide the burst interval value depending Lotek-nano tag model (scd)
data_bird <- data_all %>%
  mutate(burst_inter = ifelse(tagModel == "NTQB2-6-2", dseconds(7.1), dseconds(13.1))) %>%
  select(timeAus, tideCategory, tideHighLow, tideDiel, sunriseNewc, sunsetNewc, 
         speciesEN, tagModel, recvDeployName, recv,speciesSci, Band.ID, burst_inter)


# 5 - Adding duration variable for birds ----

bird_data_plot <- data_bird %>%
  group_by(Band.ID, speciesEN, recvDeployName, tideCategory)  %>%
  summarise(duration_sec = sum(burst_inter)) %>%
  mutate(duration_h = round_hms(as_hms(duration_sec), secs = 60),
         tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
         tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) %>%
  select(speciesEN, tideCategory, tideDiel, tideHighLow, duration_h, recvDeployName)


# 6 - Split the available time of tide for each birds ----

# Get the monitored period of each bird
period_sp <- data_all  %>%
  group_by(Band.ID) %>%
  reframe(DateAUS.Trap = first(DateAUS.Trap), 
          last_dateAus = max(dateAus),
          speciesEN = speciesEN) %>%
  unique()

# Slice for each bird the corresponding available tide time from the Tide table
results_list <- list()

for(i in 1:nrow(period_sp)) {
  bird_id <- period_sp$Band.ID[i]
  speciesEN <- period_sp$speciesEN[i]
  start_date <- as.POSIXct(period_sp$DateAUS.Trap[i])
  end_date <- as.POSIXct(period_sp$last_dateAus[i]) + hours(23) + minutes(59) + seconds(59)
  
  # Filter tide_data for each species' period
  filtered_tide <- tide_data %>%
    filter(timeAus >= start_date & timeAus <= end_date) 
  
  # Group by tideCategory and sum duration_h
  summary_tide <- filtered_tide %>%
    group_by(tideCategory) %>%
    summarise(total_duration_sec = sum(duration_h * 3600, na.rm = TRUE) ) %>%
    # Convert total duration seconds into hh:mm format rounded to minutes
    mutate(duration_h = round_hms(as_hms(total_duration_sec), secs = 60),
           bird_id = bird_id,
           tideCategory = tideCategory,
           speciesEN = speciesEN,
           tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
           tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) %>%
    ungroup() %>%
    select(speciesEN, bird_id, tideCategory, tideDiel, tideHighLow, duration_h)
  
  results_list[[i]] <- summary_tide
}

# Combine results
tide_data_plot <- bind_rows(results_list) %>%
  rename(Band.ID = bird_id)


# 7 - Plot figure ----

tide_categories <- c("Diurnal_Low", "Nocturnal_Low", "Diurnal_High", "Nocturnal_High")

plots <- purrr::map(tide_categories, function(tc) {
  bird_data_filtered <- bird_data_plot %>% filter(tideCategory == tc)
  tide_data_filtered <- tide_data_plot %>% filter(tideCategory == tc)
  
  bird_box <- bird_data_filtered %>%
    mutate(dataset = "data_bird_plot") %>%
    select(speciesEN, recvDeployName, duration_h, dataset)
  
  tide_box <- tide_data_filtered %>%
    mutate(dataset = "tide_data_plot",
           recvDeployName = "Available time") %>%
    select(speciesEN, recvDeployName, duration_h, dataset)
  
  plot_df <- bind_rows(bird_box, tide_box) %>%
    mutate(duration_h_num = as.numeric(duration_h) / 3600)
  
  p <- ggplot(plot_df, aes(x = recvDeployName, y = duration_h_num, alpha = dataset)) +
    geom_boxplot(aes(fill = if_else(recvDeployName == "Available time", "#aed7f3", "#1b9ee0")),
                 color = "black",
                 position = position_dodge(width = 0.7),
                 outlier.shape = NA) +
    scale_alpha_manual(values = c("data_bird_plot" = 1, "tide_data_plot" = 0.7)) +
    facet_wrap(~ speciesEN, scales = "free_y") +
    labs(x = paste("Receiver Location (", tc, ")", sep = ""), y = "Duration (hours)", alpha = "Dataset") +
    theme_minimal(base_size = 12) +
    theme(
      strip.text = element_text(face = "bold", size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "none"
    )
  return(p)
})


