
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Access the relative site usage from birds in regard of the survey period for each MOTUS station 
## Created:  2025 July 


# 1 - Packages ----

library(motus)
library(dplyr)
library(here)
library(forcats) 
library(ggplot2)
library(lubridate)
library(tidyr)

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

# Receivers activity
sql.motus <- DBI::dbConnect(RSQLite::SQLite(), here::here("1_data", "alltags", "project-294.motus"))
recv.act <- tbl(sql.motus, "activity")  %>% 
  collect() %>% 
  as.data.frame() %>%
  rename(deviceID = "motusDeviceID") %>%
  filter(deviceID %in% unique(recv$deviceID)) %>% # keep our deployed antennas only 
  
  # Set the time properly - IMPORTANT
  mutate(date = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC")),
         dateAus = as_datetime(as.POSIXct(hourBin* 3600, origin = "1970-01-06", tz = "UTC"), 
                             tz = "Australia/Sydney")) 

table(is.na(recv.act$pulseCount), recv.act$numTags)                           #/!\ WARNING: pulseCount = any kind of radio contact
                                                                              # numTags = number of diff tags reordered
                                                                              # If one is NA but not the other = means listening
table(is.na(recv.act$pulseCount) & is.na(recv.act$numTags))
                                                                              # IF both are NA : working but not listening ? not long enough contact to be recorded + or noise ?

# 3 - Clarifying sernoID with stationName, as devices might have been used many times at many places ----

# Sort the terminated serno (if terminated, ie. one box removed from one antenna site, a date comes along)
# but still needed for accessing survey effort as the station is currently running with another serno 
recv.act.term <- recv.act %>%
  left_join(recv %>% 
              filter(!is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Sort the currently running serno
recv.act.runn <- recv.act %>%
  left_join(recv %>% 
              filter(is.na(timeEndAus)) %>%
              select(deviceID, serno, recvDeployName),
            "deviceID") %>%
  filter(!is.na(recvDeployName)) %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno))

# Merging in one data-set
recv.act <- bind_rows(recv.act.runn, recv.act.term)

# Providing helpful variables
recv <- recv %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno),
         lisStart = timeStartAus,
         lisEnd = if_else(
           is.na(timeEndAus),
           with_tz(Sys.time(), "Australia/Sydney"),
           with_tz(as_datetime(timeEndAus, tz = "UTC"), "Australia/Sydney")) )

# 4 - Extract the period of time a station is 'listening' ----

# Generating hourly sequences per deviceID/SernoStation - expands each receiver to one row per hour between its listening start and end
recv_hours <- recv %>%
  select(recvDeployName, deviceID, SernoStation, lisStart, lisEnd) %>%
  rowwise() %>%
  mutate(hourSeq = list(seq(from = floor_date(lisStart, unit = "hour"),
                            to = floor_date(lisEnd, unit = "hour"),
                            by = "hour")) ) %>%
  unnest(cols = c(hourSeq)) %>%
  rename(hour_dt = hourSeq) %>%
  ungroup()

# Align recv.act dates to full hours (floor date)
recv.act <- recv.act %>%
  mutate(hour_dt = floor_date(dateAus, "hour"))

# Giving operational and not hours
activity_hours <- recv.act %>%
  distinct(SernoStation, hour_dt) %>%
  mutate(operational = TRUE)
recv_status <- recv_hours %>%
  left_join(activity_hours, by = c("SernoStation", "hour_dt")) %>%
  mutate(operational = if_else(is.na(operational), FALSE, TRUE))

# 5 - Displaying the survey effort from the MOTUS array ----

# Relaying on the Station name on its own only
recv.act$Station <- sub("_SG-.*", "", recv.act$SernoStation)
recv$Station <- sub("_SG-.*", "", recv$SernoStation)
recv_status$Station <- sub("_SG-.*", "", recv_status$SernoStation)

# Summary table                                                                       ## TABLE TO PRINT OUT IN THE QUARTO AS WELL + ADD %T cover over start to end date
uptime_summary <- recv_status %>%
  group_by(Station) %>%
  summarise(
    total_hours = n(), # Period of time the station into the field
    operational_hours = sum(operational), # ON
    downtime_hours = total_hours - operational_hours, # OFF
    uptime_pct = 100 * operational_hours / total_hours) %>% # % ON/station
  mutate(cont_surv_eff = 100 * operational_hours / sum(operational_hours)) %>%
  arrange(desc(uptime_pct))

# Plot (hour detailed)
motus_survey_h <- ggplot(recv_status %>% 
         filter(operational),
       aes(x = hour_dt, y = factor(Station))) +
  geom_segment(aes(
    x = hour_dt,
    xend = hour_dt + hours(1),
    y = Station,
    yend = Station
  ), color = "black", linewidth = 1) +
  scale_y_discrete(name = "Receiver Station") +
  scale_x_datetime(name = "Time") +
  theme_minimal() +
  ggtitle("Receiver Operational Periods (Gaps > 1 hours)")

motus_survey_h

# Plot (day detailed)
recv.status <- recv_status %>%
  filter(operational) %>%
  arrange(Station, hour_dt) %>%
  group_by(Station) %>%
  # Calculate gap (in hours) between consecutive operational hours
  mutate(gap_hours = as.numeric(difftime(hour_dt, lag(hour_dt), units = "hours")),
         # New run starts if gap > 24h or if first row (NA gap)
         run_group = cumsum(if_else(is.na(gap_hours) | gap_hours > 24, 1, 0))) %>%
  group_by(Station, run_group) %>%
  # Get the start and end datetime per run group
  summarise(start_hour = min(hour_dt),
            end_hour = max(hour_dt) + hours(1), # +1 hour to cover full period
            .groups = "drop") %>%
  left_join(uptime_summary %>% select(Station, cont_surv_eff), "Station") %>%
  mutate(StationP = paste0(Station, " (", round(cont_surv_eff, digits = 1), "%)")) 

motus_survey_d <- ggplot(recv.status, aes(y = factor(StationP))) +
  geom_segment(aes(x = start_hour, xend = end_hour,
                   yend = factor(StationP)),
               color = "black", linewidth = 1) +
  
  scale_y_discrete(name = "") +
  scale_x_datetime(name = "Time",
                   date_breaks = "1 month",
                   date_labels = "%b",
                   sec.axis = dup_axis(breaks = seq(from = floor_date(min(recv.status$start_hour),
                                                                      "year") + months(3),
                                                    to = floor_date(max(recv.status$end_hour), 
                                                                    "year") + months(3),
                                                    by = "1 year"),
                                       labels = function(x) format(x, "%Y"),
                                       name = NULL)) +
  
  theme_minimal() +
  theme(axis.text.y.left = element_text(face = "bold", vjust = 0.5, margin = margin(t = 5)),
        axis.text.x.top = element_text(face = "bold", vjust = 0.5, margin = margin(t = 5)),
        axis.text.x = element_text(size = 9)) +
  ggtitle("Receiver Operational Periods (gaps > 24h)")

motus_survey_d

# 5 - Adding Birds ----

data_all_plot <- left_join(data_all %>%
                             select(motusTagID, recv, recvDeployName, timeAus, tideCategory, speciesEN), 
                           recv.status %>% 
                             rename(recvDeployName = Station) %>%
                             select(recvDeployName, StationP) %>%
                             unique(), 
                           "recvDeployName")

motus_survey_d <- motus_survey_d +
  geom_point(
    data = data_all_plot ,
    aes(x = timeAus,
        y = factor(StationP), 
        color = speciesEN),
    alpha = 0.7, size = 2) +
  scale_color_discrete(name = "Species")

motus_survey_d

# Suppose your plot object is named motus_survey_d
ggsave(here::here("3_figures", "motus_survey_plot.png"), plot = motus_survey_d, width = 15, height = 5, units = "in")



