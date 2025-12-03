
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


# 5 - Split the available time of tide for each station ----

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

# Merging in one data-set to use Station's name further + pick-up the rounded hours
recv.act <- bind_rows(recv.act.runn, recv.act.term) %>%
  mutate(hour_dt = round_date(dateAus, "hour"))

# Providing helpful variables
recv <- recv %>%
  mutate(SernoStation = paste0(recvDeployName, "_", serno),
         lisStart = timeStartAus,
         lisEnd = if_else(
           is.na(timeEndAus), # means the station is still running since the last data downloading
           with_tz(Sys.time(), "Australia/Sydney"),
           with_tz(as_datetime(timeEndAus, tz = "UTC"), "Australia/Sydney")) )

# Generating hourly sequences per SernoStation from start to end dates of the deviceID at particular sites
recv_hours <- recv %>%
  select(recvDeployName, deviceID, SernoStation, lisStart, lisEnd) %>%
  group_by(SernoStation) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = round_date(lisStart, unit = "hour"),
                            to = round_date(lisEnd, unit = "hour"),
                            by = "hour")) ) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup()

# Simplify station variables (recvDeployName)
recv <- recv %>% 
  select(!recvDeployName) 
recv$recvDeployName <- sub("_SG-.*", "", recv$SernoStation)

recv_hours <- recv_hours %>% 
  select(!recvDeployName) 
recv_hours$recvDeployName <- sub("_SG-.*", "", recv_hours$SernoStation)

# Distinguish station from mixed recv + Giving operational variable (= TRUE when existing values from act table)
recv_hours <- recv_hours %>%
  left_join(recv.act %>%
              distinct(recvDeployName, hour_dt) %>%
              mutate(operational = TRUE),
            by = c("recvDeployName", "hour_dt")) %>%
  mutate(operational = if_else(is.na(operational), FALSE, TRUE))

# /!\ Due to unknown error? Have to set this manually
recv_hours <- recv_hours %>%
  mutate(operational = case_when(
    recvDeployName == "Fullerton Entrance" & 
      hour_dt > as.POSIXct("2023-04-02") & 
      hour_dt < as.POSIXct("2023-04-05") ~ FALSE,
    TRUE ~ operational))

# Summary table                                                                     
off_runs <- recv_hours %>% 
  arrange(recvDeployName, hour_dt) %>%
  group_by(recvDeployName) %>%
  mutate(off_run_id = consecutive_id(operational == FALSE)) %>%
  ungroup() %>%
  
  filter(operational == FALSE) %>%
  
  group_by(recvDeployName, off_run_id) %>%
  summarise(
    start_off = min(hour_dt),
    end_off = max(hour_dt),
    tot_off_hours = n(),
    .groups = "drop") %>%
  
  filter(tot_off_hours > 24)

# Unique recvDeployNames from off_runs
recv_names <- unique(recv_hours$recvDeployName)

# Split tide_data into a named list with one element per recvDeployName
tide_data_list <- setNames(vector("list", length(recv_names)), recv_names)

for(name in recv_names) {
  # Get off intervals for this recvDeployName
  intervals <- off_runs %>%
    filter(recvDeployName == name) %>%
    select(start_off, end_off)
  
  # Get deployment start and end dates for this recvDeployName
  deploy <- recv_hours %>%
    filter(recvDeployName == name) %>%
    summarise(
      lisStart = min(lisStart, na.rm = TRUE),
      lisEnd = max(lisEnd, na.rm = TRUE)
    )
  
  # Filter tide_data by deployment period
  td <- tide_data %>%
    filter(timeAus >= deploy$lisStart & timeAus <= deploy$lisEnd) %>%
    mutate(recvDeployName = name) 
  
  if(nrow(intervals) > 0) {
    # Vectorized exclusion of off intervals
    is_in_off <- sapply(td$timeAus, function(t) {
      any(t >= intervals$start_off & t <= intervals$end_off)
    })
    td <- td[!is_in_off, ]
  }
  tide_data_list[[name]] <- td
}

tide_data_df <- bind_rows(tide_data_list) %>%
  mutate(hour_dt = round_date(timeAus, unit = "hour"))            # AVAILABLE TIME (tide categories covering same time as recv survey effort) 

total_recv_tide_data <- tide_data_df %>%
  group_by(recvDeployName) %>%
  summarise(hour_seq = list(seq(min(hour_dt), max(hour_dt), by = "hour")), .groups = "drop") %>%
  unnest(hour_seq) %>%
  rename(hour_dt = hour_seq) %>%
  
  left_join(tide_data_df, by = c("recvDeployName", "hour_dt")) %>%
  arrange(recvDeployName, hour_dt) %>%
  group_by(recvDeployName) %>%
  mutate(across(everything(), ~ zoo::na.locf(.x, na.rm = FALSE), .names = "{.col}"))

# 6 - Split the available time of tide for each birds ----

# Get the monitored period of each bird
period_sp <- data_all  %>%
  group_by(Band.ID) %>%
  reframe(DateAUS.Trap = first(DateAUS.Trap), 
          last_dateAus = max(dateAus),
          speciesEN = speciesEN) %>%
  unique()

# Expand one row per hours to each individual across its whole period (this is the available time)
bird_hours <- period_sp %>%
  group_by(Band.ID) %>%
  rowwise() %>%
  mutate(hour_dt = list(seq(from = as.POSIXct(ymd(DateAUS.Trap), tz = "UTC"),
                            to = as.POSIXct(last_dateAus, tz = "UTC"),
                            by = "hour"))) %>%
  unnest(cols = c(hour_dt)) %>%
  ungroup() 

# Add tide category for each available hours
find_closest_tide <- function(target_time) {
  # Calculate absolute time differences
  time_diffs <- as.numeric(tide_data$timeAus - target_time, units = "mins")
  closest_idx <- which.min(abs(time_diffs))
  return(tide_data$tideCategory[closest_idx])
}
bird_hours <- bird_hours %>%
  mutate(tideCategory = sapply(hour_dt, find_closest_tide))

# Duplicate in as many list as many stations
recv_names <- unique(recv_hours$recvDeployName)
bird_data_list <- setNames(vector("list", length(recv_names)), recv_names)

for(name in recv_names) {
  
  valid_hours_recv <- total_recv_tide_data %>%
    filter(recvDeployName == name) %>%
    pull(hour_dt)
  
  valid_hours_bird_recv <- bird_hours %>%
    filter(hour_dt %in% valid_hours_recv)
  
  valid_hours_bird_recv$name <- name
  
    
  bird_data_list[[name]] <- valid_hours_bird_recv
}


# 6 - Getting FINALLY available time for each tide category (taking into account: recv survey effort*bird monitoring period) ----

# AVAILABLE TIME (tide categories covering same time as recv survey effort AND birds monitoring period)
available_bird_recv_time <- bind_rows(bird_data_list) %>%
  rename(recvDeployName = name) %>%
  group_by(Band.ID, speciesEN, recvDeployName, tideCategory)  %>%
  summarise(duration_h = n()) %>%
  mutate(tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
         tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) 

# USED TIME (amount of time each bird spent during each category of tide and at each station)

# Provide the burst interval value depending Lotek-nano tag model (scd)
data_bird <- data_all %>%
  mutate(burst_inter = ifelse(tagModel == "NTQB2-6-2", dseconds(7.1), dseconds(13.1))) %>%
  select(timeAus, tideCategory, tideHighLow, tideDiel, sunriseNewc, sunsetNewc, 
         speciesEN, tagModel, recvDeployName, recv,speciesSci, Band.ID, burst_inter)

used_bird_recv_time <- data_bird %>%
  group_by(Band.ID, speciesEN, recvDeployName, tideCategory)  %>%
  summarise(duration_sec = sum(burst_inter)) %>%
  mutate(duration_h = round(duration_sec / 3600, 0),
         tideDiel = if_else(grepl("Diurnal", tideCategory), "Diurnal", "Nocturnal"),
         tideHighLow = if_else(grepl("High", tideCategory), "High", "Low")) %>%
  select(speciesEN, tideCategory, tideDiel, tideHighLow, duration_h, recvDeployName)


# 7 - Plot figure ----

# Color per sp
species_colors <- c(
  "Bar-tailed Godwit"      = "#1b9e77",
  "Far Eastern Curlew"     = "#d95f02",
  "Masked Lapwing"         = "#7570b3",
  "Pacific Golden-Plover"  = "#e7298a",
  "Pied Stilt"             = "#66a61e",
  "Red-necked Avocet"      = "#e6ab02",
  "available"              = "grey"
)

# Track size sample
counts <- used_bird_recv_time %>%
      group_by(speciesEN) %>%
      summarise(n = n_distinct(Band.ID)) %>%
      mutate(label = paste0(speciesEN, " (n = ", n, ")"))
label_vec <- setNames(counts$label, counts$speciesEN)
  
# Combine dataset
used_bird_recv_time$type <- "used"
available_bird_recv_time$type <- "available"
combined_data <- bind_rows(used_bird_recv_time, available_bird_recv_time) %>%
  mutate(fill_color = ifelse(type == "available", "available", speciesEN),
         station_type = interaction(recvDeployName, type, lex.order = TRUE))

# Plot function available vs. used
plot_by_tide <- function(tide_cat) {
  data_subset <- combined_data %>% filter(tideCategory == tide_cat)
  
  ggplot(data_subset, 
         aes(x = recvDeployName, y = duration_h, 
             fill = fill_color)) +
    
    geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = species_colors, name = "Type") +
    
    facet_wrap(~ speciesEN, scales = "free_y", 
               labeller = labeller(speciesEN = label_vec)) +

    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  +
    
    coord_cartesian(ylim = c(0, NA)) +     
    labs(x = "MOTUS Stations",
         y = "Detection duration (hours)",
         title = paste("Tide Category:", tide_cat),
         fill = "Type",
         caption = "Used (colors) vs. Available (grey)")
  }

# Get unique tideCategory levels
tide_categories <- combined_data %>%
  filter(!is.na(tideCategory)) %>%
  pull(tideCategory) %>%
  unique()

# Generate a list of plots for all tide categories
plots_list <- purrr::map(tide_categories, plot_by_tide)
plots_list



# Plot function used ONLY
plot_used <- function(tide_cat) {
  data_subset <- used_bird_recv_time %>% filter(tideCategory == tide_cat)
  
  ggplot(data_subset, 
         aes(x = recvDeployName, y = duration_h, fill = speciesEN)) +
    
    geom_boxplot(position = position_dodge2(width = 0.9, preserve = "single")) +
    scale_fill_manual(values = species_colors, name = "Species") +
    
    facet_wrap(~ speciesEN, scales = "free_y", 
               labeller = labeller(speciesEN = label_vec)) +
    
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold", size = 8),
          axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "none")  +
    
    coord_cartesian(ylim = c(0, NA)) + 
    labs(x = "MOTUS Stations",
         y = "Detection duration (hours)",
         title = paste("Tide Category:", tide_cat),
         fill = "Species")
  }

# Get unique tideCategory levels
tide_categories <- combined_data %>%
  filter(!is.na(tideCategory)) %>%
  pull(tideCategory) %>%
  unique()

# Generate a list of plots for all tide categories
plots_list_used <- purrr::map(tide_categories, plot_used)
plots_list_used


# 8 - Plot figure 1 ----

# Combine dataset
figure_plot <- left_join(available_bird_recv_time %>% 
                           select(!type) %>%
                           group_by(recvDeployName, Band.ID, speciesEN, tideCategory) %>%
                           rename(available_t = "duration_h"),
                         used_bird_recv_time %>%
                           select(!type) %>%
                           rename(used_t = "duration_h")) %>%
  mutate(rate_use = used_t*100/available_t) %>%
  mutate(rate_use = ifelse(rate_use > 100, 100, rate_use)) %>%
  mutate(speciesType = case_when(speciesEN %in% c("Bar-tailed Godwit", "Far Eastern Curlew" , "Pacific Golden-Plover")~ "migratory",
                                 speciesEN %in% c("Masked Lapwing" , "Pied Stilt" , "Red-necked Avocet") ~ "resident") %>% 
           as_factor()) %>%
  filter(!speciesEN %in% c("Far Eastern Curlew", "Masked Lapwing"),
         rate_use != 100) 


# Plot
# Tide and species groupings
tide_levels <- c("Low", "High")
species_types <- c("resident", "migratory")

# Function to generate a plot for a given combination
make_plot <- function(tide_levels, species_types) {
  ggplot(figure_plot %>%
           filter(tideHighLow == tide_levels, speciesType == species_types),
         aes(x = factor(recvDeployName, levels = sort(unique(recvDeployName))),
             y = rate_use,
             fill = tideDiel)) +
    geom_boxplot() +
    facet_wrap(~ speciesEN,
               labeller = labeller(speciesEN = label_vec)) +    
    labs(
      x = "Receiver Deployment",
      y = "Rate of Use (%)",
      fill = "Tide Diel",
      title = paste(
        ifelse(species_types == "migratory", "Migratory species", "Resident species"),
        "during", tide_levels, "tide"
      )
    ) +
    coord_cartesian(ylim = c(0, 50)) +
    theme_minimal() +
    scale_fill_manual(values = c("Diurnal" = "white", "Nocturnal" = "darkgrey")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


# Generate and store all plots in a list
plots_used_rate <- cross2(tide_levels, species_types) %>%
  purrr::map(~ make_plot(.x[[1]], .x[[2]]))
plots_used_rate


# Save all plots as PNG files in your working directory
params <- cross2(tide_levels, species_types)
file_names <- map_chr(params, ~ paste0(.x[[1]], "_", .x[[2]], "50perc.png"))
walk2(
  plots_used_rate,
  file_names,
  ~ ggsave(
    filename = .y,
    plot = .x,
    width = 8,
    height = 6,
    dpi = 300
  )
)



# Bar plot that balances the data we got across tideCategory for each species
balance_table <- figure_plot %>%
  group_by(Band.ID, speciesEN, tideCategory) %>%
  summarise(total_used_t = sum(used_t, na.rm = TRUE),
            total_available_t = sum(available_t, na.rm = TRUE),
            total_rate_use = total_used_t*100/total_available_t) %>%
  ungroup()
custom_colors <- c(
  "Nocturnal_Low" = "darkgrey",
  "Nocturnal_High" = "darkgrey",
  "Diurnal_Low" = "white",
  "Diurnal_High" = "white"
)

ggplot(balance_table, aes(x = tideCategory, y = total_rate_use, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~speciesEN) +
  coord_cartesian(ylim = c(0, 40)) + #200 if total_used_t
  scale_fill_manual(values = custom_colors, guide = "none") +  
  labs(title = "Rate of Use - Acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Rate of Use (%)") +
  # labs(title = "Total used - Acquired data for tagged population depending Tide and Time",
  #      x = "Tide Category",
  #      y = "Total used (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# All species and tide categories 
ggplot(balance_table, aes(x = tideCategory, y = total_rate_use, fill = tideCategory)) + # or total_used_t
  geom_boxplot(outlier.shape = NA) +
  facet_wrap(~tideCategory, scales = "free_x", nrow = 1) +
  coord_cartesian(ylim = c(0, 40)) + #200 if total_used_t
  labs(title = "Rate of Use - Acquired data for tagged population depending Tide and Time",
       x = "Tide Category",
       y = "Rate of Use (%)") +
  # labs(title = "Total used - Acquired data for tagged population depending Tide and Time",
  #      x = "Tide Category",
  #      y = "Total used (h)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") +
  scale_fill_manual(values = custom_colors, guide = "none") 
  
