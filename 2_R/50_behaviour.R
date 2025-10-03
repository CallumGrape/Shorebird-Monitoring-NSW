
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Relative behavior analysis with the use of signal strength
## Created:  2025 July 

# 1 - Packages ----

library(dplyr)
library(here)
library(forcats) 
library(lubridate)
library(purrr) 
library(lubridate)
library(ggplot2)
library(stringr)


# 2 - Settings ----

# Set WD
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) 

# 3 - Load data ----

# Birds
data_all <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-data\\.rds$", full.names = TRUE
  )), 1)) # pick up the most recent .rds file

# Receivers info
recv <- readRDS(
  tail(sort(list.files(
    here::here("1_data", "alltags", "motus.rds"),
    pattern = "-recv-info\\.rds$", full.names = TRUE
  )), 1)) 


# REMINDER = We just moved up the sig values as following:
# Positive signal strength (min. = 0) for plotting
# mutate(sigPositive = sig + abs(min(sig)))

data <- data_all %>%
  select(Band.ID, recvDeployName, recv, speciesEN, sig, sigsd, sigPositive, tideCategory, timeAus)


# 4 - Compute new variables ----

# Mean signal strengh per individual, site and time categories (HD, HN, LD, LN)
data <- data %>%
  group_by(Band.ID, tideCategory, recvDeployName) %>%
  mutate(sig_indiv_tideCat = mean(sigPositive)) %>%
  ungroup()  %>%
  mutate(speciesINIT = str_split(speciesEN, " ") %>%            
           map_chr(~ str_c(str_sub(.x, 1, 1), collapse = "")) %>%    
           toupper(),
         tideCat = str_split(tideCategory, "_") %>%          
           map_chr(~ str_c(str_sub(.x, 1, 1), collapse = "")) %>%    
           toupper())
# %>%
#   group_by(speciesEN, tideCategory, recvDeployName) %>%
#   mutate(mean_sig_sp_tideCat = mean(sig_indiv_tideCat),
#          sd_sig_sp_tideCat = sd(sig_indiv_tideCat),
#          med_sig_sp_tideCat = median(sig_indiv_tideCat))


# 5 - Plot the boxplot ----

# Color codes
species_colors <- c(
  "Bar-tailed Godwit"      = "#1b9e77",  
  "Far Eastern Curlew"     = "#d95f02",  
  "Masked Lapwing"         = "#7570b3", 
  "Pacific Golden-Plover"  = "#e7298a", 
  "Pied Stilt"             = "#66a61e", 
  "Red-necked Avocet"      = "#e6ab02"   
)

speciesINIT_colors <- c(
  "BG"  = "#1b9e77", 
  "FEC" = "#d95f02",  
  "ML"  = "#7570b3",  
  "PG"  = "#e7298a",  
  "PS"  = "#66a61e", 
  "RA"  = "#e6ab02"   
)

# General plot
ggplot(data  %>%
         mutate(tideCategory = factor(tideCategory, 
                                      levels = c("Nocturnal_Low", "Nocturnal_High", 
                                                 "Diurnal_Low", "Diurnal_High"))) ,
       aes(x = tideCategory, y = sig_indiv_tideCat, fill = speciesEN)) +
  geom_boxplot() +   
  scale_fill_manual(values = species_colors) +
  geom_text(aes(x = tideCategory, y = 0, label = speciesINIT, color = speciesINIT),
            position = position_dodge(width = 0.75),  # align with boxplot boxes
            size = 3, vjust = 1) +
  scale_color_manual(values = speciesINIT_colors) + 
  facet_wrap(~ recvDeployName) +
  theme_bw() +
  labs(
    title = "Signal Strength variance by Tide Categories, Species and Stations",
    x = "Tide Category",
    y = "Signal Strength variance",
    fill = "Species"
  )

# One plot per species for better understanding
plots_list <- data %>%
  split(.$speciesEN) %>%                             # split data by speciesEN
  map(~ ggplot(.x, aes(x = tideCat, y = sig_indiv_tideCat, fill = speciesEN)) +
        geom_boxplot() +
        scale_fill_manual(values = species_colors, guide = "none") +  # remove fill legend
        facet_wrap(~ recvDeployName) +
        theme_bw() +
        labs(
          title = paste0("Signal Strength variance for ", unique(.x$speciesEN), " over MOTUS stations"),
          x = "Tide Category",
          y = "Signal Strength variance (RSSI)",
          fill = "Species"
        ))

for (species_name in names(plots_list)) {
  cat("\n\n## ", species_name, "\n\n")
  print(plots_list[[species_name]])
  flush.console()  
}


