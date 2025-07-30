
## Type   :  PhD Project
## Auteur :  Maxime Marini, Lionel Bonsaquet
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  CRAN & Git-Hub package list
## Created:  2025 July 

# Rtools package
{
user_choice_Rtools_installation_R <- readline(prompt = "You may need to first install 'Rtools' package. If yes, you will be redirected to the cran installation page. (Y/N): ")
if (tolower(substr(user_choice_Rtools_installation_R, 1, 1)) == "y") {
  browseURL("https://cran.r-project.org/bin/windows/Rtools/rtools45/rtools.html")
} else {cat("\n")}
}

cat("\nNow loading and calling the packages that are used in this study.\n")

#-- CRAN packages vector --# Pour les package sur le CRAN
cran_packages <- c( 
   
  # Tools
  "devtools",           # Package development tools
  "rstudioapi",         # RStudio API interface
  "DBI",                # Communicate with relational database systems
  "staticryptR",        # Encrypting URL and password stuff
  "Microsoft365R",      # Connect with Microsoft products

  # Plot and Visualization
  "ggplot2",            # Core plotting package
  "ggrepel",            # Repel overlapping text labels
  "ggpubr",             # Publication ready plots
  "ggpattern",          # Adding texture into geom layers
  "ggspatial",          # Map annotations
  "ggmap",              # Create maps with free background
  "ggnewscale",         # Add multiple scale fills
  "gridExtra",          # Arrange multiple plots
  "patchwork",          # Combine multiple plots
  "sjPlot",             # Model plotting (glm, etc)
  "ggtext",             # Improved text rendering for ggplot2
  "tmap",               # Create thematic maps (choropleths and bubble maps)
  
  # Spatial Analysis and Mapping
  "sf",                 # Simple features for R
  "sp",                 # Classes and methods for spatial data
  "terra",              # Spatial data analysis
  "raster",             # Geographic data analysis and modeling
  "adehabitatHR",       # Home range analysis
  "mapview",            # Interactive viewing of spatial data
  "tidyterra",          # Tidy methods for terra objects
  "rnaturalearth",      # World maps and more
  
  # Data Manipulation and Processing
  "tidyverse",          # Collection of data manipulation packages
  "dplyr",              # Data manipulation
  "tidyr",              # Tidy messy data
  "purrr",              # Functional programming tools
  "stringr",            # String manipulation
  "stringi",            # String processing
  "forcats",            # Working with factors
  "lubridate",          # Date and time manipulation
  "reshape2",           # Flexibly reshape data
  "reshape",            # Flexibly reshape data (older version)
  "data.table",         # Enhanced data.frame
  "plyr",               # Tools for splitting, applying and combining data
  "gdata",              # Various R programming tools
  "here",               # File path management
  "readr",              # Read rectangular data
  "bioRad",             # Sunset-rise & coordinates
  
  # Inferential Statistics and Modeling
  "glmmTMB",            # Generalized Linear Mixed Models
  "sjlabelled",         # Labelled data utility functions
  "sjmisc",             # Miscellaneous data management functions
  "circular",           # Circular Statistics
  "amt",                # Animal Movement Tools
  "RMark"              # Interface to Mark program for capture-recapture analysis
)

github_packages <- c(
  "rlesur/klippy",      # For outputs options
  "SebastianSosa/ANTs", # Deal with spatial manipulations and habitat selection models
  "smthfrmn/mixedSSA",  # Deal with ISSA functions to model individual variability in habitat selection and movement parameters
  "MotusWTS/motus",     # Motus wildlife tracking system tools
  "ianjonsen/aniMotum"  # Simulate and deal with GPS data
)    

## non installed package list 
n_i_p_cran <- cran_packages[!(cran_packages %in% installed.packages())]
n_i_p_ghub <- github_packages[!(github_packages %in% installed.packages())]

# Function to install CRAN packages with progress bar
install_cran_with_progress <- function(packages) {
  pblapply(packages, function(pkg) {
    install.packages(pkg, dependencies = TRUE)  })}
# Install CRAN packages
install_cran_with_progress(n_i_p_cran)

# Function to install GitHub packages with progress bar
install_ghub_with_progress <- function(packages) {
  pblapply(packages, function(pkg) {
    install_github(pkg, dependencies = TRUE)  })}
# Install GitHub packages
install_ghub_with_progress(n_i_p_ghub)

## load & call an attach packages 
suppressWarnings(invisible(lapply(cran_packages, library, character.only = TRUE)))
suppressWarnings(invisible(lapply(str_extract(github_packages, "[^/]+$"), library, character.only = TRUE)))




