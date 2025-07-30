
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Check for up to date R, Rstudio software & packages
## Created:  2025 July


## UPDATE R AND RSTUDIO
## -----------------------------------------------------------------------------------------------------------------------------------------


## Install basic packages

   if (!requireNamespace("pacman", quietly = TRUE)) {install.packages("pacman")} # pacman is an easily loaded pckg to load many others by simply typing their name
   pacman::p_load(knitr,               # to run .Rmd files properly
                  #rstudioapi,          # package to play with Google drive (download the data)
                  fs,                  # tool for file system operations (finding paths, directory, sourcing) used here for path_file()
                  installr,            # to run updateR() installing software from R
                  devtools,            # collection of tools for R (mainly used to install and load packages)
                  utils,               # to run browseURL() to load a given URL into an HTML browser
                  pbapply,             # play with progress bares
                  here)                # other tool for finding paths, directory, sourcing



   
## R version checking (depending user's choice)

   # R update process
   update_R <- function() {
   updateR(browse_news = FALSE,
           install_R = TRUE,           # Install the new R version
           copy_packages = TRUE,       # Copy packages from the old version
           copy_site_files = FALSE,    # to the new version
           keep_old_packages = TRUE,   # Keep the old packages (not deleting them)
           update_packages = FALSE,    # Not update packages (to avoid potential compatibility issues)
           start_new_R = FALSE,        # Not start a new R session 
           quit_R = FALSE)             # or quit the current one
                           }
   # R version check function
   check_r_version <- function() {
     
         #Get current R version
         current_version     <- getRversion()
         current_release_date<- as.Date(paste(R.Version()$year, R.Version()$month, R.Version()$day, sep = "-" ))
     
         #Fetch the latest R version from CRAN
         latest_version      <- gsub(".*R-([0-9.]+).*", "\\1", 
                                     grep("R-[0-9.]+.+-win\\.exe", 
                                         readLines("https://cran.rstudio.com/bin/windows/base/"), 
                                         value = TRUE)[1])
         
         # Extract release date from the page
         release_date_line   <- grep("Last change:", readLines("https://cran.rstudio.com/bin/windows/base/"), value = TRUE)[1]
         latest_release_date <- as.Date(gsub(".*Last change: (.*)$", "\\1", release_date_line))
     
         #Compare versions
   if   (package_version(latest_version) > current_version) {
         cat("A newer version of R is available!\n")
         cat("Your current version:", as.character(current_version), "released in", format(current_release_date, "%B %d, %Y"), "\n")
         cat("Latest available version:", latest_version, "released in", format(latest_release_date, "%B %d, %Y"), "\n")
 } else {
         cat("You are using the latest version of R.\n")
         cat("Current version:", as.character(current_version), "released in", format(current_release_date, "%B %d, %Y"), "\n")
         }                       }
  
## Rstudio version checking and choice for initiating update process
   
   check_r_version()
   
   # User's choice
   if (interactive()) {
   user_choice_update_R      <- readline(prompt = "Regarding the informations here above, do you want to run the R update process, and install the latest available version of R? (Y/N): ")
   
   # Set a default value for non-interactive sessions
 } else {user_choice_update_R<- "N" }
   
   # Process the user's choice
   if   (tolower(substr(user_choice_update_R, 1, 1)) == "y") {
         update_R()
 } else {cat("You choose to not initiate the update process for your R software.\n")}
   remove(user_choice_update_R, update_R)
  
## Rstudio version checking (depending user's choice)
   
   # Get the latest and current version available (Rstudio)
   latest_version            <- scan("http://download1.rstudio.org/current.ver", what = character(0), quiet = TRUE)
   current_rstudio_version   <- as.character(rstudioapi::versionInfo()$long_version)
   
   # Compare versions
   if   (#Condition is respected (Rstudio not up to date)
         latest_version != current_rstudio_version) {
         cat("Your current RStudio (interface of R) version:", current_rstudio_version, "\n")
         cat("The latest stable version of RStudio is:", latest_version, "\n")
        
         #Prompt for user input
         cat("Please, pay attention to the current and latest version items: \nIt might have slight differences (eg: symboles, etc.) - because of the `backhand code` only.\nHowever, you might are actually running the same Rstudio version than the latest available one.")
         
         if (interactive()) {
         user_choice         <- readline(prompt = "Aware of the previous message: Do you want to open the RStudio download page and update your Rstudio version? (Y/N): ")
       } else { user_choice  <- "N"  }
         
         #Check user's choice
         if (tolower(substr(user_choice, 1, 1)) == "y") {
             browseURL("https://www.rstudio.com/products/rstudio/download/")
     } else {cat("Download page not opened. You can update RStudio later. You are currently running with the Rstudio version: ", current_rstudio_version, "\n")}
        
         #Condition is not respected (Rstudio is up to date)
 } else {cat("You are using the latest version of RStudio:", current_rstudio_version, "\n")}

    remove(user_choice, latest_version, current_rstudio_version)
    
  
## INSTALL AND/OR UPDATE AMD LOAD PACKAGES (CRAN/GITHUB)                                                                                                                     
## -----------------------------------------------------------------------------------------------------------------------------------------

    ## Set project path (current_wd is provided in analysis.rmd to getwd as the .rmd location)
    #path_project <- current_wd
    
    ## Set other paths
    #path_data <- here::here("data")
    #path_outputs <- here::here("outputs")
    #path_plots <- here::here("plots")
    
    ## Function to source packages with progress bar
    source_packages <- function(path_package) {
      # Check if required variables exist
      if (!exists("cran_packages") || !exists("github_packages")) {
        stop("Required package lists are not defined. Check packages.R file.")
      }
      
      # CRAN packages
      cat("\nLoading CRAN packages:\n")
      pb <- txtProgressBar(min = 0, max = length(cran_packages), style = 3)
      for (i in seq_along(cran_packages)) {
        require(cran_packages[i], character.only = TRUE)
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
      # GitHub packages
      cat("\nLoading GitHub packages:\n")
      pb <- txtProgressBar(min = 0, max = length(github_packages), style = 3)
      for (i in seq_along(github_packages)) {
        pkg_name <- str_extract(github_packages[i], "[^/]+$")
        require(pkg_name, character.only = TRUE)
        setTxtProgressBar(pb, i)
      }
      close(pb)
      
      # Check CRAN packages
      if (sum(sapply(cran_packages, require, character.only = TRUE)) == length(cran_packages)) {
        cat("\n>> All CRAN packages loaded!\n")
      } else {
        cat("\n>>> Some CRAN packages failed to load!\n")
      }
      
      # Check GitHub packages
      github_pkg_names <- sapply(github_packages, function(x) str_extract(x, "[^/]+$"))
      if (sum(sapply(github_pkg_names, require, character.only = TRUE)) == length(github_packages)) {
        cat("\n>> All GitHub packages loaded!\n")
      } else {
        cat("\n>>> Some GitHub packages failed to load!\n")
      }
    }
    
    ## Source scripts and load packages
    tryCatch(
      {
        # Source packages.R to define package lists
        source(here::here("2_R", "packages.R"))
        
        # Load packages using the source_packages function
        source_packages(here::here("packages.R"))
        
        cat("\nYou are ready to start mate! ;)\n")
      },
      error = function(e) {
        cat("\nOne of the scripts crashed, see below:\n")
        print(paste("Error message:", e$message))
      },
      warning = function(w) {
        cat("\nOne of the scripts ran with warnings, see below:\n")
        print(paste("Warning message:", w$message))
        cat("\nBeing aware of the warnings above, you are ready to start mate! ;)\n")
        stop()
      }
    )
    
## CLEAN UP ENVIRONMENT & MEMORY                                                                                                                    
## -----------------------------------------------------------------------------------------------------------------------------------------

    rm(list = ls())
    rm(list = ls(all.names = TRUE))
    gc()
    
## LOAD FUNCTIONS INTO YOUR ENVIRONMENT                                                                                                                   
## -----------------------------------------------------------------------------------------------------------------------------------------
    
    # Source functions
    #source(here::here("2_R", "functions.R"))
    
    cat("\014")
    cat("\n",
        "R version: ", R.version.string, "\n",
        "RStudio version: ", as.character(rstudioapi::versionInfo()$long_version), "\n",
        "Platform: ", sessionInfo()$platform, "\n",
        "Running under: ", sessionInfo()$running, "\n",
        "\n  = Alrighty mate, you're ready to start! ;)\n\n")
    
  