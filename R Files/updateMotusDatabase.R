# ---------------------------------------------------------------------------- #
# Title: updateMotusDatabase.R
# Author: Callum Gapes
# Description: This script downloads Motus detection data for NSW (Hunter Region) shorebird tracking, Motus project number 294. Upon running the script, you will be prompted to enter your Motus login details; you must be registered to the Motus project to use this script. The data is downloaded from the Motus server, and stored as a SQLite database in the /Data folder.
# Project: Shorebird Monitoring NSW
# Usage Notes: Either step through and run each line, or run the entire script using "Source". Selecting the entire script and using "Run" will not work as R does not wait for you to input Motus login details (unless you are explicitly running an interactive R session for some reason).
# R Version (Last Tested): R version 4.5.1 (2025-06-13 ucrt)  
# Status: Working
# To Do: Properly understand what the metadata() calls are doing
# Resources: Code adapted from Chapter 3 of the Motus R book, https://motuswts.github.io/motus/articles/03-accessing-data.html
# Database Size: 529 MB as of 26/06/2025
# ---------------------------------------------------------------------------- #


# Load Packages ====
library(motus)
library(dplyr)

Sys.setenv(TZ="UTC") # Set timezone to UTC
motusLogout() # Refresh Motus connection

# Specify Motus project number
proj.num <- 294

# Download detections for the project (detections of all tags registered 
# to the project - this can include detections from receivers registered 
# to other projects) 

# Connect to the Motus server and download / update the local database
start_time <- Sys.time()

sql.motus <- tagme(
  projRecv = proj.num, 
  new = TRUE, 
  update = TRUE, 
  dir = "./Data/")

cat("\nMotus data download took ",Sys.time()-start_time," seconds.\n")
## Note: If you are updating an existing motus database, there will be a warning
## "Database ./data//project-294.motus already exists so I'm ignoring the 
## 'new = TRUE' option". Ignore this; have left it so that this script can be 
## run regardless of whether the database already exists without needing to be 
## edited.

# ==== Update metadata for the project ====
## This covers things like new tags / receivers within the project or if they
## have had their metadata updated.
metadata(sql.motus, proj.num)

# ==== Update entire Motus metadata ====
# DO NOT UNCOMMENT THIS SECTION
# I implemented this as I thought it would be good for completeness, but it turns out that adding the entire metadata prevents the conversion of the dataset from SQLite to a data frame in 'processShorebirdData.R 
# start_time <- Sys.time()
# metadata(sql.motus)
# end_time <- Sys.time()
# cat("\nMetadata update took ",end_time-start_time," seconds.\n")

## This adds metadata for the ENTIRE Motus network, i.e. including all tags
## registered to ANY project. This is necessary to get metadata about any
## tags that we may detect from other projects (although no tags from other
## projects have been detected as of 2025-06-10). 