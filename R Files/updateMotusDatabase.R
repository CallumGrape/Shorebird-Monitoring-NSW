library(motus)
library(dplyr)

Sys.setenv(TZ="UTC") # Set timezone to UTC
motusLogout() # Refresh Motus connection

# Specify Motus project number
proj.num <- 294

# Download detections for the project (detections of all tags registered 
# to the project - this can include detections from receivers registered 
# to other projects) ----

# Connect to the Motus server and download / update the local database
sql.motus <- tagme(projRecv = proj.num, new = TRUE, update = TRUE, dir = "./data/")

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
# Should be run occasionally, but takes a while and is not integral so is 
# commented out by default.
start_time <- Sys.time()
metadata(sql.motus)
end_time <- Sys.time()
cat("\nMetadata update took ",end_time-start_time," seconds.\n")

## This adds metadata for the ENTIRE Motus network, i.e. including all tags
## registered to ANY project. This is necessary to get metadata about any
## tags that we may detect from other projects (although no tags from other
## projects have been detected as of 2025-06-10). 

# ==== Clean up global environment ====
# Remove unecessary variables for clarity
rm(proj.num, sql.motus)

