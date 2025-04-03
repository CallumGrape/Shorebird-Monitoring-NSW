library(motus)
library(dplyr)

Sys.setenv(TZ="UTC") # Set timezone to UTC
motusLogout() # Refresh Motus connection

# Specify Motus project number
proj.num <- 294

# Download detections for the project (detections of all tags registered to the project - this can include detections from receivers registered to other projects) ----

# IMPORTANT: Select one of the following two options to run

## If you are running this code for the first time on your device (to create the .motus SQLite database)
#sql.motus <- tagme(projRecv = proj.num, new = TRUE, update = TRUE, dir = "./data/")

## If you have an existing version on your device and just need to update it with the new detections
sql.motus <- tagme(projRecv = proj.num, update = TRUE, dir = "./data/")
dbDisconnect()
#metadata(sql.motus, proj.num)


