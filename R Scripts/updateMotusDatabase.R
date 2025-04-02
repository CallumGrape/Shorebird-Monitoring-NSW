library(motus)
library(dplyr)

Sys.setenv(TZ="UTC") # Set timezone to UTC
motusLogout()

# Specify Motus project number
proj.num <- 294

### Download detections for the project (detections of all tags registered to the project - this can include detections from receivers registered to other projects) ----

## IMPORTANT: Select one of the following two options to run

# If you are running this code for the first time on your device (to create the .motus SQLite database)
#sql.motus <- tagme(projRecv = proj.num, new = TRUE, update = TRUE, dir = "./data/")

# If you have an existing version on your device and just need to update it with the new detections
sql.motus <- tagme(projRecv = proj.num, update = TRUE, dir = "./data/")
#metadata(sql.motus, proj.num)

### Get detections for each individual receiver (including detections of tags registered to other projects) ----
## NOTE: Commented out as not using at present / don't have time to make work nicely
# 
# # Get list of receiver deployments from project motus file
# tbl.recvDeps <- tbl(sql.motus, "recvDeps")
# 
# # Get unique list of receiver deployments
# df.serno <- tbl.recvDeps %>%
#    filter(projectID == proj.num) %>%
#    select(serno) %>%
#    distinct() %>%
#    collect() %>% as.data.frame()
# 
# ## IMPORTANT: Same as before, only choose new = TRUE if running for first time
# # If running for first time: loop through each receiver and download detection data
# 
# #for(row in 1:nrow(df.serno)) {
# #  tagme(df.serno[row, "serno"], new = TRUE, update = TRUE, dir = "./data/motus_receiver_data/")
# #}
# 
# # If updating existing files: loop through each receiver and download detection data
#  for(row in 1:nrow(df.serno)) {
#    tagme(df.serno[row, "serno"], update = TRUE, dir = "./data/motus_receiver_data/")
#  }
