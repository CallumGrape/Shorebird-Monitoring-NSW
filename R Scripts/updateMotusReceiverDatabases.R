library(motus)
library(dplyr)

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