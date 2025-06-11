library(motus)
library(dplyr)

library(DBI)
library(RSQLite)

### Get detections for each individual receiver (including detections of tags registered to other projects) ----
## NOTE: Commented out as not using at present / don't have time to make work nicely

# Connect to (already downloaded) project SQLite database
project294.motus <- dbConnect(SQLite(), "Data/project-294.motus")

# # Get list of receiver deployments from project motus file
tbl.recvDeps <- tbl(project294.motus, "recvDeps")

df.recvDeps <- tbl.recvDeps %>% 
  collect() %>% 
  as.data.frame()

# # Get unique list of receiver deployments
df.serno <- tbl.recvDeps %>%
    filter(projectID == 294) %>%
    select(serno) %>%
    distinct() %>%
    collect() %>% as.data.frame()

# ## Note: Same as for project .motus file, new = TRUE will raise and error but have left it this way for simplicity.
for(row in 1:nrow(df.serno)) {
  sql_motus <- tagme(df.serno[row, "serno"], new = TRUE, update = TRUE, dir = "./data/motus_receiver_data/")
  metadata(sql_motus)
}

# Disconnect from database
dbDisconnect(project294.motus)