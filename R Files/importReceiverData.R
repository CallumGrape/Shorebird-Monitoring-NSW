# Define folder with the receiver data
folder_path <- "./Data/motus_receiver_data"

# Get a list of all files within the folder
file_list <- list.files(path = folder_path, full.names = TRUE)

# Loop through each receiver
for (file in file_list) {
  # Only process .motus files
  if (grepl("\\.motus$",file)) {
    # Connect to SQLite database
    receiverMotus <- dbConnect(SQLite(), file)
    print(file %>% basename())
    receiverTable <- tbl(receiverMotus, "alltags") %>% collect() %>% as.data.frame()
    assign(paste(basename(file)), receiverTable)
    receiverTable %>% select(speciesEN) %>% unique() %>% print()
    dbDisconnect(receiverMotus)
  }
}

## Testing - accessing data for a receiver
receiverMotus <- dbConnect(RSQLite::SQLite(), "./Data/motus_receiver_data/SG-4B50RPI32DC4.motus")
file <- file_list[1]
receiverMotus <- dbConnect(RSQLite::SQLite(), file)
receiverTable <- tbl(receiverMotus, "alltags")

# List all tables
receiverMotus %>% dbListTables()
