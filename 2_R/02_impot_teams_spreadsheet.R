


library(Microsoft365R)
library(readxl)

# Connect to team
team <- get_team("StudentGroupPhD - Louise Williams and Mattea Taylor")

# Access the drive
drv <- team$get_drive()

# Download the file from Teams
drv$download_file("General/SHOREBIRD NUMBER TRACKING.xlsx", dest = "SHOREBIRD NUMBER TRACKING.xlsx")

# Save 
write.csv(read_excel("SHOREBIRD NUMBER TRACKING.xlsx"),
          here::here("1_data", "spreadsheets", paste0("teams_sheet_", Sys.Date()), 
          row.names = FALSE)

# Load df 
spreadsheet <- read.csv(here::here("1_data", "spreadsheets", paste0("teams_sheet_", Sys.Date()))
