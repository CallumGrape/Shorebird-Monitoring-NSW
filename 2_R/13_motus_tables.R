
## Type   :  PhD Project
## Auteur :  Maxime Marini
## Topic  :  Habitat selection from migratory shorebirds within and across Hunter & Port Stephen estuaries
## Main   :  Navigate through different tables stored within the motus.sql file
## Created:  2025 August

# Packages
library(motus)
library(here)
library(DBI)
library(RSQLite)
library(purrr)


# Connect to your data
sql.motus <- dbConnect(SQLite(), here::here("1_data", "alltags", "project-294.motus"))

# Get list of table names
table_names <- dbListTables(sql.motus)

# Create a named list of data frames, one per table
tables <- set_names(table_names) %>%
  map(~ tbl(sql.motus, .x) %>% collect() %>% as.data.frame())
