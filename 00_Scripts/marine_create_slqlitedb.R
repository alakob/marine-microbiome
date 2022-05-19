library(DBI)
library(RSQLite)
# Database
library(odbc)
library(RSQLite)

library(tidyverse)
library(dbplyr)

?SQLite

# Connect to non-existant database to instantiate
con <- dbConnect(drv = RSQLite::SQLite(), dbname = "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_dashboard/dashboard_marine/00_data/marine_db.db") #, create=TRUE)

########################################################
#      RETRIEVE DATA FROM BACK-END DATABASE
########################################################
basedir <- "/home/alakob/Documents/development/skill_development/practices/machine_learning/R/"
marine_dump_cmd <- str_c(basedir,"marine_dump_from_db.R", sep = "")

source (marine_dump_cmd)

# Get families data
marine_families_tbl <- pull_from_db(family = TRUE, save = FALSE)
software_versions_tbl <- pull_software_version()
#marine_species_tbl <- pull_from_db(family=FALSE, save=FALSE)

#marine_families_tbl %>%  write_rds("/home/alakob/Documents/development/skill_development/practices/machine_learning/R/marine_dashboard/00_data/marine.rds")

# Make Tables
DBI::dbWriteTable(conn = con, "marine_families", marine_families_tbl %>%  mutate(col_date= col_date %>%  as.character()) , overwrite = TRUE)
DBI::dbWriteTable(conn = con, "software_versions", software_versions_tbl, overwrite = TRUE)

# Verify Tables were created
DBI::dbListTables(con)

tbl(con, "marine_families")


DBI::dbListTables(con) %>%
    map(.f = function(x) tbl(con, x)) %>%
    set_names(dbListTables(con))

dbCommit(con)

dbDisconnect(con)
