# library(tidyverse)
# library(here)
# library(broom)
# library(Hmisc)

# mdb_path <- here::here("data", 'db', "resident_shoreline_fish_sampling_db.mdb")
# db_tables <- Hmisc::mdb.get(mdb_path, tables = TRUE)
# db <- Hmisc::mdb.get(mdb_path)

# make_csv <- function(name, db){
#   table <- db[[name]] %>% tibble::as_tibble()
#   readr::write_csv(table, here::here("data", "csv", "res_shore_fish", paste0(name, ".csv")))
# }

# purrr::walk(db_tables, make_csv, db = db)