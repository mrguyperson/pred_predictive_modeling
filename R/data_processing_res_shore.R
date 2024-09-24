# library(tidyverse)
# library(here)
# library(broom)
# library(Hmisc)
# library(dbplyr)

# mdb_path <- here::here("data", 'db', "resident_shoreline_fish_sampling_db.mdb")
# db_tables <- Hmisc::mdb.get(mdb_path)

# names(db_tables)

# make_csv <- function(name, db){
#   table <- db[[name]] %>% tibble::as_tibble()
#   readr::write_csv(table, here::here("data", "csv", "res_shore_fish", paste0(name, ".csv")))
# }

# purrr::walk(db_tables, make_csv, db = db)
get_res_fish_shore_paths <- function() {
    folder<-here::here("data", "csv", "res_shore_fish")
    files <- list.files(folder)
    map(files, ~here::here(folder, .x)) %>% unlist()
}

get_res_shore_data <- function(path_list) {
    # path_list <- get_res_fish_shore_paths()
    csv_list <- map(path_list, read_csv, show_col_types = FALSE)

    site_data <- csv_list[[3]]
    catch_data <- csv_list[[4]]
    bank_def <- csv_list[[5]]
    channel_def <- csv_list[[6]]
    # delta_def <- csv_list[[7]]
    sampling_def <- csv_list[[8]] %>% rename(Sample.Area = Sampling.Area)
    veg_def <- csv_list[[11]] %>% rename(Veg.Type = Vegetation.Type)

    df_list <- list(site_data, bank_def, channel_def, veg_def, sampling_def)
    
    combined_site_data <- reduce(df_list, left_join)

    left_join(combined_site_data, catch_data, by = c("Station.Number", "Date")) %>%
        select(Date, Time.Start, Time.Stop, Station.Number, Total.Fish.Caught, Species, Number,
             Tide.Stage, Temp, Secchi, Conductivity, Channel.Def,
            Depth, Number.Snag, Wind.Speed, Vegetation.Type.Def, Bank.Type.Def, Sampling.Area.Def, Total.Species,
            contains("RIPAR"), Shock.Distance) %>%
        drop_na(Number) %>%
        # group_by(Date, Station.Number, Species) %>%
        # mutate(total_species_count = sum(Number), tell = fifelse(total_species_count == Number, T, F)) %>%
        # group_by(Date, Station.Number) %>%
        mutate(
            count = fifelse(Species == "LMB", Number, 0) / (Shock.Distance / 1000 ), # per km shocked
            pres_abs = as.factor(fifelse(count > 0, 1, 0))
            ) %>% 
        # ungroup() %>%
        select(-c(Shock.Distance, Species, Number,Total.Fish.Caught)) %>%
        distinct() %>% 
        drop_na() %>%
        rename_with(.fn = tolower) %>%
        rename_with(.fn = ~ str_remove_all(., ".def")) %>%
        rename_with(.fn = ~str_replace_all(., "\\.", "_")) %>%
        mutate(
            across(where(is.character), ~ str_replace_all(., " ", "_")),
            across(where(is.character), ~ str_replace_all(., "-", "_")),
            across(c(where(is.character), contains("ripar"), pres_abs), as.factor),
            total_species = fifelse(pres_abs == 1, total_species - 1, total_species)
            )
}
