##### For Delta data set

get_csv_from_path <- function(file, folder) {
  path <- here(folder, file)
  read_csv(path, show_col_types = FALSE)
}
get_full_data_set <- function(data, tax, names_to_drop){

  left_join(data, tax, by = "OrganismCode") %>%
    select(-all_of(names_to_drop), -OrganismCode)
}

get_lmb_data <- function(full_data){

  full_data %>%
    rename_all(tolower) %>%
    mutate(across(c(segment_number, weathercode, channel_type, dominant_substrate, tide, bank_type), as_factor)) %>%
    filter(!is.na(seconds_per_transect) | !is.na(seconds_per_segment) | !is.na(commonname)) %>%
    # mutate(scaled_time = seconds_per_transect / min(seconds_per_transect),
    #        scaled_count = count / scaled_time) %>%
    group_by(regioncode, subregion, sampledate, starttime, segment_number, commonname) %>%
    mutate(
      segment_species_count = sum(count),
      lmb = if_else(commonname == "largemouth bass", segment_species_count, 0)
    ) %>%
    ungroup() %>%
    select(-c(
      stoptime,
      depth_sampled,
      estimated_height,
      sampling_direction:research_vessel,
        pulse_per_second:peak_watts,
      markcode:segment_species_count,
      seconds_per_segment:gps_end_longitude,
      specific_conductance
    )) %>%
    distinct() %>%
    drop_na() %>%
    mutate(across(emergent:bare, ~ .x / 100)) %>%
    select(-bare) %>%
    group_by(regioncode, subregion, sampledate, starttime, segment_number) %>%
    mutate(count = sum(lmb)) %>%
    ungroup() %>%
    select(-lmb) %>%
    distinct() %>%
    pres_abs_conversion()
}
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

pres_abs_conversion <- function(data) {
  data %>%
    mutate(pres_abs = as.factor(fifelse(count > 0, 1, 0)))
}

clean_delta_data <- function(data_path, tax_path) {
  data <- readr::read_csv(data_path)
  tax <- readr::read_csv(tax_path)
  delta_tax_cols <- tax %>% names()
  names_to_drop <- delta_tax_cols[delta_tax_cols != "CommonName"]
  delta_full_data <- get_full_data_set(data, tax, names_to_drop)
  get_lmb_data(delta_full_data)
}

##### FHAST data

# 1. load data -----------------------------------------------------------
# data_2013 <- readr::read_csv(here("data", "csv", "fishbio", "2013 Master Data.csv"))
# data_2014 <- readr::read_csv(here("data", "csv", "fishbio", "2014 Master Data.csv"))

# 2. functions ------------------------------------------------------------

update_cover_names <- function(var) {
  dplyr::case_when(
    {{ var }} == "VERY LOW" ~ "S",
    {{ var }} == "LOW" ~ "M",
    {{ var }} == "MEDIUM" ~ "M",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "VERY HIGH" ~ "H",
    {{ var }} == "HEAVY" ~ "H",
    {{ var }} == "NOW" ~ "S",
    {{ var }} == "HIGH" ~ "H",
    {{ var }} == "D" ~ "H",
    {{ var }} == "NONE" ~ "A",
    TRUE ~ {{ var }}
  )
}

convert_cover_to_val <- function(var) {
  dplyr::case_when(
    {{ var }} == "A" ~ 0,
    {{ var }} == "S" ~ 0.055,
    {{ var }} == "M" ~ 0.3,
    TRUE ~ 0.6,
  )
}

# 3. clean data -----------------------------------------------------------

# work columns so data from both years can be combined
parse_2013 <- function(path_2013){
  data_2013 <- readr::read_csv(path_2013)
  data_2013 %>%
  dplyr::select(
    -c(
      Site:CHNJ,
      MYKISS:ISS,
      TerrVeg:TerrWM,
      TempF:DiffF
    )
  )
}

parse_2014 <- function(path_2014){
  data_2014 <- readr::read_csv(path_2014)

  data_2014 %>%
  dplyr::mutate(
    BASS = rowSums(dplyr::across(LMBS:SMBL)),
    SASQ = rowSums(dplyr::across(SASQS:SASQL))
  ) %>%
  dplyr::select(
    -c(
      Site:C,
      TerrVeg:TerrWM,
      MidChannelTempF:DiffF
    )
  )
}

parse_fhast_data <- function(data_2013, data_2014){
  data_2013_parsed <- parse_2013(data_2013)
  data_2014_parsed <- parse_2014(data_2014)
  bind_rows(data_2013_parsed, data_2014_parsed)
}

# add sample ID column and fix column names

fix_fhast_names <- function(data) {
  data %>%
    rename_all(~ tolower(.)) %>% # for more consistent naming
    rename_all(~ str_replace_all(., "\\s+", "_"))

}


# make data in cover columns consistent and convert to proportions
update_fhast_cover <- function(data) {
  data %>%
    dplyr::mutate(
      dplyr::across(
        emergvegdensity:emergwmdensity,
        toupper
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        emergvegdensity:emergwmdensity,
        .fns = ~ update_cover_names(.x)
      )
    ) %>%
    dplyr::filter(!emergwmdensity == "1") %>%
    # convert cover to values
    dplyr::mutate(
      dplyr::across(
        emergvegdensity:emergwmdensity,
        .fns = ~ convert_cover_to_val(.x)
      ))

}




# update shade
update_fhast_shade <- function(data) {
  data %>%
    dplyr::mutate(
      shade = toupper(shade),
      shade = dplyr::case_when(
        shade == "ABSENT" ~ "A",
        shade == "PRESENT" ~ "P",
        shade == "C" ~ "P",
        TRUE ~ shade
      ),
      shade = as.factor(dplyr::if_else(shade == "P", 1, 0))
    )
}

# add mean depth and velocity
update_fhast_d_and_v <- function(data) {
  data %>%
    dplyr::mutate(
      mean_depth = rowMeans(dplyr::across(depth5:depth15)),
      mean_vel = rowMeans(dplyr::across(vel5:vel15))
    ) %>%
    # drop distance-dependant depth and velocity
    dplyr::select(-c(depth5:depth15, vel5:vel15))
}


# update substrate
update_fhast_substrate <- function(data) {
  data %>%
    dplyr::mutate(
      substrate = substrate10,
      substrate = dplyr::case_when(
        substrate == "G" ~ "R",
        substrate == "R/G" ~ "R",
        substrate == "R/M" ~ "R",
        substrate == "M/R" ~ "R",
        substrate == "M/G" ~ "M",
        substrate == "M/V" ~ "M",
        TRUE ~ substrate
      ),
      substrate = as.factor(dplyr::if_else(substrate == "R", 1, 0))
    ) %>%
    dplyr::select(-c(substrate5:substrate15))
}


# set pred counts to presence/absence
# add_fhast_pres_abs <- function(data) {
#   data %>%
#     dplyr::mutate(
#       dplyr::across(
#         bass:sasq,
#         .fns = ~ dplyr::if_else(.x > 0, "present", "absent"),
#         .names = "{.col}_pres_abs"
#       )
#     )
# }

add_fhast_pres_abs <- function(data) {
  data %>%
    dplyr::mutate(
      pres_abs = as.factor(dplyr::if_else(count > 0, 1, 0))
    )
}

# create predator specific data sets

pivot_species_names <- function(data) {
  data %>% 
    tidyr::pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count")

}

# rename cols to be more compatible with our modeling terms

rename_fhast_cells <- function(data){
  data %>%
    dplyr::rename(
      veg = emergvegdensity, # veg_cover
      wood = emergwmdensity, # wood_cover
      depth_ft = mean_depth,
      velocity_fps = mean_vel
    )
}
# convert to metric
convert_fhast_to_metric <- function(data){
  data %>%
    mutate(across(depth_ft:velocity_fps, .fns = ~ .x / 3.28)) %>%
    rename(
      velocity = velocity_fps,
      depth = depth_ft
    )
}
# # rename sasq to pikeminnow for clarity

# combined_data <- combined_data %>% 
#   mutate(species = if_else(species == "sasq", "pikeminnow", as.character(species)))

# # set shade and substrate as factor variables
# combined_data <- combined_data %>% 
#   mutate(across(c(shade, substrate),
#                 as.factor))

clean_fhast_data <- function(data_2013, data_2014) {
  parse_fhast_data(data_2013, data_2014) %>%
    fix_fhast_names() %>%
    update_fhast_cover() %>%
    update_fhast_shade() %>%
    update_fhast_d_and_v() %>%
    update_fhast_substrate() %>%
    pivot_species_names() %>%
    add_fhast_pres_abs() %>%
    rename_fhast_cells() %>%
    convert_fhast_to_metric() %>%
    drop_na()
}



# set character data types as factors
# combined_data <- combined_data %>%
#   dplyr::mutate_if(is.character, factor)

# drop any NA's

# combined_data <- combined_data %>%
#   tidyr::drop_na()




# lmb <- lmb %>%
#   group_by(regioncode, subregion,sampledate, starttime) %>%
#   summarize(across(c(dominant_substrate, weathercode, channel_type, tide, bank_type),
#                    find_mode),
#             sum_lmb = sum(sum_lmb),
#             across(.cols = where(is.numeric), .fns = mean),
#             .groups = "drop")

# lmb_fitting <- lmb %>%
#   select(-c(regioncode:starttime, segment_number))

# set.seed(123)
# split <- initial_split(lmb_fitting)
# train <- training(split)
# test <- testing(split)


# lm_spec <- linear_reg() %>%
#   set_engine("lm")# ##### FHAST data

# # 1. load data ------------------------------------------------------------
# pred_proj_path <- here::here("scripts","predation")
# data_2013 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2013 Master Data.csv"))
# data_2014 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2014 Master Data.csv"))

# # 2. functions ------------------------------------------------------------

# update_cover_names <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "VERY LOW" ~ "S",
#     {{ var }} == "LOW" ~ "M",
#     {{ var }} == "MEDIUM" ~ "M",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "VERY HIGH" ~ "H",
#     {{ var }} == "HEAVY" ~ "H",
#     {{ var }} == "NOW" ~ "S",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "D" ~ "H",
#     {{ var }} == "NONE" ~ "A",
#     TRUE ~ {{ var }}
#   )
# }

# convert_cover_to_val <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "A" ~ 0,
#     {{ var }} == "S" ~ 0.055,
#     {{ var }} == "M" ~ 0.3,
#     TRUE ~ 0.6,
#   )
# }

# # 3. clean data -----------------------------------------------------------

# # work columns so data from both years can be combined
# data_2013_parsed <-
#   data_2013 %>%
#   dplyr::select(
#     -c(
#       Site:CHNJ,
#       MYKISS:ISS,
#       TerrVeg:TerrWM,
#       TempF:DiffF
#     )
#   )

# data_2014_parsed <-
#   data_2014 %>%
#   dplyr::mutate(
#     BASS = rowSums(dplyr::across(LMBS:SMBL)),
#     SASQ = rowSums(dplyr::across(SASQS:SASQL))
#   ) %>%
#   dplyr::select(
#     -c(
#       Site:C,
#       TerrVeg:TerrWM,
#       MidChannelTempF:DiffF
#     )
#   )

# combined_data <- bind_rows(data_2013_parsed, data_2014_parsed)

# # add sample ID column and fix column names

# combined_data <-
#   combined_data %>%
#   rename_all(~ tolower(.)) %>% # for more consistent naming
#   rename_all(~ str_replace_all(., "\\s+", "_"))

# # make data in cover columns consistent and convert to proportions
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       toupper
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ update_cover_names(.x)
#     )
#   ) %>%
#   dplyr::filter(!emergwmdensity == "1")

# # convert cover to values

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ convert_cover_to_val(.x)
#     )
#   )

# # update shade
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     shade = toupper(shade),
#     shade = dplyr::case_when(
#       shade == "ABSENT" ~ "A",
#       shade == "PRESENT" ~ "P",
#       shade == "C" ~ "P",
#       TRUE ~ shade
#     ),
#     shade = dplyr::if_else(shade == "P", 1, 0)
#   )

# # add mean depth and velocity

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     mean_depth = rowMeans(dplyr::across(depth5:depth15)),
#     mean_vel = rowMeans(dplyr::across(vel5:vel15))
#   )

# # drop distance-dependant depth and velocity
# combined_data <-
#   combined_data %>%
#   dplyr::select(-c(depth5:depth15, vel5:vel15))

# # update substrate

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     substrate = substrate10,
#     substrate = dplyr::case_when(
#       substrate == "G" ~ "R",
#       substrate == "R/G" ~ "R",
#       substrate == "R/M" ~ "R",
#       substrate == "M/R" ~ "R",
#       substrate == "M/G" ~ "M",
#       substrate == "M/V" ~ "M",
#       TRUE ~ substrate
#     ),
#     substrate = dplyr::if_else(substrate == "R", 1, 0)
#   ) %>%
#   dplyr::select(-c(substrate5:substrate15))

# # set pred counts to presence/absence

# combined_data <- combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       bass:sasq,
#       .fns = ~ dplyr::if_else(.x > 0, "present", "absent")
#     )
#   )

# # create predator specific data sets

# combined_data <- combined_data %>%
#   tidyr::pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count")

# # set character data types as factors
# combined_data <- combined_data %>%
#   dplyr::mutate_if(is.character, factor)

# # drop any NA's

# combined_data <- combined_data %>%
#   tidyr::drop_na()

# # rename cols to be more compatible with our modeling terms

# combined_data <- combined_data %>%
#   dplyr::rename(
#     veg = emergvegdensity, # veg_cover
#     wood = emergwmdensity, # wood_cover
#     depth_ft = mean_depth,
#     velocity_fps = mean_vel
#   )

# # convert to metric
# combined_data <- combined_data %>%
#   mutate(across(depth_ft:velocity_fps, .fns = ~ .x / 3.28)) %>%
#   rename(
#     velocity = velocity_fps,
#     depth = depth_ft
#   )

# # rename sasq to pikeminnow for clarity

# combined_data <- combined_data %>% 
#   mutate(species = if_else(species == "sasq", "pikeminnow", as.character(species)))

# # set shade and substrate as factor variables
# combined_data <- combined_data %>% 
#   mutate(across(c(shade, substrate),
#                 as.factor))


# # lmb <- lmb %>%
# #   group_by(regioncode, subregion,sampledate, starttime) %>%
# #   summarize(across(c(dominant_substrate, weathercode, channel_type, tide, bank_type),
# #                    find_mode),
# #             sum_lmb = sum(sum_lmb),
# #             across(.cols = where(is.numeric), .fns = mean),
# #             .groups = "drop")

# # lmb_fitting <- lmb %>%
# #   select(-c(regioncode:starttime, segment_number))

# # set.seed(123)
# # split <- initial_split(lmb_fitting)
# # train <- training(split)
# # test <- testing(split)


# # lm_spec <- linear_reg() %>%
# #   set_engine("lm")

# # lm_fit <- lm_spec %>%
# #   fit(sum_lmb ~ ., data = train)


# # rf_spec <- rand_forest(mode = "regression") %>%
# #   set_engine("ranger")

# # rf_fit <- rf_spec %>%
# #   fit(sum_lmb ~ ., data = train)

# # rf_fit


# # results_train <- lm_fit %>%
# #   predict(new_data = train) %>%
# #   mutate(
# #     truth = train$sum_lmb,
# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = train) %>%
# #     mutate(
# #       truth = train$sum_lmb,
# #       model = "rf"
# #     ))

# # results_test <- lm_fit %>%
# #   predict(new_data = test) %>%# ##### FHAST data

# # 1. load data ------------------------------------------------------------
# pred_proj_path <- here::here("scripts","predation")
# data_2013 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2013 Master Data.csv"))
# data_2014 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2014 Master Data.csv"))

# # 2. functions ------------------------------------------------------------

# update_cover_names <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "VERY LOW" ~ "S",
#     {{ var }} == "LOW" ~ "M",
#     {{ var }} == "MEDIUM" ~ "M",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "VERY HIGH" ~ "H",
#     {{ var }} == "HEAVY" ~ "H",
#     {{ var }} == "NOW" ~ "S",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "D" ~ "H",
#     {{ var }} == "NONE" ~ "A",
#     TRUE ~ {{ var }}
#   )
# }

# convert_cover_to_val <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "A" ~ 0,
#     {{ var }} == "S" ~ 0.055,
#     {{ var }} == "M" ~ 0.3,
#     TRUE ~ 0.6,
#   )
# }

# # 3. clean data -----------------------------------------------------------

# # work columns so data from both years can be combined
# data_2013_parsed <-
#   data_2013 %>%
#   dplyr::select(
#     -c(
#       Site:CHNJ,
#       MYKISS:ISS,
#       TerrVeg:TerrWM,
#       TempF:DiffF
#     )
#   )

# data_2014_parsed <-
#   data_2014 %>%
#   dplyr::mutate(
#     BASS = rowSums(dplyr::across(LMBS:SMBL)),
#     SASQ = rowSums(dplyr::across(SASQS:SASQL))
#   ) %>%
#   dplyr::select(
#     -c(
#       Site:C,
#       TerrVeg:TerrWM,
#       MidChannelTempF:DiffF
#     )
#   )

# combined_data <- bind_rows(data_2013_parsed, data_2014_parsed)

# # add sample ID column and fix column names

# combined_data <-
#   combined_data %>%
#   rename_all(~ tolower(.)) %>% # for more consistent naming
#   rename_all(~ str_replace_all(., "\\s+", "_"))

# # make data in cover columns consistent and convert to proportions
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       toupper
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ update_cover_names(.x)
#     )
#   ) %>%
#   dplyr::filter(!emergwmdensity == "1")

# # convert cover to values

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ convert_cover_to_val(.x)
#     )
#   )

# # update shade
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     shade = toupper(shade),
#     shade = dplyr::case_when(
#       shade == "ABSENT" ~ "A",
#       shade == "PRESENT" ~ "P",
#       shade == "C" ~ "P",
#       TRUE ~ shade
#     ),
#     shade = dplyr::if_else(shade == "P", 1, 0)
#   )

# # add mean depth and velocity

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     mean_depth = rowMeans(dplyr::across(depth5:depth15)),
#     mean_vel = rowMeans(dplyr::across(vel5:vel15))
#   )

# # drop distance-dependant depth and velocity
# combined_data <-
#   combined_data %>%
#   dplyr::select(-c(depth5:depth15, vel5:vel15))

# # update substrate

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     substrate = substrate10,
#     substrate = dplyr::case_when(
#       substrate == "G" ~ "R",
#       substrate == "R/G" ~ "R",
#       substrate == "R/M" ~ "R",
#       substrate == "M/R" ~ "R",
#       substrate == "M/G" ~ "M",
#       substrate == "M/V" ~ "M",
#       TRUE ~ substrate
#     ),
#     substrate = dplyr::if_else(substrate == "R", 1, 0)
#   ) %>%
#   dplyr::select(-c(substrate5:substrate15))

# # set pred counts to presence/absence

# combined_data <- combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       bass:sasq,
#       .fns = ~ dplyr::if_else(.x > 0, "present", "absent")
#     )
#   )

# # create predator specific data sets

# combined_data <- combined_data %>%
#   tidyr::pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count")

# # set character data types as factors
# combined_data <- combined_data %>%
#   dplyr::mutate_if(is.character, factor)

# # drop any NA's

# combined_data <- combined_data %>%
#   tidyr::drop_na()

# # rename cols to be more compatible with our modeling terms

# combined_data <- combined_data %>%
#   dplyr::rename(
#     veg = emergvegdensity, # veg_cover
#     wood = emergwmdensity, # wood_cover
#     depth_ft = mean_depth,
#     velocity_fps = mean_vel
#   )

# # convert to metric
# combined_data <- combined_data %>%
#   mutate(across(depth_ft:velocity_fps, .fns = ~ .x / 3.28)) %>%
#   rename(
#     velocity = velocity_fps,
#     depth = depth_ft
#   )

# # rename sasq to pikeminnow for clarity

# combined_data <- combined_data %>% 
#   mutate(species = if_else(species == "sasq", "pikeminnow", as.character(species)))

# # set shade and substrate as factor variables
# combined_data <- combined_data %>% 
#   mutate(across(c(shade, substrate),
#                 as.factor))


# # lmb <- lmb %>%
# #   group_by(regioncode, subregion,sampledate, starttime) %>%
# #   summarize(across(c(dominant_substrate, weathercode, channel_type, tide, bank_type),
# #                    find_mode),
# #             sum_lmb = sum(sum_lmb),
# #             across(.cols = where(is.numeric), .fns = mean),
# #             .groups = "drop")

# # lmb_fitting <- lmb %>%
# #   select(-c(regioncode:starttime, segment_number))

# # set.seed(123)
# # split <- initial_split(lmb_fitting)
# # train <- training(split)
# # test <- testing(split)


# # lm_spec <- linear_reg() %>%
# #   set_engine("lm")

# # lm_fit <- lm_spec %>%
# #   fit(sum_lmb ~ ., data = train)


# # rf_spec <- rand_forest(mode = "regression") %>%
# #   set_engine("ranger")

# # rf_fit <- rf_spec %>%
# #   fit(sum_lmb ~ ., data = train)

# # rf_fit


# # results_train <- lm_fit %>%
# #   predict(new_data = train) %>%
# #   mutate(
# #     truth = train$sum_lmb,
# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = train) %>%
# #     mutate(
# #       truth = train$sum_lmb,
# #       model = "rf"
# #     ))

# # results_test <- lm_fit %>%
# #   predict(new_data = test) %>%
# #   mutate(
# #     truth = test$sum_lmb,
# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = test) %>%
# #     mutate(
# #       truth = test$sum_lmb,
# #       model = "rf"
# #     ))

# # results_train %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)

# # results_test %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)


# # folds <- vfold_cv(train)
# # wf <- workflow() %>%
# #   add_model(rf_spec)

# # rf_res <- fit_resamples(
# #   workflow(sum_lmb ~ ., rf_spec),
# #   folds,
# #   control = control_resamples(save_pred = TRUE)
# # )

# # rf_res %>%
# #   collect_metrics()

# # rf_res %>%
# #   unnest(.predictions) %>%
# #   ggplot(aes(sum_lmb, .pred, color = id)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = NULL
# #   ) +
# #   theme_bw()


# # results_test %>%
# #   mutate(train = "testing") %>%
# #   bind_rows(results_train %>%
# #     mutate(train = "training")) %>%
# #   ggplot(aes(truth, .pred, color = model)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   facet_wrap(~train) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = "Type of model"
# #   ) +
# #   xlim(0, 20) +
# #   ylim(0, 20) +
# #   theme_bw()


# # results_train %>% 
# #   filter(model == "lm") %>% 
# #   mutate(residual = truth - .pred) %>% 
# #   ggplot(aes(x = truth, y = residual)) +
# #   # geom_abline(lty = 2, color = "black", size = 1.5) +
# #   geom_point(alpha = 0.5, color = "dodgerblue") + 
# #   theme_bw() +
# #   xlim(0,25) +
# #   ylim(0,25) +
# #   xlab("observed number of fish") +
# #   ylab("residual of prediction")


# # m <- full_data %>%
# #   rename_all(tolower) %>%
# #   filter(!is.na(seconds_per_transect)) %>%
# #   group_by(sampledate, starttime, segment_number, commonname, seconds_per_transect) %>%
# #   mutate(sum = sum(count)) %>%
# #   ungroup() %>%
# #   select(sampledate, starttime, segment_number, commonname, seconds_per_transect, sum) %>%
# #   drop_na() %>%
# #   distinct()

# # model <- lm(sum ~ seconds_per_transect, data = m)

# # m2 <- m %>% mutate(pred = predict(model))


# # ggplot(data = m, aes(x = seconds_per_transect, y = sum)) +
# #   geom_point(alpha = 0.5) +
# #   geom_line(data = m2, aes(x = seconds_per_transect, y = pred), color = "red") +
# #   theme_bw()

# # data_folder <- here("data", "res_shore_fish", "csv")
# #
# # files <- list.files(data_folder)
# #
# # for (file in files) {
# #   path <- here(data_folder, file)
# #   csv <- read_csv(path, show_col_types = FALSE)
# #   output_name <- str_replace(file, ".csv", "")
# #   assign(output_name, csv)
# # }
# #
# # tblResSites <- tblResSites %>%
# #   mutate(Location = as.character(Location))
# #
# # full_data <- left_join(tblGPSStartEnd, tblResSites, by = c("ResSitesID", "Date")) %>%
# #   left_join(tblResTotalCatch, by = c("Station.Number", "Date", "Location")) %>%
# #   left_join(tblResFish, by = c("Station.Number", "Species", "Date")) %>%
# #   arrange(Date)

# # lmb <- full_data %>%
# #   filter((Species == "LMB") & (as.numeric(Length) >= 150)) %>%
# #   group_by(Date, ResTotalCatchID) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,
# #     Time.Start,
# #     Time.Stop,
# #     Transport,
# #     Start.Flow:Channel.Flow,
# #     Light,
# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width
# #   )) %>%
# #   mutate(
# #     n = n(),
# #     n_std = n / as.numeric(Shock.Distance) / as.numeric(Shock.Time)
# #   ) %>%
# #   distinct() %>%
# #   select(-c(RIPAR.BUSH:Shock.Distance)) %>%
# #   ungroup() %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   select(-c(ResTotalCatchID, GPSID, n)) %>%
# #   drop_na()
# #
# # lmb <- full_data %>%
# #   mutate(lmb = if_else(((Species == "LMB") & (as.numeric(Length) >= 150)), 1, 0)) %>%
# #   group_by(ResTotalCatchID, Date) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,# ##### FHAST data

# # 1. load data ------------------------------------------------------------
# pred_proj_path <- here::here("scripts","predation")
# data_2013 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2013 Master Data.csv"))
# data_2014 <- readr::read_csv(here(pred_proj_path, "data", "fishbio", "pres_abs", "2014 Master Data.csv"))

# # 2. functions ------------------------------------------------------------

# update_cover_names <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "VERY LOW" ~ "S",
#     {{ var }} == "LOW" ~ "M",
#     {{ var }} == "MEDIUM" ~ "M",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "VERY HIGH" ~ "H",
#     {{ var }} == "HEAVY" ~ "H",
#     {{ var }} == "NOW" ~ "S",
#     {{ var }} == "HIGH" ~ "H",
#     {{ var }} == "D" ~ "H",
#     {{ var }} == "NONE" ~ "A",
#     TRUE ~ {{ var }}
#   )
# }

# convert_cover_to_val <- function(var) {
#   dplyr::case_when(
#     {{ var }} == "A" ~ 0,
#     {{ var }} == "S" ~ 0.055,
#     {{ var }} == "M" ~ 0.3,
#     TRUE ~ 0.6,
#   )
# }

# # 3. clean data -----------------------------------------------------------

# # work columns so data from both years can be combined
# data_2013_parsed <-
#   data_2013 %>%
#   dplyr::select(
#     -c(
#       Site:CHNJ,
#       MYKISS:ISS,
#       TerrVeg:TerrWM,
#       TempF:DiffF
#     )
#   )

# data_2014_parsed <-
#   data_2014 %>%
#   dplyr::mutate(
#     BASS = rowSums(dplyr::across(LMBS:SMBL)),
#     SASQ = rowSums(dplyr::across(SASQS:SASQL))
#   ) %>%
#   dplyr::select(
#     -c(
#       Site:C,
#       TerrVeg:TerrWM,
#       MidChannelTempF:DiffF
#     )
#   )

# combined_data <- bind_rows(data_2013_parsed, data_2014_parsed)

# # add sample ID column and fix column names

# combined_data <-
#   combined_data %>%
#   rename_all(~ tolower(.)) %>% # for more consistent naming
#   rename_all(~ str_replace_all(., "\\s+", "_"))

# # make data in cover columns consistent and convert to proportions
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       toupper
#     )
#   ) %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ update_cover_names(.x)
#     )
#   ) %>%
#   dplyr::filter(!emergwmdensity == "1")

# # convert cover to values

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       emergvegdensity:emergwmdensity,
#       .fns = ~ convert_cover_to_val(.x)
#     )
#   )

# # update shade
# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     shade = toupper(shade),
#     shade = dplyr::case_when(
#       shade == "ABSENT" ~ "A",
#       shade == "PRESENT" ~ "P",
#       shade == "C" ~ "P",
#       TRUE ~ shade
#     ),
#     shade = dplyr::if_else(shade == "P", 1, 0)
#   )

# # add mean depth and velocity

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     mean_depth = rowMeans(dplyr::across(depth5:depth15)),
#     mean_vel = rowMeans(dplyr::across(vel5:vel15))
#   )

# # drop distance-dependant depth and velocity
# combined_data <-
#   combined_data %>%
#   dplyr::select(-c(depth5:depth15, vel5:vel15))

# # update substrate

# combined_data <-
#   combined_data %>%
#   dplyr::mutate(
#     substrate = substrate10,
#     substrate = dplyr::case_when(
#       substrate == "G" ~ "R",
#       substrate == "R/G" ~ "R",
#       substrate == "R/M" ~ "R",
#       substrate == "M/R" ~ "R",
#       substrate == "M/G" ~ "M",
#       substrate == "M/V" ~ "M",
#       TRUE ~ substrate
#     ),
#     substrate = dplyr::if_else(substrate == "R", 1, 0)
#   ) %>%
#   dplyr::select(-c(substrate5:substrate15))

# # set pred counts to presence/absence

# combined_data <- combined_data %>%
#   dplyr::mutate(
#     dplyr::across(
#       bass:sasq,
#       .fns = ~ dplyr::if_else(.x > 0, "present", "absent")
#     )
#   )

# # create predator specific data sets

# combined_data <- combined_data %>%
#   tidyr::pivot_longer(cols = c(bass, sasq), names_to = "species", values_to = "count")

# # set character data types as factors
# combined_data <- combined_data %>%
#   dplyr::mutate_if(is.character, factor)

# # drop any NA's

# combined_data <- combined_data %>%
#   tidyr::drop_na()

# # rename cols to be more compatible with our modeling terms

# combined_data <- combined_data %>%
#   dplyr::rename(
#     veg = emergvegdensity, # veg_cover
#     wood = emergwmdensity, # wood_cover
#     depth_ft = mean_depth,
#     velocity_fps = mean_vel
#   )

# # convert to metric
# combined_data <- combined_data %>%
#   mutate(across(depth_ft:velocity_fps, .fns = ~ .x / 3.28)) %>%
#   rename(
#     velocity = velocity_fps,
#     depth = depth_ft
#   )

# # rename sasq to pikeminnow for clarity

# combined_data <- combined_data %>% 
#   mutate(species = if_else(species == "sasq", "pikeminnow", as.character(species)))

# # set shade and substrate as factor variables
# combined_data <- combined_data %>% 
#   mutate(across(c(shade, substrate),
#                 as.factor))


# # lmb <- lmb %>%
# #   group_by(regioncode, subregion,sampledate, starttime) %>%
# #   summarize(across(c(dominant_substrate, weathercode, channel_type, tide, bank_type),
# #                    find_mode),
# #             sum_lmb = sum(sum_lmb),
# #             across(.cols = where(is.numeric), .fns = mean),
# #             .groups = "drop")

# # lmb_fitting <- lmb %>%
# #   select(-c(regioncode:starttime, segment_number))

# # set.seed(123)
# # split <- initial_split(lmb_fitting)
# # train <- training(split)
# # test <- testing(split)


# # lm_spec <- linear_reg() %>%
# #   set_engine("lm")

# # lm_fit <- lm_spec %>%
# #   fit(sum_lmb ~ ., data = train)


# # rf_spec <- rand_forest(mode = "regression") %>%
# #   set_engine("ranger")

# # rf_fit <- rf_spec %>%
# #   fit(sum_lmb ~ ., data = train)

# # rf_fit


# # results_train <- lm_fit %>%
# #   predict(new_data = train) %>%
# #   mutate(
# #     truth = train$sum_lmb,
# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = train) %>%
# #     mutate(
# #       truth = train$sum_lmb,
# #       model = "rf"
# #     ))

# # results_test <- lm_fit %>%
# #   predict(new_data = test) %>%
# #   mutate(
# #     truth = test$sum_lmb,
# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = test) %>%
# #     mutate(
# #       truth = test$sum_lmb,
# #       model = "rf"
# #     ))

# # results_train %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)

# # results_test %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)


# # folds <- vfold_cv(train)
# # wf <- workflow() %>%
# #   add_model(rf_spec)

# # rf_res <- fit_resamples(
# #   workflow(sum_lmb ~ ., rf_spec),
# #   folds,
# #   control = control_resamples(save_pred = TRUE)
# # )

# # rf_res %>%
# #   collect_metrics()

# # rf_res %>%
# #   unnest(.predictions) %>%
# #   ggplot(aes(sum_lmb, .pred, color = id)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = NULL
# #   ) +
# #   theme_bw()


# # results_test %>%
# #   mutate(train = "testing") %>%
# #   bind_rows(results_train %>%
# #     mutate(train = "training")) %>%
# #   ggplot(aes(truth, .pred, color = model)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   facet_wrap(~train) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = "Type of model"
# #   ) +
# #   xlim(0, 20) +
# #   ylim(0, 20) +
# #   theme_bw()


# # results_train %>% 
# #   filter(model == "lm") %>% 
# #   mutate(residual = truth - .pred) %>% 
# #   ggplot(aes(x = truth, y = residual)) +
# #   # geom_abline(lty = 2, color = "black", size = 1.5) +
# #   geom_point(alpha = 0.5, color = "dodgerblue") + 
# #   theme_bw() +
# #   xlim(0,25) +
# #   ylim(0,25) +
# #   xlab("observed number of fish") +
# #   ylab("residual of prediction")


# # m <- full_data %>%
# #   rename_all(tolower) %>%
# #   filter(!is.na(seconds_per_transect)) %>%
# #   group_by(sampledate, starttime, segment_number, commonname, seconds_per_transect) %>%
# #   mutate(sum = sum(count)) %>%
# #   ungroup() %>%
# #   select(sampledate, starttime, segment_number, commonname, seconds_per_transect, sum) %>%
# #   drop_na() %>%
# #   distinct()

# # model <- lm(sum ~ seconds_per_transect, data = m)

# # m2 <- m %>% mutate(pred = predict(model))


# # ggplot(data = m, aes(x = seconds_per_transect, y = sum)) +
# #   geom_point(alpha = 0.5) +
# #   geom_line(data = m2, aes(x = seconds_per_transect, y = pred), color = "red") +
# #   theme_bw()

# # data_folder <- here("data", "res_shore_fish", "csv")
# #
# # files <- list.files(data_folder)
# #
# # for (file in files) {
# #   path <- here(data_folder, file)
# #   csv <- read_csv(path, show_col_types = FALSE)
# #   output_name <- str_replace(file, ".csv", "")
# #   assign(output_name, csv)
# # }
# #
# # tblResSites <- tblResSites %>%
# #   mutate(Location = as.character(Location))
# #
# # full_data <- left_join(tblGPSStartEnd, tblResSites, by = c("ResSitesID", "Date")) %>%
# #   left_join(tblResTotalCatch, by = c("Station.Number", "Date", "Location")) %>%
# #   left_join(tblResFish, by = c("Station.Number", "Species", "Date")) %>%
# #   arrange(Date)

# # lmb <- full_data %>%
# #   filter((Species == "LMB") & (as.numeric(Length) >= 150)) %>%
# #   group_by(Date, ResTotalCatchID) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,
# #     Time.Start,
# #     Time.Stop,
# #     Transport,
# #     Start.Flow:Channel.Flow,
# #     Light,
# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width
# #   )) %>%
# #   mutate(
# #     n = n(),
# #     n_std = n / as.numeric(Shock.Distance) / as.numeric(Shock.Time)
# #   ) %>%
# #   distinct() %>%
# #   select(-c(RIPAR.BUSH:Shock.Distance)) %>%
# #   ungroup() %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   select(-c(ResTotalCatchID, GPSID, n)) %>%
# #   drop_na()
# #
# # lmb <- full_data %>%
# #   mutate(lmb = if_else(((Species == "LMB") & (as.numeric(Length) >= 150)), 1, 0)) %>%
# #   group_by(ResTotalCatchID, Date) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,
# #     Time.Start,
# #     Time.Stop,
# #     Transport,
# #     Start.Flow:Channel.Flow,
# #     Light,
# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width,
# #     RIPAR.BUSH:Shock.Distance
# #   )) %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   drop_na() %>%
# #   mutate(lmb = as_factor(if_else(sum(lmb) > 0, 1, 0))) %>%
# #   distinct() %>%
# #   ungroup() %>%
# #   select(-c(ResTotalCatchID, GPSID))

# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width,
# #     RIPAR.BUSH:Shock.Distance
# #   )) %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   drop_na() %>%
# #   mutate(lmb = as_factor(if_else(sum(lmb) > 0, 1, 0))) %>%
# #   distinct() %>%
# #   ungroup() %>%
# #   select(-c(ResTotalCatchID, GPSID))

# #     model = "lm"
# #   ) %>%
# #   bind_rows(rf_fit %>%
# #     predict(new_data = test) %>%
# #     mutate(
# #       truth = test$sum_lmb,
# #       model = "rf"
# #     ))

# # results_train %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)

# # results_test %>%
# #   group_by(model) %>%
# #   rmse(truth = truth, estimate = .pred)


# # folds <- vfold_cv(train)
# # wf <- workflow() %>%
# #   add_model(rf_spec)

# # rf_res <- fit_resamples(
# #   workflow(sum_lmb ~ ., rf_spec),
# #   folds,
# #   control = control_resamples(save_pred = TRUE)
# # )

# # rf_res %>%
# #   collect_metrics()

# # rf_res %>%
# #   unnest(.predictions) %>%
# #   ggplot(aes(sum_lmb, .pred, color = id)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = NULL
# #   ) +
# #   theme_bw()


# # results_test %>%
# #   mutate(train = "testing") %>%
# #   bind_rows(results_train %>%
# #     mutate(train = "training")) %>%
# #   ggplot(aes(truth, .pred, color = model)) +
# #   geom_abline(lty = 2, color = "gray80", size = 1.5) +
# #   geom_point(alpha = 0.5) +
# #   facet_wrap(~train) +
# #   labs(
# #     x = "Truth",
# #     y = "Predicted value",
# #     color = "Type of model"
# #   ) +
# #   xlim(0, 20) +
# #   ylim(0, 20) +
# #   theme_bw()


# # results_train %>% 
# #   filter(model == "lm") %>% 
# #   mutate(residual = truth - .pred) %>% 
# #   ggplot(aes(x = truth, y = residual)) +
# #   # geom_abline(lty = 2, color = "black", size = 1.5) +
# #   geom_point(alpha = 0.5, color = "dodgerblue") + 
# #   theme_bw() +
# #   xlim(0,25) +
# #   ylim(0,25) +
# #   xlab("observed number of fish") +
# #   ylab("residual of prediction")


# # m <- full_data %>%
# #   rename_all(tolower) %>%
# #   filter(!is.na(seconds_per_transect)) %>%
# #   group_by(sampledate, starttime, segment_number, commonname, seconds_per_transect) %>%
# #   mutate(sum = sum(count)) %>%
# #   ungroup() %>%
# #   select(sampledate, starttime, segment_number, commonname, seconds_per_transect, sum) %>%
# #   drop_na() %>%
# #   distinct()

# # model <- lm(sum ~ seconds_per_transect, data = m)

# # m2 <- m %>% mutate(pred = predict(model))


# # ggplot(data = m, aes(x = seconds_per_transect, y = sum)) +
# #   geom_point(alpha = 0.5) +
# #   geom_line(data = m2, aes(x = seconds_per_transect, y = pred), color = "red") +
# #   theme_bw()

# # data_folder <- here("data", "res_shore_fish", "csv")
# #
# # files <- list.files(data_folder)
# #
# # for (file in files) {
# #   path <- here(data_folder, file)
# #   csv <- read_csv(path, show_col_types = FALSE)
# #   output_name <- str_replace(file, ".csv", "")
# #   assign(output_name, csv)
# # }
# #
# # tblResSites <- tblResSites %>%
# #   mutate(Location = as.character(Location))
# #
# # full_data <- left_join(tblGPSStartEnd, tblResSites, by = c("ResSitesID", "Date")) %>%
# #   left_join(tblResTotalCatch, by = c("Station.Number", "Date", "Location")) %>%
# #   left_join(tblResFish, by = c("Station.Number", "Species", "Date")) %>%
# #   arrange(Date)

# # lmb <- full_data %>%
# #   filter((Species == "LMB") & (as.numeric(Length) >= 150)) %>%
# #   group_by(Date, ResTotalCatchID) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,
# #     Time.Start,
# #     Time.Stop,
# #     Transport,
# #     Start.Flow:Channel.Flow,
# #     Light,
# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width
# #   )) %>%
# #   mutate(
# #     n = n(),
# #     n_std = n / as.numeric(Shock.Distance) / as.numeric(Shock.Time)
# #   ) %>%
# #   distinct() %>%
# #   select(-c(RIPAR.BUSH:Shock.Distance)) %>%
# #   ungroup() %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   select(-c(ResTotalCatchID, GPSID, n)) %>%
# #   drop_na()
# #
# # lmb <- full_data %>%
# #   mutate(lmb = if_else(((Species == "LMB") & (as.numeric(Length) >= 150)), 1, 0)) %>%
# #   group_by(ResTotalCatchID, Date) %>%
# #   select(-c(
# #     Difficulty:Length,
# #     Electrical.Config,
# #     StartLatDeg:Survey.Month,
# #     Dead,
# #     Stomach.Taken,
# #     ResFishID,
# #     ResSitesID,
# #     BoaterID,
# #     NetterID,
# #     Time.Start,
# #     Time.Stop,
# #     Transport,
# #     Start.Flow:Channel.Flow,
# #     Light,
# #     Turbidity,
# #     Volts,
# #     DayNight,
# #     Pulse.Width,
# #     RIPAR.BUSH:Shock.Distance
# #   )) %>%
# #   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
# #          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
# #   drop_na() %>%
# #   mutate(lmb = as_factor(if_else(sum(lmb) > 0, 1, 0))) %>%
# #   distinct() %>%
# #   ungroup() %>%
# #   select(-c(ResTotalCatchID, GPSID))


# rf_spec <- rand_forest(mode = "regression") %>%
#   set_engine("ranger")

# rf_fit <- rf_spec %>%
#   fit(sum_lmb ~ ., data = train)

# rf_fit


# results_train <- lm_fit %>%
#   predict(new_data = train) %>%
#   mutate(
#     truth = train$sum_lmb,
#     model = "lm"
#   ) %>%
#   bind_rows(rf_fit %>%
#     predict(new_data = train) %>%
#     mutate(
#       truth = train$sum_lmb,
#       model = "rf"
#     ))

# results_test <- lm_fit %>%
#   predict(new_data = test) %>%
#   mutate(
#     truth = test$sum_lmb,
#     model = "lm"
#   ) %>%
#   bind_rows(rf_fit %>%
#     predict(new_data = test) %>%
#     mutate(
#       truth = test$sum_lmb,
#       model = "rf"
#     ))

# results_train %>%
#   group_by(model) %>%
#   rmse(truth = truth, estimate = .pred)

# results_test %>%
#   group_by(model) %>%
#   rmse(truth = truth, estimate = .pred)


# folds <- vfold_cv(train)
# wf <- workflow() %>%
#   add_model(rf_spec)

# rf_res <- fit_resamples(
#   workflow(sum_lmb ~ ., rf_spec),
#   folds,
#   control = control_resamples(save_pred = TRUE)
# )

# rf_res %>%
#   collect_metrics()

# rf_res %>%
#   unnest(.predictions) %>%
#   ggplot(aes(sum_lmb, .pred, color = id)) +
#   geom_abline(lty = 2, color = "gray80", size = 1.5) +
#   geom_point(alpha = 0.5) +
#   labs(
#     x = "Truth",
#     y = "Predicted value",
#     color = NULL
#   ) +
#   theme_bw()


# results_test %>%
#   mutate(train = "testing") %>%
#   bind_rows(results_train %>%
#     mutate(train = "training")) %>%
#   ggplot(aes(truth, .pred, color = model)) +
#   geom_abline(lty = 2, color = "gray80", size = 1.5) +
#   geom_point(alpha = 0.5) +
#   facet_wrap(~train) +
#   labs(
#     x = "Truth",
#     y = "Predicted value",
#     color = "Type of model"
#   ) +
#   xlim(0, 20) +
#   ylim(0, 20) +
#   theme_bw()


# results_train %>% 
#   filter(model == "lm") %>% 
#   mutate(residual = truth - .pred) %>% 
#   ggplot(aes(x = truth, y = residual)) +
#   # geom_abline(lty = 2, color = "black", size = 1.5) +
#   geom_point(alpha = 0.5, color = "dodgerblue") + 
#   theme_bw() +
#   xlim(0,25) +
#   ylim(0,25) +
#   xlab("observed number of fish") +
#   ylab("residual of prediction")


# m <- full_data %>%
#   rename_all(tolower) %>%
#   filter(!is.na(seconds_per_transect)) %>%
#   group_by(sampledate, starttime, segment_number, commonname, seconds_per_transect) %>%
#   mutate(sum = sum(count)) %>%
#   ungroup() %>%
#   select(sampledate, starttime, segment_number, commonname, seconds_per_transect, sum) %>%
#   drop_na() %>%
#   distinct()

# model <- lm(sum ~ seconds_per_transect, data = m)

# m2 <- m %>% mutate(pred = predict(model))


# ggplot(data = m, aes(x = seconds_per_transect, y = sum)) +
#   geom_point(alpha = 0.5) +
#   geom_line(data = m2, aes(x = seconds_per_transect, y = pred), color = "red") +
#   theme_bw()

# data_folder <- here("data", "res_shore_fish", "csv")
#
# files <- list.files(data_folder)
#
# for (file in files) {
#   path <- here(data_folder, file)
#   csv <- read_csv(path, show_col_types = FALSE)
#   output_name <- str_replace(file, ".csv", "")
#   assign(output_name, csv)
# }
#
# tblResSites <- tblResSites %>%
#   mutate(Location = as.character(Location))
#
# full_data <- left_join(tblGPSStartEnd, tblResSites, by = c("ResSitesID", "Date")) %>%
#   left_join(tblResTotalCatch, by = c("Station.Number", "Date", "Location")) %>%
#   left_join(tblResFish, by = c("Station.Number", "Species", "Date")) %>%
#   arrange(Date)

# lmb <- full_data %>%
#   filter((Species == "LMB") & (as.numeric(Length) >= 150)) %>%
#   group_by(Date, ResTotalCatchID) %>%
#   select(-c(
#     Difficulty:Length,
#     Electrical.Config,
#     StartLatDeg:Survey.Month,
#     Dead,
#     Stomach.Taken,
#     ResFishID,
#     ResSitesID,
#     BoaterID,
#     NetterID,
#     Time.Start,
#     Time.Stop,
#     Transport,
#     Start.Flow:Channel.Flow,
#     Light,
#     Turbidity,
#     Volts,
#     DayNight,
#     Pulse.Width
#   )) %>%
#   mutate(
#     n = n(),
#     n_std = n / as.numeric(Shock.Distance) / as.numeric(Shock.Time)
#   ) %>%
#   distinct() %>%
#   select(-c(RIPAR.BUSH:Shock.Distance)) %>%
#   ungroup() %>%
#   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
#          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
#   select(-c(ResTotalCatchID, GPSID, n)) %>%
#   drop_na()
#
# lmb <- full_data %>%
#   mutate(lmb = if_else(((Species == "LMB") & (as.numeric(Length) >= 150)), 1, 0)) %>%
#   group_by(ResTotalCatchID, Date) %>%
#   select(-c(
#     Difficulty:Length,
#     Electrical.Config,
#     StartLatDeg:Survey.Month,
#     Dead,
#     Stomach.Taken,
#     ResFishID,
#     ResSitesID,
#     BoaterID,
#     NetterID,
#     Time.Start,
#     Time.Stop,
#     Transport,
#     Start.Flow:Channel.Flow,
#     Light,
#     Turbidity,
#     Volts,
#     DayNight,
#     Pulse.Width,
#     RIPAR.BUSH:Shock.Distance
#   )) %>%
#   mutate(across(c(Tide.Stage, Channel.Type, Sample.Area, Veg.Type, Bank.Type), as_factor),
#          across(c(Temp, Secchi, Conductivity, Depth, Number.Snag, Wind.Speed), as.numeric)) %>%
#   drop_na() %>%
#   mutate(lmb = as_factor(if_else(sum(lmb) > 0, 1, 0))) %>%
#   distinct() %>%
#   ungroup() %>%
#   select(-c(ResTotalCatchID, GPSID))
