library(tidyverse)
library(here)
library(broom)
library(tidymodels)



# data_folder <- here::here("data", "csv", "iep")
# files <- list.files(data_folder)


get_csv_from_path <- function(file, folder) {
  path <- here(folder, file)
  read_csv(path, show_col_types = FALSE)
}

# file_list <- map(files, get_csv_from_path, folder = data_folder)

# tax_cols <- file_list[[2]] %>% names()
# names_to_drop <- tax_cols[tax_cols != "CommonName"]

get_full_data_set <- function(file_list, names_to_drop){

  left_join(file_list[[1]], file_list[[2]], by = "OrganismCode") %>%
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
    mutate(sum_lmb = sum(lmb)) %>%
    ungroup() %>%
    select(-lmb) %>%
    distinct()
}
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

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
#   set_engine("lm")

# lm_fit <- lm_spec %>%
#   fit(sum_lmb ~ ., data = train)


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
