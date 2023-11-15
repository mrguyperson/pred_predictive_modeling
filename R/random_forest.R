library(tidymodels)

# set.seed(123)
# splits <- initial_split(lmb, strata = seconds_per_transect)

# training_data <- training(splits)
# testing_data <- testing(splits)

# cores <- parallel::detectCores()


rf_spec <- function(engine = "ranger", mode = "regression") {
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
    # set_engine("ranger", num.threads = cores) %>% 
    set_engine(engine) %>% 
    set_mode(mode)
}

rf_rec <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- formula(glue::glue("{y_var} ~ ."))
  recipe(rec_formula, data = training_data) %>% 
    update_role(all_of(id_cols), new_role = "ID") %>% 
    step_rm(all_of(col_to_drop)) %>%
    step_dummy(all_nominal_predictors()) %>% 
    step_zv(all_predictors()) %>% 
    step_corr(all_numeric_predictors(), threshold = 0.9)
}

# data_prep <- prep(rec)
# juiced <- juice(data_prep)

tune_wf <- function(rec, spec){
  workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
}

# set.seed(234)
# folds <- vfold_cv(training_data)

set_rf_grid <- function(training_data) {
    grid_latin_hypercube(
      min_n(),
      finalize(mtry(), training_data),
      trees(),
      size = 100
  )
}


tune_model_grid <- function(wf, folds, grid){
  doParallel::registerDoParallel()
  tune_grid(
    wf,
    resamples = folds,
    grid = grid
  )
}


# show_best(tune_res, "rmse")

plot_results <- function(results, metric) {
  results %>%
    collect_metrics() %>%
    filter(.metric == metric) %>%
    select(mean,mtry:min_n) %>%
    pivot_longer(min_n:mtry,
                values_to = "value",
                names_to = "parameter"
    ) %>%
    ggplot(aes(value, mean, color = parameter)) +
    geom_point(show.legend = FALSE) +
    facet_wrap(~parameter, scales = "free_x") +
    labs(x = NULL, y = metric)
}
# rf_grid <- grid_regular(
#   mtry(range = c(5, 15)),
#   min_n(range = c(5, 15)),
#   levels = 20
# )

# set.seed(456)
# doParallel::registerDoParallel()
# regular_res <- tune_grid(
#   tune_wf,
#   resamples = folds,
#   grid = rf_grid
# )

# regular_res

# best <- select_best(regular_res, "rmse")
# best

# final_rf <- finalize_model(
#   spec,
#   best
# )

# final_rf


# library(vip)

# final_rf %>%
#   set_engine("ranger", importance = "permutation") %>%
#   fit(sum_lmb ~ .,
#       data = juiced %>% select(-c(regioncode:starttime, segment_number))
#   ) %>%
#   vip(geom = "col")

# final_wf <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(final_rf)

# final_res <- final_wf %>%
#   last_fit(splits)

# final_res %>%
#   collect_metrics()

# final_rf_regression_fit %>% 
#   collect_predictions() %>% 
#   ggplot(aes(x = sum_lmb, y = .pred)) +
#   geom_abline(lty = 2, color = "black", size = 1.5) +
#   geom_point(alpha = 0.5, color = "dodgerblue") + 
#   theme_bw() +
#   xlab("observed number of fish") +
#   ylab("predicted number of fish") +
#   xlim(0,25) +
#   ylim(0,15)

# lmb2 <- lmb %>% select(-c(Sample.Area, Wind.Speed, Conductivity, Temp, Number.Snag, Tide.Stage))
# 
# splits <- initial_split(lmb2, strata = lmb)
# 
# training_data <- training(splits)
# testing_data <- testing(splits)
# 
# spec <- parsnip::logistic_reg() %>%
#   parsnip::set_engine("glm")
# 
# rec <- recipe(lmb ~ ., data = training_data) %>% 
#   update_role(Date, new_role = "ID") %>% 
#   themis::step_rose(lmb)
# 
# wf <-   workflows::workflow() %>%
#   workflows::add_recipe(rec) %>%
#   workflows::add_model(spec)
# 
# fit <- wf %>%
#   tune::last_fit(splits)
# 
# fit %>% collect_metrics()
# 

