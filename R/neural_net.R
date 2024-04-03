

nnet_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- formula(glue::glue("{y_var} ~ ."))
  if(y_var == "count") {
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), new_role = "ID") %>% 
      step_impute_mean(all_numeric_predictors(), -pres_abs) %>%
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      # step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      # step_poly(all_numeric_predictors(), -pres_abs, degree = 2) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
      step_normalize(contains("numeric"), -pres_abs) %>% 
      step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(pres_abs, over_ratio = tune()) %>%
      step_rm(all_of(col_to_drop))
  } else {
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_impute_mean(all_numeric_predictors()) %>%
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      # step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      # step_poly(all_numeric_predictors(), degree = 2) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      # step_interact(terms = ~ starts_with("numeric"):starts_with("numeric")) %>%
      # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
      step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      step_normalize(contains("numeric"), -pres_abs) %>% 
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
          # neighbors = tune("neighbors_over_ratio")
          )
  }
}

set_nnet_grid <- function(training_data, size = 100){
  # just added for consistency with other grid functions
  training_data <- NULL
  grid_latin_hypercube(
    hidden_units(),
    penalty(),
    epochs(),
    # dropout(),
    # activation(),
    size = size
  )
}