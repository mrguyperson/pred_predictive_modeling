basic_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

interaction_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_interact(terms = ~ contains("numeric"):contains("nominal")) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

yj_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

interaction_yj_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_interact(terms = ~ contains("numeric"):contains("nominal")) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}


poly_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

interaction_poly_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_interact(terms = ~ contains("numeric"):contains("nominal")) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

poly_yj_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

interaction_poly_yj_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_interact(terms = ~ contains("numeric"):contains("nominal")) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      )
}

pca_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      ) %>%
      step_pca(all_numeric_predictors(), num_comp = tune())
}

pca_yj_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
      ) %>%
      step_pca(all_numeric_predictors(), num_comp = tune())
}

fhast_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- make_formula(y_var)
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
      step_naomit(all_predictors(), all_of(y_var)) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      recipes::step_dummy(all_nominal_predictors(), -pres_abs) %>%
      recipes::step_zv(all_predictors(),  -pres_abs) %>%
      recipes::step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(pres_abs, over_ratio = tune())
}

make_formula <- function(y_var) {
    formula(glue::glue("{y_var} ~ ."))
}