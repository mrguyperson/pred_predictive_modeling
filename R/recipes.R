basic_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      # step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      )
}

interaction_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
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
          pres_abs
      )
}

yj_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      )
}

interaction_yj_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
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
          pres_abs
      )
}


poly_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_normalize(all_numeric_predictors()) %>% 
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      )
}

interaction_poly_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
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
          pres_abs
      )
}

poly_yj_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_poly(all_numeric_predictors(), degree = 2) %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      )
}

interaction_poly_yj_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
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
          pres_abs
      )
}

pca_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      ) %>%
      step_pca(all_numeric_predictors(), num_comp = tune())
}

pca_yj_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
      step_nzv(all_predictors(), -pres_abs) %>% 
      step_normalize(all_numeric_predictors()) %>% 
      step_dummy(all_nominal_predictors(), -pres_abs) %>%
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(
          pres_abs
      ) %>%
      step_pca(all_numeric_predictors(), num_comp = tune())
}

fhast_recipe <- function(training_data) {
  
    recipe(pres_abs ~ ., data = training_data) %>% 
      step_naomit(all_predictors(), pres_abs) %>%
      step_other(all_nominal_predictors(), other = "minor_vals_pooled") %>%
      recipes::step_dummy(all_nominal_predictors(), -pres_abs) %>%
      recipes::step_zv(all_predictors(),  -pres_abs) %>%
      recipes::step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(pres_abs)
}

make_formula <- function(pres_abs) {
    formula(glue::glue("{pres_abs} ~ ."))
}