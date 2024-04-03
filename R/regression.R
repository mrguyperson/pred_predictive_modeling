
logistic_reg_spec <- function(engine = "glm", mode = "classification"){
    logistic_reg() %>%
        set_mode(mode) %>%
        set_engine(engine)
}

reg_recipe <- function(training_data, y_var, col_to_drop, id_cols){
    rec_formula <- formula(glue::glue("{y_var} ~ ."))
    if(y_var == "count") {
        recipe(rec_formula, data = training_data) %>% 
            update_role(all_of(id_cols), new_role = "ID") %>%
            step_impute_mean(all_numeric_predictors(), -pres_abs) %>%
            # step_poly(all_numeric_predictors(), -pres_abs, degree = 2) %>%
            step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
            step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
            # step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
            step_dummy(all_nominal_predictors(), -pres_abs) %>% 
            # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
            # step_interact(terms = ~ starts_with("numeric"):starts_with("numeric")) %>%
            step_zv(all_predictors(), -pres_abs) %>% 
            step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
            themis::step_upsample(pres_abs) %>%
            step_rm(all_of(col_to_drop))
    } else {
        recipe(rec_formula, data = training_data) %>% 
            update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>%
            step_impute_mean(all_numeric_predictors(), -pres_abs) %>%
            # step_poly(all_numeric_predictors(), degree = 2) %>%
            step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
            step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
            # step_YeoJohnson(all_numeric_predictors(), -pres_abs) %>%
            step_dummy(all_nominal_predictors(), -pres_abs) %>% 
            # step_interact(terms = ~ starts_with("numeric"):starts_with("numeric")) %>%
            step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
            step_zv(all_predictors(), -pres_abs) %>% 
            step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
            themis::step_upsample(pres_abs)
    }

}

reg_fit <- function(wf, split){
    wf %>% last_fit(split)
}



set_glmnet_grid <- function(training_data, size = 100){
  grid_latin_hypercube(
    penalty(),
    mixture(),
    size = size
  )
}