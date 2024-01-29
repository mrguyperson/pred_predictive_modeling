linear_reg_spec <- function(engine, mode){
    linear_reg() %>%
        set_mode(mode) %>%
        set_engine(engine)
}

logistic_reg_spec <- function(engine, mode){
    logistic_reg() %>%
        set_mode(mode) %>%
        set_engine(engine)
}

reg_recipe <- function(training_data, y_var, col_to_drop, id_cols){
    rec_formula <- formula(glue::glue("{y_var} ~ ."))
    recipe(rec_formula, data = training_data) %>% 
        update_role(all_of(id_cols), new_role = "ID") %>%

        step_dummy(all_nominal_predictors(), -pres_abs) %>% 
        step_zv(all_predictors(), -pres_abs) %>% 
        step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
        step_impute_mean(all_numeric(), -pres_abs) %>%
        themis::step_upsample(pres_abs) %>%
        step_rm(all_of(col_to_drop))
}

reg_fit <- function(wf, split){
    wf %>% last_fit(split)
}

set_reg_grid <- function(training_data, size = 100){
    return(NA)
}