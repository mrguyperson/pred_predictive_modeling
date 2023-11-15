linear_reg_spec <- function(){
    linear_reg() %>%
        set_mode("regression") %>%
        set_engine("lm")
}

logistic_reg_spec <- function(){
    logistic_reg() %>%
        set_mode("classification") %>%
        set_engine("glm")
}

reg_recipe <- function(training_data, y_var, col_to_drop, id_cols){
    rec_formula <- formula(glue::glue("{y_var} ~ ."))
    recipe(rec_formula, data = training_data) %>% 
        update_role(all_of(id_cols), new_role = "ID") %>% 
        step_rm(all_of(col_to_drop)) %>%
        step_dummy(all_nominal_predictors()) %>% 
        step_zv(all_predictors()) %>% 
        step_corr(all_numeric_predictors(), threshold = 0.9) %>%
        step_impute_mean(all_numeric())
}

reg_fit <- function(wf, split){
    wf %>% last_fit(split)
}