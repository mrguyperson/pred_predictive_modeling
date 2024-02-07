make_mtry_data_frame <- function(training_data, id_cols, col_to_drop, y_var){
    training_data %>%
        dplyr::select(-all_of(id_cols), -all_of(col_to_drop), -all_of(y_var))
}


tune_model_grid <- function(wf, folds, grid){
    # doParallel::registerDoParallel()
    tune_grid(
        wf,
        resamples = folds,
        grid = grid
    )
}


tune_wf <- function(rec, spec){
  workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)
}

split_with_seed <- function(dataset, stratum, seed) {
    set.seed(seed)
    rsample::initial_split(dataset, strata = all_of(stratum))
}

get_fhast_col_names <- function(fhast_data) {
    fhast_data %>%
        dplyr::select(-pres_abs) %>%
        names()
}

make_df_for_pred_predicitons <- function(df, col_names, substrate_option) {
    df %>%
        as_tibble() %>%
        filter(wetted == 1) %>%
        # drop_na(c(depth, velocity)) %>%
        mutate(
            species = NA_character_,
            count = NA_real_,
            shade = factor(fifelse(shade > 0.5, 1, 0)),
            substrate = rowSums(across(gravel:rock)),
            # if rocky substrate is the majority in a cell, then 1, else 0
            substrate = factor(fifelse(substrate >= fine & substrate > 0, 1, 0)),
            substrate = factor(substrate_option)
            ) %>%
        select(all_of(col_names))
}

make_fhast_pred_predicitons <- function(model, df) {
    pull(predict(model, type = "prob", new_data = df), .pred_1)
}

select_models <- function(final_fit) {
    final_fit$.workflow[[1]]
}

make_table_of_fhast_models <- function(){
    expand_grid(
        species = c("smb", "sasq"),
        model_name = c("svm", "xgboost", "random_forest", "logistic_regression"),
        mode = c("classification")
        ) %>% dplyr::mutate(
        model = syms(glue::glue("final_model_{model_name}_{mode}_{species}")),
        run_name = glue::glue("{model_name}_{species}")
        ) %>% 
        select(-mode)

}

add_pred_predicitons <- function(pred_models, pred_df) {
    species <- names(select(pred_models, -model))
    map(species, ~make_fhast_pred_predicitons(select(pred_models, all_of(.x))[[1]], pred_df))
}

make_pred_prediciton_summary <- function(species, model, model_name, df) {
    predicitons <- make_fhast_pred_predicitons(model, df)
    tibble(pred_species = species, model_name = model_name, hab_rating = predicitons)
}

make_list_of_fhast_hab_ratings <- function(){
    expand_grid(
        species = c("smb", "sasq"),
        model_name = c("svm", "xgboost", "random_forest", "logistic_regression"),
        ) %>% dplyr::mutate(
        run = glue::glue("predator_hab_ratings_{model_name}_{species}"),
        ) %>% 
        select(run) %>%
        as.list() %>%
        syms()

}