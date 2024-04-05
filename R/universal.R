make_mtry_data_frame <- function(training_data, id_cols, col_to_drop, y_var){
    training_data %>%
        dplyr::select(-all_of(id_cols), -all_of(col_to_drop), -all_of(y_var))
}

simple_model_resamples <- function(wf, folds, metrics) {
    num_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    fit_resamples(
        wf,
        resamples = folds,
        metrics = metrics,
        control = stacks::control_stack_resamples()
    )
}

tune_model_grid_race <- function(wf, folds, grid, metric_sets){
    num_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    finetune::tune_race_anova(
        wf,
        resamples = folds,
        grid = grid,
        control = finetune::control_race(verbose_elim = TRUE, save_pred = TRUE, save_workflow = TRUE),
        metrics = metric_sets
    )
}

tune_model_grid_stack <- function(wf, folds, grid, metric_sets){
    num_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))
    tune_grid(
        wf,
        resamples = folds,
        grid = grid,
        control = stacks::control_stack_grid(),
        metrics = metric_sets
    )
}


tune_wf <- function(rec, spec){
  workflow() %>%
    add_model(spec) %>%
    add_recipe(rec)
}

set_grid <- function(wf, training_data, size = 30) {
    # browser()
    params <- extract_parameter_set_dials(wf)
    param_names <- extract_parameter_set_dials(wf) %>% as_tibble() %>% .$name
    if ("mtry" %in% param_names) {
        params <- params %>% update(mtry = dials::finalize(mtry(), training_data))
    }
    grid_latin_hypercube(
        params,
        size = size
    )
}

split_with_seed <- function(dataset, stratum, seed) {
    set.seed(seed)
    rsample::initial_split(dataset, strata = all_of(stratum))
}

folds_with_seed <- function(training_data, stratum, seed) {
    set.seed(seed)
    vfold_cv(training_data, v = 5, repeats = 5, strata = all_of(stratum))
}

get_fhast_col_names <- function(fhast_data) {
    fhast_data %>%
        dplyr::select(-pres_abs) %>%
        names()
}

make_df_for_pred_predictions <- function(df, col_names, substrate_option) {
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

make_fhast_pred_predictions <- function(model, df) {
    # browser()

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

add_pred_predictions <- function(pred_models, pred_df) {
    species <- names(select(pred_models, -model))
    map(species, ~make_fhast_pred_predictions(select(pred_models, all_of(.x))[[1]], pred_df))
}

make_pred_prediction_summary <- function(species, model, model_name, df) {
    predicitons <- make_fhast_pred_predictions(model, df)
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

make_metric_sets <- function(mode) {
    if (mode == "regression") {
        yardstick::metric_set(rmse, rsq, ccc)
    } else {
        yardstick::metric_set(
            roc_auc, 
            mn_log_loss, 
            accuracy, 
            specificity, 
            sensitivity, 
            # kap, 
            # f_meas, 
            # bal_accuracy, 
            # ppv, 
            # pr_auc, 
            # mcc
            )
    }
}

plot_model_vip <- function(model) {
    model_name <- deparse(substitute(model)) %>% str_remove("best_models_")
    output_name <- here::here("output", glue::glue("vip_{model_name}.jpg"))

    if (grepl("glmnet", model_name) | grepl("bag", model_name)| grepl("svm", model_name)){
        plot <- ggplot()
        ggsave(filename = output_name, plot = plot, device = "jpg")
        return(output_name)
    }

    # if (grepl("random_forest", model_name)){
    #     plot <- finalize_model(spec, model) %>%
    #         set_engine("ranger", importance = "permutation") %>%
    #         last_fit(recipe, split) %>%
    #         vip( 
    #             geom = "col",
    #             aesthetics = list(
    #                 color = "black", 
    #                 fill = "white"
    #                 )
    #             ) +
    #         theme_classic(base_size = 15)
    #     ggsave(filename = output_name, plot = plot, device = "jpg")
    #     return(output_name)
    # }

    plot <- vip(model,
        method = "permute",
        geom = "col",
        aesthetics = list(
            color = "black", 
            fill = "white"
            )
        ) +
        theme_classic(base_size = 15)
    ggsave(filename = output_name, plot = plot, device = "jpg")
    output_name
}