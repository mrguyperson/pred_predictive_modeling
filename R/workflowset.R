
make_workflow_set <- function(model_name, training_data, y_var, col_to_drop, id_cols) {
    model_name <- as.character(model_name)
    # set recipes

    recipe_list <- make_recipe_list(training_data, y_var, col_to_drop, id_cols)[[model_name]]

    # set mode
    mode <- fifelse(y_var == "count", "regression", "classification")

    model_list <- list(make_model_list(mode)[[model_name]])

    names(model_list) <- model_name
    # set folds
    folds <- vfold_cv(training_data, v = 4, repeats = 3, strata = pres_abs)
    mtry_data <- make_mtry_data_frame(training_data, y_var, col_to_drop, id_cols)

    # basic workflow set
    workflow_set(
        preproc = recipe_list, 
        models = model_list, 
        # cross = TRUE
        ) %>%
        add_grids(mtry_data, size = 40) %>%
        option_add(
            resamples = folds, 
            control = control_race(
                verbose_elim = TRUE, 
                save_pred = TRUE, 
                save_workflow = TRUE
                ),
            metrics = make_metric_sets(mode)
        )
}

add_grids <- function(wf_set, mtry_data, size) {
    ids <- wf_set$wflow_id
    wfs <- map(ids, ~ extract_workflow(wf_set, .x))
    grids <- map(wfs, ~set_grid(.x, mtry_data, size))
    names(grids) <- ids

    for( name in names(grids)) {
        wf_set <- wf_set %>% option_add(grid = grids[[name]], id = name)
    }
    wf_set
}

get_workflow_set_results <- function(workflow_set) {
    num_cores <- parallel::detectCores(logical = FALSE)
    cl <- parallel::makeCluster(num_cores)
    doParallel::registerDoParallel(cl)
    on.exit(parallel::stopCluster(cl))

    workflow_set %>%
         workflow_map(fn = "tune_race_anova")
}

subset_wf_results <- function(workflow_set, model_type) {
    workflow_set %>%
        filter(grepl(model_type, wflow_id))
}

make_recipe_list <- function(training_data, y_var, col_to_drop, id_cols) {
    fhast <- fhast_recipe(training_data, y_var, col_to_drop, id_cols)
    basic <- basic_recipe(training_data, y_var, col_to_drop, id_cols)
    yj <- yj_recipe(training_data, y_var, col_to_drop, id_cols)
    poly <- poly_recipe(training_data, y_var, col_to_drop, id_cols)
    poly_yj <- poly_yj_recipe(training_data, y_var, col_to_drop, id_cols)
    interaction <- interaction_recipe(training_data, y_var, col_to_drop, id_cols)
    interaction_yj <- interaction_yj_recipe(training_data, y_var, col_to_drop, id_cols)
    interaction_poly <- interaction_poly_recipe(training_data, y_var, col_to_drop, id_cols)
    interaction_poly_yj <- interaction_poly_yj_recipe(training_data, y_var, col_to_drop, id_cols)

    long <- list(
            fhast = fhast,
            basic = basic,
            yj = yj, 
            poly = poly,
            poly_yj = poly_yj,
            interaction = interaction,
            interaction_yj = interaction_yj,
            interaction_poly = interaction_poly,
            interaction_poly_yj = interaction_poly_yj
            )

    medium <- list(
            yj = yj, 
            poly_yj = poly_yj,
            interaction_yj = interaction_yj,
            interaction_poly_yj = interaction_poly_yj
        )
    
    short <- list(
            fhast = fhast,
            basic = basic,
            poly = poly
        )
    
    super_short <- list(
            yj = yj, 
            poly_yj = poly_yj
    )

    list(
        regression = long,
        glmnet = long,
        rf = short,
        xgb = short,
        svm = medium,
        nnet = super_short,        
        bag = super_short
        )
}

make_model_list <- function(mode) {
    list(
        xgb = xgb_spec(mode = mode), 
        rf = rf_spec(mode = mode),
        svm = svm_spec(mode = mode),
        nnet = nnet_spec(mode = mode),
        bag = bag_spec(mode = mode),
        regression = regression_spec(mode = mode),
        glmnet = glmnet_spec(mode = mode)
        )
}

# tar_load(res_shore_id_cols)
# data <- get_res_shore_data(get_res_fish_shore_paths())
# training_data <- initial_split(data, strata = pres_abs) %>% training()

# mtry_data <- make_mtry_data_frame(training_data, "pres_abs", "count", res_shore_id_cols)

# wf <- make_workflow_set("rf", training_data, "pres_abs", "count", res_shore_id_cols)


# rec <-  interaction_recipe(training_data, "count", "pres_abs", res_shore_id_cols)

# spec <- regression_spec(mode = "regression")

# wf <- workflow() %>%
#     add_recipe(rec) %>%
#     add_model(spec)
# unregister_dopar <- function() {
#   env <- foreach:::.foreachGlobals
#   rm(list=ls(name=env), pos=env)
# }

# unregister_dopar()
# num_cores <- parallel::detectCores(logical = FALSE)
# cl <- parallel::makeCluster(num_cores)
# doParallel::registerDoParallel(cl)
# results <- finetune::tune_race_anova(
#     wf,
#     resamples = vfold_cv(training_data, v = 5, repeats = 2),
#     grid = set_grid(wf, training_data, size= 20),
#     control = finetune::control_race(
#                 verbose_elim = TRUE, 
#                 save_pred = TRUE, 
#                 save_workflow = TRUE),
#     metrics = metric_set(rmse)
# )

# finetune::tune_race_anova(
#     wf,
#     resamples = vfold_cv(training_data, v = 5, repeats = 2),
#     grid = set_grid(wf, training_data, size= 20),
#     control = finetune::control_race(
#                 verbose_elim = TRUE, 
#                 save_pred = TRUE, 
#                 save_workflow = TRUE),
#     metrics = metric_set(rmse)
# ) 


# unregister_dopar()
# results %>% collect_metrics()

# tar_load(training_data_res_shore_lmb)
# tar_load(res_shore_id_cols)
# res<-make_workflow_set("rf", training_data_res_shore_lmb, "pres_abs", "count", res_shore_id_cols) %>% get_workflow_set_results()
# res %>% rank_results(rank_metric = "roc_auc", select_best = TRUE)
# # tar_load(delta_data_lmb)
# tar_load(fhast_data_smb)
# tar_load(fhast_data_sasq)
# data %>%
#     group_by(pres_abs) %>%
#     summarize(across(where(is.numeric), mean))

# fhast_data_smb %>%
#     group_by(pres_abs) %>%
#     count(shade)
