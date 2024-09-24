
make_workflow_set <- function(model_name, training_data) {
    model_name <- as.character(model_name)
    grid_size = 30
    # set recipes
    recipe_list <- make_recipe_list(training_data)[[model_name]]
    model_list <- list(make_model_list(mode = "classification")[[model_name]])
    names(model_list) <- model_name
    # set folds
    folds <- vfold_cv(training_data, v = 5, repeats = 2, strata = "pres_abs")

    # basic workflow set
    workflow_set(
        preproc = recipe_list, 
        models = model_list, 
        # cross = TRUE
        ) %>%
        add_grids(model_name, select(training_data, -pres_abs), size = grid_size) %>%
        option_add(
            resamples = folds, 
            control = control_race(
                verbose_elim = TRUE, 
                save_pred = TRUE, 
                save_workflow = TRUE
                ),
            metrics = make_metric_sets(mode = "classification")
        )
}

add_grids <- function(wf_set, model_name, training_data, size) {
    if (model_name %in% c("regression", "fhast")) {
        return(wf_set)
    }
    ids <- wf_set$wflow_id
    wfs <- map(ids, ~ extract_workflow(wf_set, .x))
    grids <- map(wfs, ~set_grid(.x, training_data, size))
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

make_recipe_list <- function(training_data) {
    fhast <- fhast_recipe(training_data)
    basic <- basic_recipe(training_data)
    yj <- yj_recipe(training_data)
    poly <- poly_recipe(training_data)
    poly_yj <- poly_yj_recipe(training_data)
    interaction <- interaction_recipe(training_data)
    interaction_yj <- interaction_yj_recipe(training_data)
    interaction_poly <- interaction_poly_recipe(training_data)
    interaction_poly_yj <- interaction_poly_yj_recipe(training_data)

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
    
    fhast <- list(
        fhast = fhast
    )

    list(
        regression = long,
        glmnet = long,
        rf = short,
        xgb = short,
        svm = medium,
        nnet = super_short,        
        bag = super_short,
        fhast = fhast
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
        glmnet = glmnet_spec(mode = mode),
        fhast = regression_spec(mode = mode)
        )
}
