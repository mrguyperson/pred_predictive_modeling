# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes)
library(here)
library(tibble)
library(tidyr)
library(rlang)
library(crew)
library(dplyr)

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "data.table",
    "tidymodels",
    "broom",
    "bonsai",
    "themis",
    "data.table",
    "here",
    "igraph",
    "lutz",
    "smoothr",
    "exactextractr",
    "terra",
    "viridis",
    "patchwork",
    "leaflet",
    "nlrx",
    "shadow",
    "maptools",
    "sf",
    "R.utils",
    "lightgbm",
    "finetune",
    "baguette",
    "stacks",
    "vip",
    "ggpubr"
  ), # packages that your targets need to run
  format = "qs", # Optionally set the default storage format. qs is fast.
  controller = crew::crew_controller_local(workers = 8),
  seed = 1
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()

reps <- 5

datasets <- tibble(
  data = syms(c("delta_data_lmb", "fhast_data_smb", "fhast_data_sasq")),
  stratum = as.character(c("pres_abs", "pres_abs", "pres_abs")),
  names = c("delta_lmb", "smb", "sasq")
)

data_splits_mapped <- tar_map(
  unlist = FALSE,
  values = datasets,
  names = "names",
  tar_target(
    name = split,
    command = initial_split(data, strata = stratum)
  ),
  tar_target(
    name = training_data,
    command = training(split)
  ),
  tar_target(
    name = testing_data,
    command = testing(split)
  ),
  tar_target(
    name = folds,
    # command = bootstraps(training_data, times = 25, strata = stratum)
    command = vfold_cv(training_data, v = 5, repeats = 2, strata = stratum)
  )
)

workflowsets <- expand_grid(
  tibble(
    species = c("delta_lmb", "smb", "sasq"),
    id_cols = syms(c("delta_id_cols", "fhast_id_cols", "fhast_id_cols")),
    fhast = c(FALSE, TRUE, TRUE),
    training_data = syms(glue::glue("training_data_{species}")),
  ),
  tibble(
      mode = c(
        "classification"
        # "regression"
        ),
      metric = c(
        "roc_auc"
        # "rmse"
        ),
      y_var = c(
        "pres_abs"
        # "count"
        ),
      col_to_drop = c(
        "count"
        # "pres_abs"
        )
    )
  ) %>% 
  filter(!(fhast == TRUE & mode == "regression")) %>%
  mutate(run_name = glue::glue("{mode}_{species}"))

workflowsets_mapped <- tar_map(
  values = workflowsets,
  names = "run_name",
  unlist = FALSE,
  # reps = reps,
  tar_target(
    name = complete_workflow,
    command = make_workflow_set(training_data, y_var, col_to_drop, id_cols)
  ),
  tar_target(
    name = workflowsets_results,
    command = get_workflow_set_results(complete_workflow),
    deployment = "main"
  )
)

model_selections <- expand_grid(
  tibble(
    species = c("smb", "sasq", "delta_lmb"),
    fhast = c(TRUE, TRUE, FALSE),
    training_data = syms(glue::glue("training_data_{species}")),
    id_cols = syms(c("fhast_id_cols", "fhast_id_cols", "delta_id_cols"))
  ),
  tibble(
    mode = c("classification"),
    metric = c("roc_auc"),
    y_var = c("pres_abs"),
    col_to_drop = c("count")
  ),
  model_name = c(
      "svm", 
      "xgb", 
      "rf", 
      "regression",
      "glmnet",
      "nnet",
      "bag"
      )
) %>%
  filter(!(fhast == TRUE & mode == "regression")) %>%
  mutate(
    # workflowset_results = syms(glue::glue("workflowsets_results_{mode}_{species}")),
    split = syms(glue::glue("split_{species}")),
    run_name = glue::glue("{model_name}_{mode}_{species}")
  )

model_selections_mapped <- tar_map(
  values = model_selections,
  names = "run_name",
  # reps = reps,
  unlist = FALSE,
  # tar_target(
  #   name = subset,
  #   command = subset_wf_results(workflowset_results, model_name),
  #   deployment = "main"
  # ),
  # tar_target(
  #   name = spec,
  #   command = model_spec(mode = mode)
  # ),
    tar_target(
    name = complete_workflow,
    command = make_workflow_set(model_name, training_data, y_var, col_to_drop, id_cols)
  ),
  tar_target(
    name = workflowsets_results,
    command = get_workflow_set_results(complete_workflow),
    deployment = "main"
  ),
  tar_target(
    name = best_model,
    command = workflowsets_results %>% rank_results(rank_metric = metric, select_best = TRUE),
    deployment = "main"
  ),
  tar_target(
    name = best_model_name,
    command = best_model$wflow_id[[1]]
  ),
  tar_target(
    name = tuned_results,
    command = extract_workflow_set_result(workflowsets_results, best_model_name),
    deployment = "main"
  ),
  tar_target(
    name = best_fit,
    command = fit_best(tuned_results, metric = metric),
    deployment = "main"
  ),
  tar_target(
    name = metrics,
    command = make_metric_sets(mode)
  ),
  tar_target(
    name = final_fit,
    command = last_fit(object = best_fit, split = split, metrics = metrics),
    deployment = "main"
  ),
  tar_target(
    name = final_model,
    command = select_models(final_fit),
    deployment = "main"
  ),
  tar_target(
    name = performance_metrics,
    command = mutate(unnest(final_fit, .metrics), name = run_name, wflow_id = best_model_name)
  ),
  tar_target(
    name = var_imp,
    command = get_var_importance(final_model, training_data)
  )
  # tar_target(
  #   name = vip_plot,
  #   command = plot_model_vip(final_model, var_imp),
  #   format = "file"
  # )
)

train_and_test_table <- expand_grid(
  tibble(
    data = syms(c("delta_data_lmb", "fhast_data_smb", "fhast_data_sasq")),
    species = c("lmb", "smb", "sasq"),
    is_fhast = c(FALSE, TRUE, TRUE)
  ),
  model_name = c(
      "svm", 
      "xgb", 
      "rf", 
      "regression",
      "glmnet",
      "nnet",
      "bag",
      "fhast"
    )
)
# simple_models <- expand_grid(
#   tibble(
#     model = c("linear_regression", "logistic_regression"),
#     engine = c("lm", "glm"),
#     recipe_function = syms(c("reg_recipe", "reg_recipe")),
#     spec_function = syms(c("linear_reg_spec", "logistic_reg_spec")),
#   ),
#   tibble(
#     mode = c("classification", "regression"),
#     metric = c("roc_auc", "rmse"),
#     y_var = c("pres_abs", "count"),
#     col_to_drop = c("count", "pres_abs")
#   ),
#   tibble(
#     dataset = syms(c("delta_data_lmb", "fhast_data_smb", "fhast_data_sasq")),
#     species = c("lmb", "smb", "sasq"),
#     id_cols = syms(c("delta_id_cols", "fhast_id_cols", "fhast_id_cols")),
#     folds = syms(c("folds_lmb", "folds_smb", "folds_sasq")),
#     split = syms(c("split_lmb", "split_smb", "split_sasq")),
#     training_data = syms(c("training_data_lmb", "training_data_smb", "training_data_sasq"))
#     ),
# ) %>% dplyr::mutate(
#   run_name = glue::glue("{model}_{mode}_{species}")
# ) %>% dplyr::filter(
#   !(grepl("fhast", dataset) & mode == "regression"),
#   !(grepl("linear", model) & mode == "classification"),
#   !(grepl("logistic", model) & mode == "regression")
# )

fhast_predator_models <- expand_grid(
  species = c("smb", "sasq"),
  model_name = c(
    "svm", 
    "xgb", 
    "rf", 
    "regression",
    "glmnet",
    "nnet",
    "bag"
    # "lightgbm"
    ),
  mode = c("classification")
) %>% dplyr::mutate(
  model = syms(glue::glue("final_model_{model_name}_{mode}_{species}")),
  run_name = glue::glue("{model_name}_{species}")
) %>% 
select(-mode)

stack_model_data <- expand_grid(
  species = c("smb", "sasq"),
  model_name = c(
    "svm", 
    "xgb", 
    "rf", 
    "regression",
    "glmnet",
    "nnet",
    "bag"
    # "lightgbm"
    ),
  mode = c("classification")
  ) %>% dplyr::mutate(
    model = syms(glue::glue("tuned_results_{model_name}_{mode}_{species}"))
  ) %>% 
  select(-mode) %>%
  # filter(
    # model_name != "bag_nnet", 
    # model_name != "logistic_regression") %>%
  pivot_wider(names_from = model_name, values_from = model)

# advanced_models <- expand_grid(
#   tibble(
#     model = c(
#       "random_forest", 
#       "svm", 
#       "xgboost",
#       "glmnet",
#       "nnet",
#       "bag_nnet"
#       # "lightgbm"
#       ),
#     engine = c(
#       "ranger", 
#       "kernlab", 
#       "xgboost",
#       "glmnet",
#       "nnet",
#       "nnet"
#       # "lightgbm"

#       ),
#     recipe_function = syms(c(
#       "rf_recipe", 
#       "svm_recipe", 
#       "xgb_recipe",
#       "reg_recipe",
#       "nnet_recipe",
#       "nnet_recipe"
#       # "rf_recipe"
#       )),
#     spec_function = syms(c(
#       "rf_spec", 
#       "svm_spec_rbf", 
#       "xgb_spec",
#       "glmnet_spec",
#       "nnet_spec",
#       "bag_nnet_spec"
#       # "lightgbm_spec"
#       )),
#     size = c(
#       50,
#       25,
#       100,
#       30,
#       50,
#       50
#     ),
#     # grid_function = syms(c(
#     #   "set_rf_grid", 
#     #   "set_svm_grid", 
#     #   "set_xgb_grid",
#     #   "set_glmnet_grid",
#     #   "set_nnet_grid",
#     #   "set_nnet_grid",

#     #   )),
#     tune_function = syms(c(
#       "tune_model_grid_race",
#       "tune_model_grid_race",
#       "tune_model_grid_race",
#       "tune_model_grid_race",
#       "tune_model_grid_race",
#       # "tune_model_grid_race",
#       "tune_model_grid_race"
#     ))
#   ),
#   tibble(
#     mode = c("classification", "regression"),
#     metric = c("roc_auc", "rmse"),
#     y_var = c("pres_abs", "count"),
#     col_to_drop = c("count", "pres_abs")
#   ),
#   tibble(
#     dataset = syms(c("delta_data_lmb", "fhast_data_smb", "fhast_data_sasq")),
#     species = c("lmb", "smb", "sasq"),
#     id_cols = syms(c("delta_id_cols", "fhast_id_cols", "fhast_id_cols")),
#     split = syms(c("split_lmb", "split_smb", "split_sasq")),
#     training_data = syms(c("training_data_lmb", "training_data_smb", "training_data_sasq")),
#     folds = syms(c("folds_lmb", "folds_smb", "folds_sasq"))
#     ),
# ) %>% dplyr::mutate(
#   run_name = glue::glue("{model}_{mode}_{species}")
# ) %>% dplyr::filter(
#   !(grepl("fhast", dataset) & mode == "regression"),
#   !(grepl("linear", model) & mode == "classification"),
#   !(grepl("logistic", model) & mode == "regression"),
#   !(grepl("glmnet", model) & mode == "regression")
# )

# simple_models_mapped <- tar_map(
#   values = simple_models,
#   names = "run_name",
#   tar_target(
#     name = recipes,
#     command = recipe_function(training_data, y_var, col_to_drop, id_cols)
#   ),
#   tar_target(
#     name = specs,
#     command = spec_function(engine, mode)
#   ),
#   tar_target(
#     name = metrics,
#     command = make_metric_sets(mode)
#   ),
#   tar_target(
#     name = workflows,
#     command = tune_wf(recipes, specs)
#   ),
#   tar_target(
#     name = tuned_results,
#     command = simple_model_resamples(workflows, folds, metrics),
#     deployment = "main"
#   ),
#   tar_target(
#     name = final_fits,
#     command = tune::last_fit(workflows, split, metrics = metrics)
#   ),
#   tar_target(
#     name = performance_metrics,
#     command = mutate(unnest(final_fits, .metrics), name = run_name)
#   ),
#   tar_target(
#     name = final_model,
#     command = select_models(final_fits)
#   )
# )

# advanced_models_mapped <- tar_map(
#   values = advanced_models,
#   names = "run_name",
#   tar_target(
#     name = recipes,
#     command = recipe_function(training_data, y_var, col_to_drop, id_cols)
#   ),
#   tar_target(
#     name = specs,
#     command = spec_function(engine, mode)
#   ),
#   tar_target(
#     name = workflows,
#     command = tune_wf(recipes, specs)
#   ),
#   tar_target(
#     name = mtry_data_frame,
#     command = make_mtry_data_frame(training_data, id_cols, col_to_drop, y_var)
#   ),
#   tar_target(
#     name = grids,
#     command = set_grid(workflows, mtry_data_frame, size = size)
#   ),
#     tar_target(
#     name = metrics,
#     command = make_metric_sets(mode)
#   ),
#   tar_target(
#     name = tuned_results,
#     command = tune_function(workflows, folds, grids, metrics),
#     deployment = "main"
#   ),
#   tar_target(
#     name = best_models,
#     command = tune::select_best(tuned_results, metric)
#   ),
#   tar_target(
#     name = final_workflows,
#     command = tune::finalize_workflow(workflows, best_models)
#   ),
#   # tar_target(
#   #   name = final_workflows,
#   #   command = tune_wf(recipes, finalized_models)
#   # ),
#   tar_target(
#     name = final_fits,
#     command = tune::last_fit(final_workflows, split, metrics = metrics)
#   ),
#   tar_target(
#     name = performance_metrics,
#     command = mutate(unnest(final_fits, .metrics), name = run_name)
#   ),
#   tar_target(
#     name = final_model,
#     command = select_models(final_fits)
#   ),
#   tar_target(
#     name = vip_plot,
#     command = plot_model_vip(final_model),
#     format = "file"
#   )
# )

stacked_model_mapped <- tar_map(
  unlist = FALSE,
  values = stack_model_data,
  names = "species",
  tar_target(
    name = stacked_model,
    command = build_stack_model(logistic_regression, xgboost, random_forest, svm, glmnet, nnet, bag_nnet),
    deployment = "main"
  )
)

fhast_model_predictions <- tar_map(
    unlist = FALSE,
    values = fhast_predator_models,
    names = "run_name",
    tar_target(
      predator_hab_ratings,
      make_pred_prediction_summary(species, model, model_name, df_for_pred_models) %>% mutate(substrate = unlist(substrate_option)),
      pattern = map(df_for_pred_models, substrate_option),
      iteration = "list"
    ),
    tar_target(
      predator_hab_ratings_combined,
      command = bind_rows(predator_hab_ratings)
    )
  )

list(
  # initial data processing ----------
  tar_target(
    name = delta_raw_data_file,
    command = here("data", "csv", "iep", "USFWS_EFISH_data.csv"),
    format = "file"
  ),
  tar_target(
    name = delta_taxonomy_file,
    command = here("data", "csv", "iep", "USFWS_Fish_Taxonomy.csv"),
    format = "file"
  ),
  tar_target(
    name = fhast_2013_file,
    command = here("data", "csv", "fishbio", "2013 Master Data.csv"),
    format = "file"
  ),
  tar_target(
    name = fhast_2014_file,
    command = here("data", "csv", "fishbio", "2014 Master Data.csv"),
    format = "file"
  ),
  tar_target(
    name = delta_data_lmb,
    command = clean_delta_data(delta_raw_data_file, delta_taxonomy_file) %>% select(-all_of(c(delta_id_cols, "count")))
  ),
  tar_target(
    name = fhast_data,
    command = clean_fhast_data(fhast_2013_file, fhast_2014_file) 
  ),
  tar_target(
    name = fhast_data_sasq,
    command = dplyr::filter(fhast_data, species == "sasq") %>% select(-all_of(c(fhast_id_cols, "count")))
  ),
  tar_target(
    name = fhast_data_smb,
    command = dplyr::filter(fhast_data, species == "bass")%>% select(-all_of(c(fhast_id_cols, "count")))
  ),
  tar_target(
    name = res_shore_fish_paths,
    command = get_res_fish_shore_paths(),
    format = "file"
  ),
  tar_target(
    name = res_shore_data_lmb,
    command = get_res_shore_data(res_shore_fish_paths)
  ),
  # data_splits_mapped,
  tar_target(
    name = delta_id_cols,
    command = c("regioncode", "starttime", "subregion", "sampledate", "segment_number", "seconds_per_transect")
  ),
  tar_target(
    name = fhast_id_cols,
    command = c("species")
  ),
  tar_target(
    name = res_shore_id_cols,
    command = c("date", "station_number")
  ),
  tar_map_rep(
    name = modeling_results,
    command = train_and_test(data, model_name, habitat_variable, is_fhast),
    values = train_and_test_table,
    batches = 1,
    reps = 5,
    columns = all_of(c("species", "model_name")),
    names = all_of(c("species", "model_name")),
    deployment = "main"
  ),
  # model_selections_mapped
  # # FHAST Stuff ----------
  tar_target(
      name = fhast_base_folder,
      command = here("data")
    ),
    # set input file path
    tar_target(
      name = input_file_path,
      here(fhast_base_folder, "input_file.txt"),
      format = "file"
    ),
    tar_target(
      name = input_data,
      command = load_text_file(input_file_path)
    ),
    tar_target(
      name = fish_population_path,
      here(fhast_base_folder, input_data["fish population", ]),
      format = "file"),
    tar_target(
      name = daily_path, 
      here(fhast_base_folder, input_data["daily conditions", ]),
      format = "file"
    ),
  # get the name of the input file
    tar_target(
      name = fish_parameters_path,
      here(fhast_base_folder, input_data["fish parameters", ]),
      format = "file"
    ),
  # get the paths for the files
    tar_target(
      name = grid_center_line_path,
      here(fhast_base_folder, input_data["grid centerline", ]),
      format = "file"
    ),
    tar_target(
      name = grid_top_marker_path,
      here(fhast_base_folder, input_data["grid top point", ]),
      format = "file"
    ),
    tar_target(
      name = cover_path,
      here(fhast_base_folder, input_data["cover", ]),
      format = "file"
    ),
    tar_target(
      name = canopy_path,
      here(fhast_base_folder, input_data["canopy", ]),
      format = "file"
    ),
    tar_target(
      name = tree_growth_path,
      here(fhast_base_folder, input_data["tree growth", ]),
      format = "file"
    ),
    tar_target(
      name = hab_path,
      here(fhast_base_folder, input_data["habitat parameters", ]),
      format = "file"
    ),
    tar_target(
      name = interaction_path,
      here(fhast_base_folder, input_data["interaction parameters", ]),
      format = "file"
    ),
    tar_target(
      name = predator_path,
      here(fhast_base_folder, input_data["predator parameters", ]),
      format = "file"
    ),
    tar_target(
      name = aoi_path,
      get_aoi_path(input_data, fhast_base_folder)
    ),
    # Location of rasters
    tar_target(
      name = raster_folder,
      here(fhast_base_folder, input_data["raster folder", ]),
      format = "file"
    ),
    tar_target(
      name = grid_center_line,
      st_zm(st_read(grid_center_line_path, quiet = TRUE))
    ),
    tar_target(
      base_crs,
      command = st_crs(cover_shape)
    ),
    tar_target(
      grid_top_marker,
      command = make_grid_top_marker(grid_top_marker_path, base_crs)
    ),
    tar_target(
      name = canopy_shape,
      st_read(canopy_path, quiet = TRUE)
    ),
    tar_target(
      name = tree_growth_parms_in,
      read.csv(file = tree_growth_path, sep = ",", header = TRUE)
    ),
    tar_target(
      name = cover_shape,
      st_read(cover_path, quiet = TRUE),
    ),
    tar_target(
      name = daily_inputs,
      load_text_file(daily_path)
    ),
    tar_target(
      name = fish_daily_inputs,
       read.csv(file = fish_population_path, sep = ",", header = TRUE) %>%
          mutate(date = mdy(date))
    ),
    tar_target(
      name = fish_parm_temp,
      read_csv(file = fish_parameters_path,
                           col_types = cols(.default = "d", species = "c"),
                           progress = FALSE)
    ),
    tar_target(
      name = pred_parm_temp,
      read_csv(file = predator_path,
                           col_types = cols(.default = "d", species = "c"),
                           progress = FALSE) %>%
                rename(term = species) %>%
                mutate(term = str_remove(term, "pred_glm_"))
    ),
    tar_target(
      name = hab_parm_temp,
      load_text_file(hab_path)
    ),
    tar_target(
      name = int_parm_temp,
      load_text_file(interaction_path)
    ),
    ###### skipped compareCRS step and check centerline step
    tar_target(
      name = species_used,
      unique(fish_daily_inputs$species)
    ),
    tar_target(
      name = fish_parm,
      get_fish_params(fish_parm_temp, species_used)
    ),
    tar_target(
      name = tree_growth_parms,
      get_tree_growth_params(tree_growth_parms_in)
    ),
    tar_target(
      name = turbidity_params,
      convert_logistic_parameters(
        int_parm_temp["turbidity cover 10", ],
        int_parm_temp["turbidity cover 90", ]
      )
    ),
    tar_target(
      name = dis_to_cover_params,
      convert_logistic_parameters(
        int_parm_temp["distance to cover 10", ],
        int_parm_temp["distance to cover 90", ]
      )
    ),
    tar_target(
      name = habitat_parm,
      get_habitat_params(
        hab_parm_temp, 
        int_parm_temp,
        dis_to_cover_params,
        turbidity_params
      )
    ),
    tar_target(
      name = pred_model_list,
      make_predation_models(pred_parm_temp)
    ),
    tar_target(
      name = pred_model_params,
      pred_model_list[[1]]
    ),
    tar_target(
      name = pred_length_data,
      pred_model_list[[2]]
    ),
    tar_target(
      name = pred_temp_params,
      pred_temperature_model_to_logistic(pred_model_list[[3]])
    ),
    tar_target(
      name = gape_params,
      pred_model_list[[4]]
    ),

    tar_target(
      name = synth_cover_data,
      make_synth_cover_data(habitat_parm)
    ),
    tar_target(
      name = pct_cover_model,
      model_cover_data(synth_cover_data)
    ),
    tar_target(
      name = synth_cover_benefit_data,
      make_synth_cover_benefit_data(habitat_parm)
    ),
    tar_target(
      name = dis_to_cover_model,
      model_cover_benefit(synth_cover_benefit_data)
    ),

    # Make daily fish input ---------- 
    tar_target(
      name = daily_input_data,
      command = load_daily_conditions(daily_inputs)
    ),
    tar_target(
      name = daily_w_photo_period,
      command = calc_photo_period(grid_top_marker, daily_input_data)
    ),
    tar_target(
      name = fish_schedule,
      load_fish_timeseries(fish_daily_inputs, habitat_parm)
    ),
    tar_target(
      name = juvenile_run,
      fifelse(nrow(filter(fish_schedule, lifestage == "juvenile")) > 0,
            TRUE,
            FALSE)
    ),
    tar_target(
      name = adult_run,
      fifelse(nrow(filter(fish_schedule, lifestage == "adult")) > 0,
              TRUE,
              FALSE)

    ),
    ##### Make the grid
    tar_target(
      name = grid_file,
      full_grid_processing(grid_center_line, 
                                grid_top_marker, 
                                habitat_parm)
    ),
  ##### Make shade file
  # removed error catching
  tar_target(
    name = times_list,
    command = paste0("2010-", seq(1,12,1), "-15 12:00:00")

  ),
  tar_target(
    name = shade_calcs,
    command = full_shade_shape_calculations(grid_file, 
                                 grid_center_line, 
                                  canopy_shape, 
                                  tree_growth_parms, 
                                  habitat_parm,
                                  juvenile_run)
  ),
  tar_target(
    name = shade_location,
    command = get_shade_location(shade_calcs)
  ),
  tar_target(
    name = converted_time_list,
    command = convert_time(times_list, shade_location),
    pattern = map(times_list)
  ),
  tar_target(
    name = shade_monthly_shapes,
    command = make_shade_shape(shade_calcs, converted_time_list, shade_location),
    pattern = map(converted_time_list)

  ),
  tar_target(
    name = summarized_shade_monthly_shapes,
    command = summarize_shade_calcs(shade_monthly_shapes),
    pattern = map(shade_monthly_shapes)
  ),
  tar_target(
    name = shade_months,
    command = seq(1, 12, 1)
  ),
  tar_target(
    name = renamed_monthly_shade_shapes,
    command = rename_shade_cals(shade_months, summarized_shade_monthly_shapes),
    pattern = map(shade_months, summarized_shade_monthly_shapes)
  ),
  tar_target(
    shade_file,
    st_as_sf(renamed_monthly_shade_shapes),
    pattern = map(renamed_monthly_shade_shapes),
    iteration = "list"
  ),
    ##### Sample raster
    tar_target(
      name = raster_file,
      full_grid_sampling(raster_folder, grid_file, fish_parm)

    ),
  ##### Sample cover
  tar_target(
    name = cover_names,
    command = c("veg", "wood", "fine", "gravel", "cobble", "rock")
  ),
  tar_target(
    name = shape_dfs,
    command = purrr::map(cover_names, ~dplyr::select(cover_shape, matches(.x))) 
  ),
  tar_target(
    name = cover_variables,
    command = c(cover_names, as.list(paste0("shade_", seq(1,12,1)))),
    iteration = "list"
  ),
  tar_target(
    name = combined_shapes,
    command = c(shape_dfs, shade_file),
    iteration = "list"
  ),
  tar_target(
    name = shape_sampled_with_grid,
    command = sample_shape_with_grid(
      grid_file,
      combined_shapes,
      cover_variables
    ),
    pattern = map(combined_shapes, cover_variables),
    iteration = "list"
  ),
  tar_target(
    reduced_cover_files,
    combine_all_sampled_cover_files(shape_sampled_with_grid)
  ),
  tar_target(
    name = cover_file_with_habitat,
    command = add_habitat_info_to_cover_file(reduced_cover_files, habitat_parm)
  ),
  tar_target(
    name = shape_file,
    command = add_aoi_to_shape(aoi_path, grid_file, cover_file_with_habitat)
  ),
  ##### Habitat summary stuff
  tar_target(
    name = raster_joined_with_shape,
    command = join_raster_and_shape(raster_file, shape_file)
  ),
  tar_target(
    name = spread_flows_inputs,
    command = c("mean.D", "mean.V", "wetd."),
    iteration = "list"
  ),
  tar_target(
    name = spread_flows_outputs,
    command = c("depth", "velocity", "wetted_fraction"),
    iteration = "list"
  ),
  tar_target(
    name = spread_flows_list,
    command = spread_flows(raster_joined_with_shape, spread_flows_inputs, spread_flows_outputs),
    pattern = map(spread_flows_inputs, spread_flows_outputs),
    iteration = "list"
  ),
  tar_target(
    name = reduced_spread_flows,
    command = purrr::reduce(spread_flows_list, left_join, by = c("lat_dist", "distance", "flow"))
  ),
  tar_target(
    habitat_flows_all,
    command = make_final_habitat_df(raster_joined_with_shape, reduced_spread_flows)
  ),
  tar_target(
    name = flows,
    command = as.numeric(unique(habitat_flows_all$flow))
  ),
  tar_target(
    name = max_flow,
    command = flows[min(which(flows > max(daily_input_data$flow_cms)))]
  ),
  tar_target(
    name = habitat_open,
    calc_open_habitat(habitat_flows_all, max_flow)
  ),
  tar_target(
    name = analyzed_cells,
    command = nrow(habitat_open)/nrow(filter(habitat_flows_all, flow == min(flow)))
  ),
  tar_target(
    name = habitat_flows,
    command = calc_habitat_flow(habitat_open, habitat_flows_all)
  ),
  tar_target(
    fish_combos,
    command = fish_daily_inputs %>% select(species, lifestage) %>% rename("life_stage" = "lifestage", "prey_species" = "species") 
  ),
  tar_target(
    name = migration_area,
    command = calc_migration_area(raster_file)
  ),
  tar_target(
    name = habitat_fixed,
    command = select_fixed_habitat(shape_file, habitat_open)
  ),
  tar_target(
    pred_parm, 
    make_pred_parm(predator_path)
  ),
  tar_target(
    name = daily_data_sets,
    command = get_daily_data(daily_w_photo_period,
                            habitat_data = habitat_flows,
                            fixed_data = shape_file,
                            flows_list = flows,
                            fish_schedule = fish_schedule,
                            migration_area = migration_area,
                            adult_run = adult_run,
                            sig_figs = 10),
    pattern = map(daily_w_photo_period),
    iteration = "list"
  ),
  tar_target(
    name = v_and_d_daily_data_sets,
    command = subset_v_and_d(daily_data_sets),
    patter = map(daily_data_sets),
    iteration = "list"
  ),
  tar_target(
    name = habitat_variable,
    command = select_variable_habitat(v_and_d_daily_data_sets, habitat_fixed, habitat_parm)
  ),
  # tar_target(
  #   name = col_names_fhast_pred_models,
  #   command = get_fhast_col_names(fhast_data)
  # ),
  # tar_target(
  #   name = substrate_option,
  #   command = list(1, 0)
  # ),
  # tar_target(
  #   name = df_for_pred_models,
  #   command = make_df_for_pred_predictions(df = habitat_variable, col_names = col_names_fhast_pred_models),
  #   # pattern = map(substrate_option),
  #   # iteration = "list"
  # ),
  tar_target(
    name = var_imp_summary,
    command = summarize_var_imp(modeling_results, permutations = 30),
    deployment = "main"
  ),
  tar_target(
    name = palette,
    command = c("#999999", "#D55E00",
                         "#F0E442", "#56B4E9", "#E69F00",
                         "#0072B2", "#009E73", "#CC79A7"
                         )

  ),
  tar_target(
    name = var_imp_plot,
    command = plot_var_imp(var_imp_summary, palette),
    format = "file"
  ),
  tar_target(
    name = roc_plot,
    command = plot_auroc(modeling_results),
    format = "file",
    deployment = "main"
  ),
  tar_quarto(
    name = manuscript,
    path = here::here("manuscript.qmd")
  )
  # fhast_model_predictions,
  # tar_combine(
  #   name = pred_prediction_summary,
  #   fhast_model_predictions[["predator_hab_ratings_combined"]],
  #   command = dplyr::bind_rows(!!!.x) %>%
  #     expand_grid(fish_combos)
  # ),
  # tar_target(
  #   name = pred_prediction_summary_grouped,
  #   command = pred_prediction_summary %>%
  #     group_by(prey_species, model_name, life_stage, substrate) %>%
  #     targets::tar_group(),
  #   iteration = "group"
  # ),
  # tar_target(
  #   name = pred_prediction_differences,
  #   command = make_pred_model_difference_df(pred_prediction_summary_grouped),
  #   iteration = "group"
  # ),
  # tar_target(
  #   name = summed_pred_predction_differences,
  #   command = make_summed_pred_model_df(pred_prediction_differences)
  # ),
  # tar_target(
  #   name = summed_pred_predictions,
  #   command = make_summed_pred_model_df(pred_prediction_summary_grouped)
  # ),
  # tar_target(
  #   name = summed_pred_predictions_w_hab,
  #   command = join_pred_and_hab(summed_pred_predictions, habitat_variable)
  # ),
  # tar_target(
  #   name = data_summary,
  #   command = make_data_summary(
  #     fish_parm, habitat_variable, pred_parm, pred_prediction_summary_grouped, habitat_parm, juvenile_run, pct_cover_model, dis_to_cover_model
  #   ),
  #   pattern = map(pred_prediction_summary_grouped),
  #   iteration = "list"
  # ),
  # tar_target(
  #   name = map_data,
  #   command = make_map_data(data_summary, grid_file, "average_map"),
  #   pattern = map(data_summary),
  #   iteration = "list"
  # ),
  # tar_target(
  #   name = map_data_full,
  #   command = make_map_data(data_summary, grid_file, "average_map_full"),
  #   pattern = map(data_summary),
  #   iteration = "list"
  # ),
  # tar_target(
  #   name = mean_map,
  #   command = make_mean_map(map_data, habitat_parm),
  #   pattern = map(map_data),
  #   iteration = "list"
  # ),
  # tar_target(
  #   name = mean_map_full,
  #   command = make_mean_map(map_data_full, habitat_parm),
  #   pattern = map(map_data_full),
  #   iteration = "list"
  # ),
  # tar_target(
  #   name = pred_maps,
  #   command = make_predator_maps(mean_map, hab_rating, "Predator\nHabitat Rating", "aoi"),
  #   pattern = map(mean_map),
  #   iteration = "list",
  #   format = "file"
  # ),
  # tar_target(
  #   name = mortality_maps,
  #   command = make_predator_maps(map_data, pred_mort_risk, "Predator\nMort. Risk", "aoi"),
  #   pattern = map(map_data),
  #   iteration = "list",
  #   format = "file"
  # ),
  #   tar_target(
  #   name = pred_maps_full,
  #   command = make_predator_maps(mean_map_full, hab_rating, "Predator\nHabitat Rating", "full"),
  #   pattern = map(mean_map_full),
  #   iteration = "list",
  #   format = "file"
  # ),
  # tar_target(
  #   name = mortality_maps_full,
  #   command = make_predator_maps(map_data_full, pred_mort_risk, "Predator\nMort. Risk", "full"),
  #   pattern = map(map_data_full),
  #   iteration = "list",
  #   format = "file"
  # ),
  # tar_combine(
  #   name = performance_summary,
  #   model_selections_mapped[["performance_metrics"]],
  #   command = dplyr::bind_rows(!!!.x)
  # ),
  # tar_target(
  #   name = pred_histograms,
  #   command = make_pred_model_histograms(pred_prediction_summary_grouped),
  #   format = "file"
  # ),
  # tar_target(
  #   name = multimodel_pred_hab_map,
  #   command = make_multimodel_river_plot(
  #     df = summed_pred_predictions_w_hab, 
  #     fill_option = hab_rating, 
  #     scale_name = "Predator Habitat Rating"),
  #   format = "file"
  # )
)


