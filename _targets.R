# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
library(here)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c(
    "tidyverse",
    "tidymodels",
    "broom",
    "bonsai",
    "themis",
    "data.table"
  ) # packages that your targets need to run
  # format = "qs", # Optionally set the default storage format. qs is fast.
  #
  # For distributed computing in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller with 2 workers which will run as local R processes:
  #
  #   controller = crew::crew_controller_local(workers = 2)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package. The following
  # example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     workers = 50,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. "module load R/4.3.0".
  #     # Check with your system administrator if you are unsure.
  #     script_lines = "module load R"
  #   )
  #
  # Set other options as needed.
)

# tar_make_clustermq() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
options(clustermq.scheduler = "multicore")
set.seed(123)
# tar_make_future() is an older (pre-{crew}) way to do distributed computing
# in {targets}, and its configuration for your machine is below.
# Install packages {{future}}, {{future.callr}}, and {{future.batchtools}} to allow use_targets() to configure tar_make_future() options.

# Run the R scripts in the R/ folder with your custom functions:
# tar_source()
source(here("R", "data_cleaning.R"))
source(here("R", "random_forest.R"))
source(here("R", "regression.R"))
source(here("R", "svm.R"))

list(
  tar_target(
    name = data_folder,
    command = here::here("data", "csv", "iep")
  ),
  tar_target(
    name = files,
    command = list.files(data_folder)
  ),
  tar_target(
    name = file_list,
    command = map(files, get_csv_from_path, folder = data_folder)
  ),
  tar_target(
    name = tax_cols,
    command = file_list[[2]] %>% names()
  ),
  tar_target(
    name = names_to_drop,
    command = tax_cols[tax_cols != "CommonName"]
  ),
  tar_target(
    name = full_data,
    command = get_full_data_set(file_list, names_to_drop)
  ),
  tar_target(
    name = lmb_data,
    command = get_lmb_data(full_data)
  ),
  tar_target(
    name = id_cols,
    command = c("regioncode", "starttime", "subregion", "sampledate", "segment_number", "seconds_per_transect")
  ),

  ##### set up for models
  tar_target(
    name = split,
    command = rsample::initial_split(lmb_data, strata = seconds_per_transect)
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
    command = vfold_cv(training_data)
  ),
  ##### linear regression
  tar_target(
    name = lin_reg_spec,
    command = linear_reg_spec()
  ),
  tar_target(
    name = lin_reg_recipe,
    command = reg_recipe(training_data, "sum_lmb", "count_lmb", id_cols)
  ),
  tar_target(
    name = linear_reg_wf,
    command = tune_wf(lin_reg_recipe, lin_reg_spec)
  ),
  tar_target(
    name = final_linear_reg,
    command = reg_fit(linear_reg_wf, split)
  ),
  ##### random forest ---- regression
  tar_target(
    name = rf_spec_regression,
    command = rf_spec()
  ),
  tar_target(
    name = rf_recipe_regression,
    command = rf_rec(training_data, "sum_lmb", "count_lmb", id_cols)
  ),
  tar_target(
    name = rf_regression_workflow,
    command = tune_wf(rf_recipe_regression, rf_spec_regression)
  ),
  tar_target(
    name = rf_grid,
    command = set_rf_grid(training_data)
  ),
  tar_target(
    name = tuned_rf_regression_results,
    command = tune_model_grid(rf_regression_workflow, folds, rf_grid)
  ),
  tar_target(
    name = best_rf_regession,
    command = tune::select_best(tuned_rf_regression_results, "rmse")
  ),
  tar_target(
    name = final_rf_regression,
    command = tune::finalize_model(rf_spec_regression, best_rf_regession)
  ),
  tar_target(
    name = final_rf_regression_workflow,
    command = tune_wf(rf_recipe_regression, final_rf_regression)
  ),
  tar_target(
    name = final_rf_regression_fit,
    command = tune::last_fit(final_rf_regression_workflow, split)
  ),
  ##### svm -------- regression
  tar_target(
    name = svm_spec_regression,
    command = svm_spec_rbf(mode = "regression")
  ),
  tar_target(
    name = svm_recipe_regression,
    command = svm_recipe(training_data, y_var = "sum_lmb", col_to_drop = "count_lmb", id_cols)
  ),
  tar_target(
    name = svm_regression_wf,
    command = tune_wf(svm_recipe_regression, svm_spec_regression)
  ),
  tar_target(
    name = svm_grid,
    command = set_svm_grid()
  ),
  tar_target(
    name = tuned_svm_regression_results,
    command = tune_model_grid(svm_regression_wf, folds, svm_grid)
  ),
  tar_target(
    name = best_svm_regession,
    command = tune::select_best(tuned_svm_regression_results, "rmse")
  ),
  tar_target(
    name = final_svm_regression,
    command = tune::finalize_model(svm_spec_regression, best_svm_regession)
  ),
  tar_target(
    name = final_svm_regression_workflow,
    command = tune_wf(svm_recipe_regression, final_svm_regression)
  ),
  tar_target(
    name = final_svm_regression_fit,
    command = tune::last_fit(final_svm_regression_workflow, split)
  ),
  ##### logistic regression
  tar_target(
    name = log_reg_spec,
    command = logistic_reg_spec()
  ),
  tar_target(
    name = log_reg_recipe,
    command = reg_recipe(training_data, "count_lmb", "sum_lmb", id_cols) %>% themis::step_rose()
  ),
  tar_target(
    name = logistic_reg_wf,
    command = tune_wf(log_reg_recipe, log_reg_spec)
  ),
  tar_target(
    name = final_logistic_reg,
    command = reg_fit(logistic_reg_wf, split)
  ),
  ##### random forest ---- classification
  tar_target(
    name = rf_spec_classification,
    command = rf_spec(mode = "classification")
  ),
  tar_target(
    name = rf_recipe_classification,
    command = rf_rec(training_data, "count_lmb", "sum_lmb", id_cols) %>% themis::step_rose()
  ),
  tar_target(
    name = rf_classification_workflow,
    command = tune_wf(rf_recipe_classification, rf_spec_classification)
  ),
  tar_target(
    name = tuned_rf_classification_results,
    command = tune_model_grid(rf_classification_workflow, folds, rf_grid)
  ),
  tar_target(
    name = best_rf_classification,
    command = tune::select_best(tuned_rf_classification_results, "roc_auc")
  ),
  tar_target(
    name = final_rf_classification,
    command = tune::finalize_model(rf_spec_classification, best_rf_classification)
  ),
  tar_target(
    name = final_rf_classification_workflow,
    command = tune_wf(rf_recipe_classification, final_rf_classification)
  ),
  tar_target(
    name = final_rf_classification_fit,
    command = tune::last_fit(final_rf_classification_workflow, split)
  ),
  ##### svm -------- classification
  tar_target(
    name = svm_spec_classification,
    command = svm_spec_rbf(mode = "classification")
  ),
  tar_target(
    name = svm_recipe_classification,
    command = svm_recipe(training_data, y_var = "count_lmb", col_to_drop = "sum_lmb", id_cols) %>% themis::step_rose()
  ),
  tar_target(
    name = svm_classification_wf,
    command = tune_wf(svm_recipe_classification, svm_spec_classification)
  ),
  tar_target(
    name = tuned_svm_classification_results,
    command = tune_model_grid(svm_classification_wf, folds, svm_grid)
  ),
  tar_target(
    name = best_svm_classification,
    command = tune::select_best(tuned_svm_classification_results, "roc_auc")
  ),
  tar_target(
    name = final_svm_classification,
    command = tune::finalize_model(svm_spec_classification, best_svm_classification)
  ),
  tar_target(
    name = final_svm_classification_workflow,
    command = tune_wf(svm_recipe_classification, final_svm_classification)
  ),
  tar_target(
    name = final_svm_classification_fit,
    command = tune::last_fit(final_svm_classification_workflow, split)
  )
)
