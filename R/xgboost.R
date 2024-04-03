# library(tidymodels)
# library(tidyverse)
# library(here)

# doParallel::registerDoParallel()


# load and prep data ------------------------------------------------------

#source(file = here("R", "data_cleaning.R")) # clean and load the data

# add labels to predictor variables
# predictors <- lmb_fitting %>%
#   select(-sum_lmb) %>%
#   rename_if(is.factor, .funs = ~ paste(., "_Dummy", sep = "")) %>%
#   rename_if(is.numeric, .funs = ~ paste(., "_Numeric", sep = ""))

# xgb_data <- lmb_fitting %>% 
#   select(sum_lmb) %>% 
#   bind_cols(predictors)


# test-train split --------------------------------------------------------

# set.seed(123)
# xgb_split <- initial_split(xgb_data, strata = sum_lmb)
# xgb_train <- training(xgb_split)
# xgb_test <- testing(xgb_split)

# recipe to allow upsampling

# xgb_recipe <- recipe(sum_lmb ~ ., data = xgb_train) %>%
#   # step_poly(all_numeric_predictors()) %>%
#   step_dummy(all_nominal_predictors()) %>% 
#   step_zv(all_predictors()) %>% 
#   step_corr(all_numeric_predictors(), threshold = 0.9)


# apply recipe

# xgb_juiced <- xgb_rec %>% prep() %>% juice()

# set up folds; use bootstraps because of small sample size

#xgb_folds <- bootstraps(xgb_juiced, strata = sasq)
# set.seed(234)
# xgb_folds <- vfold_cv(xgb_juiced)

# build model -------------------------------------------------------------

# model tuning


xgb_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
  rec_formula <- formula(glue::glue("{y_var} ~ ."))
  if(y_var == "count") {
    recipe(rec_formula, data = training_data) %>% 
      update_role(all_of(id_cols), new_role = "ID") %>% 
      step_impute_mean(all_numeric_predictors(), -pres_abs) %>%
      # step_poly(all_numeric_predictors(), -pres_abs, degree = 2) %>%
      step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
      step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
      step_dummy(all_nominal_predictors(), -pres_abs) %>% 
      # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
      step_zv(all_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(pres_abs, over_ratio = tune(), skip = TRUE) %>%
      step_rm(all_of(col_to_drop))
  } else {
      recipe(rec_formula, data = training_data) %>% 
        update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
        step_impute_mean(all_numeric_predictors()) %>%
        # step_poly(all_numeric_predictors(), -pres_abs, degree = 2) %>%
        step_rename_at(all_numeric_predictors(), -pres_abs, -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
        step_rename_at(all_nominal_predictors(), -pres_abs, -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
        step_dummy(all_nominal_predictors(), -pres_abs) %>% 
        # step_interact(terms = ~ starts_with("numeric"):starts_with("numeric")) %>%
        # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
        step_zv(all_predictors(), -pres_abs) %>% 
        step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
        themis::step_upsample(
            pres_abs, 
            over_ratio = tune()
            # neighbors = tune("neighbors_over_ratio")
            )
      }

}
# tuning grid
set_xgb_grid <- function(training_data, size = 100){
  grid_latin_hypercube(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    sample_size = sample_prop(),
    dials::finalize(mtry(), training_data),
    # learn_rate(),
    size = size
  )
}
# create a workflow
# xgb_wf <- workflow() %>% 
#   add_formula(sum_lmb ~ .) %>%
#   add_model(xgb_spec)




# fit the model
# xgb_res <- tune_grid(
#   xgb_wf,
#   resamples = xgb_folds,
#   grid = xgb_grid,
#   control = control_grid(save_pred = TRUE, verbose = TRUE)
# )

# evaluate model performance ----------------------------------------------

# # check metrics
# xgb_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   select(mean, mtry:sample_size) %>%
#   pivot_longer(mtry:sample_size,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "rmse")

# # best models
# show_best(xgb_res, "rmse")

# best_auc <- select_best(xgb_res, "rmse")
# best_auc

# # create workflow with best model
# final_xgb <- finalize_workflow(
#   xgb_wf,
#   best_auc
# )

# final_xgb


# # check for variable importance
# library(vip)

# final_fit_xgb <- final_xgb %>%
#   fit(data = xgb_juiced) 

# final_fit_xgb  %>%
#   extract_fit_parsnip() %>%
#   vip(geom = "col")

# # final_xgb %>% last_fit(xgb_split) %>% collect_metrics()
# # final_xgb %>% last_fit(xgb_split) %>% conf_mat_resampled()

# # xgb_model <- final_xgb %>% last_fit(xgb_split)

# # xgb_model <- xgb_model$.workflow[[1]]


# # check performance of best model -----------------------------------------

# final_xgb_split <- final_xgb %>% 
#   last_fit(xgb_split)

# final_xgb_split %>% 
#   collect_predictions() %>% 
#   mutate(
#     residual = sum_lmb - .pred) %>% 
#   ggplot() +
#   geom_point(aes(x = sum_lmb, y = residual), color = "dodgerblue") +
#   # geom_abline(lty = 2, color = "red", size = 1.5)
#   theme_bw()

# final_xgb_split %>% 
#   collect_predictions() %>% 
#   ggplot() +
#   geom_point(aes(x = sum_lmb, y = .pred), color = "dodgerblue") +
#   geom_abline(lty = 2, color = "red", size = 1.5) +
#   theme_bw() +
#   xlim(0,25) +
#   ylim(0,15)


# final_xgb_split %>% 
#   collect_metrics()

# # final_fit_xgb %>% 
# #   predict(xgb_test) %>% 
# #   bind_cols(xgb_test) %>% 
# #   group_by(sasq) %>% 
# #   count(.pred_class)
# # 
# # final_fit_xgb %>% 
# #   predict(xgb_test, type = "prob") %>% 
# #   bind_cols(xgb_test) %>% 
# #   roc_curve(sasq, .pred_absent) %>% 
# #   autoplot()
# # 
# # xgb_test_output <-   final_fit_xgb %>% 
# #   predict(xgb_test, type = "prob") %>% 
# #   bind_cols(xgb_test)
# # 
# # roc_auc(xgb_test_output, truth = sasq, .pred_absent)
