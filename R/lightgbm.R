lightgbm_spec <- function(engine = "lightgbm", mode = "regression"){
  boost_tree(
    trees = 1000,
    tree_depth = tune(),
    min_n = tune(),
    # loss_reduction = tune(),
    mtry = tune(),
    # learn_rate = tune()
) %>%
  set_engine(engine) %>%
  set_mode(mode)
}

set_lightgbm_grid <- function(training_data, size = 100){
  grid_latin_hypercube(
    trees(),
    tree_depth(),
    min_n(),
    loss_reduction(),
    dials::finalize(mtry(), training_data),
    # learn_rate(),
    size = size
  )
}


# library(tidyverse)
# library(tidymodels)
# # library(doParallel)
# library(bonsai) # experimental package to run lightgbm in tidymodels
# # all_cores <- parallel::detectCores(logical = FALSE) - 4
# # registerDoParallel(cores = all_cores) 
# # set.seed(123)

# light_split <- initial_split(
#   lmb_fitting, 
#   prop = 0.8, 
#   strata = sum_lmb
# )

# light_training <- training(light_split)
# light_testing <- testing(light_split)

# preprocessing_recipe <- 
#   recipes::recipe(sum_lmb ~ ., data = light_training) %>%
#   # combine low frequency factor levels
#   # recipes::step_other(all_nominal(), threshold = 0.01) %>%
#   # # remove no variance predictors which provide no predictive information
#   # recipes::step_nzv(all_nominal())
#   step_zv(all_predictors()) %>% 
#   step_corr(all_numeric_predictors(), threshold = 0.9)

# light_juiced <-   preprocessing_recipe %>%  
#     prep() %>% 
#     juice()

# light_cv_folds <- 
#   light_juiced %>% 
#   rsample::vfold_cv(v = 10)

# lightgbm_model <- boost_tree(
#   trees = tune(),
#   tree_depth = tune(),
#   min_n = tune(),
#   loss_reduction = tune(),
#   # sample_size = tune(),
#   mtry = tune(),
#   # learn_rate = tune()
# ) %>%
#   set_engine("lightgbm") %>%
#   set_mode("regression")

# # lightgbm_params <-
# #   dials::parameters(
# #     # The parameters have sane defaults, but if you have some knowledge 
# #     # of the process you can set upper and lower limits to these parameters.
# #       tree_depth(),
# #       min_n(),
# #       loss_reduction(),
# #       sample_size = sample_prop(),
# #       finalize(mtry(), light_training),
# #       learn_rate()
# # )
# lgbm_grid <-
#       dials::grid_latin_hypercube(
#         trees(),
#         tree_depth(),
#         min_n(),
#         loss_reduction(),
#         # sample_size = sample_prop(),
#         finalize(mtry(), light_training),
#         # learn_rate(),
#         size = 50 # set this to a higher number to get better results
#         # I don't want to run this all night, so I set it to 30
#       )

# lgbm_wf <-
#   workflows::workflow() %>%
#   # add_formula(sum_lmb ~ .) %>% 
#   add_model(lightgbm_model) %>% 
#   add_recipe(preprocessing_recipe)
  
# # 
# # extract_parameter_set_dials(lgbm_wf)
# # 
# # fit <- fit_resamples(lgbm_wf, resamples = light_cv_folds)

# lgbm_res <- tune::tune_grid(
#   object = lgbm_wf,
#   resamples = light_cv_folds,
#   grid = lgbm_grid,
#   metrics = yardstick::metric_set(rmse, rsq, mae),
#   control = control_grid(save_pred = TRUE, verbose = TRUE) # set this to TRUE to see
#   # in what step of the process you are. But that doesn't look that well in
#   # a blog.
# )

# lgbm_res %>%
#   tune::show_best(metric = "rmse",n = 5)


# # check metrics
# lgbm_res %>%
#   collect_metrics() %>%
#   filter(.metric == "rmse") %>%
#   select(mean, mtry:loss_reduction) %>%
#   pivot_longer(mtry:loss_reduction,
#                values_to = "value",
#                names_to = "parameter"
#   ) %>%
#   ggplot(aes(value, mean, color = parameter)) +
#   geom_point(alpha = 0.8, show.legend = FALSE) +
#   facet_wrap(~parameter, scales = "free_x") +
#   labs(x = NULL, y = "rmse")

# # best models
# show_best(lgbm_res, "rmse")

# best_light <- select_best(lgbm_res, "rmse")
# best_light

# # create workflow with best model
# final_light <- finalize_workflow(
#   lgbm_wf,
#   best_light
# )

# final_light


# # check for variable importance
# # library(vip)
# # 
# # final_fit_light <- final_light %>%
# #   fit(data = light_juiced) 
# # 
# # final_fit_light  %>%
# #   extract_fit_parsnip() %>%
# #   vip(geom = "col")


# # check performance of best model -----------------------------------------

# final_light_split <- final_light %>% 
#   last_fit(light_split)

# final_light_split %>% 
#   collect_predictions() %>% 
#   mutate(
#     residual = sum_lmb - .pred) %>% 
#   ggplot() +
#   geom_point(aes(x = sum_lmb, y = residual), color = "dodgerblue") +
#   # geom_abline(lty = 2, color = "red", size = 1.5)
#   theme_bw()

# final_light_split %>% 
#   collect_predictions() %>% 
#   ggplot() +
#   geom_point(aes(x = sum_lmb, y = .pred), color = "dodgerblue") +
#   geom_abline(lty = 2, color = "red", size = 1.5) +
#   theme_bw() +
#   xlim(0,25) +
#   ylim(0,15)


# final_light_split %>% 
#   collect_metrics()