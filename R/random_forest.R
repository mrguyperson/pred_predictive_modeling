
# set.seed(123)
# splits <- initial_split(lmb, strata = seconds_per_transect)

# training_data <- training(splits)
# testing_data <- testing(splits)

# cores <- parallel::detectCores()




rf_recipe <- function(training_data, y_var, col_to_drop, id_cols) {
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
      # step_integer(all_nominal_predictors(), -pres_abs) %>%
      step_zv(all_numeric_predictors(), -pres_abs) %>% 
      step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
      themis::step_upsample(pres_abs, 
        over_ratio = tune()
      ) %>%
      step_rm(all_of(col_to_drop))
  } else {
      recipe(rec_formula, data = training_data) %>% 
        update_role(all_of(id_cols), all_of(col_to_drop), new_role = "ID") %>% 
        step_impute_mean(all_numeric_predictors(), -pres_abs) %>%
        # step_poly(all_numeric_predictors(), -pres_abs, degree = 2) %>%
        step_rename_at(all_numeric_predictors(), -pres_abs, fn = ~ glue::glue("numeric_{.}")) %>%
        step_rename_at(all_nominal_predictors(), -pres_abs, fn = ~ glue::glue("nominal_{.}")) %>%
        step_dummy(all_nominal_predictors(), -pres_abs) %>%
        # step_interact(terms = ~ starts_with("numeric"):starts_with("nominal")) %>%
        step_zv(all_numeric_predictors(), -pres_abs) %>% 
        step_corr(all_numeric_predictors(), -pres_abs, threshold = 0.9) %>%
        themis::step_upsample(
          pres_abs, 
          over_ratio = tune()
          # neighbors = tune("neighbors_over_ratio")
          )
  }

}

# data_prep <- prep(rec)
# juiced <- juice(data_prep)


# set.seed(234)
# folds <- vfold_cv(training_data)

# set_rf_grid <- function(training_data, size = 100) {
#     grid_latin_hypercube(
#       min_n(),
#       dials::finalize(mtry(), training_data),
#       trees(),
#       size = size
#   )
# }



# show_best(tune_res, "rmse")

# plot_results <- function(results, metric) {
#   results %>%
#     collect_metrics() %>%
#     filter(.metric == metric) %>%
#     select(mean,mtry:min_n) %>%
#     pivot_longer(min_n:mtry,
#                 values_to = "value",
#                 names_to = "parameter"
#     ) %>%
#     ggplot(aes(value, mean, color = parameter)) +
#     geom_point(show.legend = FALSE) +
#     facet_wrap(~parameter, scales = "free_x") +
#     labs(x = NULL, y = metric)
# }
# rf_grid <- grid_regular(
#   mtry(range = c(5, 15)),
#   min_n(range = c(5, 15)),
#   levels = 20
# )

# set.seed(456)
# doParallel::registerDoParallel()
# regular_res <- tune_grid(
#   tune_wf,
#   resamples = folds,
#   grid = rf_grid
# )

# regular_res

# best <- select_best(regular_res, "rmse")
# best

# final_rf <- finalize_model(
#   spec,
#   best
# )

# final_rf


# library(vip)

# final_rf %>%
#   set_engine("ranger", importance = "permutation") %>%
#   fit(sum_lmb ~ .,
#       data = juiced %>% select(-c(regioncode:starttime, segment_number))
#   ) %>%
#   vip(geom = "col")

# final_wf <- workflow() %>%
#   add_recipe(rec) %>%
#   add_model(final_rf)

# final_res <- final_wf %>%
#   last_fit(splits)

# final_res %>%
#   collect_metrics()

# final_rf_regression_fit %>% 
#   collect_predictions() %>% 
#   ggplot(aes(x = sum_lmb, y = .pred)) +
#   geom_abline(lty = 2, color = "black", size = 1.5) +
#   geom_point(alpha = 0.5, color = "dodgerblue") + 
#   theme_bw() +
#   xlab("observed number of fish") +
#   ylab("predicted number of fish") +
#   xlim(0,25) +
#   ylim(0,15)

# lmb2 <- lmb %>% select(-c(Sample.Area, Wind.Speed, Conductivity, Temp, Number.Snag, Tide.Stage))
# 
# splits <- initial_split(lmb2, strata = lmb)
# 
# training_data <- training(splits)
# testing_data <- testing(splits)
# 
# spec <- parsnip::logistic_reg() %>%
#   parsnip::set_engine("glm")
# 
# rec <- recipe(lmb ~ ., data = training_data) %>% 
#   update_role(Date, new_role = "ID") %>% 
#   themis::step_rose(lmb)
# 
# wf <-   workflows::workflow() %>%
#   workflows::add_recipe(rec) %>%
#   workflows::add_model(spec)
# 
# fit <- wf %>%
#   tune::last_fit(splits)
# 
# fit %>% collect_metrics()
# 

# library(recipes)
# library(modeldata)
# data(hpc_data)
# hpc_data0 <- hpc_data %>%
#   select(-protocol, -day)
# orig <- count(hpc_data0, class, name = "orig")
# orig
# up_rec <- recipe(class ~ ., data = hpc_data0) %>%
#   # Bring the minority levels up to about 1000 each
#   # 1000/2211 is approx 0.4523
#   step_smote(class, over_ratio = 0.4523) %>%
#   prep()
# training <- up_rec %>%
#   bake(new_data = NULL) %>%
#   count(class, name = "training")
# training
# # Since `skip` defaults to TRUE, baking the step has no effect
# baked <- up_rec %>%
#   bake(new_data = hpc_data0) %>%
#   count(class, name = "baked")
# baked
# # Note that if the original data contained more rows than the
# # target n (= ratio * majority_n), the data are left alone:
# orig %>%
#   left_join(training, by = "class") %>%
#   left_join(baked, by = "class")
# library(ggplot2)
# ggplot(circle_example, aes(x, y, color = class)) +
#   geom_point() +
#   labs(title = "Without SMOTE")
# recipe(class ~ x + y, data = circle_example) %>%
#   step_smote(class) %>%
#   prep() %>%
#   bake(new_data = NULL) %>%
#   ggplot(aes(x, y, color = class)) +
#   geom_point() +
#   labs(title = "With SMOTE")
