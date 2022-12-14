set.seed(123)
splits <- initial_split(lmb_fitting, strata = seconds_per_transect)

training_data <- training(splits)
testing_data <- testing(splits)

set.seed(234)
folds <- vfold_cv(training_data, strata = sum_lmb)

#folds <- bootstraps(training_data, strata = sum_lmb, times = 100)

cores <- parallel::detectCores()
rec <- recipe(sum_lmb ~ ., data = training_data) %>%
  # update_role(regioncode, subregion, sampledate, segment_number, new_role = "ID") %>% 
  #step_poly(all_numeric_predictors(), degree = 3) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_corr(all_numeric_predictors(), threshold = 0.9)



svm_spec <-
  svm_rbf(cost = tune(), rbf_sigma = tune(), margin = tune()) %>%
  set_mode("regression") %>%
  set_engine("kernlab")

wf <-
  workflow() %>%
  add_recipe(rec)

doParallel::registerDoParallel()

# svm_results <-
#   wf %>%
#   add_model(svm_spec) %>%
#   fit_resamples(
#     resamples = folds,
#     control = control_resamples(save_pred = TRUE, verbose = TRUE)
#   )

grid <- grid_latin_hypercube(
  cost(),
  rbf_sigma(),
  svm_margin(),
  size = 100
)
set.seed(123)
svm_results <- tune_grid(
  wf %>%
  add_model(svm_spec),
  resamples = folds,
  grid = grid)

svm_results %>% 
  collect_metrics()

best <- svm_results %>% select_best("rmse")

final_svm <- finalize_model(
  svm_spec,
  best
)


final_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_svm)


final_res <- final_wf %>%
  last_fit(splits)

final_res %>%
  collect_metrics()

final_res %>% 
  collect_predictions() %>% 
  ggplot(aes(x = sum_lmb, y = .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, color = "dodgerblue") + 
  theme_bw() +
  # xlim(0,25) +
  # ylim(0,25) +
  xlab("observed number of fish") +
  ylab("predicted number of fish")

final_res %>% 
  collect_predictions() %>% 
  mutate(residual = sum_lmb - .pred) %>% 
  ggplot(aes(x = sum_lmb, y = residual)) +
  # geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, color = "dodgerblue") + 
  theme_bw() +
  # xlim(0,25) +
  # ylim(0,25) +
  xlab("observed number of fish") +
  ylab("residual of prediction")

# svm_fit %>%
#   conf_mat_resampled()
# 
# svm_fit %>% 
#   collect_metrics()

# svm_fit$.workflow[[1]] %>% 
#   predict(test, type = "prob") %>% 
#   bind_cols(test) %>% 
#   roc_auc(truth = bass, estimate = .pred_absent)

augment(svm_results) %>% roc_curve(bass, .pred_absent) %>% autoplot()


# poisson -----------------------------------------------------------------
library(poissonreg)

poisson_spec <- poisson_reg() %>% 
  set_engine("zeroinfl")

vars <- lmb_fitting %>% names()
dep <- "sum_lmb"
ind <- vars[vars != dep]

formula <- formula(paste(dep, " ~ ", paste(ind, collapse = " + ")))

wf <- 
  workflow() %>% 
  add_variables(outcomes = dep, predictors = ind) %>% 
  add_model(poisson_spec, formula = formula)

doParallel::registerDoParallel()

poisson_results <-
  wf %>%
  fit_resamples(
    resamples = folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

poisson_results %>% collect_metrics()

poisson_fit <-
  wf %>%
  last_fit(splits)

poisson_fit$.workflow[[1]] %>%
  tidy() %>%
  filter(p.value  <= 0.05)

poisson_fit %>%
  collect_predictions() %>% 
  ggplot(aes(x = sum_lmb, y = .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, color = "dodgerblue") + 
  theme_bw() +
  # xlim(0,15) +
  # ylim(0,15) +
  xlab("observed number of fish") +
  ylab("predicted number of fish")

