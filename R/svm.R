
splits <- initial_split(lmb_fitting)

training_data <- training(splits)
testing_data <- testing(splits)

set.seed(234)
folds <- vfold_cv(training_data)

cores <- parallel::detectCores()

set.seed(345)
rec <- recipe(sum_lmb ~ ., data = training_data) %>%
  # update_role(regioncode, subregion, sampledate, segment_number, new_role = "ID") %>% 
  #step_poly(all_numeric_predictors(), degree = 3) %>% 
  step_dummy(all_nominal_predictors()) %>%
  step_center(all_predictors())



svm_spec <-
  svm_rbf() %>%
  set_mode("regression") %>%
  set_engine("kernlab")


wf <-
  workflow() %>%
  add_recipe(rec)

doParallel::registerDoParallel()

svm_results <-
  wf %>%
  add_model(svm_spec) %>%
  fit_resamples(
    resamples = folds,
    control = control_resamples(save_pred = TRUE, verbose = TRUE)
  )

svm_results %>%
  collect_metrics()

svm_results %>% 
  conf_mat_resampled()

svm_results  %>%
  collect_predictions() %>%
  group_by(bass) %>%
  count(.pred_class)

svm_fit <-
  wf %>%
  add_model(svm_spec) %>%
  last_fit(split)


svm_fit %>% 
  collect_predictions() %>% 
  ggplot(aes(x = sum_lmb, y = .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, color = "dodgerblue") + 
  theme_bw() +
  xlim(0,25) +
  ylim(0,25) +
  xlab("observed number of fish") +
  ylab("predicted number of fish")

svm_fit %>%
  conf_mat_resampled()

svm_fit %>% 
  collect_metrics()

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
  last_fit(split)

poisson_fit$.workflow[[1]] %>%
  tidy() %>%
  filter(p.value  <= 0.05)

augment(poisson_results) %>%
  ggplot(aes(x = sum_lmb, y = .pred)) +
  geom_abline(lty = 2, color = "black", size = 1.5) +
  geom_point(alpha = 0.5, color = "dodgerblue") + 
  theme_bw() +
  # xlim(0,15) +
  # ylim(0,15) +
  xlab("observed number of fish") +
  ylab("predicted number of fish")

