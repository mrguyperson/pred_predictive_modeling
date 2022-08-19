library(tidymodels)

splits <- initial_split(lmb)

training_data <- training(splits)
testing_data <- testing(splits)

cores <- parallel::detectCores()


spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  # set_engine("ranger", num.threads = cores) %>% 
  set_engine("ranger") %>% 
  set_mode("regression")

rec <- recipe(sum_lmb ~ ., data = training_data) %>% 
  update_role(regioncode, subregion, sampledate, segment_number, new_role = "ID") %>% 
  step_dummy(all_nominal_predictors())

data_prep <- prep(rec)
juiced <- juice(data_prep)

tune_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(spec)

set.seed(234)
folds <- vfold_cv(training_data)

set.seed(345)
doParallel::registerDoParallel()
tune_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = 20
)

tune_res

tune_res %>%
  collect_metrics() %>%
  filter(.metric == "rmse") %>%
  select(mean, min_n, mtry) %>%
  pivot_longer(min_n:mtry,
               values_to = "value",
               names_to = "parameter"
  ) %>%
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x") +
  labs(x = NULL, y = "rmse")

rf_grid <- grid_regular(
  mtry(range = c(5, 15)),
  min_n(range = c(10, 20)),
  levels = 10
)

set.seed(456)
doParallel::registerDoParallel()
regular_res <- tune_grid(
  tune_wf,
  resamples = folds,
  grid = rf_grid
)

regular_res

best <- select_best(regular_res, "rmse")
best

final_rf <- finalize_model(
  spec,
  best
)

final_rf


library(vip)

final_rf %>%
  set_engine("ranger", importance = "permutation") %>%
  fit(sum_lmb ~ .,
      data = juiced %>% select(-c(regioncode:starttime, segment_number))
  ) %>%
  vip(geom = "col")

final_wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(final_rf)

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
  xlab("observed number of fish") +
  ylab("predicted number of fish")

lmb2 <- lmb %>% select(-c(Sample.Area, Wind.Speed, Conductivity, Temp, Number.Snag, Tide.Stage))

splits <- initial_split(lmb2, strata = lmb)

training_data <- training(splits)
testing_data <- testing(splits)

spec <- parsnip::logistic_reg() %>%
  parsnip::set_engine("glm")

rec <- recipe(lmb ~ ., data = training_data) %>% 
  update_role(Date, new_role = "ID") %>% 
  themis::step_rose(lmb)

wf <-   workflows::workflow() %>%
  workflows::add_recipe(rec) %>%
  workflows::add_model(spec)

fit <- wf %>%
  tune::last_fit(splits)

fit %>% collect_metrics()

