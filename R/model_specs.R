nnet_spec <- function(engine = "nnet", mode = "classification") {
  mlp(
    hidden_units = tune(), 
    penalty = tune(), 
    epochs = tune()
    ) %>%
    set_mode(mode) %>%
    set_engine(engine)
}

bag_spec <- function(engine = "nnet", mode = "classification") {
  bag_mlp(
    hidden_units = tune(),
    penalty = tune(), 
    epochs = tune()
    ) %>%
    set_mode(mode) %>%
    set_engine(engine)
}

rf_spec <- function(engine = "ranger", mode = "regression") {
  rand_forest(mtry = tune(), min_n = tune(), trees = tune()) %>% 
    set_engine(engine, importance = "permutation") %>% 
    set_mode(mode)
}

svm_spec <- function(engine = "kernlab", mode = "regression"){
  svm_rbf(cost = tune(), 
            rbf_sigma = tune()
            ) %>%
    set_mode(mode) %>%
    set_engine(engine)
}

xgb_spec <- function(engine = "xgboost", mode = "regression"){

  if (mode == "regression") {
    obj <- "count:poisson"
  } else {
    obj <- "binary:logistic" 
  }

  boost_tree(
    trees = tune(),
    tree_depth = tune(),
    min_n = tune(),
    loss_reduction = tune(),
    sample_size = tune(),
    mtry = tune(),
    learn_rate = tune()
) %>%
  set_engine(engine, objective = obj, lambda = tune(), alpha = tune()) %>%
  set_mode(mode)
}

regression_spec <- function(engine = "lm", mode = "regression"){
    if(mode == "regression") {
        linear_reg() %>%
            set_mode(mode) %>%
            set_engine(engine)
    } else {
        logistic_reg() %>%
            set_mode(mode) %>%
            set_engine(engine = "glm")
    }
}

glmnet_spec <- function(engine = "glmnet", mode = "regression"){

    if(mode == "regression") {
        linear_reg(penalty = tune(), mixture = tune()) %>%
            set_mode(mode) %>%
            set_engine(engine)
    } else {
        logistic_reg(penalty = tune(), mixture = tune()) %>%
            set_mode(mode) %>%
            set_engine(engine)
    }
}
