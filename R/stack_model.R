build_stack_model <- function(logistic_regression, xgboost, random_forest, svm, glmnet, nnet, bag_nnet) {
    # cl <- parallel::makeCluster(16)
    # doParallel::registerDoParallel(cl)
    # on.exit(parallel::stopCluster(cl))
    stacks() %>%
        add_candidates(xgboost) %>%
        add_candidates(random_forest) %>%
        add_candidates(svm) %>%
        add_candidates(logistic_regression) %>%
        add_candidates(glmnet) %>%
        add_candidates(nnet) %>%
        add_candidates(bag_nnet) %>%
        blend_predictions(mixture = seq(0,1,.1)) %>%
        fit_members()

}    
# tar_load(tuned_results_random_forest_classification_smb)
# tar_load(tuned_results_xgboost_classification_smb)
# tar_load(tuned_results_svm_classification_smb)
# tar_load(tuned_results_glmnet_classification_smb)
# tar_load(tuned_results_nnet_classification_smb)

# build_stack_model(
#     tuned_results_xgboost_classification_smb,
#     tuned_results_random_forest_classification_smb,
#     tuned_results_svm_classification_smb,
#     tuned_results_glmnet_classification_smb,
#     tuned_results_nnet_classification_smb
#     )
# tar_load(stacked_model_sasq)
# tar_load(stacked_model_smb)
# tar_load(testing_data_sasq)
# tar_load(training_data_sasq)
# tar_load(testing_data_smb)
# tar_load(training_data_smb)
# tar_load(final_model_xgboost_classification_sasq)
# tar_load(final_fits_xgboost_classification_sasq)
# tar_load(tuned_results_bag_nnet_classification_sasq)

# m<-stacks() %>% 
#     add_candidates(tuned_results_bag_nnet_classification_sasq) %>%
#     blend_predictions() %>%
#     fit_members()


# testing_w_preds <- testing_data_smb %>%
#     bind_cols(stats::predict(stacked_model_smb, .,  type = "prob"))
# yardstick::roc_auc(testing_w_preds, truth = pres_abs, contains(".pred_0"))

# tar_load(final_model_glmnet_classification_smb)

# vip(final_model_glmnet_classification_smb)
