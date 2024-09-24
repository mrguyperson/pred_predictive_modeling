
summarize_auroc <- function(modeling_results, train_or_test) {
   train_or_test <- enquo(train_or_test)
    modeling_results %>%
      select(!!train_or_test, model_name, species, tar_group)%>%
      unnest(!!train_or_test) %>%
      filter(.metric == "roc_auc") %>% 
      select(.estimate, model_name, species) %>% 
      mutate(type = object_to_string(!!train_or_test))
    
}

summarize_all_auroc <- function(modeling_results) {

    return(tibble(
        training_results = list(summarize_auroc(modeling_results, training_results)),
        testing_results = list(summarize_auroc(modeling_results, testing_results))
    ))

}

clean_auroc <- function(modeling_results) {
   summarized_res <- summarize_all_auroc(modeling_results)
   training <- summarized_res$training_results[[1]]
   testing <- summarized_res$testing_results[[1]]

   bind_rows(training, testing) %>% 
      clean_model_names() %>% 
      mutate(type = factor(type, levels = c("Testing", "Training", ordered = TRUE)))

}

plot_auroc <- function(modeling_results) {
   res <- clean_auroc(modeling_results)

   plot <- res %>% 
      ggplot(aes(x = .estimate, y = model_name, fill = type)) +
      geom_boxplot() +
      scale_fill_manual(
         values = c("dodgerblue", "#d83030"),
         breaks = c("Training", "Testing")
         ) +
      geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 1.1) +
      facet_wrap(
         ~species, nrow = 3, 
         strip.position = 'left',
         labeller = as_labeller(c(smb = "Sm. bass", lmb = "Lm. bass", sasq = "Pikeminnow"))) +
      labs(fill = NULL) +
      ylab("") +
      xlab("AUROC") +
      scale_y_discrete(limits = rev(c(
               "FHAST",
               "GLM",
               "Regularized GLM",
               "SVM",
               "Random forest",
               "XGBoost",
               "NNet",
               "Ensemble NNet"
            ))) +
      scale_x_continuous(breaks = seq(0.4,1,.1), limits = c(0.45, .95)) +
      theme_classic(
         base_size = 25
         ) +
      theme(
         strip.background = element_blank(),
         strip.placement='outside',
         strip.text = element_text(face = "bold"),
         legend.position = c(0.85, 0.5),
         legend.box.background = element_rect(colour = "black", linewidth = 1.25)
         )
   
   save_path <- here("output", "auroc.png")
   ggsave(save_path, plot, height = 10, width = 14)
   save_path
}
                     
summarize_var_imp <- function(modeling_results, permutations) {
   modeling_results %>%
      select(variable_importances, species, model_name, tar_seed) %>%
      group_by(tar_seed) %>%
      mutate(var_imp_expanded = map(variable_importances, ~get_table_of_all_vi_perms(.x, permutations))) %>%
      ungroup() %>% 
      unnest(var_imp_expanded) %>% 
      select(-c(tar_seed, variable_importances))
}


object_to_string <- function(var) {

    var <- enquo(var)
    as_label(var) %>%
       label_train_or_test()

}

label_train_or_test <- function(string) {
   if(grepl("train", string, ignore.case = TRUE)) {
      return("Training")
   } else {
      return("Testing")
   }

}

clean_model_names <- function(df) {
   df %>% 
       mutate(
         model_name = fcase(
            model_name == "xgb", "XGBoost", 
            model_name == "svm", "SVM", 
            model_name == "rf", "Random forest",
            model_name == "regression", "GLM",
            model_name == "nnet", "NNet",
            model_name == "glmnet", "Regularized GLM",
            model_name == "fhast", "FHAST",
            model_name == "bag", "Ensemble NNet"
    ),
         model_name = factor(
            model_name,
            levels = rev(c(
               "FHAST",
               "GLM",
               "Regularized GLM",
               "SVM",
               "Random forest",
               "XGBoost",
               "NNet",
               "Ensemble NNet"
            )),
            ordered = TRUE
         )
    ) 
}


clean_var_names <- function(df) {
   df %>%
      mutate(variable = fcase(
         variable == "turbidity", "turbidity",
         variable == "weathercode", "weather",
         variable == "do", "diss. oxy.",
         variable == "ambient_conductivity", "conductivity",
         variable == "watertemperature", "water temp.",
         variable == "channel_type", "channel type",
         variable %like% "substrate", "substrate",
         variable == "tide", "tide stage",
         variable == "emergent", "emergent veg.",
         variable == "floating", "floating veg.",
         grepl("wood|structure", variable), "woody structure",
         variable %like% "depth", "depth",
         variable == "bank_type", "bank type",
         variable == "veg", "vegetation",
         variable == "velocity", "velocity",
         variable == "shade", "shade"
         )
      )
}

