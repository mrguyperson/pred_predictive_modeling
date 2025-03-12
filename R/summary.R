
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
         # labeller = as_labeller(c(smb = "Sm. bass", lmb = "Lm. bass", sasq = "Pikeminnow"))
         ) +
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
   
   save_path <- file.path("output", "auroc.png")
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

plot_var_imp <- function(var_imp_summary) {
   # plot1<-var_imp_summary %>%
   #  filter(species == "lmb") %>%
   #  clean_model_names() %>%
   #  clean_var_names() %>% 
   #  ggplot(aes(
   #      x = var_imp,
   #      y = variable,
   #      fill = model_name
   #      )) +
   #  geom_boxplot() +
   #  scale_fill_manual(
   #       values = palette,
   #       breaks = c(
   #             "FHAST",
   #             "GLM",
   #             "Regularized GLM",
   #             "SVM",
   #             "Random forest",
   #             "XGBoost",
   #             "NNet",
   #             "Ensemble NNet", name = ""
   #          )
   #       ) +
   #  scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
   #  ylab("") +
   #  xlab("Variable importance") +
   #  theme_classic(
   #      base_size = 25
   #      ) +
   #  facet_wrap(
   #      ~species,
   #    #   labeller = as_labeller(c(lmb = "Lm. bass"))
   #      )


   # plot2<-var_imp_summary %>%
   #    filter(species == "sasq") %>%
   #    clean_model_names() %>%
   #    clean_var_names() %>% 
   #    ggplot(aes(
   #       x = var_imp,
   #       y = variable,
   #       fill = model_name
   #       )) +
   #    geom_boxplot() +
   #    scale_fill_manual(
   #          values = palette,
   #          breaks = c(
   #                "FHAST",
   #                "GLM",
   #                "Regularized GLM",
   #                "SVM",
   #                "Random forest",
   #                "XGBoost",
   #                "NNet",
   #                "Ensemble NNet", name = ""
   #             )
   #          ) +

   #    scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
   #    ylab("") +
   #    xlab("") +
   #    theme_classic(
   #       base_size = 25
   #       ) +
   #    facet_wrap(
   #       ~species,
   #       # strip.position = 'left',
   #       ncol = 2,
   #       # labeller = as_labeller(c(smb = "Sm. bass",sasq = "Pikeminnow"))
   #       )

   # plot3<-var_imp_summary %>%
   #    filter(species == "smb") %>%
   #    clean_model_names() %>%
   #    clean_var_names() %>% 
   #    ggplot(aes(
   #       x = var_imp,
   #       y = variable,
   #       fill = model_name
   #       )) +
   #    geom_boxplot() +
   #    scale_fill_manual(
   #          values = palette,
   #          breaks = c(
   #                "FHAST",
   #                "GLM",
   #                "Regularized GLM",
   #                "SVM",
   #                "Random forest",
   #                "XGBoost",
   #                "NNet",
   #                "Ensemble NNet"
   #             )
   #          ) +
   #    scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
   #    ylab("") +
   #    xlab("Variable importance") +
   #    labs(fill = NULL) +
   #    theme_classic(
   #       base_size = 25
   #       ) +
   #    facet_wrap(
   #       ~species,
   #       # strip.position = 'left',
   #       ncol = 2,
   #       # labeller = as_labeller(c(smb = "Sm. bass",sasq = "Pikeminnow"))
   #       ) +
   #    guides(fill = guide_legend(nrow = 2))

   # leg <- get_legend(plot3) %>%
   #    ggarrange() +
   #    theme_classic(base_size = 40)

   # plot4 <- ggarrange(
   #    plot2,
   #    plot3,
   #    ncol = 1,
   #    legend = "none"
   # )

   # plot5<- ggarrange(
   #    plot4,
   #    plot1,
   #    ncol = 2,
   #    common.legend = TRUE,
   #    legend = "bottom",
   #    legend.grob = get_legend(plot3)
   #    )
   # save_path <- file.path("output", "var_imp.png")
   # ggsave(save_path, plot5, height = 20, width = 16)
   # save_pathfont_add_google("Libre Franklin", "franklin")
   showtext_opts(dpi=600)
   showtext_auto()

   data <- var_imp_summary %>%
      clean_model_names() %>%
      clean_var_names() %>%
      summarize(var_imp = median(var_imp), .by = c(species, model_name, variable)) %>%
      mutate(var_imp = if_else(var_imp >= 0, var_imp, 0))

   model_names <- data %>%
      pull(model_name) %>% 
      unique()

   make_var_imp_subplot <- function(data, species_name) {
      data_filtered <- data %>%
         filter(species %in% species_name)

      y_lim_upper <- data_filtered %>%
         pull(variable) %>%
         unique() %>%
         length()

      data_filtered %>%
         ggplot(aes(x = model_name, y = variable, fill = var_imp)) +
         geom_tile() +
         scale_fill_continuous(
            type = "viridis", 
            limits = c(0, 0.15),
            breaks = seq(0, 0.15, by = 0.05),
            # low = "dodgerblue",
            # mid = "white",
            # high = "#d83030"
                  ) +
         labs(
            x = NULL, 
            y = NULL, 
            fill = "Variable importance",
            title = species_name
            ) +
         coord_cartesian(expand = FALSE, clip = "off", ylim = c(0.5, y_lim_upper + 0.5)) +
         theme_bw() +
         theme(
            axis.text.y = element_text(margin = margin(l = 0)),
            axis.ticks = element_blank(),
            panel.border = element_rect(color = "black", linewidth = 0.25)

         )
         # facet_wrap(~species, ncol = 1)
   }

   # smb_sasq <- make_var_imp_subplot(data, c("Sm. bass","Pikeminnow"))
   smb <- make_var_imp_subplot(data, c("Sm. bass")) +
         annotate(         
            geom = "segment",
            x = 1:7 + 0.5,
            xend = 1:7 + 0.5, 
            y = 0.5, 
            yend = 0,
            linewidth = 0.2)
   sasq <- make_var_imp_subplot(data, "Pikeminnow") +
      theme(
         axis.text.x = element_blank(),
      )

   lmb <- make_var_imp_subplot(data, "Lm. bass") +
         annotate(         
            geom = "segment",
            x = 1:7 + 0.5,
            xend = 1:7 + 0.5, 
            y = 0.5, 
            yend = -0,
            linewidth = 0.2)

   p3 <- (sasq / smb | lmb) + 
      plot_layout(guides = "collect") &
      # plot_annotation(tag_levels = c("A")) & 

      theme(
         axis.text = element_text(size = 6, color = "#000000"),
         legend.axis.line = element_blank(),
         legend.frame = element_rect(color = "black", linewidth = 0.2),
         legend.position = "bottom",
         legend.text = element_text(size = 6, color = "black"),
         legend.text.position = "bottom",
         legend.ticks = element_line(color = "black", linewidth = 0.1),
         legend.title = element_text(hjust = 0.5, size = 7, color = "black"),
         legend.title.position = "top",
         # plot.margin = margin(l = 5, t = 5, r = 5),
         plot.title = element_text(size = 8, face = "bold"),
         # strip.text = element_text(family = "franklin", hjust = -0.02, face = "bold"),
         # strip.background = element_blank(),
         text = element_text(family = "franklin")

         )


   save_path <- file.path("output", "var_imp.png")

   ggsave(save_path, p3, height = 6, width = 7, dpi = 600)
   save_path

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
            model_name == "rf", "Random\nforest",
            model_name == "regression", "GLM",
            model_name == "nnet", "NNet",
            model_name == "glmnet", "L1+L2\nGLM",
            model_name == "fhast", "FHAST",
            model_name == "bag", "Bagged\nNNet"
    ),
         model_name = factor(
            model_name,
            levels = (c(
               "FHAST",
               "GLM",
               "L1+L2\nGLM",
               "SVM",
               "Random\nforest",
               "XGBoost",
               "NNet",
               "Bagged\nNNet"
            )),
            ordered = TRUE
         ),
         species = fcase(
            species == "smb", "Sm. bass", 
            species == "lmb", "Lm. bass", 
            species == "sasq", "Pikeminnow"),
         species = factor(
            species,
            levels = c(
               "Pikeminnow",
               "Sm. bass",
               "Lm. bass"
            )
         )

    ) 
}


clean_var_names <- function(df) {
   df %>%
      mutate(variable = fcase(
         variable == "turbidity", "turbidity",
         variable == "weathercode", "weather",
         variable == "do", "diss.\noxy.",
         variable == "ambient_conductivity", "conductivity",
         variable == "watertemperature", "water\ntemp.",
         variable == "channel_type", "channel\ntype",
         variable %like% "substrate", "substrate",
         variable == "tide", "tide\nstage",
         variable == "emergent", "emergent\nveg.",
         variable == "floating", "floating\nveg.",
         grepl("wood|structure", variable), "woody\nstructure",
         variable %like% "depth", "depth",
         variable == "bank_type", "bank\ntype",
         variable == "veg", "vegetation",
         variable == "velocity", "velocity",
         variable == "shade", "shade"
         )
      )
}

