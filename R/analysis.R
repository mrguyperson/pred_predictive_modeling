train_and_test <- function(data, model_type, habitat_variable, is_fhast) {
    metric <- "roc_auc"
    mode <- "classification"
    splits <- initial_split(data, strata = "pres_abs")
    training_data <- training(splits)
    wf_sets <- make_workflow_set(model_type, training_data)
    start <- Sys.time()
    wf_results <- get_workflow_set_results(wf_sets)
    end <- Sys.time()
    duration <- as.numeric(end - start)
    wf_results_ranked <- wf_results %>% 
        rank_results(rank_metric = metric, select_best = TRUE)
    name_best_recipe <- wf_results_ranked$wflow_id[[1]]
    full_training_results <- extract_workflow_set_result(wf_results, name_best_recipe)
    params_best_model <- select_best(full_training_results, metric = metric)
    best_model_training_results <- full_training_results %>%
        unnest(.metrics) %>%
        filter(.config == params_best_model$.config)
    wf_best_model <- extract_workflow(wf_results, name_best_recipe)
    final_wf <- finalize_workflow(wf_best_model, params_best_model)
    testing_results <- last_fit(final_wf, splits, metrics = make_metric_sets(mode = mode)) %>%
        unnest(.metrics)
    model_for_predictions <- select_models(testing_results)
    var_imp <- get_var_importance(model_for_predictions, training_data)
    predictions <- NULL
    if(is_fhast) {
         
        predictions <- habitat_variable %>%
            filter(wetted == 1) %>%
            mutate(hab_rating = make_fhast_pred_predictions(
                model = model_for_predictions, 
                df = make_df_for_pred_predictions(habitat_variable))
                ) %>% 
            select(distance, lat_dist, hab_rating)
    }


    return(tibble(
        training_results = list(best_model_training_results),
        testing_results = list(testing_results),
        variable_importances = list(var_imp),
        name_best_model_recipe = name_best_recipe,
        fhast_predictions = list(predictions),
        duration = duration
    ))
}


# tar_load(fhast_data_smb)
# tar_load(df_for_pred_models)
# data <- fhast_data_smb
# model_type <- "nnet"
# is_fhast <- TRUE
# tar_load(habitat_variable)
# tar_load(grid_file)

# train_and_test(data, model_type, habitat_variable, TRUE)

# tar_load(modeling_results)




# # # test <- summarize_all_modeling_results(modeling_results)

# # modeling_results %>%
# #     select(variable_importances) %>%
# #     .[1,] %>%
# #     pull(variable_importances)
# #     # unnest(variable_importances) %>%
# #     # filter(species == "smb", model_name == "svm")

# # test

# train_res <- modeling_results %>%
#     # filter(species == "smb") %>%
#     select(training_results, model_name, species, tar_group)%>%
#     unnest(training_results) %>%
#     # unnest(.metrics) %>%
#     filter(.metric == "roc_auc") %>% 
#     select(.estimate, model_name, species) %>%
#     mutate(type = "Training")

# # # modeling_results %>%
# # #     # filter(species == "smb") %>%
# # #     # select(training_results, model_name, species, tar_group)%>%
# # #     unnest(training_results) %>%
# # #     filter(species == "lmb", model_name == "svm") %>%
# # #     head(10) %>%
# # #     select(-c(testing_results:tar_group)) %>% 
# # #     unnest(.metrics) %>%
# # #     filter(.metric == "roc_auc") %>%
# # #     arrange(cost, rbf_sigma)


# # # modeling_results %>%
# # #     filter(species == "lmb", model_name == "svm") %>%
# # #     filter(tar_batch == 1, tar_rep == 1) %>%
# # #     select(training_results) %>%
# # #     unnest(training_results) %>%
# # #     unnest(.metrics) %>% select_best()

# test_res<-modeling_results %>%
#     # filter(species == "smb") %>%
#     select(testing_results, model_name, species) %>%
#     unnest(testing_results) %>%
#     # unnest(.metrics) %>%

#     filter(.metric == "roc_auc") %>%
#     select(.estimate, model_name, species) %>%
#     mutate(type = "Testing")

# df <- bind_rows(train_res, test_res) %>%
#     mutate(model_name = fcase(
#         model_name == "xgb", "XGBoost", 
#         model_name == "svm", "SVM", 
#         model_name == "rf", "Random forest",
#         model_name == "regression", "GLM",
#         model_name == "nnet", "Neural network",
#         model_name == "glmnet", "L1 + L2 regularized GLM",
#         model_name == "fhast", "FHAST model",
#         model_name == "bag", "Ensemble neural network"
#     )) %>%
#     group_by(species, model_name) %>%
#     mutate(mean_roc_auc = mean(.estimate)) %>%
#     arrange(desc(mean_roc_auc)) %>%
#     ungroup() %>%
#     select(-mean_roc_auc)

# df %>%
#     ggplot(aes(x = .estimate, y = model_name, fill = type)) +
#     geom_boxplot() +
#     scale_fill_manual(values = c("dodgerblue", "#d83030")) +
#     geom_vline(xintercept = 0.5, linetype = "dashed", linewidth = 1.1) +
#     facet_wrap(
#         ~species, nrow = 3, 
#         strip.position = 'left',
#         labeller = as_labeller(c(smb = "Smallmouth\nbass", lmb = "Largemouth\nbass", sasq = "Sacramento\npikeminnow"))) +
#     labs(fill = NULL) +
#     ylab("") +
#     xlab("AUROC") +
#     theme_classic(
#         base_size = 25
#         ) +
#     theme(
#         strip.background = element_blank(),
#         strip.placement='outside')
# dt_res <- setDT(modeling_results)

# # dt_res[species != "lmb", 
# #         .(fhast_predictions = unlist(fhast_predictions)), 
# #         by = .(model_name, species)][
# #             , row := 1:.N, by = .(model_name, species)
# #             ][,
# #                 .(habitat_rating = sum(fhast_predictions)),
# #                 by = .(model_name, row)
# #             ]

# #     modeling_results %>%
# #         filter(species != "lmb") %>%
# #         unnest(fhast_predictions) %>%
# #         group_by(model_name, species) %>%
# #         mutate(row = 1:n()) %>%
# #         group_by(model_name, row) %>%
# #         summarize(habitat_rating = sum(fhast_predictions))


# # summary_data <- dt_res[species != "lmb",
# #         .(
# #             tar_group, tar_batch, tar_rep, species, model_name,
# #             fhast_predictions = (unlist(fhast_predictions))
# #             ),
# #         # by = .(species, model_name, tar_batch, tar_rep)
# #         ][,
# #             row := 1:.N,
# #             by = .(species, model_name, tar_batch, tar_rep)
# #         ][,
# #             .(habitat_rating = mean(fhast_predictions)),
# #             by = .(species, model_name, row)
# #         ][,
# #             .(habitat_rating = sum(habitat_rating)),
# #             by = .(model_name, row)
# #         ][rowid_to_column(habitat_variable, "row"), on = "row"][,
# #             .(habitat_rating = mean(habitat_rating)),
# #             by = .(model_name, distance, lat_dist)
# #         ][grid_file, on = c("distance", "lat_dist"), nomatch = 0] %>%
# #         st_as_sf()


# # dt_res[species == "smb" & model_name == "svm", .(
# #             tar_group, tar_batch, tar_rep, species, model_name,
# #             fhast_predictions = (unlist(fhast_predictions))
# #             )]

# make_map(
#     data_frame = summary_data,
#     fill = habitat_rating,
#     scale_name =  "Predator\nHabitat Rating"
#   ) +
# facet_wrap(~model_name)

# glmnet_data <- test %>%
#     filter(model_name == "glmnet") %>% 
#     # st_crop(xmin = -121.37, xmax = -121.34,
#     #         ymin = 38.56, ymax = 38.58)

# st_transform(glmnet_data, crs = "WGS84")

# box <- test %>%
#     filter(model_name == "glmnet") %>% 
#     st_bbox()

# fhast_data <- test %>%
#     filter(model_name == "fhast")

# make_map(
#     data_frame = glmnet_data,
#     fill = hab_rating,
#     scale_name =  "Predator\nHabitat Rating"
#   ) +
#   coord_sf(xlim = c(-121.37, -121.34), ylim = c(38.56, 38.58), expand = FALSE, crs = st_crs(glmnet_data))
# make_map(
#     data_frame = fhast_data,
#     fill = hab_rating,
#     scale_name =  "Predator\nHabitat Rating"
#   )


# test <- modeling_results %>%
#     # filter(species == "smb", model_name == "svm") %>%
#     unnest(fhast_predictions) %>%
#     select(hab_rating, species, model_name, distance, lat_dist) %>%
#     setDT() %>%
#     .[,
#         .(hab_rating = mean(hab_rating)),
#         by = .(species, model_name, distance, lat_dist)
#     ] %>% 
#     .[,
#         .(hab_rating = sum(hab_rating)),
#         by = .(model_name, distance, lat_dist)
#     ] %>% 
#     .[grid_file, on = c("distance", "lat_dist"), nomatch = 0] %>%
#     st_as_sf()


# make_map(
#     data_frame = test,
#     fill = hab_rating,
#     scale_name =  "Predator\nHabitat Rating"
#   ) +
# facet_wrap(~model_name)


# var_imp <- summarize_var_imp(modeling_results, 30)

# var_imp %>%
#     filter(species == "lmb") %>%
#     clean_model_names() %>%
#     clean_var_names() %>% 
#     ggplot(aes(x = var_imp, y = variable, fill = model_name)) +
#     geom_boxplot() +
#     scale_fill_viridis(
#         discrete = TRUE,
#         option = "G",
#         begin = 0.25,
#         name = "Model"
#         # end = 0.9
#         ) +
#     ylab("") +
#     xlab("Variable importance") +
#     theme_classic(
#         base_size = 25
#         ) +
#     theme(
#         strip.background = element_blank(),
#         strip.placement='outside')


# # var_imp %>%
# #     filter(species == "lmb") %>% 
# #     pull(variable) %>% 
# #     unique()


# # variable == "turbidity", "turbidity",
# # variable == "weathercode", "weather",
# # variable == "do",
# # variable == "ambient_conductivity",
# # variable == "watertemperature",
# # variable == "channel_type",
# # variable == "dominant_substrate",
# # variable == "tide",
# # variable == "emergent",
# # variable == "floating",
# # variable == "structure",
# # variable == "waterdepth",
# # variable == "bank_type",

# # new_test <- list(
# #     c("turbidity", "turbidity"),
# #     c("weathercode", "weather"),
# #     c("do", "diss. oxy."),
# #     c("ambient_conductivity", "conductivity"),
# #     c("watertemperature", "water temp."),
# #     c("channel_type", "channel type"),
# #     c("dominant_substrate", "substrate"),
# #     c("tide", "tide stage"),
# #     c("emergent", "emergent veg."),
# #     c("floating", "floating veg."),
# #     c("structure", "woody structure"),
# #     c("waterdepth", "depth"),
# #     c("bank_type", "bank type")
# # )
# # test <- c("turbidity", "weathercode", "do", "ambient_conductivity", 
# #             "watertemperature", "channel_type", "dominant_substrate,",
# #             "tide", "emergent", "floating", "structure", "waterdepth",
# #             "bank_type")
# # test2 <- c("turbidity", "weather", "diss. oxy.", "conductivity", 
# #             "water temp.", "channel type", "substrate", 
# #             "tide stage", "emergent veg.", "floating veg.", "woody structure", "depth",
# #             "bank_type")
# # things <- map(new_test, ~glue::glue('variable == {as.character(.x[1])}, {as.character(.x[2])},'))
# # things2 <- map(1:length(test2), ~expr(!!test2[.x]))


# # paste(things, test2)


# # test1 <- function(var1, var2) {
# #     expr(fcase(variable == {{var1}},{{var1}}))
# # }

# # map2(test, test2, test1)



# plot1<-var_imp %>%
#     filter(species == "lmb") %>%
#     clean_model_names() %>%
#     clean_var_names() %>% 
#     ggplot(aes(
#         x = var_imp,
#         y = variable,
#         fill = model_name
#         )) +
#     geom_boxplot() +
#     scale_fill_manual(
#          values = cbPalette,
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
#     # scale_fill_viridis(
#     #     discrete = TRUE,
#     #     option = "D",
#     #     begin = 0.25,
#     #     end = 1,
#     #     name = "Model",
#     #     breaks = (c(
#     #            "FHAST",
#     #            "GLM",
#     #            "Regularized GLM",
#     #            "SVM",
#     #            "Random forest",
#     #            "XGBoost",
#     #            "NNet",
#     #            "Ensemble NNet"
#     #         ))
#     #     ) +
#     scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
#     ylab("") +
#     xlab("Variable importance") +
#     theme_classic(
#         base_size = 25
#         ) +
#     # theme(
#     #     strip.background = element_blank(),
#     #     strip.placement = 'outside',
#     #     strip.text = element_blank()
#     #     ) +
#     facet_wrap(
#         ~species,
#         # strip.position = 'left',
#         # ncol = 2,
#         labeller = as_labeller(c(lmb = "Lm. bass"))
#         )
# # ggsave(here("test.png"), plot1, height = 8, width = 12)


# plot2<-var_imp %>%
#     filter(species == "sasq") %>%
#     clean_model_names() %>%
#     clean_var_names() %>% 
#     ggplot(aes(
#         x = var_imp,
#         y = variable,
#         fill = model_name
#         )) +
#     geom_boxplot() +
#     scale_fill_manual(
#          values = cbPalette,
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
#     # scale_fill_viridis(
#     #     discrete = TRUE,
#     #     option = "D",
#     #     end = 1,
#     #     begin = 0.25,
#     #     name = "Model",
#     #     breaks = (c(
#     #            "FHAST",
#     #            "GLM",
#     #            "Regularized GLM",
#     #            "SVM",
#     #            "Random forest",
#     #            "XGBoost",
#     #            "NNet",
#     #            "Ensemble NNet"
#     #         ))
#     #     ) +
#     scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
#     ylab("") +
#     xlab("") +
#     theme_classic(
#         base_size = 25
#         ) +
#     facet_wrap(
#         ~species,
#         # strip.position = 'left',
#         ncol = 2,
#         labeller = as_labeller(c(smb = "Sm. bass",sasq = "Pikeminnow"))
#         )
#     # theme(
#     #     strip.background = element_blank(),
#     #     strip.text = element_blank(),
#     #     strip.placement = 'outside'
#     #     )
# # ggsave(here("test.png"), plot2, height = 5, width = 12)

# plot3<-var_imp %>%
#     filter(species == "smb") %>%
#     clean_model_names() %>%
#     clean_var_names() %>% 
#     ggplot(aes(
#         x = var_imp,
#         y = variable,
#         fill = model_name
#         )) +
#     geom_boxplot() +
#     scale_fill_manual(
#          values = cbPalette,
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
#     # scale_fill_viridis(
#     #     discrete = TRUE,
#     #     option = "D",
#     #     end = 1,
#     #     begin = 0.25,
#     #     name = "Model",
#     #     breaks = (c(
#     #            "FHAST",
#     #            "GLM",
#     #            "Regularized GLM",
#     #            "SVM",
#     #            "Random forest",
#     #            "XGBoost",
#     #            "NNet",
#     #            "Ensemble NNet"
#     #         ))
#     #     ) +
#     scale_x_continuous(breaks = seq(0,0.2,.1), limits = c(0, 0.2)) +
#     ylab("") +
#     xlab("Variable importance") +
#     labs(fill = NULL) +
#     theme_classic(
#         base_size = 25
#         ) +
#     facet_wrap(
#         ~species,
#         # strip.position = 'left',
#         ncol = 2,
#         labeller = as_labeller(c(smb = "Sm. bass",sasq = "Pikeminnow"))
#         ) +
#      guides(fill = guide_legend(nrow = 2))

#     # theme(
#     #     strip.background = element_blank(),
#     #     strip.text = element_blank(),
#     #     strip.placement = 'outside'
#     #     )
# # ggsave(here("test.png"), plot2, height = 5, width = 12)

# leg <- get_legend(plot3) %>%
#     ggarrange() +
#     theme_classic(base_size = 40)

# plot4 <- ggarrange(
#     plot2,
#     plot3,
#     ncol = 1,
#     legend = "none"
# )

# plot5<- ggarrange(
#     plot4,
#     plot1,
#     # leg,
#     # nrow=2,
#     ncol = 2,
#     common.legend = TRUE,
#     legend = "bottom",
#     # guide = guide_legend(title = "Title")
#     # legend.title = ""
#     legend.grob = get_legend(plot3)
#     # heights = c(2,1)
#     )
#     #  guides(fill = guide_legend(title = "Title"))
# ggsave(here("test2.png"), plot5, height = 20, width = 16)


# cbPalette <- c("#999999", "#D55E00",
#                          "#F0E442", "#56B4E9", "#E69F00",
#                          "#0072B2", "#009E73", "#CC79A7"
#                          )

# df <- modeling_results %>%
#     select(testing_results,model_name, species, tar_seed) %>%
#     unnest(testing_results) %>%
#     filter(.metric == "roc_auc") %>%
#     select(estimate = .estimate, model_name, species, tar_seed)

# df %>% 
#     group_by(species) %>% 
#     group_split() %>%
#     map(~ aov(estimate ~ model_name, data = .x)) %>%
#     map(tidy)

# aov(estimate ~ model_name + species, data =df) %>% tidy()

# quantiles_testing <- df %>%
#     summarize(
#         q25 = quantile(estimate, 1/4),
#         median = median(estimate),
#         q75 = quantile(estimate, 3/4),
#         .by = c(model_name, species)
#         ) %>%
#     arrange(species, median)

# quantiles_testing %>%
#     slice_tail(n=3, by = species) %>%
#     count(model_name)


# training <- modeling_results %>%
#     select(training_results,model_name, species, tar_seed) %>%
#     unnest(training_results) %>%
#     filter(.metric == "roc_auc") %>%
#     select(estimate = .estimate, model_name, species, tar_seed)

# quantiles_training <- training %>%
#     summarize(
#         q25 = quantile(estimate, 1/4),
#         median = median(estimate),
#         q75 = quantile(estimate, 3/4),
#         .by = c(model_name, species)
#         ) %>%
#     arrange(median, species)

# quantiles_training %>%
#     left_join(quantiles_testing, join_by(model_name, species)) %>%
#     mutate(diff = (median.y - median.x)) %>%
#     select(model_name, species, diff) %>%
#     arrange(-abs(diff)) %>%
#     pull(diff) %>% hist()
