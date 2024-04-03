# Function to make summaries of daily data

make_data_summary = function(fish_parm, variable_habitat, pred_parm, predator_predictions, habitat_parm, juvenile_run, cover_fra_model, dis_to_cover_model){
  prey_species <- predator_predictions %>% pull(prey_species) %>% unique()

  # get the life stage
  ls = predator_predictions %>% pull(life_stage) %>% unique()
  
  # Which species are we looking at
  id = which(fish_parm$specie == prey_species)
  
  # Give the parameter list a shorter name 
  pl = fish_parm
  
  # Get the example length
  fish_length =  fifelse(ls == "juvenile", pl$eg_juvenile_length[id], pl$eg_adult_length[id])
  
  # Calculate the fish mass
  fish_mass = pl$length_mass_a[id] * fish_length^pl$length_mass_b[id]
  
  # get example width for benthic feeding based on fish having the density of water 1g/cm^3
  # and being approximated as a rectangle
  fish_width = sqrt(fish_mass/fish_length)
  
  # Check if the fish feeds
  feeding_flag = fifelse(ls == "adult" & pl$adult_feeding[id] == 0, 0, 1)

  # calculate the wall factor reduction in velocity
  wall_factor = fifelse(pl$benthic_fish[id] == 0, 1,
                       # all the following hard coded constants from the
                       # relationship for the law of the wall
                       habitat_parm$base_wall_factor)

  # If there are juveniles do predation calculations
  if(juvenile_run & ls == "juvenile"){
    pred_results = map_dfc(pred_parm$species,
                           ~calc_all_preds(.x, pred_parm, predator_predictions, variable_habitat, fish_length, habitat_parm, cover_fra_model, dis_to_cover_model)) %>% 
      mutate(hab_rating = rowSums(across(starts_with("hab_rating"))),
             pred_mort_risk = rowSums(across(starts_with("pred_mort_risk")))) %>% 
      as_tibble() %>%
      select(hab_rating, pred_mort_risk) 
    
    # Bind the pred column to the habitat ones
    habitat = bind_cols(variable_habitat, pred_results)
    
    # Clean up
    rm(pred_results)
    gc()
  }
  
  # Do the calculation for habitat and feeding
  habitat = habitat %>% 
    mutate(# Set if they use small cover
      cover_fra = if(pl$small_cover_length[id] >= fish_length) small_cover_fra else cover_fra,
      # Calculate max swim speed temperature function value
      max_swim_speed_temp = calc_beta_sig(parm_A = pl$ucrit_c[id],
                                          parm_B = pl$ucrit_d[id],
                                          temp = temp),
      # Calculate the max swim speed
      max_swim_speed = (pl$ucrit_a[id] / fish_length + pl$ucrit_b[id]) *
        max_swim_speed_temp * fish_length / 100,
      # Reduce velocity for benthic fish and in cover fish if habitat is available
      benthic_flag = pl$benthic_fish[id], 
      shelter_fraction = fifelse(fish_length^2/1e4 < wetted_area*cover_fra & pl$benthic_fish[id] == 0,
                                 habitat_parm$shelter_frac,
                                 1),
      experienced_vel = wall_factor*velocity*shelter_fraction,
      # Calculate teh active and passive metabolic rate
      fish_met_j_per_day_active = calc_met(params = pl,
                                           fish_index = id,
                                           length = fish_length,
                                           temp = temp,
                                           velocity = experienced_vel),
      fish_met_j_per_day_passive = calc_met(params = pl,
                                            fish_index = id,
                                            length = fish_length,
                                            temp = temp,
                                            velocity = 0),
      # Calculate the total metabolic rate (benthic fish are assumed to be active at night)
      fish_met_j_per_day = fifelse(benthic_flag == 0,
                                  fish_met_j_per_day_active * (photoperiod) +
                                    fish_met_j_per_day_passive * (1 - photoperiod),
                                  fish_met_j_per_day_active * (1 - photoperiod) +
                                    fish_met_j_per_day_passive * (photoperiod)),
      # Remove places where the velocity is higher than Ucrit
      fish_met_j_per_day = fifelse(experienced_vel>max_swim_speed | depth <= 0, NA_real_, fish_met_j_per_day),
      # Calculate cmax
      cmax = pl$cmax_a[id] * fish_mass^(1 + pl$cmax_b[id]) *
        calc_beta_sig(parm_A = pl$cmax_c[id],
                      parm_B = pl$cmax_d[id],
                      temp = temp),
      # Calculate the turbidity function
      turbidity_fun = fifelse(turb <= pl$turbid_threshold[id], 1,
                             pl$turbid_min[id] + (1 - pl$turbid_min[id]) *
                               exp(pl$turbid_exp[id] *
                                     (turb - pl$turbid_threshold[id]))),
      # Calculate detection distance, capture area, and success
      detection_dist = (pl$react_dist_a[id] +
                          pl$react_dist_b[id] * fish_length) * turbidity_fun,
      capture_area = 2 * detection_dist * pmin(depth, detection_dist),
      capture_success = calc_logistic(parm_10 = pl$capture_V1[id],
                                      parm_90 = pl$capture_V9[id],
                                      value = velocity/max_swim_speed),
      # Calculate food eaten per day
      drift_eaten = capture_success *capture_area * habitat_parm$hab_drift_con *
        velocity * 86400 * photoperiod,
      ben_eaten = pi * pl$feeding_speed[id] * fish_length / fish_width *
        86400 * (1 - photoperiod)  *
        fish_width^2 * habitat_parm$hab_bentic_con / (1E4 *
        log(pl$feeding_speed[id] * fish_length / fish_width *
              (1 - photoperiod) * 86400)),
      ben_avaiable = habitat_parm$hab_bentic_con *
        ben_food_fra * wetted_area,
      # Calculate the food eaten
      intake_ben_energy = pmin(ben_eaten, ben_avaiable, cmax)* habitat_parm$hab_bentic_ene,
      intake_drift_energy = pmin(drift_eaten, cmax)* habitat_parm$hab_drift_ene,
      # Calculate the energy intake and net energy
      energy_intake = (intake_ben_energy *pl$benthic_fish[id] + 
                         intake_drift_energy * (1 - pl$benthic_fish[id])) * feeding_flag,
      net_energy = energy_intake - fish_met_j_per_day) %>% 
    # Remove the temportay columns
    select(-ben_food_fra, -max_swim_speed_temp, -max_swim_speed, -shelter_fraction,
           -experienced_vel, -fish_met_j_per_day_active, -fish_met_j_per_day_passive,
           -cmax, -turbidity_fun, -detection_dist, -capture_area, -capture_success,
           -drift_eaten, -ben_eaten, -ben_avaiable, -intake_ben_energy,
           -intake_drift_energy, -small_cover_fra)

  # Make the average map
  average_map_full = habitat %>% 
    select(-date) %>% 
    group_by(lat_dist, distance) %>% 
    summarise_all(list(~mean(., na.rm = TRUE))) %>% 
    ungroup()
  
  average_map = habitat %>% 
    filter(aoi == 1) %>% 
    select(-date) %>% 
    group_by(lat_dist, distance) %>% 
    summarise_all(list(~mean(., na.rm = TRUE))) %>% 
    ungroup()
  
  # Make the average daily data 
  average_day_full = habitat %>%
    mutate(total_cover = wetted_area * cover_fra,
           date = mdy(date)) %>% 
    group_by(date) %>% 
    summarise_all(list(~mean(., na.rm = TRUE))) %>% 
    ungroup()
  
  average_day = habitat %>%
    filter(aoi == 1) %>% 
    mutate(total_cover = wetted_area * cover_fra,
           date = mdy(date)) %>% 
    group_by(date) %>% 
    summarise_all(list(~mean(., na.rm = TRUE))) %>% 
    ungroup()
  
  # Remove the habitat file and clean up 
  rm(habitat)
  gc()
  
  return(list(average_map = average_map,
              average_day =  average_day,
              average_map_full = average_map_full,
              average_day_full =  average_day_full,
              lifestage = ls,
              species = pl$specie[id],
              model = unique(predator_predictions$model_name),
              substrate = fifelse(unique(predator_predictions$substrate) == 1, "rock", "fine")
              ))
}

make_map_data <- function(data_summary, grid_file, subset){
    df_name <- glue::glue("{subset}_{data_summary$model}_{data_summary$species}_{data_summary$lifestage}_{data_summary$substrate}")
    left_join(as_tibble(data_summary[[subset]]), as_tibble(grid_file), 
      by = c("distance", "lat_dist", "geometry", "left_or_right", "area")) %>%
      mutate(run_name = df_name) %>%
      st_as_sf()
      # setNames(map(data_summary, ~paste0(.x$species, "-", .x$lifestage)))
}

make_mean_map <- function(df, habitat_parm) {
  df %>%
    #remove fish specific things
    select(-pred_mort_risk, -benthic_flag, -fish_met_j_per_day, -energy_intake, -net_energy) %>% 
    mutate(below_d_cutoff = fifelse(depth < habitat_parm$dep_cutoff, 1, 0) ,
          below_v_cutoff = fifelse(velocity < habitat_parm$vel_cutoff, 1, 0)) 
}

add_daily_flow <- function(data_summary, daily_file, subset){
  df_name <- glue::glue("{subset}_{data_summary$model}_{data_summary$species}_{data_summary$lifestage}")
  left_join(as_tibble(data_summary[[subset]]), select(daily_file, date, day, flow_cms), 
    by = "date") %>%
    mutate(
      run_name = df_name,
      wetted = wetted * analyzed_cells) %>%
    st_as_sf()
}

make_predator_maps <- function(df, fill_option, scale_name, area) {
  plot <- make_map(
    data_frame = df,
    fill = {{fill_option}},
    scale_name = scale_name
  )
  fill_option_as_str <- deparse(substitute(fill_option))
  run_name <- df$run_name %>% unique()
  file_name <- glue::glue("{area}_{fill_option_as_str}_{run_name}")
  path <- glue::glue("{here('output', file_name)}.jpg")
  ggsave(filename = path, device = "jpg")
  path
}

make_pred_model_difference_df <- function(pred_prediction_summary){
  pred_prediction_summary %>%
    as.data.table() %>%
    .[!(model_name == "regression"), hab_rating := hab_rating - .[model_name == "regression", .(hab_rating)], by = c("model_name")] %>%
    .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
    as_tibble()
}

make_summed_pred_model_df <- function(pred_model_difference_df) {
  pred_model_difference_df %>%
    as.data.table() %>%
    .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
    .[, cell_num := 1:.N, by = .(tar_group, pred_species)] %>%
    .[, .(hab_rating = sum(hab_rating)), by = .(model_name, cell_num, substrate, tar_group, prey_species, life_stage)] %>%
    .[order(tar_group, cell_num)] %>%
    # .[, cell_num := NULL] %>%
    as_tibble()
}

  # pred_prediction_summary %>%
  #   as.data.table() %>%
  #   .[!(model_name == "logistic_regression"), hab_rating := hab_rating - .[model_name == "logistic_regression", .(hab_rating)], by = c("model_name")] %>%
  #   .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
  #   .[, cell_num := 1:.N, by = c("tar_group", "pred_species")] %>%
  #   dcast(model_name + cell_num + tar_group + prey_species + substrate + life_stage ~ .,
  #         fun.aggregate = sum, 
  #         value.var = c("hab_rating")) %>%
  #   setnames(".", "hab_rating") %>% 
  #   .[order(tar_group, cell_num)] %>%
  #   .[, cell_num := NULL] %>%
  #   as_tibble()


  # t<- pred_prediction_summary %>%
  #   as.data.table() %>%
  #   .[!(model_name == "logistic_regression"), hab_rating := hab_rating - .[model_name == "logistic_regression", .(hab_rating)], by = .(model_name)] %>%
  #   .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
  #   .[, cell_num := 1:.N, by = .(tar_group, pred_species)] %>%
  #   .[, .(hab_rating = sum(hab_rating)), by = .(model_name, cell_num, substrate, tar_group, prey_species, life_stage)] %>%
  #   .[order(tar_group, cell_num)] %>%
  #   .[, cell_num := NULL] %>%
  #   as_tibble()

make_pred_model_histograms <- function(pred_prediction_summary) {

  plot<-pred_prediction_summary %>%
    filter(substrate == 1) %>%
    mutate(model_name = fcase(
      model_name == "regression", "Logistic Regression",
      model_name == "rf", "Random Forest", 
      model_name == "svm", "Support Vector Machine",
      model_name == "glmnet", "GLMnet",
      model_name == "nnet", "Neural Net",
      model_name == "bag", "Bagged Neural Net",
      model_name == "lightgbm", "LightGBM",
      default = "XGBoost"
    ),pred_species = fifelse(
      pred_species == "sasq", "Pikeminnow", "Smallmouth bass"
    )) %>%
    rename("Species" = "pred_species") %>%
    ggplot(aes(
      x = hab_rating, 
      # color = pred_species,
      fill = Species
      # after_stat(density)
      )) +
    geom_histogram(alpha = 0.5, color = "black") +
    # geom_density() +
    xlab("Habitat rating") +
    ylab("Count") +
    theme_classic(base_size = 15) +
    theme(legend.position = "bottom") +
    facet_wrap(~model_name)
  path <- here::here("output", "predator_histograms.jpg")
  ggsave(filename = path, device = "jpg", width = 9, height = 6)
  path
}



# microbenchmark(
#   option1 = {  pred_prediction_summary %>%
#     as.data.table() %>%
#     .[!(model_name == "logistic_regression"), hab_rating := hab_rating - .[model_name == "logistic_regression", .(hab_rating)], by = c("model_name")] %>%
#     .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
#     .[, cell_num := 1:.N, by = c("tar_group", "pred_species")] %>%
#     dcast(model_name + cell_num + tar_group + prey_species + substrate + life_stage ~ .,
#           fun.aggregate = sum, 
#           value.var = c("hab_rating")) %>%
#     setnames(".", "hab_rating") %>% 
#     .[order(tar_group, cell_num)] %>%
#     .[, cell_num := NULL] %>%
#     as_tibble()},
#   option2 = {
#       pred_prediction_summary %>%
#     as.data.table() %>%
#     .[!(model_name == "logistic_regression"), hab_rating := hab_rating - .[model_name == "logistic_regression", .(hab_rating)], by = c("model_name")] %>%
#     .[, substrate := fifelse(substrate == 1, "rock", "fine")] %>%
#     .[, cell_num := 1:.N, by = c("tar_group", "pred_species")] %>%
#     .[, hab_rating := sum(hab_rating), by = c("model_name", "cell_num", "substrate")] %>%
#     .[order(tar_group, cell_num)] %>%
#     as_tibble()
#   },
#   times = 10
# )

join_pred_and_hab <- function(pred_predictions, habitat_variable) {
  dt1 <- pred_predictions %>%
    as.data.table() %>%
    .[substrate == "rock"]

  dt2 <- habitat_variable %>% 
    as.data.table() %>% 
    .[, cell_num := 1:.N]

  dt1[dt2[, .(date, geometry, cell_num)], on = c("cell_num")] %>%
    .[, .(hab_rating = mean(hab_rating), geometry = geometry), by = c("model_name", "prey_species", "life_stage", "cell_num")] %>%
    st_as_sf()
}

make_multimodel_river_plot <- function(df, fill_option = NULL, scale_name = NULL, title = NULL) {

  plot<-df %>%
    mutate(model_name = fcase(
      model_name == "regression", "Logistic Regression",
      model_name == "rf", "Random Forest", 
      model_name == "svm", "Support Vector Machine",
      model_name == "glmnet", "GLMnet",
      model_name == "nnet", "Neural Net",
      model_name == "bag", "Bagged Neural Net",
      model_name == "lightgbm", "LightGBM",
      default = "XGBoost"
      )) %>%
    make_map(
      fill = {{fill_option}},
      scale_name = scale_name,
      title = title
    ) +
    theme_classic(base_size = 12) +
    theme(legend.position = "bottom") +
    facet_wrap(~model_name)

  
  fill_option_as_str <- deparse(substitute(fill_option))

  file <- glue::glue("{here::here('output', fill_option_as_str)}_all_models.jpg")
  ggsave(filename = file, device = "jpg", height = 6, width = 8)
  file

}
# make_multimodel_river_plot(summed_pred_predictions_w_hab, fill_option = "hab_rating", scale_name = "hab_rating")


# t[t2[, .(date, geometry, cell_num)], on = c("cell_num")] %>%
#   .[, .(hab_rating = mean(hab_rating), geometry = geometry), by = c("model_name", "prey_species", "life_stage", "cell_num")] %>%
#   st_as_sf() %>%
#   make_map(
#     fill = hab_rating,
#     scale_name = "Habitat rating"
#   ) +
#   facet_wrap(~model_name)
