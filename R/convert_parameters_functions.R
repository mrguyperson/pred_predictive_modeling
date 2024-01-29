########################################
# This script loads all functions to help parametr conversion
########################################

# 2 Pivot functions
params_pivot_longer <- function(df) {
  df %>% 
    pivot_longer(cols = -term, names_to = "species", values_to = "estimate") %>% 
    select(species, term, estimate) %>% 
    arrange(species)
}

params_pivot_wider <- function(df) {
  df %>% 
    select(1:2) %>% 
    pivot_wider(names_from = term, values_from = pikeminnow)
}

write_rds_temp_folder <- function(df, filename) {
  write_rds(df, here(temp_folder,  filename))
}

select_model_param <- function(df, param) {
  df %>% 
    filter(name == param) %>% 
    pull(value) %>% as.numeric()
}

make_variables <- function(df, filename) {
  do.call("<<-",list(filename,df))
}

# adds some additional parameters to the fish_parm list for madult migration
calculate_adult_parameters <- function(fish_parm){
  num_species <- length(fish_parm$specie)
  fish_parm_lol <- map(1:num_species, ~ map(fish_parm, .x))
  fish_parm[["fish_mass_g"]] <- map_dbl(fish_parm_lol, get_fish_body_mass)
  fish_parm[["swim_speed_max_m_per_s"]] <- map_dbl(fish_parm_lol, get_max_swim_speed)
  return(fish_parm)
}

# calculates the parameters need to estimate the optimal swim speed curve 
get_swim_speed_model_params <- function(fish_parm, fish_index){

  #fish_parm <- map(fish_parm, ~.x[[fish_id]])
  fish_length <- fish_parm$eg_adult_length[fish_index]
  fish_mass <- fish_parm$fish_mass_g[fish_index]
  swim_speed_max <- fish_parm$swim_speed_max_m_per_s[fish_index]
  
  dt <- make_environment_dt(swim_speed_max) %>% 
    .[, ucrit := get_ucrit(fish_parm, fish_index, temperature)]
  dt$swim_speed_martin <- pmap_dbl(list(dt$velocity, dt$temperature, dt$ucrit), 
                                   ~ optimize(get_cost_of_travel, 
                                              interval =  c(..1 + 1e-6, swim_speed_max), 
                                              water_velocity_m_per_s = ..1, 
                                              fish_parm = fish_parm,
                                              fish_index = fish_index, 
                                              temperature_C = ..2, 
                                              ucrit_m_per_s = ..3,
                                              ratio = 1)$minimum)
  
  min_speeds <- dt[, .(min_speed = min(swim_speed_martin)), by = "temperature"]
  
  ucrit_cutoff <- min_speeds[dt, on = "temperature"] %>%
    .[min_speed == max(min_speed)] %>%
    .$min_speed %>%
    .[[1]]
  
  min_vel_ucrit <- dt[signif(swim_speed_martin,4) == signif(ucrit,4), .(min = min(velocity)), by = "temperature"]
  parameters_for_min_water_velocity_at_ucrit <- min_vel_ucrit[dt, on = "temperature", nomatch = 0] %>%
    .[, c("ucrit", "min")] %>%
    .[ucrit > ucrit_cutoff,] %>% 
    lm(min ~ ucrit, data = .) %>%
    .[[1]]
  
  max_vel_ucrit <- dt[signif(swim_speed_martin,4) == signif(ucrit,4), .(max = max(velocity)), by = "temperature"]
  
  water_velocity_at_max_burst <- dt[signif(swim_speed_martin,4) == signif(swim_speed_max, 4)] %>% 
    .$velocity %>% 
    min()
  
  parameters_for_max_water_velocity_at_ucrit <- max_vel_ucrit[dt, on = "temperature", nomatch = 0] %>%
    .[, c("ucrit", "max")] %>%
    lm(max ~ ucrit, data = .) %>%
    .[[1]]
  
  list(
    max_water_vel_m_per_s = water_velocity_at_max_burst,
    ucrit_cutoff_m_per_s = ucrit_cutoff,
    pars_min_water_vel_ucrit_int = parameters_for_min_water_velocity_at_ucrit[[1]],
    pars_min_water_vel_ucrit_slope = parameters_for_min_water_velocity_at_ucrit[[2]],
    pars_max_water_vel_ucrit_int = parameters_for_max_water_velocity_at_ucrit[[1]],
    pars_max_water_vel_ucrit_slope = parameters_for_max_water_velocity_at_ucrit[[2]]
  )
}

# runs through the main swim speed param function for all species 
get_swim_speed_parameters_for_all_species <- function(fish_parm){
  fish_ids <- 1:length(fish_parm$specie)
  map(fish_ids, 
             get_swim_speed_model_params, 
             fish_parm = fish_parm) %>% 
    pmap(c)
}

# makes a simulated environment with different temp and water velocity
make_environment_dt <- function(swim_speed_max){
  
  CJ(
    temperature = seq(1,25,1),
    velocity = seq(0, swim_speed_max, swim_speed_max/50))
}

# Martin et al. model of movement cost vs. distance 
get_cost_of_travel <- function(swim_speed_m_per_s, 
                               water_velocity_m_per_s, 
                               ucrit_m_per_s,
                               fish_parm,
                               fish_index,
                               temperature_C,
                               ratio) {
  
  anaerobic_fuel_recovery_parameter <- 1.82
  length = fish_parm$eg_adult_length[fish_index] 
  seconds_per_day <- 86400
  
  base_metabolic_rate <- calc_met(fish_parm,
                                  fish_index,
                                  length,
                                  temperature_C,
                                  0)/seconds_per_day

  critical_metabolic_rate <- calc_met(fish_parm,
                                      fish_index,
                                      length,
                                      temperature_C,
                                      ucrit_m_per_s)/seconds_per_day

  total_metabolic_rate <- calc_met(fish_parm,
                                   fish_index,
                                   length,
                                   temperature_C,
                                   swim_speed_m_per_s)/seconds_per_day

  (total_metabolic_rate + # aerobic component
      base_metabolic_rate * pmax(0, anaerobic_fuel_recovery_parameter * (total_metabolic_rate - critical_metabolic_rate) / (critical_metabolic_rate - base_metabolic_rate)) + # energy spent due to waiting to recover from anerobic activity
      pmax(0, (anaerobic_fuel_recovery_parameter - 1) * (total_metabolic_rate - critical_metabolic_rate))) / # cost of recovering fuel after anerobic activity
    (ratio * swim_speed_m_per_s - water_velocity_m_per_s)
}

##### I made the functions below for this script; they aren't in FHAST!!!

get_fish_params <- function(fish_parm_temp, species_used){
  fish_parm_temp %>%
    # the 1 ensures the names are kept
    dplyr::select(1,any_of(species_used)) %>%
    rename(species_temp = species) %>%
    pivot_longer(cols=c(-species_temp), names_to="specie")%>%
    pivot_wider(names_from=c(species_temp)) %>%
    as.list() %>%
    calculate_adult_parameters() %>%
    append(get_swim_speed_parameters_for_all_species(.))
}

get_tree_growth_params <- function(tree_growth_parms_in){
  tree_growth_parms_in %>%
    pivot_longer(cols = !starts_with("species"), names_to = c ("item")) %>%
    pivot_wider(names_from = "species") %>%
    rename(species = item) %>%
    mutate(species = str_replace(species, "[.]", " "),
          d_max = as.numeric(d_max),
          h_max = as.numeric(h_max),
          g = as.numeric(g),
          a = as.numeric(a),
          b = as.numeric(b),
          c = as.numeric(c),
          d = as.numeric(d),)

}

get_habitat_params <- function(hab_parm_temp, 
                                int_parm_temp,
                                dis_to_cover_params,
                                turbidity_params
                                ){
  tibble(
    # from the habitat file
    hab_bentic_ene = hab_parm_temp["benthic food energy density", ],
    hab_drift_ene = hab_parm_temp["drift food energy density", ],
    hab_benthic_hab = hab_parm_temp["benthic food habitat", ],
    cover_hab = hab_parm_temp["cover habitat", ],
    small_cover_hab = hab_parm_temp["small cover habitat", ],
    hab_drift_con = hab_parm_temp["drift food density", ],
    hab_bentic_con = hab_parm_temp["benthic food density", ],
    vel_cutoff = hab_parm_temp["velocity cutoff", ],
    dep_cutoff = hab_parm_temp["depth cutoff", ],
    resolution = hab_parm_temp["resolution", ],
    buffer = hab_parm_temp["buffer", ],
    pred_per_area = hab_parm_temp["predators per area", ],
    veg_growth_years = hab_parm_temp["vegetation growth years", ],
    # from the interaction file
    shelter_frac = int_parm_temp["cover velocity fraction", ],
    reaction_distance = int_parm_temp["temperature predator area baseline", ],
    pred_success = int_parm_temp["predator success baseline", ],
    int_pct_cover = int_parm_temp["percent cover intercept", ],
    sqrt_pct_cover = int_parm_temp["percent cover root", ],
    pct_cover = int_parm_temp["percent cover slope", ],
    sqrt_pct_cover_x_pct_cover = int_parm_temp["percent cover 3-2 root", ],
    dis_to_cover_m = dis_to_cover_params$log_b,
    dis_to_cover_int = dis_to_cover_params$log_a,
    turbidity_int = turbidity_params$log_a,
    turbidity_slope = turbidity_params$log_b,
    d84_size = int_parm_temp["d84 size", ],
    superind_ratio = int_parm_temp["superindividual ratio", ],
    ben_vel_height = int_parm_temp["benthic velocity height", ]
  ) %>%
  # make everything which is a number a doubble
    mutate(across(!c(hab_benthic_hab, cover_hab, small_cover_hab), ~as.numeric(.)))  %>%
    # Do a calculation for the law of the wall
    # all the following hard coded constants from the
    # relationship for the law of the wall
    mutate(base_wall_factor = 0.07/0.41*log((ben_vel_height/d84_size)*(30/3.5)))

}

make_predation_models <- function(pred_parm_temp){

  # make lists of parameters per file, so the dataframe can be separated appropriately
  log_model_par_names <- c("int", "shade", "veg",
                          "wood", "depth", "velocity", "substrate")
  temp_model_par_names <- c("area_pred_10", "area_pred_90")
  gape_par_names <- c("gape_a", "gape_b")
  length_dist_par_names <- c("pred_length_mean", "pred_length_sd")

  # combined all parameter lists into a list of lists to use with map
  par_lol <- list(log_model_par_names,
                  temp_model_par_names,
                  gape_par_names,
                  length_dist_par_names)

  # make a list of separate dataframes
  models_separated <- map(par_lol, ~ pred_parm_temp %>%
                            filter(term %in% .x))

  # reshape data depending on needs
  longer <- map(
    list(
      models_separated[[1]],
      models_separated[[4]]
    ),
    params_pivot_longer
  )
  wider <- map(
    list(
      models_separated[[2]],
      models_separated[[3]]
    ),
    params_pivot_wider
  )

  # make a list of lists into a single list
  df_list <- flatten(list(longer, wider))

  # final reshape to the length distribution model params
  df_list[[2]] <- df_list[[2]] %>%
    pivot_wider(names_from = "term", values_from = "estimate")
  df_list
}

pred_temperature_model_to_logistic <- function(pred_temp_params){

  converted_pred_temperature_par <- convert_logistic_parameters(
    pred_temp_params$area_pred_10,
    pred_temp_params$area_pred_90
  )

  tibble(
    area_pred_a = converted_pred_temperature_par$log_a,
    area_pred_b = converted_pred_temperature_par$log_b
  )
}

make_synth_cover_data <- function(habitat_parm){
  tibble(
    pct_cover = seq(0.01, 0.99, 0.01),
    dis_to_cover_m = habitat_parm$int_pct_cover +
      habitat_parm$sqrt_pct_cover * sqrt(pct_cover) +
      habitat_parm$pct_cover * pct_cover +
      habitat_parm$sqrt_pct_cover_x_pct_cover * pct_cover ^ 1.5
  )
}

model_cover_data <- function(cover_data){
  lm(dis_to_cover_m ~ pct_cover * sqrt(pct_cover),
                      data = cover_data)
}

make_synth_cover_benefit_data <- function(habitat_parm){
  tibble(
    dis_to_cover_m = seq(0, 5, 0.1),
    unitless_y = 1 / (1 + exp(-1 * (dis_to_cover_m * habitat_parm$dis_to_cover_m +
                                      habitat_parm$dis_to_cover_int)))
  )
}

model_cover_benefit <- function(cover_benefit_data){
  glm(unitless_y ~ dis_to_cover_m,
                       data = cover_benefit_data,
                       family = stats::quasibinomial(logit))
}


make_grid_top_marker <- function(grid_top_marker_path, base_crs) {
  st_read(grid_top_marker_path, quiet = TRUE) %>% 
  st_zm() %>% 
  st_transform(base_crs)
}