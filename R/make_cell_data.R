join_raster_and_shape <- function(raster_file, shape_file) {
  raster_file %>%
    # remove some unneeded columns
    select(-starts_with("mig_path_"),
           -bottom_area,
           -mean.correction_factor) %>% 
    left_join(select(shape_file, distance, lat_dist, aoi),
              by = c("distance", "lat_dist"))

}

make_final_habitat_df <- function(raster_joined_with_shape, spread_data) {
  raster_joined_with_shape %>%
    select(-starts_with("mean.D"),
           -starts_with("mean.V"),
           -starts_with("wetd.D")) %>%
    right_join(spread_data, by = c("lat_dist", "distance"))
}


# This function makes dataframe with info on each cell at each flow
make_cell_data <- function(){
  habitat_temp <- raster_file %>%
    # remove some unneeded columns
    select(-starts_with("mig_path_"),
           -bottom_area,
           -mean.correction_factor) %>% 
    left_join(select(shape_file, distance, lat_dist, aoi),
              by = c("distance", "lat_dist"))

  # Make the data just one depth, vel and wetted area per flow per cell
  input_variables <- c("mean.D", "mean.V", "wetd.")
  output_variables <- c("depth", "velocity", "wetted_fraction")
  spread_data <- future_map2(
    input_variables, output_variables,
    ~ spread_flows(habitat_temp, .x, .y)) %>%
    reduce(left_join, by = c("lat_dist", "distance", "flow"))
  
  #construct a data frame with all possible flows over the model area
  habitat <- habitat_temp %>%
    select(-starts_with("mean.D"),
           -starts_with("mean.V"),
           -starts_with("wetd.D")) %>%
    right_join(spread_data, by = c("lat_dist", "distance")) #%>% 
    # get only the AOI parts
    #filter(aoi == 1)
  
  # remove some unused things
  rm(spread_data, habitat_temp)
  
  return(habitat)
}

# this function spreads out the flows so they are just one depth and v per flow
spread_flows <- function(habitat_data, input_variable, output_variable) {
  spread_df <- habitat_data %>%
    # spread out the flows
    select(
      starts_with(input_variable),
      distance,
      lat_dist
    ) %>%
    pivot_longer(
      cols = starts_with(input_variable),
      names_to = "temp_flow",
      values_to = output_variable
    ) %>%
    mutate(flow = as.numeric(str_sub(string = temp_flow, start = 7))) %>%
    select(-temp_flow) %>% 
    distinct()
}

calc_open_habitat <- function(habitat_flows_all, max_flow) {
  habitat_flows_all %>% 
    filter(flow == max_flow,
          wetted_fraction != 0) %>% 
    select(lat_dist, distance)
}

calc_habitat_flow <- function(habitat_open, habitat_flows_all) {
  habitat_open %>% 
    left_join(habitat_flows_all, by = c("lat_dist", "distance"))
}

calc_fish_combos <- function(fish_parm){
  expand.grid(
  species = fish_parm$specie,
  life_stage = c("juvenile", "adult"))
}

calc_migration_area <- function(raster_file){
  raster_file %>% 
  select(starts_with("mig_path_"),
         distance, lat_dist)
}

select_fixed_habitat <- function(shape_file, habitat_open) {
  shape_file %>% 
    right_join(habitat_open,  by = c("distance", "lat_dist")) %>% 
    select(-starts_with("shade_"))
}

subset_v_and_d <- function(daily_data_set) {
  daily_data_set$v_and_d
}

select_variable_habitat <- function(v_and_d_daily_data_sets, habitat_fixed, habitat_parm) {
  # browser()
  bind_rows(v_and_d_daily_data_sets) %>%
    as_tibble() %>%
    select(-area) %>% 
    left_join(as_tibble(habitat_fixed), by = c("lat_dist", "distance", "geometry")) %>% 
    group_by(date) %>% 
    mutate(reach_preds = round(habitat_parm$pred_per_area * sum(wetted_area)),
          # Add in variable to calculate inundation
          wetted = fifelse(wetted_area > 0, 1, 0)) %>% 
    ungroup() %>%
    st_as_sf()
}

make_pred_parm <- function(predator_path) {
  read_csv(file = predator_path,
                      col_types = cols(.default = "d", species = "c")) %>%
  # the 1 ensures the names are kept
    rename(species_temp = species) %>% 
    pivot_longer(cols=c(-species_temp), names_to="specie") %>%
    pivot_wider(names_from=c(species_temp)) %>% 
    mutate(area_pred_a = convert_logistic_parameters(area_pred_10, area_pred_90)[[1]],
          area_pred_b = convert_logistic_parameters(area_pred_10, area_pred_90)[[2]]) %>%
    select(-specie) %>% 
    as.list() 
}