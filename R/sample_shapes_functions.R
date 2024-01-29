# This function takes a shape file and a grid and samples a specified column form
# the shape file on the grid. It then returns the grid with a new column of the
# values

##### sample_shape_with_grid #####
sample_shape_with_grid <- function(grid, shape_file, column_name, output_name) {
  # browser()
  # column_name <- enquo(column_name)
  # fix up the grid file
  grid <- grid %>%
    select(distance, area, left_or_right, geometry, lat_dist) %>%
    rowid_to_column("ID")

  # This suppresses a warning about attributes
  sf::st_agr(grid) <- "constant"
  sf::st_agr(shape_file) <- "constant"

  # First intersect the shape file and grid and only save the unique IDs of each
  # resulting shape
  
  grid_samples_check <- shape_file %>%
    st_intersection(grid) 
  
  if(NROW(grid_samples_check)==0){
    stop(paste0("The ", output_name, " shape does not overlap the grid.\nPlease ensure overlap of all shape files and the grid."))
  }
    
  grid_samples = grid_samples_check %>% 
    # there are duplicates
    distinct() %>%
    mutate(sample_area = as.numeric(st_area(.))) %>%
    as_tibble() %>%
    select(sample_area, {{column_name}}, ID)

  # Now take all those shapes and used a weighted average to get the average
  # (including areas without values as 0s) value over each grid cell
  # apparently just calling the variable "column_name" fixes an issue below when using the string variable as a new column name....
  # maybe when it's used above as a column reference, the string version gets over-written?
  column_name
  new_grid <- grid %>%
    as_tibble() %>%
    select(ID, area) %>%
    full_join(grid_samples, by = "ID") %>%
    replace(is.na(.), 0) %>%
    mutate(across({{column_name}}, .fns = ~ (. * sample_area), .names = "weights")) %>%
    group_by(ID) %>%
    summarize(
      value = sum(weights),
      total_area = sum(area) / n()
    ) %>%
    ungroup() %>%
    mutate({{column_name}} := round(value / total_area, 2)) %>%
    dplyr::select({{column_name}}, ID) %>%
    full_join(grid, by = "ID")
    # rename({{ output_name }} := !!quo_name(column_name))

  return(new_grid)
}

##### sample_all_shapes #####
sample_all_shapes <- function(grid = NULL,
                              shape_file = NULL,
                              column_name = NULL,
                              output_name = NULL) {

  output <- pmap(
    .l = list(shape_files, column_name, output_name),
    .f = ~ sample_shape_with_grid(grid, ...)
  ) %>%
    # Join the data frames together
    reduce(~ left_join(.x, .y, by = c("ID", "distance", "area", "left_or_right", "geometry", "lat_dist"))) %>%
    # Make it a shape file
    st_as_sf(sf_column_name = "geometry")

  return(output)
}

##### sampled_to_csv #####
sampled_to_csv <- function(sampled_shapes = NULL) {
  shapes_csv <- sampled_shapes %>%
    as_tibble() %>%
    select(-geometry, -left_or_right, -ID)
  return(shapes_csv)
}

get_habitat_subset <- function(subset){
  subset %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    str_trim(side = "both")
}

combine_all_sampled_cover_files <- function(shape_sampled_with_grid) {
  shape_sampled_with_grid %>%
      # Join the data frames together
    reduce(~ left_join(.x, .y, by = c("ID", "distance", "area", "left_or_right", "geometry", "lat_dist"))) %>%
    # Make it a shape file
    st_as_sf(sf_column_name = "geometry")
}

add_habitat_info_to_cover_file <-function(full_cover_file, habitat_parm){

  benthic_food_habitat<-get_habitat_subset(habitat_parm$hab_benthic_hab)
  cover_habitat<-get_habitat_subset(habitat_parm$cover_hab)
  small_cover_habitat<-get_habitat_subset(habitat_parm$small_cover_hab)

  full_cover_file %>%
        mutate(ben_food_fra = rowSums(across(benthic_food_habitat)),
           cover_fra = rowSums(across(cover_habitat)),
           small_cover_fra = rowSums(across(small_cover_habitat)))
}

add_aoi_to_shape <- function(aoi_path, grid_file, sampled_shapes) {
    # Add in the AOI
  if(is.na(aoi_path)){
    sampled_w_aoi <- sampled_shapes %>%
      mutate(aoi = 1) %>% 
      st_as_sf(sf_column_name = "geometry") 
  } else {
    # Load the aoi shape
    aoi_shape <- st_read(aoi_path, quiet = TRUE) %>% 
    mutate(aoi = 1) %>% 
    dplyr::select(aoi)
    
    sampled_w_aoi <- sample_shape_with_grid(grid_file, aoi_shape, "aoi", "aoi") %>%
      mutate(aoi = fifelse(aoi>0,1,0)) %>% 
      dplyr::select(aoi, ID) %>% 
      right_join(sampled_shapes, by = c("ID")) %>% 
      st_as_sf(sf_column_name = "geometry")
  }
}

full_cover_sample <- function(habitat_parm, shade_file, grid_file, aoi_path, cover_shape){
  # get what is classified as benthic food habitat
  benthic_food_habitat <- habitat_parm$hab_benthic_hab %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    str_trim(side = "both")
  # get what is classified as cover habitat
  cover_habitat <- habitat_parm$cover_hab %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    str_trim(side = "both")
  # get what is classified as small cover habitat
  small_cover_habitat <- habitat_parm$small_cover_hab %>% 
    str_split(",") %>% 
    pluck(1) %>% 
    str_trim(side = "both")
  
  # make a list of files and variable names
  cover_names <- list("veg", "wood", "fine", "gravel", "cobble", "rock")
  
  # Get a list of data frames with just one for each df that 
  shape_dfs <- map(cover_names, ~dplyr::select(cover_shape, matches(.x))) 
  
  the_variables <- c(cover_names, as.list(paste0("shade_", seq(1,12,1))))
  shape_files <- c(shape_dfs, shade_file)
                    
  # Sample all the shapes over the grid
  sampeled_shapes <- sample_all_shapes(grid_file,
                                      shape_files,
                                      the_variables,
                                      the_variables) %>%
    # Add in column fo benthic food, cover and small cover
    mutate(ben_food_fra = rowSums(across(benthic_food_habitat)),
           cover_fra = rowSums(across(cover_habitat)),
           small_cover_fra = rowSums(across(small_cover_habitat)))
  
  
  # Add in the AOI
  if(is.na(aoi_path)){
    sampeled_w_aoi <- sampeled_shapes %>%
      mutate(aoi = 1) %>% 
      st_as_sf(sf_column_name = "geometry") 
  } else {
    # Load the aoi shape
    aoi_shape <- st_read(aoi_path, quiet = TRUE) %>% 
    mutate(aoi = 1) %>% 
    dplyr::select(aoi)
    
    sampeled_w_aoi <- sample_shape_with_grid(grid_file, aoi_shape, "aoi", "aoi") %>%
      mutate(aoi = fifelse(aoi>0,1,0)) %>% 
      dplyr::select(aoi, ID) %>% 
      right_join(sampeled_shapes, by = c("ID")) %>% 
      st_as_sf(sf_column_name = "geometry")
  }
  
}