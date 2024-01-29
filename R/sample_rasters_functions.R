# This folder contains the functions for sample_raster.R

##### get_type_letter #####
# this function translates the type word into the letter
get_type_letter = function(type = NULL){
  if(type == "depth"){
    type_letter = "D"
  } else if (type == "velocity"){
    type_letter = "V"
  } else {
    stop("Invalid type of raster. Choose 'depth' or 'velocity'.")
  }
  
  return(type_letter)
}

##### load_rasters #####
# This function loads 
load_rasters = function(type = NULL,
                        folder = NULL,
                        clip_mask = NULL){

    # Get the extent of the grid
  extent = terra::ext(clip_mask)

  # Get the letter type
  type_letter = get_type_letter(type)

  # Find the files that match the type.
  files = list.files(folder, paste0(type_letter, "\\d+.tif"), full.names=TRUE)
  # Sort the files so they are in flow order of low to high.
  files_sorted = files[order(strtoi(str_extract(str_extract(files, paste0(type_letter, "\\d+.tif")), "\\d+")))]
  # make the raster stack and crop it
  stack = rast(map(files_sorted, ~rast(.x))) 

  if(type_letter == "D"){
    # Use terrain to calculate the slope and the correction for area
    original_raster = rast(tail(files_sorted, n=1))
    slope_raster = original_raster %>% 
      terra::crop(extent) %>% 
      terrain(v="slope", neighbors = 4, unit = "radians")  %>%
      app(fun = function(x){1/cos(x)}) %>% 
      terra::extend(original_raster)
    # Give the value a name
    names(slope_raster) = "correction_factor"

    stack = stack %>%
      c(slope_raster)

  }

  return(stack)
}

# function to calculate the proportion of the area that has depth > 0 for each flow tiff
get_wetted_area <- function(df, ...){
  df %>%
    # selects all the depth or velocity columns
    summarize(across(-c("correction_factor", "coverage_fraction"), 
                     .fns = ~ sum(coverage_fraction[.x > 0]) / sum(coverage_fraction),
                     .names = "wetd.{.col}"))
  
}


##### sample_grid #####
# This function samples the raster stack using the river grid
# if depth is selected it also gets the bottom area
sample_grid = function(stack = NULL,
                       grid = NULL,
                       flows = NULL,
                       type = NULL){

  type_letter = get_type_letter(type)
  select <- dplyr::select
  samples = stack %>% 
    #take mean excluding NAs
    exact_extract(grid,
                  fun = 'mean',
                  progress = FALSE) %>%  
    data.frame() %>% 
    # bind it back to the polygons
    cbind(data.frame(grid)) %>% 
    # set left and right bank correctly
    # mutate(lat_dist = ifelse(lat_dist>0,ifelse(left_or_right>0, lat_dist, -lat_dist),0)) %>% 
    # Convert to simple feature then a df and remove uncessary rows
    data.frame() %>% 
    dplyr::select(all_of(paste0("mean.", type_letter, flows)),
                  lat_dist,
                  # if velocity the correction factor is not inside 
                  contains("mean.correction_factor"),
                  distance,
                  area) 
  

  if(type_letter == "D"){
    
    wetter_area = stack %>%
      exact_extract(grid,
                    fun = NULL,
                    progress = FALSE,
                    default_value = 0) %>%
      #removed future_map_dfr
      map_dfr(get_wetted_area) %>%
      as_tibble()
    
    samples = samples %>% 
      mutate(bottom_area = ifelse(is.na(mean.correction_factor), area, area*(mean.correction_factor))) %>% 
      bind_cols(wetter_area)
  }
  
  return(samples)
}

full_grid_sampling <- function(raster_folder, grid_file, fish_parm){
  # conflicted::conflict_prefer("select", "dplyr")
    # find all the depth and velocity rasters
  d_files <- list.files(raster_folder, "D\\d+.tif", full.names=TRUE)
  v_files <- list.files(raster_folder, "V\\d+.tif", full.names=TRUE)
  # remove just the values from the file lists.
  d_values <- stringr::str_remove(d_files, raster_folder) %>% 
    stringr::str_sub(start = 3) %>% 
    stringr::str_extract(".*(?=\\.)") %>% 
    as.numeric() %>% 
    sort()
  v_values <- stringr::str_remove(v_files, raster_folder) %>% 
    stringr::str_sub(start = 3) %>% 
    stringr::str_extract(".*(?=\\.)") %>% 
    as.numeric() %>% 
    sort()

  ##### Main Part #####
  # Put all the rasters in a stack
  raster_stack_d <- load_rasters(type = "depth",
                                folder = raster_folder,
                                clip_mask = grid_file)
  
  
  # Put all the rasters in a stack
  raster_stack_v <- load_rasters(type = "velocity",
                                folder = raster_folder,
                                clip_mask = grid_file) 


  # Check that teh values all match
  if (!(all(d_values==v_values))) {
    stop('The velocity and depth flow values do not mtach.')
  }
  # Check that all CRSs are the same
  if (!(raster::compareCRS(raster_stack_v, raster_stack_d) &
        raster::compareCRS(crs(grid_file), crs(raster_stack_d)))) {
    stop('The CRSs of some of your files are not the same.')
  }

  # Sample the grid over the raster stack
  sampled_grid_d <- sample_grid(stack = raster_stack_d,
                                grid = grid_file,
                                flows = d_values,
                                type = "depth")
  
  # Sample the grid over the raster stack
  sampled_grid_both <- sample_grid(stack = raster_stack_v,
                              grid = grid_file,
                              flows = d_values,
                              type = "velocity") %>% 
    left_join(sampled_grid_d, by = c("lat_dist", "distance", "area"))
  
  # Add in the accessible migration cells for each species
  # Find the flow level
  sampled_grid <- map2_dfc(.x = fish_parm$specie,
                           .y = fish_parm$adult_migration_min_flow,
                           .f = ~ make_migration_paths(flows = d_values,
                                                       flow_level = .y,
                                                       species = .x,
                                                       df_in = sampled_grid_both)) %>% 
    bind_cols(sampled_grid_both)

}