get_shade_location <- function(shape_file_in, crs = "EPSG:4326"){
  shape_file_in %>% 
    summarize(geometry = st_union(geometry)) %>% 
    st_centroid() %>% 
    st_transform(st_crs(crs))
}

convert_time <- function(time, location) {
  as.POSIXct(
    x = time,
    tz = lutz::tz_lookup(location, method = "accurate")
  )
}

# Make a function to make the shade file
make_shade_shape = function(shape_file_in,
                            time,
                            location){

  # Get the location for the center of the shape and put it in EPSG 4326
  # to allow the solar calculator to work
  # location = shape_file_in %>% 
  #   summarize(geometry = st_union(geometry)) %>% 
  #   st_centroid() %>% 
  #   st_transform(st_crs("EPSG:4326"))
  
  # Get the time and time zone based on location
  # time = as.POSIXct(
  #   x = time_in,
  #   tz = tz_lookup(location, method = "fast")
  # )
  
  # get the sunset time
  set = maptools::sunriset(
    crds =st_coordinates(location),
    dateTime = time,
    proj4string=CRS(st_crs(location)$proj4string),
    direction="sunset",
    POSIXct.out=TRUE
  )
  
  # get the sunrise time
  rise = maptools::sunriset(
    crds = st_coordinates(location),
    dateTime = time,
    proj4string=CRS(st_crs(location)$proj4string),
    direction="sunrise",
    POSIXct.out=TRUE
  )
  
  # Get a set number of times in between rise and set and drop the 
  # rise and set times
  times_seq = seq(rise$time, set$time, length.out = 5)
  adjusted_times = times_seq[-c(1,length(times_seq))]
  
  make_1_day_shade = function(time_in, location_in, shape_in){

    # calculate the solar angel
    solar_pos = solarpos(
      crds = st_coordinates(location_in),
      dateTime = time_in,
      proj4string=CRS(st_crs(location_in)$proj4string)
    )
    
    # calculate the foot print of the shade
    # the height attribute needs to be in the same units as the crs
    footprint = shadowFootprint(
      obstacles = as(shape_in, "Spatial"),
      obstacles_height_field = "height",
      solar_pos = solar_pos) %>% 
      st_as_sf() %>% 
      mutate(id = 1:n()) %>% 
      select(geometry, id) %>% 
      mutate(shade = 1,
             month = month(time_in),
             hour = hour(time_in))
    
    return(footprint)
  }

  output = map_dfr(adjusted_times,
                   ~make_1_day_shade(.x, location, shape_file_in))
  
  return(output)
}

full_shade_shape_calculations <- function(grid_file, 
                                  grid_center_line, 
                                  canopy_shape, 
                                  tree_growth_parms, 
                                  habitat_parm,
                                  juvenile_run)
                                  {
  
  shadeError <- FALSE
  tryCatch({                       
    simplify_tolerance <- habitat_parm$resolution/2
    
    clip_mask <- grid_file %>%
      summarise() %>% 
      st_buffer(dist = 100)
    
    # Load the canopy cover zone file 
    # simplify it to speed up
    if(juvenile_run == TRUE){
      shade_shape = canopy_shape %>% 
        st_intersection(clip_mask) %>% 
        # Filter out empty ones
        filter(!st_is_empty(.)) %>% 
        grow_trees(parms = tree_growth_parms,
                  years = habitat_parm$veg_growth_years) %>%
        st_simplify(dTolerance = simplify_tolerance) %>%
        group_by(height) %>%
        summarize() %>% 
        ungroup()
    } else {
      # Make a dummy shape for shade if just adults in the run 
      shade_shape <- grid_center_line %>% 
        st_centroid() %>% 
        mutate(height = 1) %>% 
        dplyr::select(height) %>% 
        st_buffer(1, nQuadSegs = 2)
    }
    
    # Make a list on months but in time format
    # also add in an arbitrary year and time
    # times_list <- as.list(paste0("2010-", seq(1,12,1), "-15 12:00:00"))
    
    ##### removed future map here!
    # output <- map(times_list, ~make_shade_shape(shade_shape, .x)) %>% 
    #   map(~summarise(.x, shade = sum(shade)/sum(shade), do_union = TRUE)) %>% 
    #   map2(seq(1,12,1), ~rename(.x, !!paste0("shade_", .y) := shade)) %>% 
    #   map(st_as_sf)

  },
  error = function(e) {
           print(e)
           shadeError <- TRUE
           }
  )
  if (shadeError) {
    stop('Error while generating shade, check canopy cover shape file. The canopy shape file has two attributes per shape, a height, and an optional species (used for growth). The height is used in the shade calculations and the species is used when the tree growth module is used to determine the change in the height of the new shape and the area that new shape covers. ')
  } else {
    return(shade_shape)
  }
}

summarize_shade_calcs <- function(shade_monthly_shape) {
  summarise(shade_monthly_shape, shade = sum(shade)/sum(shade), do_union = TRUE)
}

rename_shade_cals <- function(month_no, shade_calc) {
  rename(shade_calc, !!paste0("shade_", month_no) := shade)
}