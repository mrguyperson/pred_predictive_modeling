add_group_flow_values <- function(daily_path, flow_means){
  pivoted <- load_text_file(daily_path)  %>% 
    as_tibble(rownames = "rownames") %>% 
    # mutate(rownames = str_replace(rownames, " ", "_") %>% 
    #                   str_remove(" "), 
    #       rownames = fifelse(rownames == "type_", "type", rownames)
    #                   ) %>% 
    pivot_wider(names_from = "rownames")

  map_dfr(pivoted, ~rep(.x, times = length(flow_means))) %>% 
    mutate('flow mean ' := flow_means)
    # group_by(flow_mean) %>% 
    # tar_group()
}

reshape_daily_inputs <- function(daily_inputs){
    daily_inputs  %>% 
        mutate(across(.cols = everything(), .fns = as.character)) %>% 
        # rename_with(.cols = everything(), ~str_replace(., "_", " ")) %>% 
        pivot_longer(cols = everything(), values_to = "value", names_to = "rownames") %>% 
        column_to_rownames(var = "rownames")

}

make_pathfinding_map <- function(data_frame_in, stamen_map, flow_mean){
  df <- data_frame_in %>% 
    split(by = c("species")) %>% 
    map(st_as_sf)
  
  color_palette <- "viridis"
  scale_name <- "Frequency\nof Use"

  maps <- map(df, function(x)
    ggmap(stamen_map) +
      coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
      theme_bw(base_size = 25) +
      theme(axis.text = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank()
            ) +

      geom_sf(data = st_transform(x, crs = 3857), 
              aes(fill = num_paths, color = num_paths),
              inherit.aes = FALSE) +
      scale_fill_viridis(
        option = color_palette, 
        name = scale_name
        ) +
      scale_color_viridis(
        option = color_palette, 
        name = scale_name
        )
      # ggtitle(glue::glue('{str_replace_all(paste0(x$species[1]),
      #                         c("-" = " ", "_" = " "))}, mean flow: {flow_mean} cu. m/s'))
  )
  }

# Define a function to fix the bbox to be in EPSG:3857
ggmap_bbox <- function(map) {
  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector, 
  # and set the names to what sf::st_bbox expects:
  map_bbox <- setNames(unlist(attr(map, "bb")), 
                       c("ymin", "xmin", "ymax", "xmax"))

  # Coonvert the bbox to an sf polygon, transform it to 3857, 
  # and convert back to a bbox (convoluted, but it works)
  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))

  # Overwrite the bbox of the ggmap object with the transformed coordinates 
  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
  map
}

make_stamen_map_file <- function(grid_file){
  bbox_vals <- st_bbox(st_transform(grid_file, crs = 4326))

  get_stamenmap(
    bbox = c(left = bbox_vals$xmin[[1]], 
              bottom = bbox_vals$ymin[[1]], 
              right = bbox_vals$xmax[[1]], 
              top = bbox_vals$ymax[[1]]
              ),
    zoom = 15,
    maptype = "terrain",
    color = "color") %>% 
  ggmap_bbox()
}

adult_migr_energy_plots <- function(adult_energy_cost) {
  plot <- list(ggplot(adult_energy_cost) +
    theme_classic(base_size = 20) +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    geom_density(aes(x = energy_cost / 1000,
                     y = ggplot2::after_stat(scaled)),
                 fill = "grey") +
    labs(x = "Energy Cost (kJ)") +
    facet_wrap(species ~ ., scales = "free", ncol = 1))
}

summarize_adult_energy <- function(adult_energy_cost) {
  adult_energy_cost[,
  .(
    mean_energy_cost = mean(energy_cost / 1e3),
    stderr_energy_cost = sd(energy_cost / 1e3),
    total_paths = .N
    ), 
    by = c("flow", "species")]
}


make_velocity_map <- function(data_frame_in, stamen_map, min_max_velocities){
  df <- data_frame_in %>% 
    st_as_sf()

  color_palette <- "mako"
  scale_name <- "Velocity\n(m/s)"
  ggmap(stamen_map) +
    coord_sf(crs = st_crs(3857)) + # force the ggplot2 map to be in 3857
    theme_bw(base_size = 25) +
    theme(axis.text = element_blank(),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks = element_blank()
          ) +
    geom_sf(data = st_transform(df, crs = 3857), 
            aes(fill = velocity, color = velocity),
          inherit.aes = FALSE) +
    scale_fill_viridis(
      # limits = min_max_velocities, 
      option = color_palette, 
      name = scale_name) +
    scale_color_viridis(
      # limits = min_max_velocities, 
      option = color_palette, 
      name = scale_name) 
    # ggtitle(glue::glue('Water velocity, mean flow: {flow_mean} cu. m/s'))
  }

  save_plot <- function(plot, flow_mean){
    filename <- glue::glue()
    ggsave()
  }

  get_min_max_velocity <- function(depth_and_velocity){
    data.table(velocity = range(depth_and_velocity$velocity))
  }

  extract_string_from_obj <- function(obj){
    deparse(substitute(obj))
  }

  make_path_vel_fig <- function(path1, vel1){

    a <- path1$chinook
    # b <- path2$chinook
    c <- vel1
    # d <- vel2

    plots <- list(
      a,
      # b,
      c
      # d
      )

    arrange <- ggarrange(
      plotlist = plots,
      labels = c("A)", "B)"),
      legend = "none",
      nrow = 2
    )
      # annotate(
      #   "text",
      #   x = 0.25,
      #   y = 1,
      #   label = glue::glue("{low_flow_val} cu. m/s")
      # ) +
      # annotate(
      #   "text",
      #   x = 0.75,
      #   y = 1,
      #   label = glue::glue("{high_flow_val} cu. m/s")
      # )
    legend_1 <- get_legend(a)
    legend_2 <- get_legend(c)
    legends <- ggarrange(legend_1, legend_2, nrow = 2, align = "v")


    final <- ggarrange(arrange, legends, widths = c(3, 1), heights = c(3, 2))
    filename <- "poster_files/path_vel_fig.png"
    ggsave(filename, final, width = 7, height = 7, units = "in")
    filename

  }

get_pathing_complexity <- function(all_paths){
  unique(all_paths, by = c("distance", "lat_dist", "flow_mean")) %>% 
  .[, .(count = .N), by = "flow_mean"] %>% 
  .[, ":=" (
    flow_mean = as.numeric(stringr::str_extract(flow_mean, "\\d+")),
    path_complexity = count/max(count)
    )]
}

make_pathing_complexity_figure <- function(pathing_complexity_data){
  ggplot(data = pathing_complexity_data, 
          aes(x = flow_mean, y = path_complexity))+
    geom_point(size = 3) +
    # geom_line(aes(y = predictions)) +
    theme_classic(base_size = 25) +
    xlab("Flow mean (cu. m/s)") +
    ylab("Relative pathing complexity")
}

make_mean_energy_fig <- function(adult_energy_summary){
  ggplot(data = adult_energy_summary,
        aes(x = flow, y = mean_energy_cost)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = mean_energy_cost - stderr_energy_cost,
                      ymax = mean_energy_cost + stderr_energy_cost),
                      width = .1) +
    theme_classic(base_size = 25) +
    xlab("Flow mean (cu. m/s)") +
    ylab("Mean energy cost (kJ)")
}

