# --- Map Generation Function ---
generate_and_save_map <- function(
  year,
  variable_name,
  map_title,
  data_source = clean_data
) {
  shp_path_maps <- "../QGIS/"
  # Construct shapefile path for the current year
  current_shp_file <- check_file(paste0(
    shp_path_maps,
    year,
    "_ward_data.shp"
  ))
  # Construct shapefile path for the current year
  municipality_shp_file <- check_file(paste0(
    shp_path_maps,
    "M",
    year,
    ".gdtable"
  ))
  if (is.null(current_shp_file) || is.null(municipality_shp_file)) {
    return(NULL)
  }
  # Construct shapefile path for the current year
  # municipality_shp_file <- check_shapefile(paste0(
  #   shp_path_maps,
  #   year,
  #   "_manicipality.shp"
  # ))
  # if (is.null(municipality_shp_file)) {
  #   return(NULL)
  # }

  # Load year-specific ward shapefile
  current_wards_sf <- read_sf(current_shp_file)
  # municipality_sf <- read_sf(municipality_shp_file)

  # Shapefile includes manicupalities for whole south africa
  # Filter for gauteng
  # gauteng_municipalities_sf <- municipality_sf %>%
  #   filter(PROVINCE_CODE == "GT")

  # Filter data for the current year
  current_data <- data_source %>% filter(year == .env$year)

  # --- Data Preprocessing for Specific Map Types ---
  map_var_aes <- NULL # Placeholder for aesthetic mapping
  scale_fn <- NULL # Placeholder for scale function
  legend_name <- "" # Placeholder for legend title

  # Dominant population group
  if (variable_name == "dominent_p") {
    # Harmonize and set factor levels, including a 'No Data' level
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = case_when(
          is.na(dominent_p) ~ "No Data",
          dominent_p == "Asian/Indian" ~ "Indian/Asian", # Harmonize
          dominent_p == "Indian or Asian" ~ "Indian/Asian", # Harmonize
          TRUE ~ as.character(dominent_p)
        ),
        map_var = factor(map_var, levels = c(names(group_colors), "No Data")) # Ensure No Data is last
      )
    # Define custom colors including "No Data"
    map_colors <- c(group_colors, "No Data" = "grey90") # Use a light grey for no data
    scale_fn <- scale_fill_manual(
      values = map_colors,
      na.translate = FALSE,
      name = "Dominant\nGroup"
    )
    legend_name <- "Dominant Population Group"
    map_var_aes <- sym("map_var") # Use the new 'map_var' column
  } else if (variable_name == "income_bracket") {
    # Income bracket
    # Create income brackets from numerical 'income' using the dynamically generated breaks/labels
    breaks = c(-3, -2, -1, 0, 200, 600, 1200, 2400, 4800, 9600, Inf)
    labels = c(
      "No Data",
      "Respondent refused or did not know",
      "No Income",
      "R0 - R200",
      "R200 - R600",
      "R600 - R1200",
      "R1200 - R2400",
      "R2400 - R4800",
      "R4800 - R9600",
      "R9600+"
    )
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = case_when(
          is.na(avrage_inc) ~ "No Data",
          avrage_inc == "NaN" ~ "Respondent refused or did not know",
          avrage_inc == 0 ~ "No Income",
          TRUE ~
            as.character(cut(
              parse_number(avrage_inc),
              breaks = breaks,
              labels = labels,
              include.lowest = TRUE,
              right = FALSE
            ))
        ),
        map_var = factor(map_var, levels = labels)
      )

    map_colors <- setNames(
      c("grey80", "red", get_greens_palette(length(labels) - 2)),
      labels
    )
    scale_fn <- scale_fill_manual(
      values = map_colors,
      na.translate = FALSE,
      name = "Average\nIncome",
      drop = TRUE
    )
    legend_name <- "Average Income Bracket"
    map_var_aes <- sym("map_var")
  } else if (variable_name == "avrage_ace") {
    # Average access to water
    # Ensure it's a factor and handle NA
    labels <- c(
      "Piped, into dwelling",
      "Piped, into yard only",
      "Street taps or standpipes",
      "Other"
    )
    manual_colors <- c("darkblue", "blue", "lightblue", "grey50")
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = factor(current_wards_sf$avrage_ace, levels = labels)
      )

    # 3. Use scale_fill_gradientn with breaks and labels
    scale_fn <- scale_fill_manual(
      values = setNames(manual_colors, labels),
      na.value = "grey90",
      name = "Average Water Access"
    )

    legend_name <- "Average Water Access"
    map_var_aes <- sym("map_var") # if using tidy evaluation
  } else if (variable_name == "pop_density") {
    # Population dencity
    # Calculate population density (total_pop / area)
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = (as.numeric(total_pop) /
          (st_area(.) %>% as.numeric() * 10e-6))
      )
    breaks <- c(0, 1, 3, 10, 30, 100, 300, 1000, 3000, Inf)
    manual_colors <- rev(heat.colors(length(breaks) - 1))
    labels <- c(
      "<1",
      "1 - 3",
      "3 - 10",
      "10 - 30",
      "30 - 100",
      "100 - 300",
      "300 - 1000",
      "1000 - 3000",
      ">3000"
    )
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var_bin = cut(
          map_var,
          breaks = breaks,
          include.lowest = TRUE,
          labels = labels,
          dig.lab = 10
        )
      )
    scale_fn <- scale_fill_brewer(
      palette = "YlOrRd",
      na.value = "grey80",
      name = "Pop. Density\n(per km\u00b2)",
      drop = FALSE
    )
    legend_name <- "Population Density"
    map_var_aes <- sym("map_var_bin")
  } else if (variable_name == "kl_diverge") {
    breaks <- c(0, 0.5, 1, 1.5, 2, 100)
    labels <- c("Very low", "Low", "Moderate", "High", "Very high")
    colors <- rev(heat.colors(length(breaks) - 1))
    # KL-divergence
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = cut(
          parse_number(.data[[variable_name]]),
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE,
          right = FALSE
        ),
      )
    scale_fn <- scale_fill_brewer(
      palette = "YlOrRd",
      na.value = "grey80",
      name = "Racial Segregation (KL Divergence)",
      drop = FALSE
    )
    legend_name <- "Racial Segregation (KL Divergence)"
    map_var_aes <- sym("map_var")
  } else if (variable_name == "interrupti") {
    # Water interruption frequency
    breaks <- c(-1, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, Inf)
    labels <- c(
      "0-10%",
      "10-20%",
      "20-30%",
      "30-40%",
      "40-50%",
      "50-60%",
      "60-70%",
      "70-80%",
      "80-90%",
      "90-100%"
    )
    labels_with_no_data = c(labels, "No Data")
    current_wards_sf <- current_wards_sf %>%
      mutate(
        # Step 1: cut() for numeric bins only
        map_var = cut(
          parse_number(current_wards_sf$interrupti),
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE,
          right = FALSE,
          limit = labels_with_no_data
        ),
        map_var = factor(map_var, levels = labels_with_no_data),
        # # Step 2: Assign "No Data" for NA values
        # map_var = ifelse(is.na(map_var), "No Data", as.character(map_var)),
        # # Step 3: Make it a factor with all levels
        # map_var = factor(map_var, levels = c(labels, "No Data"))
      )
    base_colors <- brewer.pal(9, "YlOrRd")
    extra_color <- colorRampPalette(c(base_colors[9], "#4d0000"))(2)[2] # Create a deeper red
    map_colors <- c(base_colors, extra_color, "No Data" = "gray80")
    # names(map_colors) <- labels
    scale_fn <- scale_fill_manual(
      values = setNames(map_colors, labels_with_no_data),
      na.value = "grey80",
      name = "Interruption\nFrequency",
      drop = FALSE,
      limits = labels
    )
    legend_name <- "Water Interruption Frequency"
    map_var_aes <- sym("map_var")
  } else if (variable_name == "dist_over_") {
    # Distance over 200m
    breaks <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
    labels <- c(
      "0-10%",
      "10-20%",
      "20-30%",
      "30-40%",
      "40-50%",
      "50-60%",
      "60-70%",
      "70-80%",
      "80-90%",
      "90-100%"
    )
    current_wards_sf <- current_wards_sf %>%
      mutate(
        map_var = cut(
          parse_number(current_wards_sf$dist_over_),
          breaks = breaks,
          labels = labels,
          include.lowest = TRUE,
          right = FALSE
        )
      )
    scale_fn <- scale_fill_brewer(
      palette = "YlOrRd",
      na.value = "grey80",
      name = "Share >200m\nDistance",
      drop = TRUE
    )
    legend_name <- "Share of Households >200m from Water"
    map_var_aes <- sym("map_var")
  } else {
    warning(paste0(
      "Unknown variable_name for mapping: ",
      variable_name,
      ". Skipping."
    ))
    return(NULL)
  }

  # --- Create the Plot --- p <- ggplot(current_wards_sf) +
  geom_sf(aes(fill = !!map_var_aes), color = "white", linewidth = 0.1) + # Ward borders
    geom_sf(
      data = municipality_sf,
      fill = NA,
      color = "black",
      linewidth = 0.7
    ) + # Municipality borders
    scale_fn + # Apply the specific scale function
    labs(
      title = paste0(map_title, " (", year, ")"),
      caption = "Source: Your Study Data"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
      legend.position = "right", # Default position
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(), # Remove axis text (lat/lon)
      axis.title = element_blank() # Remove axis labels
    ) +
    # Add Scale Bar
    ggspatial::annotation_scale(
      location = "br", # Bottom right
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      text_cex = 0.8
    ) +
    # Add North Arrow
    ggspatial::annotation_north_arrow(
      location = "tl", # Top left
      which_north = "true", # "true" for true north, "grid" for grid north
      pad_x = unit(0.5, "cm"),
      pad_y = unit(0.5, "cm"),
      style = ggspatial::north_arrow_fancy_orienteering(text_size = 8)
    )

  # --- Save the Map ---
  output_filename <- file.path(
    OUTPUT_DIR,
    paste0("Map_", year, "_", gsub(" ", "_", variable_name), ".png")
  )
  ggsave(output_filename, p, width = 9, height = 8, dpi = 300)
  message(paste0("Map saved: ", output_filename))

  return(p) # Return the plot object (optional)
}
