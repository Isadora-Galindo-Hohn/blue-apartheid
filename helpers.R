check_file <- function(filename) {
  # Check if shapefile exists
  if (!file.exists(municipality_shp_file)) {
    warning(paste0(
      "File not found for",
      filename,
      ": ",
      "Skipping map for this year/variable."
    ))
    return(NULL)
  }
  return(municipality_shp_file)
}


# Helper function to fit linear models for each year and dependent variable
run_model <- function(df, y_var, year) {
  df_filtered <- df %>%
    filter(
      !is.na(.data[[y_var]]),
      !is.na(income),
      !is.na(dominent_pop_group),
      !is.na(kl_divergence)
    )

  if (nrow(df_filtered) < 10) {
    message(
      "Skipping ",
      y_var,
      " for year ",
      year,
      ": not enough valid data rows (",
      nrow(df_filtered),
      ")"
    )
    return(NULL)
  }

  formula <- as.formula(paste(
    y_var,
    "~ log(income) + dominent_pop_group + kl_divergence"
  ))

  model_result <- tryCatch(
    lm(formula, data = df_filtered),
    error = function(e) {
      message(
        "Error fitting model for ",
        y_var,
        " year ",
        year,
        ": ",
        e$message
      )
      return(NULL)
    },
    warning = function(w) {
      message(
        "Warning fitting model for ",
        y_var,
        " year ",
        year,
        ": ",
        w$message
      )
      invokeRestart("muffleWarning")
      return(lm(formula, data = df_filtered))
    }
  )
  return(model_result)
}

load_and_preprocess_yearly_data <- function(years, data_path) {
  map_dfr(years, function(y) {
    file <- paste0(data_path, "data_", y, ".csv")
    df <- read.csv2(file, fileencoding = "latin1", stringsasfactors = false)

    # robust numeric conversion for all relevant columns
    df$avrage_income_bracket <- df$avrage_income_bracket %>%
      as.character() %>%
      str_replace_all(",", ".") %>%
      suppresswarnings(as.numeric(.))
    df$total_pop <- df$total_pop %>%
      as.character() %>%
      str_replace_all(",", ".") %>%
      suppresswarnings(as.numeric(.))
    df$interruption_freq <- df$interruption_freq %>%
      as.character() %>%
      str_replace_all(",", ".") %>%
      suppresswarnings(as.numeric(.))
    df$dist_over_200 <- df$dist_over_200 %>%
      as.character() %>%
      str_replace_all(",", ".") %>%
      suppresswarnings(as.numeric(.))
    df$kl_divergence <- df$kl_divergence %>%
      as.character() %>%
      str_replace_all(",", ".") %>%
      suppresswarnings(as.numeric(.))

    df$year <- y # add year column
    return(df)
  })
}
