# Load libraries
library(tidyverse) # For data manipulation (dplyr, readr, purrr) and plotting (ggplot2)
library(broom) # For tidy() to extract model coefficients
library(forcats) # For fct_drop (useful for factor levels)
library(RColorBrewer) # For color palettes
library(stats) # Explicitly load stats for quasibinomial if not already loaded by tidyverse
library(kableExtra) # Load the library
library(writexl) # Load the library
library(sf) # For spatial data operations
library(ggplot2) # For plotting
library(dplyr) # For data manipulation
library(viridis) # For nice color palettes (e.g., for continuous data)
library(purrr)
library(RColorBrewer)
library(ggplot2)
library(patchwork)

source("helpers.R")
source("maps.R")
source("constants.R")

# Set working directory and data path
setwd(".")
data_path <- "../" 
years <- c(2009, 2011, 2014, 2016, 2018, 2022, 2024)

# Define years for each dependent variable upfront
years_interrupt <- c(2018, 2022, 2024)
years_distance <- c(2009, 2011, 2014, 2016, 2018)

# Creating output folder
OUTPUT_DIR <- "output"
if (!dir.exists(OUTPUT_DIR)) {
  dir.create(OUTPUT_DIR)
}

# Load and preprocess all yearly data files
all_data <- load_and_preprocess_yearly_data(years, data_path)

# Clean column names and ensure correct data types
all_data <- all_data %>%
  rename_with(tolower) %>% # Convert all column names to lowercase
  mutate(
    # Standardize 'Indian or Asian' and 'Asian/Indian' to 'Indian/Asian' before converting to factor
    dominent_pop_group = case_when(
      dominent_pop_group == "Indian or Asian" ~ "Indian/Asian",
      dominent_pop_group == "Asian/Indian" ~ "Indian/Asian", # Added this line for consistency
      TRUE ~ dominent_pop_group
    ),
    dominent_pop_group = fct_drop(as.factor(dominent_pop_group)), # Convert to factor, drop unused levels
    average_access_to_water = fct_drop(as.factor(avrage_acess_to_water)),
    income = as.double(avrage_income_bracket),
    non_white = as.numeric(non_white),
    share_dom = as.numeric(share_dom),
    equal_distru = fct_drop(as.factor(equal_distru)),
    dist_over_200 = as.numeric(dist_over_200),
    interruption_freq = as.numeric(interruption_freq),
    total_pop = as.numeric(total_pop)
  ) %>%
  # Select and order final columns for analysis
  select(
    wardid,
    year,
    dominent_pop_group,
    income,
    non_white,
    share_dom,
    equal_distru,
    average_access_to_water,
    dist_over_200,
    interruption_freq,
    total_pop
  )

# Filter data for regression: remove NAs in key predictors and handle log(income)
clean_data <- all_data 

# --- Define custom income axis breaks and labels for plots ---
# These are the midpoints from your income bracket definition
income_midpoints_numeric <- c(
  NA,
  "NaN",
  0,
  200,
  600,
  1200,
  2400,
  4800,
  9600,
  19200,
  38400,
  76800,
  153600,
  300000
)
# Corresponding labels (can be original brackets or just the midpoints)
income_labels_text <- c(
  "No data",
  "Refuse or Don't know",
  "No Income",
  "R1-R400",
  "R401-R800",
  "R801-R1.6k",
  "R1.6k-R3.2k",
  "R3.2k-R6.4k",
  "R6.4k-R12.8k",
  "R12.8k-R25.6k",
  "R25.6k-R51.2k",
  "R51.2k-R102.4k",
  "R102.4k-R204.8k",
  "R204.8k+"
)


############# Creating log income plots
numeric_income <- clean_data$income %>%
  # Keep, same as filter for lists
  keep(
    !is.na(clean_data$income),
    !clean_data$income == 0,
    !clean_data$income == "NaN"
  )

# Calculate the log of these midpoints to use as breaks on the log-transformed axis
log_income_breaks <- log(as.numeric(income_midpoints_numeric[4:length(income_midpoints_numeric)]))
log_income_labels <- income_labels_text[4:length(income_labels_text)]

message("income brackes")
print(log_income_breaks)
message("labels")
print(log_income_labels)

# Plot 1: Income Distribution by Dominant Population Group (across all years)
for (yr in years) {
  yearly_data <- clean_data %>% filter(
    !is.na(clean_data$income),
    !clean_data$income == 0,
    !clean_data$income == "NaN"
  )

  year  
  if (nrow(yearly_data) > 0) {
    p <- ggplot(yearly_data, aes(x = log(yearly_data$income), fill = dominent_pop_group)) +
      geom_density(alpha = 0.6) +
      labs(
        title = paste(
          "Distribution of Income by Dominant Population Group -",
          yr
        ),
        x = "Average Monthly Household Income (log scale)",
        y = "Density",
        fill = "Dominant Group"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_x_continuous(breaks = log_income_breaks, labels = log_income_labels) +
      scale_fill_manual(values = group_colors)

    ggsave(
      filename = file.path(
        OUTPUT_DIR,
        paste0("Plot 1 - income_distribution_", yr, ".png")
      ),
      plot = p,
      width = 10,
      height = 6
    )
  } else {
    message(paste("Skipping year", yr, "– no valid data"))
  }
}

# Create one plot per year
income_plots_by_year <- lapply(years, function(y) {
  df_year <- clean_data %>% filter(
    year == y,
    !is.na(clean_data$income),
    !clean_data$income == 0,
    !clean_data$income == "NaN"
  )
  
  ggplot(df_year, aes(x = log(df_year$income), fill = dominent_pop_group)) +
    geom_density(alpha = 0.6) +
    labs(
      title = paste("Income Distribution -", y),
      x = "Log(Monthly Household Income)",
      y = "Density",
      fill = "Dominant Group"
    ) +
    theme_minimal(base_size = 11) +
    scale_x_continuous(breaks = log_income_breaks, labels = log_income_labels) +
    scale_fill_manual(values = group_colors) +
    theme(legend.position = "none") # Turn on only for one final plot if needed
})

################# END OF LOG INCOME



income_stats <- clean_data %>%
  filter(!is.na(income)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_income = mean(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(year, dominent_pop_group)

income_summary_table <- clean_data %>%
  filter(!is.na(income)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_income = round(mean(income), 0),
    sd_income = round(sd(income), 0),
    n_wards = n(),
    .groups = "drop"
  ) %>%
  mutate(
    income_summary = paste0(mean_income, " ± ", sd_income)
  ) %>%
  select(year, dominent_pop_group, income_summary, n_wards) %>%
  pivot_wider(
    names_from = year,
    values_from = c(income_summary, n_wards),
    names_glue = "{year}_{.value}"
  )

# Render
kable(
  income_summary_table,
  caption = "Table X: Income (Mean ± SD) and Ward Count by Dominant Group and Year"
) %>%
  kable_styling(full_width = FALSE)

# Prepare summary data
income_summary_for_plot <- clean_data %>%
  filter(!is.na(income)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_income = mean(income, na.rm = TRUE),
    sd_income = sd(income, na.rm = TRUE),
    .groups = "drop"
  )

# Plot with ribbon for ±1 SD
ggplot(income_summary_for_plot, aes(
  x = factor(year),
  y = mean_income,
  group = dominent_pop_group,
  color = dominent_pop_group,
  fill = dominent_pop_group
)) +
  geom_ribbon(
    aes(
      ymin = mean_income - sd_income,
      ymax = mean_income + sd_income
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  labs(
    title = "Average Household Income by Dominant Group (±SD Ribbon)",
    x = "Year",
    y = "Mean Monthly Household Income (R)",
    color = "Dominant Group",
    fill = "Dominant Group"
  ) +
  theme_minimal(base_size = 13) +
  scale_color_manual(values = group_colors) +
  scale_fill_manual(values = group_colors) +
  theme(legend.position = "bottom")

# Save
ggsave("output/income_mean_sd_ribbon_by_group_year.png", width = 10, height = 6)

# Combine into one figure using patchwork
combined_income_plot <- wrap_plots(income_plots_by_year, ncol = 2) +
  plot_annotation(title = "Income Distribution by Dominant Group (2009–2024)")

ggsave(
  "output/combined_income_distribution_by_year.png",
  combined_income_plot,
  width = 14,
  height = 10
)

# Summary by group
share_dom_summary <- all_data %>%
  filter(!is.na(share_dom)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    wards_over_95 = sum(share_dom > 0.95, na.rm = TRUE),
    total_wards = n(),
    percent_over_95 = 100 * wards_over_95 / total_wards,
    .groups = "drop"
  )

share_dom_total <- share_dom_summary %>%
  group_by(year) %>%
  summarise(
    wards_over_95 = sum(wards_over_95),
    total_wards = sum(total_wards),
    percent_over_95 = 100 * wards_over_95 / total_wards,
    dominent_pop_group = "Total",
    .groups = "drop"
  )

share_dom_combined <- bind_rows(share_dom_summary, share_dom_total)

# Plot
ggplot(share_dom_combined, aes(x = factor(year), y = percent_over_95, group = dominent_pop_group)) +
  geom_line(aes(color = dominent_pop_group), linewidth = 1.2) +
  geom_point(aes(color = dominent_pop_group), size = 2.5) +
  labs(
    title = "Share of Wards with >95% Dominant Group by Year",
    x = "Year",
    y = "Share of Wards (%)",
    color = "Dominant Group"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  scale_color_manual(
    values = c(group_colors, "Total" = "black")
  )

# Save the figure
ggsave("output/share_dom_over_95_by_group_year.png", width = 10, height = 6)


# Count number of wards per dominant group per year
dominant_group_counts <- all_data %>%
  filter(!is.na(dominent_pop_group)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(ward_count = n(), .groups = "drop")

# Total number of wards per year
total_wards_per_year <- all_data %>%
  filter(!is.na(dominent_pop_group)) %>%
  group_by(year) %>%
  summarise(total_wards = n(), .groups = "drop")

# Join to compute proportion
dominant_group_share <- dominant_group_counts %>%
  left_join(total_wards_per_year, by = "year") %>%
  mutate(share = ward_count / total_wards * 100)

# Plot
ggplot(dominant_group_share, aes(x = factor(year), y = share, group = dominent_pop_group)) +
  geom_line(aes(color = dominent_pop_group), linewidth = 1.2) +
  geom_point(aes(color = dominent_pop_group), linewidth = 2) +
  labs(
    title = "Share of Wards Dominated by Each Population Group (Normalized)",
    x = "Year",
    y = "Share of Wards (%)",
    color = "Dominant Group"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_color_manual(values = group_colors) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")
  

# Save to file
ggsave("output/ward_dominance_share_by_group_over_time.png", width = 10, height = 6)

# --- RELATIONAL PLOTS ---


###########
message("\n--- Checking clean_data for Year 2018 ---")
clean_data_2018 <- clean_data %>% filter(year == 2018)

message("Number of rows in clean_data for 2018:")
print(nrow(clean_data_2018))

message("\nCounts of dominent_pop_group in clean_data for 2018:")
clean_data_2018 %>%
  count(dominent_pop_group) %>%
  print()

message("\nNA check for key variables in clean_data for 2018:")
clean_data_2018 %>%
  summarise(
    na_income = sum(is.na(income)),
    na_dominent_pop_group = sum(is.na(dominent_pop_group)),
    na_non_white = sum(is.na(non_white)),
    na_dist_over_200 = sum(is.na(dist_over_200)),
    na_interruption_freq = sum(is.na(interruption_freq))
  ) %>%
  print()

message("\n--- Data for interruption_trajectories (before summarise) ---")
data_for_interruption_summary <- clean_data %>%
  filter(year %in% years_interrupt, !is.na(interruption_freq))

message("Number of rows in data_for_interruption_summary:")
print(nrow(data_for_interruption_summary))

message("\nCounts of dominent_pop_group in data_for_interruption_summary:")
data_for_interruption_summary %>%
  count(year, dominent_pop_group) %>%
  print()

interruption_trajectories <- data_for_interruption_summary %>% # Use the filtered data here
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_interruption_freq = mean(interruption_freq, na.rm = TRUE),
    .groups = "drop"
  )

######


# Plot 2: Share of Distance >200m vs. Log(Income) by Dominant Group (for a representative year, e.g., 2011)
year_for_distance_plot <- 2011 # Or pick another year from years_distance
plot_data_distance <- clean_data %>%
  filter(year == year_for_distance_plot, !is.na(dist_over_200))

if (nrow(plot_data_distance) > 0) {
  ggplot(
    plot_data_distance,
    aes(x = log(income), y = dist_over_200, color = dominent_pop_group)
  ) +
    geom_point(alpha = 0.3) +
    # Use GLM with quasibinomial family for proportion data to keep predictions between 0 and 1
    geom_smooth(
      method = "glm",
      method.args = list(family = quasibinomial(link = "logit")),
      se = FALSE
    ) +
    labs(
      title = paste0(
        "Share of Distance >200m vs. Income by Dominant Group (Year ",
        year_for_distance_plot,
        ")"
      ),
      x = "Average Monthly Household Income", # Label reflects the original scale
      y = "Share of Households with Distance >200m (%)",
      color = "Dominant Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))+
    scale_x_continuous(
      breaks = log_income_breaks,
      labels = income_labels_text
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = group_colors) # Apply consistent line/point colors
  ggsave(
    file.path(OUTPUT_DIR, "Plot 2 - distance_vs_income_by_group.png"),
    width = 10,
    height = 6
  )
} else {
  message(paste0(
    "No data to plot distance vs. income for year ",
    year_for_distance_plot,
    "."
  ))
}

# Plot 3: Interruption Frequency vs. Log(Income) by Dominant Group (for a representative year, e.g., 2024)
year_for_interrupt_plot <- 2024 # Or pick another year from years_interrupt
plot_data_interrupt <- clean_data %>%
  filter(year == year_for_interrupt_plot, !is.na(interruption_freq))

if (nrow(plot_data_interrupt) > 0) {
  ggplot(
    plot_data_interrupt,
    aes(x = log(income), y = interruption_freq, color = dominent_pop_group)
  ) +
    geom_point(alpha = 0.3) +
    # Use GLM with quasibinomial family for proportion data to keep predictions between 0 and 1
    geom_smooth(
      method = "glm",
      method.args = list(family = quasibinomial(link = "logit")),
      se = FALSE
    ) +
    labs(
      title = paste0(
        "Interruption Frequency vs. Income by Dominant Group (Year ",
        year_for_interrupt_plot,
        ")"
      ),
      x = "Average Monthly Household Income", # Label reflects the original scale
      y = "Share of People with Frequent Water Interruptions (%)",
      color = "Dominant Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom") +
    scale_y_continuous(labels = scales::percent_format(scale = 1))+
    scale_x_continuous(
      breaks = log_income_breaks,
      labels = income_labels_text
    ) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_color_manual(values = group_colors) # Apply consistent line/point colors
  ggsave(
    file.path(OUTPUT_DIR, "Plot 3 - interruptions_vs_income_by_group.png"),
    width = 10,
    height = 6
  )
} else {
  message(paste0(
    "No data to plot interruptions vs. income for year ",
    year_for_interrupt_plot,
    "."
  ))
}


# Prepare data for interruption frequency trajectories by dominant group
interruption_trajectories <- clean_data %>%
  filter(year %in% years_interrupt, !is.na(interruption_freq)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_interruption_freq = mean(interruption_freq, na.rm = TRUE),
    .groups = "drop"
  )

# Plot 4: Trajectories of Mean Water Interruption Frequency by Dominant Population Group
if (nrow(interruption_trajectories) > 0) {
  ggplot(
    interruption_trajectories,
    aes(
      x = year,
      y = mean_interruption_freq,
      color = dominent_pop_group,
      group = dominent_pop_group
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Trajectories of Mean Water Interruption Frequency by Dominant Group",
      x = "Year",
      y = "Mean Share of Frequent Interruptions",
      color = "Dominant Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = years_interrupt) +
    scale_color_manual(values = group_colors) # Apply consistent line colors
  ggsave(
    file.path(OUTPUT_DIR, "Plot 4 - interruption_trajectories_by_group.png"),
    width = 10,
    height = 6
  )
} else {
  message("No data to plot interruption frequency trajectories.")
}


# Prepare data for distance >200m trajectories by dominant group
distance_trajectories <- clean_data %>%
  filter(year %in% years_distance, !is.na(dist_over_200)) %>%
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_dist_over_200 = mean(dist_over_200, na.rm = TRUE),
    .groups = "drop"
  )



##############
message("\n--- interruption_trajectories dataframe content ---")
print(interruption_trajectories)

# --- DIAGNOSTIC STEP 4: Inspect data *before* summarising for distance_trajectories ---
message("\n--- Data for distance_trajectories (before summarise) ---")
data_for_distance_summary <- clean_data %>%
  filter(year %in% years_distance, !is.na(dist_over_200))

message("Number of rows in data_for_distance_summary:")
print(nrow(data_for_distance_summary))

message("\nCounts of dominent_pop_group in data_for_distance_summary:")
data_for_distance_summary %>%
  count(year, dominent_pop_group) %>%
  print()

# ... (Your existing code for calculating distance_trajectories) ...
distance_trajectories <- data_for_distance_summary %>% # Use the filtered data here
  group_by(year, dominent_pop_group) %>%
  summarise(
    mean_dist_over_200 = mean(dist_over_200, na.rm = TRUE),
    .groups = "drop"
  )


message("\n--- distance_trajectories dataframe content ---")
print(distance_trajectories)

# --- DIAGNOSTIC STEP 6: Check group_colors mapping ---
message("\n--- Checking group_colors mapping ---")
print(group_colors)
message("Levels present in interruption_trajectories$dominent_pop_group:")
print(levels(factor(interruption_trajectories$dominent_pop_group))) # Ensure it's a factor for levels()
message("Levels present in distance_trajectories$dominent_pop_group:")
print(levels(factor(distance_trajectories$dominent_pop_group)))

################


# Plot 5: Trajectories of Mean Distance >200m by Dominant Population Group
if (nrow(distance_trajectories) > 0) {
  ggplot(
    distance_trajectories,
    aes(
      x = year,
      y = mean_dist_over_200,
      color = dominent_pop_group,
      group = dominent_pop_group
    )
  ) +
    geom_line(linewidth = 1.2) +
    geom_point(size = 3) +
    labs(
      title = "Trajectories of Mean Distance >200m from Water by Dominant Group",
      x = "Year",
      y = "Mean Share of Households with Distance >200m",
      color = "Dominant Group"
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom") +
    scale_x_continuous(breaks = years_distance) +
    scale_color_manual(values = group_colors) 
  ggsave(
    file.path(OUTPUT_DIR, "Plot 5 - distance_trajectories_by_group.png"),
    width = 10,
    height = 6
  )
} else {
  message("No data to plot distance >200m trajectories.")
}


income_breaks_10pct_raw <- quantile(
  clean_data$income,
  probs = seq(0, 1, by = 0.1),
  na.rm = TRUE
)
# Ensure breaks are unique and sorted to avoid cut.default() errors
income_breaks_unique <- unique(sort(income_breaks_10pct_raw))

# Create labels based on the actual number of unique breaks
# There will be (length(income_breaks_unique) - 1) intervals
num_income_intervals <- length(income_breaks_unique) - 1
income_interval_labels_10pct_new <- character(num_income_intervals)

for (i in 1:num_income_intervals) {
  lower_bound <- round(income_breaks_unique[i])
  upper_bound <- round(income_breaks_unique[i + 1])
  if (i == num_income_intervals) {
    # For the last interval, handle it as "RXXX+" for the highest bracket
    income_interval_labels_10pct_new[i] <- paste0("R", lower_bound, "+")
  } else {
    income_interval_labels_10pct_new[i] <- paste0(
      "R",
      lower_bound,
      "-R",
      upper_bound
    )
  }
}

income_category_data_10pct <- clean_data %>%
  filter(!is.na(income)) %>% # Filter out NAs for income
  mutate(
    # Use cut to categorize into 10% intervals based on income quantiles
    income_category = cut(
      income,
      breaks = income_breaks_unique, # Use unique breaks
      labels = income_interval_labels_10pct_new, # Use dynamically generated labels
      include.lowest = TRUE,
      right = FALSE # Intervals like [0, 0.1), [0.1, 0.2)
    ) %>%
      fct_drop() # Drop unused levels if any
  ) %>%
  filter(!is.na(income_category)) # Filter out any NAs from categorization

# Define a color palette for income categories (e.g., a sequential green palette)
# The number of colors should match the number of *actual* labels generated
income_interval_colors_10pct <- get_greens_palette(length(
  income_interval_labels_10pct_new
))
names(income_interval_colors_10pct) <- income_interval_labels_10pct_new # Assign names to match labels

### Linerar regression

# Fit models for water interruptions
models_interrupt <- map(
  years_interrupt,
  ~ run_model(clean_data %>% filter(year == .x), "interruption_freq", .x)
)
names(models_interrupt) <- years_interrupt

# Fit models for distance to water source
models_distance <- map(
  years_distance,
  ~ run_model(clean_data %>% filter(year == .x), "dist_over_200", .x)
)
names(models_distance) <- years_distance

# Tidy and combine results into data frames for plotting
summary_interrupt <- map2_dfr(
  compact(models_interrupt),
  names(compact(models_interrupt)),
  ~ tidy(.x) %>% mutate(year = as.numeric(.y))
)

summary_distance <- map2_dfr(
  compact(models_distance),
  names(compact(models_distance)),
  ~ tidy(.x) %>% mutate(year = as.numeric(.y))
)

# Add significance labels and clean plot_term for better legends
summary_interrupt <- summary_interrupt %>%
  filter(term != "(Intercept)") %>%
  mutate(
    plot_term = case_when(
      str_detect(term, "dominent_pop_group") ~
        str_replace(term, "dominent_pop_group", ""),
      TRUE ~ term
    ),
    # IMPORTANT: Trim any leading/trailing whitespace
    plot_term = trimws(plot_term),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
    # The factor conversion is now done immediately before plotting to ensure freshness
  )

summary_distance <- summary_distance %>%
  filter(term != "(Intercept)") %>%
  mutate(
    plot_term = case_when(
      str_detect(term, "dominent_pop_group") ~
        str_replace(term, "dominent_pop_group", ""),
      TRUE ~ term
    ),
    # IMPORTANT: Trim any leading/trailing whitespace
    plot_term = trimws(plot_term),
    significance = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      p.value < 0.1 ~ ".",
      TRUE ~ ""
    )
    # The factor conversion is now done immediately before plotting to ensure freshness
  )

# --- NEW: Re-assert plot_term as a factor and add structural diagnostics ---

# Define the desired order for factor levels for 'plot_term' globally
desired_plot_term_levels <- c(
  "log(income)",
  "non_white",
  "Coloured",
  "Indian/Asian",
  "White",
  "Other"
)

# Apply this explicit factor level order to your dataframes RIGHT BEFORE plotting
summary_interrupt$plot_term <- factor(
  as.character(summary_interrupt$plot_term),
  levels = desired_plot_term_levels
)
summary_distance$plot_term <- factor(
  as.character(summary_distance$plot_term),
  levels = desired_plot_term_levels
)

# Diagnostic: Print structure of dataframes just before plotting
cat("\n--- str(summary_interrupt) before Plot 6 ---\n")
str(summary_interrupt)
cat("---\n")

cat("\n--- str(summary_distance) before Plot 7 ---\n")
str(summary_distance)
cat("---\n")

# Define a combined palette that ensures all needed colors are together and explicitly listed.
combined_plot_colors <- c(
  "log(income)" = "black",
  "non_white" = "darkgreen",
  "Black African" = "#E41A1C", # Red
  "Coloured" = "#377EB8", # Blue
  "Indian/Asian" = "#4DAF4A", # Green (now covers both variations)
  "White" = "#FF7F00", # Orange
  "Other" = "#984EA3" # Purple
)


# Plot 6: Interruption Coefficients
ggplot(
  summary_interrupt,
  aes(x = factor(year), y = estimate, color = plot_term, group = plot_term)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = significance), vjust = -1, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Predictors of Water Interruptions (2018–2024)",
    x = "Year",
    y = "Coefficient",
    color = "Variable"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = combined_plot_colors, drop = FALSE) # Added drop = FALSE
ggsave(
  file.path(
    OUTPUT_DIR,
    "Plot 6 - interruption_coefficients_with_significance.png"
  ),
  width = 10,
  height = 6
)


# 🟡 Updated Plot 7: Distance Coefficients
ggplot(
  summary_distance,
  aes(x = factor(year), y = estimate, color = plot_term, group = plot_term)
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_text(aes(label = significance), vjust = -1, size = 3.5) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "Predictors of Distance >200m (2009–2016)",
    x = "Year",
    y = "Coefficient",
    color = "Variable"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom") +
  scale_color_manual(values = combined_plot_colors, drop = FALSE) # Added drop = FALSE
ggsave(
  file.path(OUTPUT_DIR, "Plot 7 - distance_coefficients_with_significance.png"),
  width = 10,
  height = 6
)

# --- Define desired plot_term levels if not already defined globally ---
# This ensures consistent row order in your tables
desired_plot_term_levels <- c(
  "log(income)",
  "non_white",
  "Coloured",
  "Indian/Asian",
  "White",
  "Other"
)

# --- Generate Table for Interruption Coefficients ---
# Prepare the data for the table: combine estimate, std.error, and significance
table_interrupt_data <- summary_interrupt %>%
  mutate(
    # Format the coefficient string to include estimate, std.error, and significance stars
    # Round estimates and std.errors to 3 decimal places for table readability
    formatted_coefficient = sprintf(
      "%.3f (%.3f)%s",
      estimate,
      std.error,
      significance
    )
  ) %>%
  select(plot_term, year, formatted_coefficient) %>%
  # Pivot the table wider to have years as columns
  pivot_wider(
    names_from = year,
    values_from = formatted_coefficient,
    names_prefix = "", # No prefix, use year directly as column name
    values_fill = "" # Fill missing values (for terms not present in a specific year) with empty string
  ) %>%
  # Arrange rows by your predefined plot_term levels for consistent order
  arrange(factor(plot_term, levels = desired_plot_term_levels))

# Render the table for Interruption Coefficients
cat(
  "### Table 1: Regression Coefficients for Water Interruption Frequency by Year\n"
)
table_interrupt_data %>%
  knitr::kable(
    caption = "Table 1: Regression Coefficients for Water Interruption Frequency by Year",
    align = "l" # Align columns to the left
  ) %>%
  kable_styling(full_width = FALSE) %>% # Adjust width for better display in various outputs
  # Add footnotes for reference group and significance codes
  add_footnote(
    c(
      "Reference group for dominant population group is 'Black African'.",
      "Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1"
    ),
    notation = "none", # Do not use numeric/alphabetic markers for footnotes
  ) %>%
  print() # Print the kable object to the console (or to your R Markdown/Quarto output)


# --- Generate Table for Distance Coefficients ---
# Prepare the data for the table: combine estimate, std.error, and significance
table_distance_data <- summary_distance %>%
  mutate(
    formatted_coefficient = sprintf(
      "%.3f (%.3f)%s",
      estimate,
      std.error,
      significance
    )
  ) %>%
  select(plot_term, year, formatted_coefficient) %>%
  # Pivot the table wider to have years as columns
  pivot_wider(
    names_from = year,
    values_from = formatted_coefficient,
    names_prefix = "",
    values_fill = ""
  ) %>%
  # Arrange rows by your predefined plot_term levels for consistent order
  arrange(factor(plot_term, levels = desired_plot_term_levels))

# Render the table for Distance Coefficients
cat(
  "\n\n### Table 2: Regression Coefficients for Distance to Water Source by Year\n"
)
table_distance_data %>%
  knitr::kable(
    caption = "Table 2: Regression Coefficients for Distance to Water Source by Year",
    align = "l"
  ) %>%
  kable_styling(full_width = FALSE) %>%
  add_footnote(
    c(
      "Reference group for dominant population group is 'Black African'.",
      "Significance codes: *** p < 0.001, ** p < 0.01, * p < 0.05, . p < 0.1"
    ),
    notation = "none",
  ) %>%
  print() # Print the kable object

# --- Save Table for Interruption Coefficients as XLSX ---
write_xlsx(
  table_interrupt_data,
  path = file.path(OUTPUT_DIR, "Table 1 - Interruption Coefficients.xlsx")
)
message("Table 1 saved as 'Table 1 - Interruption Coefficients.xlsx'")

# --- Save Table for Distance Coefficients as XLSX ---
write_xlsx(
  table_distance_data,
  path = file.path(OUTPUT_DIR, "Table 2 - Distance Coefficients.xlsx")
)
message("Table 2 saved as 'Table 2 - Distance Coefficients.xlsx'")


## Mapping

# Path to your shapefiles

# Define income brackets and their labels for the map legend (using dynamically created ones from Plot 14 logic)
# These are derived directly from your clean_data for consistency.
# If you haven't run the Plot 14 section, these might not be defined.
# We'll explicitly re-calculate them here to ensure they exist for the mapping function.
# Calculate income breaks based on the actual range of income data.
income_breaks_10pct_raw_maps <- quantile(
  clean_data$income,
  probs = seq(0, 1, by = 0.1),
  na.rm = TRUE
)
income_breaks_unique_maps <- unique(sort(income_breaks_10pct_raw_maps))
num_income_intervals_maps <- length(income_breaks_unique_maps) - 1
income_interval_labels_10pct_new_maps <- character(num_income_intervals_maps)

for (i in 1:num_income_intervals_maps) {
  lower_bound <- round(income_breaks_unique_maps[i])
  upper_bound <- round(income_breaks_unique_maps[i + 1])
  if (i == num_income_intervals_maps) {
    income_interval_labels_10pct_new_maps[i] <- paste0("R", lower_bound, "+")
  } else {
    income_interval_labels_10pct_new_maps[i] <- paste0(
      "R",
      lower_bound,
      "-R",
      upper_bound
    )
  }
}

# Define a color palette for income categories for maps
# Re-using get_greens_palette from above
income_interval_colors_10pct_maps <- get_greens_palette(length(
  income_interval_labels_10pct_new_maps
))
names(
  income_interval_colors_10pct_maps
) <- income_interval_labels_10pct_new_maps

# --- Generate All Maps ---

map_specs <- list(
  list(
    years = years,
    var = "dominent_p",
    title = "Dominant Population Group"
  ),
  list(years = years, var = "income_bracket", title = "Average Income Bracket"),
  list(
    years = years,
    var = "non_white",
    title = "Non white population share"
  ),
  list(years = years, var = "avrage_ace", title = "Average Water Access"),
  list(
    years = years_interrupt,
    var = "interrupti",
    title = "Water Interruption Frequency"
  ),
  list(
    years = years_distance,
    var = "dist_over_",
    title = "Share of Households >200m from Water"
  ),
  list(years = 2011, var = "pop_density", title = "Population Density")
)

for (spec in map_specs) {
  message(paste0("\n--- Generating ", spec$title, " Maps ---"))
  for (year in spec$years) {
    generate_and_save_map(year, spec$var, spec$title, clean_data)
  }
}
