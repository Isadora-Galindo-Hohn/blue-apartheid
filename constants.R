# --- Define a consistent color palette for dominent_pop_group ---
group_colors <- c(
  "Black African" = "#E41A1C", # Red
  "Coloured" = "#377EB8", # Blue
  "Indian/Asian" = "#4DAF4A", # Green (now covers both variations)
  "White" = "#FF7F00", # Orange
  "Other" = "#984EA3" # Purple
)

data_legend_sources <- c(
  "2009" = "GCRO QoL 1",
  "2011" = "GCRO QoL 2 and Cencus 2011",
  "2014" = "GCRO QoL 3",
  "2016" = "GCRO QoL 4",
  "2018" = "GCRO QoL 5",
  "2022" = "GCRO QoL 6",
  "2024" = "GCRO QoL 7"
)

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
