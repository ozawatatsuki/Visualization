suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    path <- rstudioapi::getActiveDocumentContext()$path
    if (nzchar(path)) {
      return(dirname(normalizePath(path, winslash = "/", mustWork = FALSE)))
    }
  }
  getwd()
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(mean(x, na.rm = TRUE))
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

read_load1a_weights <- function(path) {
  raw <- as_tibble(rgdx.param(path, "load1a"))
  if (nrow(raw) == 0) {
    stop("load1a has no records.")
  }

  raw %>%
    transmute(
      Area = as.character(.data[["i1"]]),
      Segment = as.character(.data[["i2"]]),
      R = as.character(.data[["i3"]]),
      WeightType = as.character(.data[["i4"]]),
      Weight = as.numeric(.data[["value"]])
    ) %>%
    filter(WeightType == "Value") %>%
    group_by(R, Area, Segment) %>%
    summarise(Weight = sum(Weight, na.rm = TRUE), .groups = "drop")
}

choose_national_records <- function(df, weights) {
  national_segments <- c("Higher", "Middle", "Low", "Lowest")
  segment_area_pattern <- "^(Higher|Middle|Low|Lowest)_(urban|rural)$"
  segment_national_pattern <- paste0("^(", paste(national_segments, collapse = "|"), ")$")
  decile_area_pattern <- "^DECILE[0-9]+_(urban|rural)$"
  decile_pattern <- "^DECILE[0-9]+$"
  quintile_pattern <- "^QUINTILE[0-9]+$"

  availability <- df %>%
    distinct(R, H) %>%
    mutate(
      has_segment_area = grepl(segment_area_pattern, H),
      has_segment_national = grepl(segment_national_pattern, H),
      has_decile_area = grepl(decile_area_pattern, H),
      has_decile = grepl(decile_pattern, H),
      has_quintile = grepl(quintile_pattern, H)
    ) %>%
    group_by(R) %>%
    summarise(
      has_segment_area = any(has_segment_area),
      has_segment_national = any(has_segment_national),
      has_decile_area = any(has_decile_area),
      has_decile = any(has_decile),
      has_quintile = any(has_quintile),
      .groups = "drop"
    )

  weight_availability <- weights %>%
    group_by(R) %>%
    summarise(
      has_weight_segment_area = any(Area %in% c("Urban", "Rural") & Segment %in% national_segments),
      has_weight_segment_national = any(Area == "National" & Segment %in% national_segments),
      .groups = "drop"
    )

  selection <- availability %>%
    left_join(weight_availability, by = "R") %>%
    mutate(
      has_weight_segment_area = coalesce(has_weight_segment_area, FALSE),
      has_weight_segment_national = coalesce(has_weight_segment_national, FALSE),
      selected_system = case_when(
        has_segment_area & has_weight_segment_area ~ "segment_area_weighted",
        has_segment_national & has_weight_segment_national ~ "segment_national_weighted",
        has_decile_area ~ "decile_area_unweighted",
        has_decile ~ "decile_unweighted",
        has_quintile ~ "quintile_unweighted",
        has_segment_area ~ "segment_area_unweighted",
        has_segment_national ~ "segment_national_unweighted",
        TRUE ~ "all_unweighted"
      )
    ) %>%
    select(R, selected_system)

  df %>%
    left_join(selection, by = "R") %>%
    mutate(
      keep_record = case_when(
        selected_system %in% c("segment_area_weighted", "segment_area_unweighted") ~ grepl(segment_area_pattern, H),
        selected_system %in% c("segment_national_weighted", "segment_national_unweighted") ~ grepl(segment_national_pattern, H),
        selected_system == "decile_area_unweighted" ~ grepl(decile_area_pattern, H),
        selected_system == "decile_unweighted" ~ grepl(decile_pattern, H),
        selected_system == "quintile_unweighted" ~ grepl(quintile_pattern, H),
        TRUE ~ TRUE
      )
    ) %>%
    filter(keep_record) %>%
    select(-keep_record)
}

read_income_elasticity <- function(path, variant, weights, countries, ssps, goods_map) {
  raw <- as_tibble(rgdx.param(path, "PoutputIncomeElaNational"))
  if (nrow(raw) == 0) {
    stop(sprintf("PoutputIncomeElaNational has no records: %s", path))
  }
  if (!all(c("i1", "i2", "i3", "i4", "i5", "value") %in% names(raw))) {
    stop("Unexpected PoutputIncomeElaNational schema.")
  }

  df <- raw %>%
    transmute(
      Variant = variant,
      R = as.character(.data[["i1"]]),
      SSP = as.character(.data[["i2"]]),
      H = as.character(.data[["i3"]]),
      Y = as.numeric(as.character(.data[["i4"]])),
      I_raw = as.character(.data[["i5"]]),
      Value = as.numeric(.data[["value"]])
    ) %>%
    filter(R %in% countries, SSP %in% ssps)

  selected <- choose_national_records(df, weights) %>%
    mutate(
      Segment = case_when(
        grepl("^(Higher|Middle|Low|Lowest)_(urban|rural)$", H) ~ sub("_.*$", "", H),
        H %in% c("Higher", "Middle", "Low", "Lowest") ~ H,
        TRUE ~ NA_character_
      ),
      Area = case_when(
        grepl("_urban$", H) ~ "Urban",
        grepl("_rural$", H) ~ "Rural",
        H %in% c("Higher", "Middle", "Low", "Lowest") ~ "National",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(weights, by = c("R", "Area", "Segment"))

  weighted <- selected %>%
    filter(grepl("_weighted$", selected_system)) %>%
    group_by(Variant, R, SSP, Y, I_raw) %>%
    summarise(Value = weighted_mean_safe(Value, Weight), .groups = "drop")

  unweighted <- selected %>%
    filter(!grepl("_weighted$", selected_system)) %>%
    group_by(Variant, R, SSP, Y, I_raw) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  bind_rows(weighted, unweighted) %>%
    group_by(Variant, R, SSP, I_raw) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    left_join(goods_map, by = "I_raw") %>%
    mutate(
      I_label = ifelse(is.na(I_label), I_raw, I_label),
      R = factor(R, levels = countries),
      SSP = factor(SSP, levels = ssps),
      I_label = factor(I_label, levels = goods_map$I_label)
    )
}

plot_representative_lines <- function(df, variant, output_dir, data_variant, country_levels, goods_levels) {
  plot_df <- df %>%
    filter(Variant == variant) %>%
    mutate(
      R = factor(as.character(R), levels = country_levels),
      I_label = factor(as.character(I_label), levels = goods_levels)
    )

  p <- ggplot(plot_df, aes(x = I_label, y = Value, colour = SSP, group = SSP)) +
    geom_line(linewidth = 0.9, alpha = 0.9) +
    geom_point(size = 2.0, alpha = 0.95) +
    facet_wrap(~R, ncol = 3, scales = "free_y", drop = FALSE) +
    scale_colour_manual(values = c("SSP1" = "#1b9e77", "SSP2" = "#333333", "SSP3" = "#d95f02"), drop = FALSE) +
    labs(
      title = sprintf("PoutputIncomeElaNational by SSP | %s | %s", data_variant, variant),
      subtitle = "Representative countries: FRA, DNK, DEU / IND, PAK, LAO",
      x = "Good",
      y = "Income elasticity",
      colour = "SSP"
    ) +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "grey95", colour = "grey75"),
      strip.text = element_text(face = "bold", size = 12),
      axis.text.x = element_text(angle = 55, hjust = 1, size = 8),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )

  out_png <- file.path(output_dir, sprintf("PoutputIncomeElaNational_representative_lines_%s.png", variant))
  ggsave(
    filename = out_png,
    plot = p,
    width = 16,
    height = 9.5,
    dpi = 300,
    device = if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"
  )
  cat("Saved:", normalizePath(out_png, winslash = "/", mustWork = FALSE), "\n")
}

script_dir <- get_script_dir()
trailing_args <- commandArgs(trailingOnly = TRUE)
data_variant <- if (length(trailing_args) >= 1 && nzchar(trailing_args[1])) trailing_args[1] else "weight1_bm1.5"
output_collection <- if (length(trailing_args) >= 2 && nzchar(trailing_args[2])) trailing_args[2] else NA_character_

data_root <- file.path(script_dir, "..", "data", "260608_Labseminar", data_variant)
weights_gdx <- file.path(script_dir, "..", "data", "GlobalConsumptionDataPop.gdx")
output_dir <- if (is.na(output_collection)) {
  file.path(script_dir, "..", "output", "260608_Labseminar", data_variant, "PoutputIncomeElaNational_representative_lines")
} else {
  file.path(script_dir, "..", "output", "260608_Labseminar", output_collection, data_variant, "PoutputIncomeElaNational_representative_lines")
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

country_levels <- c("FRA", "DNK", "DEU", "IND", "PAK", "LAO")
ssp_levels <- c("SSP1", "SSP2", "SSP3")

goods_map <- tibble::tribble(
  ~I_raw, ~I_label,
  "Food and nonalcoholic beverages", "CP01 Food",
  "Alcoholic beverages, tobacco, and narcotics", "CP02 Alcohol/tobacco",
  "Clothing and footwear", "CP03 Clothing",
  "Housing", "CP04 Housing",
  "Water", "CP044 Water",
  "Energy", "CP045 Energy",
  "Furnishings, household equipment and maintenance", "CP05 Furnishings",
  "Health", "CP06 Health",
  "Transport", "CP07 Transport",
  "Communication", "CP08 Communication",
  "Recreation and culture", "CP09 Recreation",
  "Education", "CP10 Education",
  "Restaurants and hotels", "CP11 Restaurants",
  "Miscellaneous goods and services", "CP12 Misc"
)

gdx_files <- c(
  all = file.path(data_root, "all", "AIDADSCalibNationResults_SSPs.gdx"),
  selected = file.path(data_root, "selected", "AIDADSCalibNationResults_SSPs.gdx")
)

missing_files <- c(gdx_files, weights_gdx)[!file.exists(c(gdx_files, weights_gdx))]
if (length(missing_files) > 0) {
  stop(sprintf("Missing input file(s): %s", paste(normalizePath(missing_files, winslash = "/", mustWork = FALSE), collapse = ", ")))
}

weights <- read_load1a_weights(weights_gdx)

plot_data <- bind_rows(lapply(names(gdx_files), function(variant) {
  read_income_elasticity(
    path = gdx_files[[variant]],
    variant = variant,
    weights = weights,
    countries = country_levels,
    ssps = ssp_levels,
    goods_map = goods_map
  )
}))

for (variant in names(gdx_files)) {
  plot_representative_lines(
    df = plot_data,
    variant = variant,
    output_dir = output_dir,
    data_variant = data_variant,
    country_levels = country_levels,
    goods_levels = goods_map$I_label
  )
}

cat("Done. Output directory:", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
