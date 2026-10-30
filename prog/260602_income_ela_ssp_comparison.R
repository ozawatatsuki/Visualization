suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(gridExtra)
  library(ragg)
  library(scales)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      path <- rstudioapi::getActiveDocumentContext()$path
      if (nzchar(path)) {
        return(dirname(normalizePath(path, winslash = "/", mustWork = FALSE)))
      }
    }
  }
  getwd()
}

script_dir <- get_script_dir()
data_dir <- file.path(script_dir, "..", "data", "260602_AIDADScalib")
output_dir <- file.path(script_dir, "..", "output", "260602", "income_ela_ssp_comparison")
by_good_dir <- file.path(output_dir, "by_good")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(by_good_dir, recursive = TRUE, showWarnings = FALSE)

cross_country_gdx <- file.path(data_dir, "AIDADSCalibCrossCountry_SSP.gdx")
national_gdx <- file.path(data_dir, "AIDADSCalibNationResults_SSPs.gdx")
weights_gdx <- file.path(data_dir, "GlobalConsumptionDataPop.gdx")
weight_ssps_gdx <- file.path(data_dir, "WeightSSPs_selectedGoods_CP01_CP045_CP07_CP10_CP06.gdx")

required_files <- c(cross_country_gdx, national_gdx, weights_gdx, weight_ssps_gdx)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(sprintf("Missing input file(s): %s", paste(normalizePath(missing_files, winslash = "/", mustWork = FALSE), collapse = ", ")))
}

baseline_ssp <- "SSP2"

goods_map <- tibble::tribble(
  ~I, ~I_label,
  "Food and nonalcoholic beverages", "CP01 Food",
  "Alcoholic beverages, tobacco, and narcotics", "CP02 Alcohol/tobacco",
  "Clothing and footwear", "CP03 Clothing",
  "Housing", "CP04 Housing",
  "Water", "CP04 Water",
  "Energy", "CP04 Energy",
  "Furnishings, household equipment and maintenance", "CP05 Furnishings",
  "Health", "CP06 Health",
  "Transport", "CP07 Transport",
  "Communication", "CP08 Communication",
  "Recreation and culture", "CP09 Recreation",
  "Education", "CP10 Education",
  "Restaurants and hotels", "CP11 Restaurants",
  "Miscellaneous goods and services", "CP12 Misc"
)

write_csv_safe <- function(df, path) {
  write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
  cat("Saved:", normalizePath(path, winslash = "/", mustWork = FALSE), "\n")
}

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(mean(x, na.rm = TRUE))
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

safe_name <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  x
}

clip_upper <- function(x, prob = 0.95) {
  upper <- as.numeric(quantile(x, prob, na.rm = TRUE, names = FALSE))
  if (!is.finite(upper)) {
    upper <- max(x, na.rm = TRUE)
  }
  upper
}

clip_abs_limit <- function(x, prob = 0.95) {
  limit <- as.numeric(quantile(abs(x), prob, na.rm = TRUE, names = FALSE))
  if (!is.finite(limit) || limit == 0) {
    limit <- max(abs(x), na.rm = TRUE)
  }
  if (!is.finite(limit) || limit == 0) {
    limit <- 1
  }
  limit
}

diverging_limit <- function(x, prob = 0.95) {
  limit <- clip_abs_limit(x, prob = prob)
  c(-limit, limit)
}

add_good_labels <- function(df) {
  df %>%
    left_join(goods_map, by = "I") %>%
    mutate(
      I_label = ifelse(is.na(I_label), I, I_label),
      I_label = factor(I_label, levels = goods_map$I_label),
      SSP = factor(SSP, levels = sort(unique(as.character(SSP))))
    )
}

read_cross_country <- function(path) {
  raw <- as_tibble(rgdx.param(path, "PoutputIncomeEla"))
  if (nrow(raw) == 0) {
    stop("PoutputIncomeEla has no records.")
  }
  if (!all(c("SSP", "Seg", "Y", "R", "I", "PoutputIncomeEla") %in% names(raw))) {
    stop("Unexpected PoutputIncomeEla schema.")
  }
  raw %>%
    transmute(
      Symbol = "PoutputIncomeEla",
      R = as.character(.data[["R"]]),
      SSP = as.character(.data[["SSP"]]),
      Seg = as.character(.data[["Seg"]]),
      Y = as.numeric(as.character(.data[["Y"]])),
      I = as.character(.data[["I"]]),
      Value = as.numeric(.data[["PoutputIncomeEla"]])
    ) %>%
    filter(Seg == "TOTAL") %>%
    group_by(Symbol, R, SSP, I) %>%
    summarise(ValueAvgY = mean(Value, na.rm = TRUE), .groups = "drop") %>%
    add_good_labels()
}

read_load1a <- function(path) {
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

read_weight_ssps <- function(path) {
  raw <- as_tibble(rgdx.param(path, "WeightSSPsIn"))
  if (nrow(raw) == 0) {
    stop("WeightSSPsIn has no records.")
  }
  if (!all(c("SSP", "R", "WeightSSPsIn") %in% names(raw))) {
    stop("Unexpected WeightSSPsIn schema.")
  }
  raw %>%
    transmute(
      SSP = as.character(.data[["SSP"]]),
      R = as.character(.data[["R"]]),
      WeightSSPsIn = as.numeric(.data[["WeightSSPsIn"]])
    ) %>%
    mutate(SSP = factor(SSP, levels = sort(unique(as.character(SSP)))))
}

write_anomaly_diagnostics <- function(cross_country_path, national_path, cross_country_comp, national_comp) {
  low_threshold <- 0.01
  high_threshold <- 100

  cross_country_raw <- as_tibble(rgdx.param(cross_country_path, "PoutputIncomeEla")) %>%
    transmute(
      Symbol = "PoutputIncomeEla",
      R = as.character(.data[["R"]]),
      SSP = as.character(.data[["SSP"]]),
      Seg = as.character(.data[["Seg"]]),
      Y = as.numeric(as.character(.data[["Y"]])),
      I = as.character(.data[["I"]]),
      Value = as.numeric(.data[["PoutputIncomeEla"]])
    ) %>%
    add_good_labels() %>%
    filter(.data[["Value"]] <= low_threshold | .data[["Value"]] > high_threshold) %>%
    mutate(
      Flag = case_when(
        .data[["Value"]] <= low_threshold ~ sprintf("raw_value_le_%s", low_threshold),
        .data[["Value"]] > high_threshold ~ sprintf("raw_value_gt_%s", high_threshold),
        TRUE ~ "raw_value_check"
      )
    ) %>%
    arrange(desc(abs(.data[["Value"]])), .data[["I_label"]], .data[["R"]], .data[["SSP"]])

  national_raw <- as_tibble(rgdx.param(national_path, "PoutputIncomeElaNational")) %>%
    transmute(
      Symbol = "PoutputIncomeElaNational",
      R = as.character(.data[["i1"]]),
      SSP = as.character(.data[["i2"]]),
      H = as.character(.data[["i3"]]),
      Y = as.numeric(as.character(.data[["i4"]])),
      I = as.character(.data[["i5"]]),
      Value = as.numeric(.data[["value"]])
    ) %>%
    add_good_labels() %>%
    filter(.data[["Value"]] <= low_threshold | .data[["Value"]] > high_threshold) %>%
    mutate(
      Flag = case_when(
        .data[["Value"]] <= low_threshold ~ sprintf("raw_value_le_%s", low_threshold),
        .data[["Value"]] > high_threshold ~ sprintf("raw_value_gt_%s", high_threshold),
        TRUE ~ "raw_value_check"
      )
    ) %>%
    arrange(desc(abs(.data[["Value"]])), .data[["I_label"]], .data[["R"]], .data[["SSP"]])

  averaged_anomalies <- bind_rows(cross_country_comp, national_comp) %>%
    filter(.data[["ValueAvgY"]] <= low_threshold | .data[["ValueAvgY"]] > high_threshold) %>%
    mutate(
      Flag = case_when(
        .data[["ValueAvgY"]] <= low_threshold ~ sprintf("avg_value_le_%s", low_threshold),
        .data[["ValueAvgY"]] > high_threshold ~ sprintf("avg_value_gt_%s", high_threshold),
        TRUE ~ "avg_value_check"
      )
    ) %>%
    arrange(.data[["Symbol"]], desc(abs(.data[["ValueAvgY"]])), .data[["I_label"]], .data[["R"]], .data[["SSP"]])

  write_csv_safe(cross_country_raw, file.path(output_dir, "PoutputIncomeEla_raw_anomaly_diagnostics.csv"))
  write_csv_safe(national_raw, file.path(output_dir, "PoutputIncomeElaNational_raw_anomaly_diagnostics.csv"))
  write_csv_safe(averaged_anomalies, file.path(output_dir, "income_ela_averaged_anomaly_diagnostics.csv"))
}

choose_national_records <- function(df, weights) {
  national_segments <- c("Higher", "Middle", "Low", "Lowest")
  segment_area_pattern <- "^(Higher|Middle|Low|Lowest)_(urban|rural)$"
  decile_area_pattern <- "^DECILE[0-9]+_(urban|rural)$"
  decile_pattern <- "^DECILE[0-9]+$"
  quintile_pattern <- "^QUINTILE[0-9]+$"

  availability <- df %>%
    distinct(R, H) %>%
    mutate(
      has_segment_area = grepl(segment_area_pattern, H),
      has_segment_national = H %in% national_segments,
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
        selected_system %in% c("segment_national_weighted", "segment_national_unweighted") ~ H %in% national_segments,
        selected_system == "decile_area_unweighted" ~ grepl(decile_area_pattern, H),
        selected_system == "decile_unweighted" ~ grepl(decile_pattern, H),
        selected_system == "quintile_unweighted" ~ grepl(quintile_pattern, H),
        TRUE ~ TRUE
      )
    ) %>%
    filter(keep_record) %>%
    select(-keep_record)
}

read_national <- function(path, weights) {
  raw <- as_tibble(rgdx.param(path, "PoutputIncomeElaNational"))
  if (nrow(raw) == 0) {
    stop("PoutputIncomeElaNational has no records.")
  }
  if (!all(c("i1", "i2", "i3", "i4", "i5", "value") %in% names(raw))) {
    stop("Unexpected PoutputIncomeElaNational schema.")
  }

  df <- raw %>%
    transmute(
      Symbol = "PoutputIncomeElaNational",
      R = as.character(.data[["i1"]]),
      SSP = as.character(.data[["i2"]]),
      H = as.character(.data[["i3"]]),
      Y = as.numeric(as.character(.data[["i4"]])),
      I = as.character(.data[["i5"]]),
      Value = as.numeric(.data[["value"]])
    )

  selected <- choose_national_records(df, weights) %>%
    mutate(
      Segment = case_when(
        grepl("^(Higher|Middle|Low|Lowest)_(urban|rural)$", H) ~ sub("_.*$", "", H),
        H %in% c("Higher", "Middle", "Low", "Lowest") ~ H,
        TRUE ~ NA_character_
      ),
      Area = case_when(
        grepl("_(urban)$", H) ~ "Urban",
        grepl("_(rural)$", H) ~ "Rural",
        H %in% c("Higher", "Middle", "Low", "Lowest") ~ "National",
        TRUE ~ NA_character_
      )
    ) %>%
    left_join(weights, by = c("R", "Area", "Segment"))

  weighted <- selected %>%
    filter(grepl("_weighted$", selected_system)) %>%
    group_by(Symbol, R, SSP, Y, I, selected_system) %>%
    summarise(Value = weighted_mean_safe(Value, Weight), .groups = "drop")

  unweighted <- selected %>%
    filter(!grepl("_weighted$", selected_system)) %>%
    group_by(Symbol, R, SSP, Y, I, selected_system) %>%
    summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

  bind_rows(weighted, unweighted) %>%
    group_by(Symbol, R, SSP, I) %>%
    summarise(
      ValueAvgY = mean(Value, na.rm = TRUE),
      Aggregation = paste(sort(unique(selected_system)), collapse = ";"),
      .groups = "drop"
    ) %>%
    add_good_labels()
}

add_baseline_comparison <- function(df) {
  baseline <- df %>%
    filter(as.character(SSP) == baseline_ssp) %>%
    select(Symbol, R, I, BaselineSSP2 = ValueAvgY)

  df %>%
    left_join(baseline, by = c("Symbol", "R", "I")) %>%
    mutate(
      DeltaFromSSP2 = ValueAvgY - BaselineSSP2,
      PercentFromSSP2 = ifelse(is.finite(BaselineSSP2) & BaselineSSP2 != 0, 100 * DeltaFromSSP2 / BaselineSSP2, NA_real_)
    )
}

country_order <- function(df) {
  df %>%
    filter(as.character(SSP) == baseline_ssp) %>%
    group_by(R) %>%
    summarise(order_value = mean(ValueAvgY, na.rm = TRUE), .groups = "drop") %>%
    arrange(order_value, R) %>%
    pull(R)
}

apply_country_order <- function(df) {
  levels_r <- country_order(df)
  df %>%
    mutate(R_factor = factor(R, levels = levels_r))
}

plot_overview_absolute <- function(df, symbol) {
  df <- apply_country_order(df)
  fill_limits <- diverging_limit(df$ValueAvgY)
  p <- ggplot(df, aes(x = SSP, y = R_factor, fill = ValueAvgY)) +
    geom_tile(color = "grey92", linewidth = 0.08) +
    facet_wrap(~I_label, ncol = 4) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = fill_limits,
      oob = scales::squish,
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = sprintf("%s: absolute value by SSP, good, and country", symbol),
      subtitle = "Income elasticity averaged arithmetically over Y; red = large positive, white = near zero, blue = negative; color clipped at p95",
      x = "SSP",
      y = "Country",
      fill = "Avg. value"
    ) +
    theme_bw(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
      axis.text.y = element_text(size = 4.2),
      panel.grid = element_blank(),
      strip.text = element_text(size = 9),
      legend.position = "bottom"
    )

  out <- file.path(output_dir, sprintf("%s_absolute_by_ssp_good_country.png", symbol))
  ggsave(out, p, width = 20, height = 42, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_overview_delta <- function(df, symbol) {
  df <- apply_country_order(df) %>%
    filter(as.character(SSP) != baseline_ssp)
  delta_limit <- clip_abs_limit(df$DeltaFromSSP2)
  p <- ggplot(df, aes(x = SSP, y = R_factor, fill = DeltaFromSSP2)) +
    geom_tile(color = "grey92", linewidth = 0.08) +
    facet_wrap(~I_label, ncol = 4) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-delta_limit, delta_limit),
      oob = scales::squish,
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = sprintf("%s: change from SSP2 by good and country", symbol),
      subtitle = "Positive values mean larger than SSP2; averaged over Y; color clipped at p95",
      x = "SSP",
      y = "Country",
      fill = sprintf("Value - %s", baseline_ssp)
    ) +
    theme_bw(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
      axis.text.y = element_text(size = 4.2),
      panel.grid = element_blank(),
      strip.text = element_text(size = 9),
      legend.position = "bottom"
    )

  out <- file.path(output_dir, sprintf("%s_delta_from_ssp2_by_good_country.png", symbol))
  ggsave(out, p, width = 20, height = 42, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_top_changes <- function(df, symbol) {
  top_df <- df %>%
    filter(as.character(SSP) != baseline_ssp) %>%
    mutate(abs_delta = abs(DeltaFromSSP2)) %>%
    arrange(desc(abs_delta)) %>%
    slice_head(n = 40) %>%
    mutate(label = factor(paste(R, as.character(I_label), as.character(SSP), sep = " | "), levels = rev(paste(R, as.character(I_label), as.character(SSP), sep = " | "))))

  p <- ggplot(top_df, aes(x = DeltaFromSSP2, y = label, fill = DeltaFromSSP2)) +
    geom_col(width = 0.75) +
    geom_vline(xintercept = 0, color = "grey40", linewidth = 0.3) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, guide = "none") +
    scale_x_continuous(trans = pseudo_log_trans(sigma = 1), labels = label_number(accuracy = 0.01)) +
    labs(
      title = sprintf("%s: top absolute changes from SSP2", symbol),
      subtitle = "Country | good | SSP; x-axis uses pseudo-log scaling",
      x = sprintf("Value - %s", baseline_ssp),
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank())

  out <- file.path(output_dir, sprintf("%s_top40_delta_from_ssp2.png", symbol))
  ggsave(out, p, width = 12, height = 11, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_good_pair <- function(df, symbol, good_label) {
  good_df <- apply_country_order(df) %>%
    filter(as.character(I_label) == good_label)
  delta_df <- good_df %>%
    filter(as.character(SSP) != baseline_ssp)

  abs_limits <- diverging_limit(good_df$ValueAvgY)
  delta_limit <- clip_abs_limit(delta_df$DeltaFromSSP2)

  p_abs <- ggplot(good_df, aes(x = SSP, y = R_factor, fill = ValueAvgY)) +
    geom_tile(color = "grey90", linewidth = 0.12) +
    geom_text(aes(label = number(ValueAvgY, accuracy = 0.01)), size = 1.45, color = "grey10") +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = abs_limits,
      oob = scales::squish,
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = "Absolute value",
      subtitle = "Averaged over Y; red = positive, white = near zero, blue = negative; color clipped at p95",
      x = "SSP",
      y = "Country",
      fill = "Avg. value"
    ) +
    theme_bw(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_text(size = 5.6),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )

  p_delta <- ggplot(delta_df, aes(x = SSP, y = R_factor, fill = DeltaFromSSP2)) +
    geom_tile(color = "grey90", linewidth = 0.12) +
    geom_text(aes(label = number(DeltaFromSSP2, accuracy = 0.01)), size = 1.45, color = "grey10") +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-delta_limit, delta_limit),
      oob = scales::squish,
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = sprintf("Change from %s", baseline_ssp),
      subtitle = "Positive = larger than SSP2; color clipped at p95",
      x = "SSP",
      y = NULL,
      fill = "Diff."
    ) +
    theme_bw(base_size = 8) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )

  arranged <- gridExtra::arrangeGrob(
    p_abs,
    p_delta,
    ncol = 2,
    top = grid::textGrob(sprintf("%s | %s", symbol, good_label), gp = grid::gpar(fontsize = 14, fontface = "bold"))
  )

  out <- file.path(by_good_dir, sprintf("%s_%s_abs_and_delta.png", symbol, safe_name(good_label)))
  ggsave(out, arranged, width = 12, height = 30, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
  arranged
}

plot_by_good_pdf <- function(df, symbol) {
  goods <- levels(droplevels(df$I_label))
  out <- file.path(output_dir, sprintf("%s_by_good_abs_and_delta.pdf", symbol))
  grDevices::pdf(out, width = 12, height = 30, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (good_label in goods) {
    grid::grid.newpage()
    grid::grid.draw(plot_good_pair(df, symbol, good_label))
  }
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

add_weighted_influence <- function(df, weight_ssps) {
  weight_sums <- weight_ssps %>%
    group_by(SSP) %>%
    summarise(WeightSum = sum(WeightSSPsIn, na.rm = TRUE), .groups = "drop")

  df %>%
    filter(as.character(SSP) != baseline_ssp) %>%
    left_join(weight_ssps, by = c("SSP", "R")) %>%
    left_join(weight_sums, by = "SSP") %>%
    mutate(
      WeightSSPsIn = coalesce(WeightSSPsIn, 0),
      WeightedValueTerm = WeightSSPsIn * ValueAvgY,
      BaselineValueTerm = BaselineSSP2,
      WeightedContributionToMeanChange = (WeightedValueTerm - BaselineValueTerm) / WeightSum,
      WeightedDeltaTerm = WeightSSPsIn * DeltaFromSSP2 / WeightSum
    ) %>%
    group_by(Symbol, SSP, I, I_label) %>%
    mutate(
      WeightedMeanValue = sum(WeightedValueTerm, na.rm = TRUE) / first(WeightSum),
      BaselineMeanValue = sum(BaselineValueTerm, na.rm = TRUE) / first(WeightSum),
      WeightedMeanDeltaFromSSP2 = sum(WeightedContributionToMeanChange, na.rm = TRUE),
      AbsContributionShare = abs(WeightedContributionToMeanChange) / sum(abs(WeightedContributionToMeanChange), na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      AbsContributionShare = ifelse(is.finite(AbsContributionShare), AbsContributionShare, NA_real_),
      WeightRankInSSP = dense_rank(desc(WeightSSPsIn))
    )
}

plot_weight_ssps_heatmap <- function(weight_ssps) {
  levels_r <- weight_ssps %>%
    filter(as.character(SSP) != baseline_ssp) %>%
    group_by(R) %>%
    summarise(max_weight = max(WeightSSPsIn, na.rm = TRUE), .groups = "drop") %>%
    arrange(max_weight, R) %>%
    pull(R)

  plot_df <- weight_ssps %>%
    mutate(R_factor = factor(R, levels = levels_r))

  p <- ggplot(plot_df, aes(x = SSP, y = R_factor, fill = WeightSSPsIn)) +
    geom_tile(color = "grey92", linewidth = 0.12) +
    scale_fill_gradient(low = "white", high = "#b2182b", labels = label_number(accuracy = 0.01)) +
    labs(
      title = "WeightSSPsIn by SSP and country",
      subtitle = "Redder countries receive larger weights in the weighted SSP average",
      x = "SSP",
      y = "Country",
      fill = "Weight"
    ) +
    theme_bw(base_size = 10) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 9),
      axis.text.y = element_text(size = 4.8),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )

  out <- file.path(output_dir, "WeightSSPsIn_by_ssp_country.png")
  ggsave(out, p, width = 8, height = 28, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_weight_ssps_top <- function(weight_ssps) {
  top_df <- weight_ssps %>%
    filter(as.character(SSP) != baseline_ssp) %>%
    group_by(SSP) %>%
    slice_max(order_by = WeightSSPsIn, n = 30, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(SSP, WeightSSPsIn) %>%
    mutate(label = factor(paste(R, as.character(SSP), sep = " | "), levels = paste(R, as.character(SSP), sep = " | ")))

  p <- ggplot(top_df, aes(x = WeightSSPsIn, y = label, fill = WeightSSPsIn)) +
    geom_col(width = 0.75) +
    facet_wrap(~SSP, scales = "free_y") +
    scale_fill_gradient(low = "white", high = "#b2182b", guide = "none") +
    scale_x_continuous(labels = label_number(accuracy = 0.01)) +
    labs(
      title = "Top WeightSSPsIn countries",
      subtitle = "Country | SSP",
      x = "WeightSSPsIn",
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank())

  out <- file.path(output_dir, "WeightSSPsIn_top30_by_ssp.png")
  ggsave(out, p, width = 12, height = 10, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

weighted_summary <- function(influence) {
  influence %>%
    distinct(Symbol, SSP, I, I_label, WeightedMeanValue, BaselineMeanValue, WeightedMeanDeltaFromSSP2) %>%
    arrange(Symbol, I_label, SSP)
}

plot_weighted_summary <- function(influence) {
  summary_df <- weighted_summary(influence)
  limit <- clip_abs_limit(summary_df$WeightedMeanDeltaFromSSP2)

  p <- ggplot(summary_df, aes(x = SSP, y = I_label, fill = WeightedMeanDeltaFromSSP2)) +
    geom_tile(color = "grey85", linewidth = 0.25) +
    geom_text(aes(label = number(WeightedMeanDeltaFromSSP2, accuracy = 0.01)), size = 3) +
    facet_wrap(~Symbol) +
    scale_fill_gradient2(
      low = "#2166ac",
      mid = "white",
      high = "#b2182b",
      midpoint = 0,
      limits = c(-limit, limit),
      oob = scales::squish,
      labels = label_number(accuracy = 0.01)
    ) +
    labs(
      title = "WeightSSPs-weighted mean change from SSP2",
      subtitle = "Weighted mean = sum(WeightSSPsIn x country value) / sum(WeightSSPsIn)",
      x = "SSP",
      y = "Good",
      fill = "Weighted\nmean diff."
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )

  out <- file.path(output_dir, "WeightSSPs_weighted_mean_delta_by_good.png")
  ggsave(out, p, width = 12, height = 8, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_influence_top_contributions <- function(influence, symbol) {
  top_df <- influence %>%
    filter(Symbol == symbol) %>%
    arrange(desc(abs(WeightedContributionToMeanChange))) %>%
    slice_head(n = 40) %>%
    mutate(
      label_text = sprintf("%s | %s | %s | w=%s", R, as.character(I_label), as.character(SSP), number(WeightSSPsIn, accuracy = 0.01)),
      label = factor(label_text, levels = rev(label_text))
    )

  limit <- clip_abs_limit(top_df$WeightedContributionToMeanChange, prob = 1)

  p <- ggplot(top_df, aes(x = WeightedContributionToMeanChange, y = label, fill = WeightedContributionToMeanChange)) +
    geom_col(width = 0.75) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.3) +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-limit, limit), guide = "none") +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    labs(
      title = sprintf("%s: top country-good pulls after WeightSSPs weighting", symbol),
      subtitle = "Country | good | SSP | weight; contribution = (WeightSSPsIn x value - SSP2 value) / sum(weights)",
      x = "Contribution to weighted mean change from SSP2",
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank())

  out <- file.path(output_dir, sprintf("%s_WeightSSPs_top40_country_good_pulls.png", symbol))
  ggsave(out, p, width = 14, height = 11, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

plot_influence_country_summary <- function(influence, symbol) {
  country_df <- influence %>%
    filter(Symbol == symbol) %>%
    group_by(Symbol, SSP, R) %>%
    summarise(
      WeightSSPsIn = first(WeightSSPsIn),
      SignedContributionAcrossGoods = sum(WeightedContributionToMeanChange, na.rm = TRUE),
      AbsContributionAcrossGoods = sum(abs(WeightedContributionToMeanChange), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(SSP) %>%
    slice_max(order_by = AbsContributionAcrossGoods, n = 25, with_ties = FALSE) %>%
    ungroup() %>%
    arrange(SSP, SignedContributionAcrossGoods) %>%
    mutate(
      label_text = sprintf("%s | %s | w=%s", R, as.character(SSP), number(WeightSSPsIn, accuracy = 0.01)),
      label = factor(label_text, levels = label_text)
    )

  limit <- clip_abs_limit(country_df$SignedContributionAcrossGoods, prob = 1)

  p <- ggplot(country_df, aes(x = SignedContributionAcrossGoods, y = label, fill = SignedContributionAcrossGoods)) +
    geom_col(width = 0.75) +
    geom_vline(xintercept = 0, color = "grey35", linewidth = 0.3) +
    facet_wrap(~SSP, scales = "free_y") +
    scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0, limits = c(-limit, limit), guide = "none") +
    scale_x_continuous(labels = label_number(accuracy = 0.001)) +
    labs(
      title = sprintf("%s: country-level pull across all goods", symbol),
      subtitle = "Country | SSP | weight; signed sum of country-good contributions",
      x = "Signed contribution across goods",
      y = NULL
    ) +
    theme_bw(base_size = 10) +
    theme(panel.grid.minor = element_blank())

  out <- file.path(output_dir, sprintf("%s_WeightSSPs_country_pull_summary.png", symbol))
  ggsave(out, p, width = 14, height = 10, dpi = 300, device = ragg::agg_png)
  cat("Saved:", normalizePath(out, winslash = "/", mustWork = FALSE), "\n")
}

weights <- read_load1a(weights_gdx)
weight_ssps <- read_weight_ssps(weight_ssps_gdx)
cross_country <- read_cross_country(cross_country_gdx)
national <- read_national(national_gdx, weights)

cross_country_comp <- add_baseline_comparison(cross_country)
national_comp <- add_baseline_comparison(national)
weighted_influence <- bind_rows(
  add_weighted_influence(cross_country_comp, weight_ssps),
  add_weighted_influence(national_comp, weight_ssps)
)

write_csv_safe(cross_country_comp, file.path(output_dir, "PoutputIncomeEla_comparison_data.csv"))
write_csv_safe(national_comp, file.path(output_dir, "PoutputIncomeElaNational_comparison_data.csv"))
write_anomaly_diagnostics(cross_country_gdx, national_gdx, cross_country_comp, national_comp)
write_csv_safe(weight_ssps, file.path(output_dir, "WeightSSPsIn_data.csv"))
write_csv_safe(weighted_summary(weighted_influence), file.path(output_dir, "WeightSSPs_weighted_mean_delta_by_good.csv"))
write_csv_safe(weighted_influence, file.path(output_dir, "WeightSSPs_country_good_influence_data.csv"))

national_methods <- national %>%
  distinct(R, Aggregation) %>%
  arrange(Aggregation, R)
write_csv_safe(national_methods, file.path(output_dir, "PoutputIncomeElaNational_aggregation_methods.csv"))

plot_overview_absolute(cross_country_comp, "PoutputIncomeEla")
plot_overview_delta(cross_country_comp, "PoutputIncomeEla")
plot_top_changes(cross_country_comp, "PoutputIncomeEla")
plot_by_good_pdf(cross_country_comp, "PoutputIncomeEla")

plot_overview_absolute(national_comp, "PoutputIncomeElaNational")
plot_overview_delta(national_comp, "PoutputIncomeElaNational")
plot_top_changes(national_comp, "PoutputIncomeElaNational")
plot_by_good_pdf(national_comp, "PoutputIncomeElaNational")

plot_weight_ssps_heatmap(weight_ssps)
plot_weight_ssps_top(weight_ssps)
plot_weighted_summary(weighted_influence)
plot_influence_top_contributions(weighted_influence, "PoutputIncomeEla")
plot_influence_country_summary(weighted_influence, "PoutputIncomeEla")
plot_influence_top_contributions(weighted_influence, "PoutputIncomeElaNational")
plot_influence_country_summary(weighted_influence, "PoutputIncomeElaNational")

cat("Done. Output directory:", normalizePath(output_dir, winslash = "/", mustWork = FALSE), "\n")
