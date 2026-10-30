suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(tidyr)
  library(ragg)
  library(viridis)
  library(ComplexHeatmap)
  library(circlize)
  library(countrycode)
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
gdx_file <- file.path(script_dir, "..", "data", "AIDADSCalibNationResults_SSPs.gdx")
weights_gdx_file <- file.path(script_dir, "..", "data", "GlobalConsumptionDataPop.gdx")
output_dir <- file.path(script_dir, "..", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
heatmap_output_dir <- file.path(output_dir, "heatmap")
dir.create(heatmap_output_dir, recursive = TRUE, showWarnings = FALSE)
gdx_output_dir <- file.path(output_dir, "gdx")
dir.create(gdx_output_dir, recursive = TRUE, showWarnings = FALSE)

write_csv_safe <- function(df, path) {
  tryCatch(
    {
      write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8")
      cat("Saved:", normalizePath(path, winslash = "/", mustWork = FALSE), "\n")
      path
    },
    error = function(e) {
      ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
      alt <- sub("\\.csv$", paste0("_", ts, ".csv"), path)
      write.csv(df, alt, row.names = FALSE, fileEncoding = "UTF-8")
      cat("Saved (fallback):", normalizePath(alt, winslash = "/", mustWork = FALSE), "\n")
      warning(sprintf("Failed to write %s (%s). Wrote fallback %s.", path, conditionMessage(e), alt))
      alt
    }
  )
}

if (!file.exists(gdx_file)) {
  stop(sprintf("GDX file was not found: %s", normalizePath(gdx_file, winslash = "/", mustWork = FALSE)))
}
if (!file.exists(weights_gdx_file)) {
  stop(sprintf("Weights GDX file was not found: %s", normalizePath(weights_gdx_file, winslash = "/", mustWork = FALSE)))
}

raw <- as_tibble(rgdx.param(gdx_file, "PoutputIncomeElaNational"))
if (nrow(raw) == 0) {
  stop("PoutputIncomeElaNational has no records.")
}
if (!"value" %in% names(raw)) {
  stop("Unexpected schema: value column was not found.")
}
if (!all(c("i1", "i2", "i3", "i4", "i5") %in% names(raw))) {
  stop("Unexpected schema: expected domain columns i1..i5 were not found.")
}

# Select goods (I) for clustering
goods_map <- tibble(
  I_raw = c(
    "Food and nonalcoholic beverages",
    "Energy",
    "Transport",
    "Education",
    "Health"
  ),
  I = c(
    "CP01 Food",
    "CP045 Energy",
    "CP07 Transport",
    "CP10 Education",
    "CP06 Health"
  )
)

# ---------------------------------------------------------------------------
# Weight settings (SSP x country) based on clustering output (selected goods).
#
# - SSP2 is treated as the baseline: weight = 1 for all countries.
# - For other SSPs, specify which cluster represents that SSP-like group.
clustering_variant <- "selectedGoods_CP01_CP045_CP07_CP10_CP06"
# SSP-to-group mapping:
# - Preferred: edit `SSPGroup_mapping_input_{clustering_variant}.csv` in output/gdx and re-run.
# - Fallback: edit the `ssp_group_map_default` below.
# - Use "Group1".."Group{k}" for target_group.
# - Do NOT include SSP2 here (SSP2 is baseline: weight = 1 for all countries).
mapping_input_csv <- file.path(gdx_output_dir, sprintf("SSPGroup_mapping_input_%s.csv", clustering_variant))

ssp_group_map_default <- tibble::tribble(
  ~SSP, ~target_group,
  "SSP1", "Group1",
  "SSP3", "Group3"
  # "SSP4", "Group?",
  # "SSP5", "Group?"
)

if (file.exists(mapping_input_csv)) {
  mapping_raw <- read.csv(mapping_input_csv, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  if (!all(c("SSP", "target_group") %in% names(mapping_raw))) {
    stop(sprintf(
      "Mapping CSV must contain columns SSP and target_group: %s",
      normalizePath(mapping_input_csv, winslash = "/", mustWork = FALSE)
    ))
  }

  ssp_group_map <- mapping_raw %>%
    tibble::as_tibble() %>%
    transmute(
      SSP = as.character(.data[["SSP"]]),
      target_group = as.character(.data[["target_group"]])
    ) %>%
    filter(!is.na(SSP) & nzchar(SSP) & !is.na(target_group) & nzchar(target_group))

  cat("Loaded SSP-group mapping from:", normalizePath(mapping_input_csv, winslash = "/", mustWork = FALSE), "\n")
} else {
  ssp_group_map <- ssp_group_map_default
  cat("Mapping CSV not found; using default SSP-group mapping in code:", "\n")
  print(ssp_group_map_default)
}

baseline_ssp <- "SSP2"
bandwidth_multiplier <- 1.5

# Domain interpretation (based on observed values):
# i1: country code (iso3c)
# i2: SSP
# i3: income tier / segment (multiple possible; average is requested)
# i4: year
# i5: consumption category (I)
df <- raw %>%
  transmute(
    R = as.character(.data[["i1"]]),
    SSP = as.character(.data[["i2"]]),
    H = as.character(.data[["i3"]]),
    Y = as.numeric(as.character(.data[["i4"]])),
    I_raw = as.character(.data[["i5"]]),
    Value = as.numeric(.data[["value"]])
  ) %>%
  filter(.data[["I_raw"]] %in% goods_map$I_raw) %>%
  left_join(goods_map, by = "I_raw") %>%
  mutate(I = factor(.data[["I"]], levels = goods_map$I))

if (anyNA(df$Y)) {
  stop("Some Y values could not be converted to numeric.")
}
if (any(is.na(df$I))) {
  stop("Some selected I values could not be mapped to labels.")
}

country_map <- tibble(R = sort(unique(df$R))) %>%
  mutate(
    country_name = countrycode::countrycode(R, origin = "iso3c", destination = "country.name", warn = FALSE),
    country_name = ifelse(is.na(country_name) | !nzchar(country_name), R, country_name)
  )

# ---------------------------------------------------------------------------
# Weighted averaging for (Area x ConsumptionSegment) tiered countries
load1a_raw <- as_tibble(rgdx.param(weights_gdx_file, "load1a"))
if (nrow(load1a_raw) == 0) {
  stop("load1a has no records in GlobalConsumptionDataPop.gdx.")
}

load1a <- load1a_raw %>%
  transmute(
    Area = as.character(.data[["i1"]]),
    Segment = as.character(.data[["i2"]]),
    R = as.character(.data[["i3"]]),
    Weight = as.numeric(.data[["value"]])
  ) %>%
  filter(
    Area %in% c("Urban", "Rural"),
    Segment %in% c("Higher", "Middle", "Low", "Lowest")
  ) %>%
  group_by(R, Area, Segment) %>%
  summarise(Weight = sum(Weight, na.rm = TRUE), .groups = "drop")

seg_pattern <- "^(Higher|Middle|Low|Lowest)_(urban|rural)$"
seg_countries <- df %>%
  distinct(R, H) %>%
  group_by(R) %>%
  summarise(only_seg = all(grepl(seg_pattern, H)), .groups = "drop") %>%
  filter(.data[["only_seg"]]) %>%
  pull(R)

weighted_countries <- intersect(seg_countries, unique(load1a$R))

weighted_mean_safe <- function(x, w) {
  ok <- is.finite(x) & !is.na(x) & is.finite(w) & !is.na(w) & (w > 0)
  if (!any(ok)) {
    return(mean(x, na.rm = TRUE))
  }
  sum(x[ok] * w[ok]) / sum(w[ok])
}

# Requested aggregation:
# - If multiple income tiers exist, average within (R,SSP,Y,I) first.
#   - For countries split by Area×Segment tiers: weighted by load1a
#   - Else: simple mean
# - Then average over years to get (R,I) features.
df_weighted <- df %>%
  filter(R %in% weighted_countries) %>%
  mutate(
    Segment = sub("_.*$", "", H),
    Area_raw = sub("^.*_", "", H),
    Area = case_when(
      Area_raw == "urban" ~ "Urban",
      Area_raw == "rural" ~ "Rural",
      TRUE ~ NA_character_
    )
  ) %>%
  left_join(load1a, by = c("R", "Area", "Segment"))

per_year_weighted <- df_weighted %>%
  group_by(R, SSP, Y, I) %>%
  summarise(Value = weighted_mean_safe(Value, Weight), .groups = "drop")

per_year_unweighted <- df %>%
  filter(!R %in% weighted_countries) %>%
  group_by(R, SSP, Y, I) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop")

per_year <- bind_rows(per_year_weighted, per_year_unweighted)

features <- per_year %>%
  group_by(R, I) %>%
  summarise(Value_mean = mean(Value, na.rm = TRUE), .groups = "drop")

wide <- features %>%
  pivot_wider(names_from = I, values_from = Value_mean)

if (nrow(wide) == 0) {
  stop("No country features could be constructed.")
}

mat <- wide %>%
  select(-R) %>%
  as.matrix()
rownames(mat) <- wide$R

if (anyNA(mat)) {
  stop("Feature matrix contains NA values. Please check for missing combinations in the input.")
}

mat_z <- scale(mat)

row_dist <- dist(mat_z)
row_hc <- hclust(row_dist, method = "ward.D2")

# 5 groups, re-labeled so the top slice is Group1 (top-to-bottom order).
k <- 5
clusters_raw <- cutree(row_hc, k = k)
global_row_order <- row_hc$labels[row_hc$order]
clusters_raw_ordered <- clusters_raw[global_row_order]
cluster_order <- unique(as.integer(clusters_raw_ordered))
cluster_rank_map <- setNames(seq_along(cluster_order), as.character(cluster_order))
clusters <- as.integer(cluster_rank_map[as.character(clusters_raw)])
names(clusters) <- names(clusters_raw)
cluster_factor <- factor(clusters, levels = seq_len(k), labels = paste0("Group", seq_len(k)))

ssp_group_map_parsed <- ssp_group_map %>%
  mutate(
    target_cluster = as.integer(sub("^Group", "", target_group)),
    target_group = as.character(target_group)
  )

if (any(ssp_group_map_parsed$SSP %in% baseline_ssp)) {
  stop(sprintf("ssp_group_map must not contain baseline_ssp (%s).", paste(baseline_ssp, collapse = ", ")))
}
if (anyNA(ssp_group_map_parsed$target_cluster)) {
  stop("ssp_group_map has invalid target_group. Use 'Group1'..'Group{k}'.")
}
if (any(ssp_group_map_parsed$target_cluster < 1 | ssp_group_map_parsed$target_cluster > k)) {
  stop(sprintf("ssp_group_map target_group must be within Group1..Group%d.", k))
}
if (anyDuplicated(ssp_group_map_parsed$SSP)) {
  stop("ssp_group_map contains duplicated SSP entries.")
}

ssp_cluster_map <- setNames(ssp_group_map_parsed$target_cluster, ssp_group_map_parsed$SSP)

cluster_palette <- setNames(viridis::viridis(k, option = "D", end = 0.9), levels(cluster_factor))

anno <- rowAnnotation(
  Cluster = cluster_factor,
  col = list(Cluster = cluster_palette),
  show_annotation_name = TRUE,
  annotation_name_gp = grid::gpar(fontsize = 14)
)

z_lim <- 2.5
col_fun <- circlize::colorRamp2(
  c(-z_lim, 0, z_lim),
  c("#2166ac", "#f7f7f7", "#b2182b")
)

ssp_label <- paste(sort(unique(df$SSP)), collapse = ", ")
y_min <- min(df$Y, na.rm = TRUE)
y_max <- max(df$Y, na.rm = TRUE)

title_text <- sprintf(
  "PoutputIncomeElaNational grouping (goods: %s; load1a-weighted tiers; then years) | SSP: %s | Y: %d-%d | k=%d",
  paste(goods_map$I, collapse = ", "),
  ssp_label, y_min, y_max, k
)

ht_opt$TITLE_PADDING <- grid::unit(c(6, 6), "mm")

ht <- Heatmap(
  mat_z,
  name = "z",
  col = col_fun,
  column_title = title_text,
  column_title_gp = grid::gpar(fontsize = 18),
  column_title_side = "top",
  row_title = paste0("Group", seq_len(k)),
  row_title_rot = 0,
  cluster_rows = row_hc,
  row_split = k,
  cluster_row_slices = FALSE,
  show_row_dend = TRUE,
  cluster_columns = TRUE,
  show_column_dend = FALSE,
  show_row_names = TRUE,
  row_names_gp = grid::gpar(fontsize = 6),
  column_names_gp = grid::gpar(fontsize = 12),
  column_names_rot = 0,
  left_annotation = anno,
  heatmap_legend_param = list(
    direction = "horizontal",
    title_position = "lefttop",
    legend_width = grid::unit(7, "cm"),
    title_gp = grid::gpar(fontsize = 14),
    labels_gp = grid::gpar(fontsize = 12)
  )
)

suffix <- "goods_CP01_CP045_CP07_CP10_CP06"
out_png <- file.path(heatmap_output_dir, sprintf("PoutputIncomeElaNational_country_grouping_heatmap_k%d_%s.png", k, suffix))
ragg::agg_png(out_png, width = 18, height = 11.5, units = "in", res = 300)
draw(
  ht,
  heatmap_legend_side = "bottom",
  annotation_legend_side = "bottom",
  main_heatmap = "z",
  padding = grid::unit(c(4, 4, 10, 4), "mm")
)
dev.off()
cat("Saved:", normalizePath(out_png, winslash = "/", mustWork = FALSE), "\n")

country_groups_csv <- file.path(heatmap_output_dir, sprintf("PoutputIncomeElaNational_country_groups_k%d_%s.csv", k, suffix))
group_ssp_map <- ssp_group_map_parsed %>%
  group_by(target_group) %>%
  summarise(mapped_SSP = paste(sort(SSP), collapse = ";"), .groups = "drop") %>%
  rename(group = target_group)

write_csv_safe(
  tibble(R = names(clusters)) %>%
    left_join(country_map, by = "R") %>%
    mutate(cluster = as.integer(clusters), group = as.character(cluster_factor)) %>%
    left_join(group_ssp_map, by = "group") %>%
    select(R, country_name, cluster, group, mapped_SSP),
  country_groups_csv
)

representative_countries <- tibble::tibble(
  representative_SSP = rep(c("SSP1", "SSP3"), each = 3),
  R = c("FRA", "JPN", "AUS", "BDI", "GHA", "NGA")
)

missing_representatives <- setdiff(representative_countries$R, rownames(mat_z))
if (length(missing_representatives) > 0) {
  warning(sprintf("Representative countries not found in mat_z: %s", paste(missing_representatives, collapse = ", ")))
}

representative_rows <- representative_countries %>%
  filter(R %in% rownames(mat_z)) %>%
  left_join(country_map, by = "R") %>%
  mutate(
    country_label = if_else(is.na(country_name) | !nzchar(country_name), R, paste0(R, " - ", country_name)),
    group = as.character(cluster_factor[R])
  )

if (nrow(representative_rows) > 0) {
  mat_rep <- mat_z[representative_rows$R, , drop = FALSE]

  rep_anno <- rowAnnotation(
    Group = representative_rows$group,
    col = list(Group = cluster_palette),
    show_annotation_name = TRUE,
    annotation_name_gp = grid::gpar(fontsize = 12)
  )

  ht_rep <- Heatmap(
    mat_rep,
    name = "z",
    col = col_fun,
    column_title = sprintf("Representative countries for SSP1/SSP3 | %s | k=%d", clustering_variant, k),
    column_title_gp = grid::gpar(fontsize = 16),
    column_title_side = "top",
    row_labels = representative_rows$country_label,
    row_split = representative_rows$representative_SSP,
    row_title_rot = 0,
    cluster_rows = FALSE,
    cluster_columns = TRUE,
    show_column_dend = FALSE,
    show_row_names = TRUE,
    row_names_gp = grid::gpar(fontsize = 12),
    column_names_gp = grid::gpar(fontsize = 11),
    column_names_rot = 0,
    left_annotation = rep_anno,
    heatmap_legend_param = list(
      direction = "horizontal",
      title_position = "lefttop",
      legend_width = grid::unit(7, "cm"),
      title_gp = grid::gpar(fontsize = 12),
      labels_gp = grid::gpar(fontsize = 11)
    )
  )

  out_rep_png <- file.path(heatmap_output_dir, sprintf("PoutputIncomeElaNational_representative_countries_heatmap_k%d_%s.png", k, suffix))
  ragg::agg_png(out_rep_png, width = 10, height = 5.8, units = "in", res = 300)
  draw(
    ht_rep,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom",
    main_heatmap = "z",
    padding = grid::unit(c(4, 4, 10, 4), "mm")
  )
  dev.off()
  cat("Saved:", normalizePath(out_rep_png, winslash = "/", mustWork = FALSE), "\n")
}

# ---------------------------------------------------------------------------
# SSP-like weights from clustering (Soft centroid).
# - For SSPs mapped to a group, compute a centroid from that group (in z-space),
#   then compute weights for *all* countries based on distance to the centroid.
# - SSP2 is baseline: weight = 1 for all countries.

calc_ssp_weights_soft_centroid <- function(X_scaled,
                                           clusters,
                                           ssp_cluster_map,
                                           baseline_ssp = "SSP2",
                                           bandwidth_multiplier = 1.0) {
  stopifnot(!is.null(rownames(X_scaled)))

  countries <- rownames(X_scaled)
  clusters_vec <- as.integer(clusters[countries])

  if (length(clusters_vec) != nrow(X_scaled)) {
    stop("Length of clusters must match nrow(X_scaled) after aligning by rownames.")
  }

  result_list <- list()

  # 1) Baseline SSP(s): all countries get weight = 1
  if (!is.null(baseline_ssp) && length(baseline_ssp) > 0) {
    for (ssp in baseline_ssp) {
      result_list[[ssp]] <- tibble::tibble(
        SSP = ssp,
        R = countries,
        weight = 1
      )
    }
  }

  # 2) Soft centroid weights for SSPs mapped to clusters
  for (ssp in names(ssp_cluster_map)) {
    target_cluster <- as.integer(unname(ssp_cluster_map[[ssp]]))
    target_idx <- which(clusters_vec == target_cluster)

    if (length(target_idx) == 0) {
      warning(sprintf("No countries found for target cluster %d for %s. Using weight=1.", target_cluster, ssp))
      weight <- rep(1, length(countries))
    } else {
      X_target <- X_scaled[target_idx, , drop = FALSE]
      centroid <- colMeans(X_target, na.rm = TRUE)

      centroid_mat <- matrix(centroid, nrow = nrow(X_scaled), ncol = ncol(X_scaled), byrow = TRUE)
      distance_all <- sqrt(rowSums((X_scaled - centroid_mat) ^ 2))
      distance_target <- distance_all[target_idx]

      sigma_s <- mean(distance_target, na.rm = TRUE) * bandwidth_multiplier
      if (is.na(sigma_s) || sigma_s <= 0) {
        sigma_s <- mean(distance_all, na.rm = TRUE) * bandwidth_multiplier
      }
      if (is.na(sigma_s) || sigma_s <= 0) {
        sigma_s <- 1
      }

      w_raw <- exp(-(distance_all ^ 2) / (2 * sigma_s ^ 2))
      w_raw[is.na(w_raw) | is.nan(w_raw) | is.infinite(w_raw)] <- 0

      mean_w <- mean(w_raw, na.rm = TRUE)
      if (is.na(mean_w) || mean_w <= 0) {
        weight <- rep(1, length(countries))
      } else {
        weight <- w_raw / mean_w
      }
    }

    result_list[[ssp]] <- tibble::tibble(
      SSP = ssp,
      R = countries,
      weight = as.numeric(weight)
    )
  }

  df_weight_ssps <- dplyr::bind_rows(result_list) %>%
    dplyr::mutate(
      weight = dplyr::if_else(
        is.na(weight) | is.nan(weight) | is.infinite(weight),
        0,
        weight
      )
    )

  df_weight_ssps
}

calc_ssp_weights_hard_cluster <- function(X_scaled,
                                          clusters,
                                          ssp_cluster_map,
                                          baseline_ssp = "SSP2",
                                          target_weight = 10,
                                          other_weight = 0.1) {
  stopifnot(!is.null(rownames(X_scaled)))

  countries <- rownames(X_scaled)
  clusters_vec <- as.integer(clusters[countries])

  if (length(clusters_vec) != nrow(X_scaled)) {
    stop("Length of clusters must match nrow(X_scaled) after aligning by rownames.")
  }

  result_list <- list()

  if (!is.null(baseline_ssp) && length(baseline_ssp) > 0) {
    for (ssp in baseline_ssp) {
      result_list[[ssp]] <- tibble::tibble(
        SSP = ssp,
        R = countries,
        weight = 1
      )
    }
  }

  for (ssp in names(ssp_cluster_map)) {
    target_cluster <- as.integer(unname(ssp_cluster_map[[ssp]]))
    weight <- ifelse(clusters_vec == target_cluster, target_weight, other_weight)

    result_list[[ssp]] <- tibble::tibble(
      SSP = ssp,
      R = countries,
      weight = weight
    )
  }

  dplyr::bind_rows(result_list)
}

df_weight_ssps <- calc_ssp_weights_soft_centroid(
  X_scaled = mat_z,
  clusters = clusters,
  ssp_cluster_map = ssp_cluster_map,
  baseline_ssp = baseline_ssp,
  bandwidth_multiplier = bandwidth_multiplier
)

df_weight_ssps_hard <- calc_ssp_weights_hard_cluster(
  X_scaled = mat_z,
  clusters = clusters,
  ssp_cluster_map = ssp_cluster_map,
  baseline_ssp = baseline_ssp
)

df_weight_ssps_one_or_point_one <- calc_ssp_weights_hard_cluster(
  X_scaled = mat_z,
  clusters = clusters,
  ssp_cluster_map = ssp_cluster_map,
  baseline_ssp = baseline_ssp,
  target_weight = 1,
  other_weight = 0.1
)

df_weight_check <- df_weight_ssps %>%
  group_by(SSP) %>%
  summarise(
    n_total = n(),
    n_positive = sum(weight > 0),
    min_weight = min(weight, na.rm = TRUE),
    max_weight = max(weight, na.rm = TRUE),
    mean_weight = mean(weight, na.rm = TRUE),
    sd_weight = sd(weight, na.rm = TRUE),
    sum_weight = sum(weight, na.rm = TRUE),
    .groups = "drop"
  )

print(df_weight_check)

# Optional visualization: countries by SSP weight (ALL countries; tall figure)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  df_top <- df_weight_ssps %>%
    left_join(country_map, by = "R") %>%
    mutate(country_label = if_else(is.na(country_name) | !nzchar(country_name), R, country_name)) %>%
    arrange(SSP, weight) %>%
    mutate(country_f = factor(paste0(country_label, "___", SSP), levels = unique(paste0(country_label, "___", SSP)))) %>%
    ungroup()

  p_top <- ggplot2::ggplot(df_top, ggplot2::aes(x = weight, y = country_f)) +
    ggplot2::geom_col(width = 0.8, fill = "#2c7fb8") +
    ggplot2::facet_wrap(~SSP, scales = "free_y", ncol = 1) +
    ggplot2::scale_y_discrete(labels = function(x) sub("___.*$", "", x)) +
    ggplot2::labs(
      title = sprintf("Countries by weight (Soft centroid) | %s | k=%d", clustering_variant, k),
      x = "weight",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 7),
      strip.text = ggplot2::element_text(size = 12, face = "bold")
    )

  n_countries <- dplyr::n_distinct(df_top$R)
  n_ssps <- dplyr::n_distinct(df_top$SSP)
  height_in <- max(12, 0.07 * n_countries * n_ssps)

  out_top_png_variant <- file.path(heatmap_output_dir, sprintf("WeightSSPs_top_countries_%s_k%d.png", clustering_variant, k))
  ragg::agg_png(out_top_png_variant, width = 14, height = height_in, units = "in", res = 300)
  print(p_top)
  dev.off()
  cat("Saved:", normalizePath(out_top_png_variant, winslash = "/", mustWork = FALSE), "\n")

  out_top_png_std <- file.path(heatmap_output_dir, "WeightSSPs_top_countries.png")
  file.copy(out_top_png_variant, out_top_png_std, overwrite = TRUE)
  cat("Saved:", normalizePath(out_top_png_std, winslash = "/", mustWork = FALSE), "\n")
} else {
  cat("ggplot2 not installed; skipping WeightSSPs_top_countries.png\n")
}

# GDX export: WeightSSPsIn(SSP, R)
ssp_uels <- sort(unique(df_weight_ssps$SSP))
r_uels <- sort(unique(df_weight_ssps$R))

set_ssp <- data.frame(i = factor(ssp_uels, levels = ssp_uels))
attr(set_ssp, "symName") <- "SSP"

set_r <- data.frame(i = factor(r_uels, levels = r_uels))
attr(set_r, "symName") <- "R"

param_weight <- df_weight_ssps %>%
  transmute(
    SSP = factor(SSP, levels = ssp_uels),
    R = factor(R, levels = r_uels),
    value = as.numeric(weight)
  ) %>%
  as.data.frame()
attr(param_weight, "symName") <- "WeightSSPsIn"
attr(param_weight, "domains") <- c("SSP", "R")

out_gdx <- file.path(gdx_output_dir, sprintf("WeightSSPs_%s.gdx", clustering_variant))
wgdx.lst(out_gdx, list(set_ssp, set_r, param_weight))
cat("Saved:", normalizePath(out_gdx, winslash = "/", mustWork = FALSE), "\n")

hard_gdx_output_dir <- file.path(gdx_output_dir, "VER0")
dir.create(hard_gdx_output_dir, recursive = TRUE, showWarnings = FALSE)

ssp_uels_hard <- sort(unique(df_weight_ssps_hard$SSP))
r_uels_hard <- sort(unique(df_weight_ssps_hard$R))

set_ssp_hard <- data.frame(i = factor(ssp_uels_hard, levels = ssp_uels_hard))
attr(set_ssp_hard, "symName") <- "SSP"

set_r_hard <- data.frame(i = factor(r_uels_hard, levels = r_uels_hard))
attr(set_r_hard, "symName") <- "R"

param_weight_hard <- df_weight_ssps_hard %>%
  transmute(
    SSP = factor(SSP, levels = ssp_uels_hard),
    R = factor(R, levels = r_uels_hard),
    value = as.numeric(weight)
  ) %>%
  as.data.frame()
attr(param_weight_hard, "symName") <- "WeightSSPsIn"
attr(param_weight_hard, "domains") <- c("SSP", "R")

out_hard_gdx <- file.path(hard_gdx_output_dir, sprintf("WeightSSPs_%s.gdx", clustering_variant))
wgdx.lst(out_hard_gdx, list(set_ssp_hard, set_r_hard, param_weight_hard))
cat("Saved hard cluster weights:", normalizePath(out_hard_gdx, winslash = "/", mustWork = FALSE), "\n")

one_or_point_one_gdx_output_dir <- file.path(gdx_output_dir, "VER0_1or0.1")
dir.create(one_or_point_one_gdx_output_dir, recursive = TRUE, showWarnings = FALSE)

ssp_uels_one_or_point_one <- sort(unique(df_weight_ssps_one_or_point_one$SSP))
r_uels_one_or_point_one <- sort(unique(df_weight_ssps_one_or_point_one$R))

set_ssp_one_or_point_one <- data.frame(i = factor(ssp_uels_one_or_point_one, levels = ssp_uels_one_or_point_one))
attr(set_ssp_one_or_point_one, "symName") <- "SSP"

set_r_one_or_point_one <- data.frame(i = factor(r_uels_one_or_point_one, levels = r_uels_one_or_point_one))
attr(set_r_one_or_point_one, "symName") <- "R"

param_weight_one_or_point_one <- df_weight_ssps_one_or_point_one %>%
  transmute(
    SSP = factor(SSP, levels = ssp_uels_one_or_point_one),
    R = factor(R, levels = r_uels_one_or_point_one),
    value = as.numeric(weight)
  ) %>%
  as.data.frame()
attr(param_weight_one_or_point_one, "symName") <- "WeightSSPsIn"
attr(param_weight_one_or_point_one, "domains") <- c("SSP", "R")

out_one_or_point_one_gdx <- file.path(one_or_point_one_gdx_output_dir, sprintf("WeightSSPs_%s.gdx", clustering_variant))
wgdx.lst(out_one_or_point_one_gdx, list(set_ssp_one_or_point_one, set_r_one_or_point_one, param_weight_one_or_point_one))
cat("Saved 1-or-0.1 cluster weights:", normalizePath(out_one_or_point_one_gdx, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Per-group exports (Group1..k), slide-friendly.
global_row_order <- row_hc$labels[row_hc$order]
group_levels <- levels(cluster_factor)

for (g in group_levels) {
  g_rows <- names(cluster_factor)[cluster_factor == g]
  n_countries <- length(g_rows)
  g_order <- global_row_order[global_row_order %in% g_rows]
  mat_g <- mat_z[g_order, , drop = FALSE]

  ht_g <- Heatmap(
    mat_g,
    name = "z",
    col = col_fun,
    column_title = sprintf("%s | %s only (n=%d)", title_text, g, n_countries),
    column_title_gp = grid::gpar(fontsize = 18),
    column_title_side = "top",
    cluster_rows = TRUE,
    show_row_dend = TRUE,
    cluster_columns = TRUE,
    show_column_dend = FALSE,
    show_row_names = TRUE,
    row_names_gp = grid::gpar(fontsize = 10),
    column_names_gp = grid::gpar(fontsize = 12),
    column_names_rot = 0,
    heatmap_legend_param = list(
      direction = "horizontal",
      title_position = "lefttop",
      legend_width = grid::unit(7, "cm"),
      title_gp = grid::gpar(fontsize = 14),
      labels_gp = grid::gpar(fontsize = 12)
    )
  )

  out_png_g <- file.path(heatmap_output_dir, sprintf("PoutputIncomeElaNational_country_grouping_heatmap_k%d_%s_%s.png", k, suffix, g))
  height_in <- if (g == "Group1") 11 else 10
  ragg::agg_png(out_png_g, width = 20, height = height_in, units = "in", res = 300)
  draw(
    ht_g,
    heatmap_legend_side = "bottom",
    annotation_legend_side = "bottom",
    main_heatmap = "z",
    padding = grid::unit(c(4, 4, 10, 4), "mm")
  )
  dev.off()
  cat("Saved:", normalizePath(out_png_g, winslash = "/", mustWork = FALSE), "\n")
}
