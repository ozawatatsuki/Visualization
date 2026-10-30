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
gdx_file <- file.path(script_dir, "..", "data", "AIDADSCalibCrossCountry_SSP.gdx")
output_dir <- file.path(script_dir, "..", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_file)) {
  stop(sprintf("GDX file was not found: %s", normalizePath(gdx_file, winslash = "/", mustWork = FALSE)))
}

raw <- as_tibble(rgdx.param(gdx_file, "PoutputIncomeEla"))
if (nrow(raw) == 0) {
  stop("PoutputIncomeEla has no records.")
}
if (!"PoutputIncomeEla" %in% names(raw)) {
  stop("Unexpected schema: value column PoutputIncomeEla was not found.")
}

df <- raw %>%
  rename(Value = PoutputIncomeEla) %>%
  transmute(
    SSP = as.character(.data[["SSP"]]),
    Seg = as.character(.data[["Seg"]]),
    Y = as.numeric(as.character(.data[["Y"]])),
    R = as.character(.data[["R"]]),
    I = as.character(.data[["I"]]),
    Value = as.numeric(Value)
  )

if (anyNA(df$Y)) {
  stop("Some Y values could not be converted to numeric.")
}

country_map <- tibble(R = sort(unique(df$R))) %>%
  mutate(
    country_name = countrycode::countrycode(R, origin = "iso3c", destination = "country.name", warn = FALSE),
    country_name = ifelse(is.na(country_name) | !nzchar(country_name), R, country_name)
  )

# Build a country feature vector from PoutputIncomeEla patterns:
# - Take mean over available years per country & category (I)
# - Standardize within each category before clustering (z-score)
features <- df %>%
  group_by(R, I) %>%
  summarise(Value_mean = mean(Value, na.rm = TRUE), .groups = "drop")

wide <- features %>%
  pivot_wider(names_from = I, values_from = Value_mean)

if (nrow(wide) == 0) {
  stop("No country features could be constructed.")
}

country_ids <- wide$R
mat <- wide %>%
  select(-R) %>%
  as.matrix()
rownames(mat) <- country_ids

if (anyNA(mat)) {
  stop("Feature matrix contains NA values. Please check for missing combinations in the input.")
}

mat_z <- scale(mat)

row_dist <- dist(mat_z)
row_hc <- hclust(row_dist, method = "ward.D2")

# Default: 3 clusters as a starting point for SSP1/SSP2/SSP3-like grouping.
k <- 3
clusters_raw <- cutree(row_hc, k = k)
cluster_sizes <- sort(table(clusters_raw), decreasing = TRUE)
cluster_rank_map <- setNames(seq_along(cluster_sizes), names(cluster_sizes))
clusters <- cluster_rank_map[as.character(clusters_raw)]
names(clusters) <- names(clusters_raw)
cluster_factor <- factor(clusters, levels = seq_len(k), labels = paste0("Group", seq_len(k)))

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
seg_label <- paste(sort(unique(df$Seg)), collapse = ", ")
y_min <- min(df$Y, na.rm = TRUE)
y_max <- max(df$Y, na.rm = TRUE)

title_text <- sprintf(
  "PoutputIncomeEla country grouping (mean over years) | SSP: %s | Seg: %s | Y: %d-%d | k=%d",
  ssp_label, seg_label, y_min, y_max, k
)

ht_opt$TITLE_PADDING <- grid::unit(c(6, 6), "mm")

ht <- Heatmap(
  mat_z,
  name = "z",
  col = col_fun,
  column_title = title_text,
  column_title_gp = grid::gpar(fontsize = 18),
  column_title_side = "top",
  cluster_rows = row_hc,
  row_split = k,
  cluster_row_slices = FALSE,
  show_row_dend = TRUE,
  cluster_columns = TRUE,
  show_column_dend = FALSE,
  show_row_names = TRUE,
  row_names_gp = grid::gpar(fontsize = 6),
  column_names_gp = grid::gpar(fontsize = 12),
  column_names_rot = 45,
  left_annotation = anno,
  heatmap_legend_param = list(
    direction = "horizontal",
    title_position = "lefttop",
    legend_width = grid::unit(7, "cm"),
    title_gp = grid::gpar(fontsize = 14),
    labels_gp = grid::gpar(fontsize = 12)
  )
)

out_png <- file.path(output_dir, "PoutputIncomeEla_country_grouping_heatmap_k3.png")
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

cluster_csv <- file.path(output_dir, "PoutputIncomeEla_country_clusters_k3.csv")
write.csv(
  tibble(R = names(clusters)) %>%
    left_join(country_map, by = "R") %>%
    mutate(cluster = as.integer(clusters), group = as.character(cluster_factor)) %>%
    select(R, country_name, cluster, group),
  cluster_csv,
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
cat("Saved:", normalizePath(cluster_csv, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Per-group exports (Group1/2/3) for slide-friendly inspection.

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
    column_names_rot = 45,
    heatmap_legend_param = list(
      direction = "horizontal",
      title_position = "lefttop",
      legend_width = grid::unit(7, "cm"),
      title_gp = grid::gpar(fontsize = 14),
      labels_gp = grid::gpar(fontsize = 12)
    )
  )

  out_png_g <- file.path(output_dir, sprintf("PoutputIncomeEla_country_grouping_heatmap_k3_%s.png", g))
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

  out_csv_g <- file.path(output_dir, sprintf("PoutputIncomeEla_country_clusters_k3_%s.csv", g))
  write.csv(
    tibble(R = g_rows) %>%
      left_join(country_map, by = "R") %>%
      mutate(cluster = as.integer(clusters[g_rows]), group = g) %>%
      select(R, country_name, cluster, group),
    out_csv_g,
    row.names = FALSE,
    fileEncoding = "UTF-8"
  )
  cat("Saved:", normalizePath(out_csv_g, winslash = "/", mustWork = FALSE), "\n")
}
