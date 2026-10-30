suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

gdx_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*", "Fooddem_PriceEffect.gdx"))
if (length(gdx_candidates) != 1L) {
  stop("Expected exactly one Fooddem_PriceEffect.gdx under data/260807_FDM*; found: ", length(gdx_candidates))
}
gdx_path <- gdx_candidates[[1]]

target_country <- "AFG"
target_year <- "2050"
symbol_name <- "CrossPriceEla_Food"
out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "cross_price_elasticity_food_AFG_2050"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

universe_values <- rgdx(gdx_path)$uels
if (length(universe_values) == 0L) {
  stop("The GDX universe is empty.")
}

request <- list(
  name = symbol_name,
  form = "sparse",
  uels = list(universe_values, target_country, universe_values, target_year, universe_values)
)
cross_raw <- rgdx(gdx_path, request)
if (cross_raw$dim != 5L || nrow(cross_raw$val) == 0L) {
  stop(symbol_name, " has no data for ", target_country, " in ", target_year, ".")
}

cross <- data.table(
  Ref = cross_raw$uels[[1L]][cross_raw$val[, 1L]],
  R = cross_raw$uels[[2L]][cross_raw$val[, 2L]],
  seg2 = cross_raw$uels[[3L]][cross_raw$val[, 3L]],
  Y = cross_raw$uels[[4L]][cross_raw$val[, 4L]],
  I = cross_raw$uels[[5L]][cross_raw$val[, 5L]],
  value = as.numeric(cross_raw$val[, 6L])
)
cross[, `:=`(
  Ref = as.character(Ref),
  R = as.character(R),
  seg2 = as.character(seg2),
  Y = as.character(Y),
  I = as.character(I)
)]

budget <- as.data.table(rgdx.param(gdx_path, "BudgetInitial"))
required_budget_cols <- c("seg2", "BudgetInitial")
if (!all(required_budget_cols %in% names(budget))) {
  stop("Unexpected BudgetInitial columns: ", paste(names(budget), collapse = ", "))
}
budget <- unique(budget[, .(
  seg2 = as.character(seg2),
  budgetinitial = as.numeric(BudgetInitial)
)])
if (anyDuplicated(budget$seg2)) {
  stop("BudgetInitial contains duplicate seg2 rows.")
}

plot_data <- merge(cross, budget, by = "seg2", all.x = TRUE, sort = FALSE)
if (anyNA(plot_data$budgetinitial)) {
  stop("Missing BudgetInitial for seg2: ", paste(unique(plot_data[is.na(budgetinitial), seg2]), collapse = ", "))
}
if (any(!is.finite(plot_data$budgetinitial)) || any(plot_data$budgetinitial <= 0)) {
  stop("BudgetInitial must be finite and positive for the logarithmic x-axis.")
}
if (anyDuplicated(plot_data[, .(Ref, I, seg2)])) {
  stop("Duplicate Ref-I-seg2 observations found.")
}
setorder(plot_data, I, Ref, budgetinitial)

present_refs <- unique(plot_data$Ref)
ref_labels <- c(
  SSP2_400C_2025CP_NoCC_No = "2025CP",
  SSP2_400C_2030CP_NoCC_No = "2030CP",
  SSP2_BaU_NoCC_No = "BaU"
)
plot_data[, Scenario := fifelse(Ref %chin% names(ref_labels), ref_labels[Ref], Ref)]
if (length(present_refs) == 1L) {
  p <- ggplot(plot_data, aes(x = budgetinitial, y = value, group = 1L)) +
    geom_hline(yintercept = 0, color = "grey68", linewidth = 0.5) +
    geom_line(color = "#0072B2", linewidth = 1.05)
} else {
  p <- ggplot(plot_data, aes(x = budgetinitial, y = value, color = Scenario, group = Scenario)) +
    geom_hline(yintercept = 0, color = "grey68", linewidth = 0.5) +
    geom_line(linewidth = 1.05) +
    scale_color_viridis_d(end = 0.9)
}

p <- p +
  facet_wrap(
    vars(I),
    ncol = 3,
    scales = "free_y",
    labeller = labeller(I = label_wrap_gen(width = 20))
  ) +
  scale_x_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_y_continuous(labels = label_number()) +
  labs(
    title = "CrossPriceEla_Food: Afghanistan, 2050",
    x = "BudgetInitial corresponding to seg2 (log scale)",
    y = "Value",
    color = "Scenario"
  ) +
  theme_minimal(base_size = 17, base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey86", linewidth = 0.4),
    panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.5),
    axis.text = element_text(color = "grey20", size = 16),
    axis.title = element_text(color = "grey10", size = 18),
    strip.text = element_text(
      face = "bold",
      color = "grey10",
      size = 14.5,
      lineheight = 0.95,
      margin = margin(7, 4, 7, 4)
    ),
    strip.background = element_rect(fill = "grey96", color = NA),
    plot.title = element_text(face = "bold", color = "grey10", size = 24),
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 17),
    legend.key.width = grid::unit(1.3, "cm"),
    legend.position = if (length(present_refs) > 1L) "bottom" else "none",
    plot.margin = margin(14, 16, 12, 14)
  )

png_path <- file.path(out_dir, "CrossPriceEla_Food_AFG_2050_by_I.png")
csv_path <- file.path(out_dir, "CrossPriceEla_Food_AFG_2050_data.csv")
manifest_path <- file.path(out_dir, "CrossPriceEla_Food_AFG_2050_manifest.csv")

ggsave(
  filename = png_path,
  plot = p,
  width = 11.5,
  height = 13.8,
  units = "in",
  dpi = 300,
  bg = "white"
)
fwrite(plot_data[, .(Ref, R, Y, I, seg2, budgetinitial, value)], csv_path)
fwrite(
  data.table(
    source_gdx = normalizePath(gdx_path, winslash = "/", mustWork = TRUE),
    symbol = symbol_name,
    country = target_country,
    year = target_year,
    ref_count = length(present_refs),
    refs = paste(present_refs, collapse = " | "),
    facet_count = uniqueN(plot_data$I),
    segment_count = uniqueN(plot_data$seg2),
    observation_count = nrow(plot_data),
    x_scale = "log10"
  ),
  manifest_path
)

cat("Created:", normalizePath(png_path, winslash = "/", mustWork = TRUE), "\n")
cat("Refs:", paste(present_refs, collapse = ", "), "\n")
cat("Facets:", uniqueN(plot_data$I), " Segments:", uniqueN(plot_data$seg2), " Rows:", nrow(plot_data), "\n")
