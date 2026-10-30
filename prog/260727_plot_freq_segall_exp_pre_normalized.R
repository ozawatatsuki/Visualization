suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

analysis_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "AnalysisExpenditureMerged.gdx"))
if (length(analysis_candidates) != 1) {
  stop("Expected exactly one AnalysisExpenditureMerged.gdx under data/260727_FDM*, found: ", length(analysis_candidates))
}
analysis_gdx_path <- analysis_candidates[[1]]

agg_candidates <- c(
  Sys.glob(file.path(project_dir, "data", "260727_FDM*", "Fooddem_Agg.gdx")),
  Sys.glob(file.path(project_dir, "data", "260727_FDM*", "Fooddem_Agg_seg.gdx"))
)
agg_candidates <- unique(agg_candidates)
if (length(agg_candidates) < 1) {
  stop("Expected Fooddem_Agg.gdx or Fooddem_Agg_seg.gdx under data/260727_FDM*.")
}
agg_gdx_path <- agg_candidates[[1]]

map_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "region_iso_R17PHI.map"))
if (length(map_candidates) != 1) {
  stop("Expected exactly one region_iso_R17PHI.map under data/260727_FDM*, found: ", length(map_candidates))
}
region_map_path <- map_candidates[[1]]

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update", "freq_segall_exp_pre_normalized")
country_out_dir <- file.path(out_dir, "country_by_r17")
dir.create(country_out_dir, recursive = TRUE, showWarnings = FALSE)

plot_year <- 2050L
scenario <- "SSP2_400C_2025CP_NoCC_No"
baseline_ref <- "SSP2_BaU_NoCC_No"

is_country_code <- function(x) {
  !x %in% "WLD" & !grepl("^R17", x)
}

read_region_map <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !grepl("^\\*", lines)]
  parts <- tstrsplit(lines, ".", fixed = TRUE)
  if (length(parts) != 2) {
    stop("region_iso_R17PHI.map must contain rows like ISO3.R17.")
  }

  out <- data.table(
    R = trimws(parts[[1]]),
    R17 = trimws(parts[[2]])
  )
  out <- out[nzchar(R) & nzchar(R17)]
  if (anyDuplicated(out$R) > 0) {
    dup <- out[duplicated(R), unique(R)]
    stop("Duplicate ISO3 rows in region map: ", paste(head(dup, 20), collapse = ", "))
  }
  out
}

theme_report <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.major.y = element_line(color = "grey86", linewidth = 0.25),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey10"),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey30"),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.text = element_text(face = "bold", color = "grey10"),
      strip.background = element_rect(fill = "grey96", color = NA),
      plot.margin = margin(8, 10, 8, 8)
    )
}

clear_pngs <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(NULL))
  }
  png_files <- list.files(path, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
  if (length(png_files) > 0) {
    unlink(png_files, force = TRUE)
  }
  invisible(NULL)
}

budget_bar_bounds <- function(budget) {
  bounds <- unique(budget[is.finite(BudgetInitial) & BudgetInitial > 0, .(seg, BudgetInitial)])
  setorder(bounds, BudgetInitial)
  if (nrow(bounds) == 1L) {
    bounds[, `:=`(bar_xmin = BudgetInitial * 0.95, bar_xmax = BudgetInitial * 1.05)]
    return(bounds[, .(seg, bar_xmin, bar_xmax)])
  }

  budget_values <- bounds$BudgetInitial
  mids <- (head(budget_values, -1L) + tail(budget_values, -1L)) / 2
  lower <- c(max(0, budget_values[1L] - (mids[1L] - budget_values[1L])), mids)
  last_idx <- length(budget_values)
  upper <- c(mids, budget_values[last_idx] + (budget_values[last_idx] - mids[length(mids)]))
  bounds[, `:=`(bar_xmin = lower, bar_xmax = upper)]
  bounds[, .(seg, bar_xmin, bar_xmax)]
}

standardize_freq_columns <- function(freq_dt) {
  generic_cols <- paste0("i", 1:5)
  if (all(c(generic_cols, "value") %in% names(freq_dt))) {
    setnames(freq_dt, c(generic_cols, "value"), c("source", "R", "Ref", "Y", "seg", "FreqSegall_exp"))
  } else if ("seg2" %in% names(freq_dt)) {
    setnames(freq_dt, "seg2", "seg")
    freq_dt[, source := "AnalysisExpenditure"]
  } else if (!"source" %in% names(freq_dt)) {
    freq_dt[, source := "AnalysisExpenditure"]
  }
  freq_dt
}

cat("Reading BudgetInitial...\n")
budget <- as.data.table(rgdx.param(agg_gdx_path, "BudgetInitial"))
if ("seg2" %in% names(budget)) {
  setnames(budget, "seg2", "seg")
}
budget[, `:=`(
  seg = as.character(seg),
  BudgetInitial = as.numeric(BudgetInitial)
)]
bar_bounds <- budget_bar_bounds(budget)

cat("Reading pre-normalized FreqSegall_exp...\n")
freq_segall <- as.data.table(rgdx.param(analysis_gdx_path, "FreqSegall_exp"))
freq_segall <- standardize_freq_columns(freq_segall)
freq_segall <- freq_segall[
  Ref %in% c(baseline_ref, scenario) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(FreqSegall_exp),
  .(
    source = as.character(source),
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    frequency = as.numeric(FreqSegall_exp)
  )
]

freq_segall <- merge(freq_segall, budget, by = "seg", all.x = TRUE)
freq_segall <- merge(freq_segall, bar_bounds, by = "seg", all.x = TRUE)
if (anyNA(freq_segall$BudgetInitial) || anyNA(freq_segall$bar_xmin) || anyNA(freq_segall$bar_xmax)) {
  missing_seg <- unique(freq_segall[is.na(BudgetInitial) | is.na(bar_xmin) | is.na(bar_xmax), seg])
  stop("Missing BudgetInitial for pre-normalized FreqSegall_exp seg: ", paste(head(missing_seg, 20), collapse = ", "))
}

region_map <- read_region_map(region_map_path)
country_map <- merge(
  data.table(R = sort(unique(freq_segall$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_map[is.na(R17), R17 := "_unmapped"]
unmapped <- country_map[R17 == "_unmapped", R]
if (length(unmapped) > 0) {
  fwrite(data.table(R = unmapped), file.path(country_out_dir, "unmapped_freq_segall_exp_pre_normalized_iso3.csv"))
}

freq_segall <- merge(freq_segall, country_map, by = "R", all.x = TRUE)
freq_segall[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
freq_summary <- freq_segall[
  ,
  .(
    frequency_sum = sum(frequency, na.rm = TRUE),
    max_frequency = max(frequency, na.rm = TRUE),
    min_frequency = min(frequency, na.rm = TRUE)
  ),
  by = .(source, R17, R, Ref, year)
]

fwrite(
  freq_segall[order(R17, R, Ref, BudgetInitial)],
  file.path(out_dir, "freq_segall_exp_pre_normalized_2050_country.csv")
)
fwrite(
  freq_summary[order(R17, R, Ref)],
  file.path(out_dir, "freq_segall_exp_pre_normalized_2050_country_summary.csv")
)

clear_pngs(country_out_dir)
fill_values <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)

cat("Plotting country pre-normalized FreqSegall_exp figures...\n")
for (country in sort(unique(freq_segall$R))) {
  r17 <- country_map[R == country, R17][[1]]
  target_dir <- file.path(country_out_dir, r17)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  p <- ggplot(freq_segall[R == country]) +
    geom_rect(
      aes(
        xmin = bar_xmin,
        xmax = bar_xmax,
        ymin = 0,
        ymax = frequency,
        fill = Ref
      ),
      alpha = 0.46,
      color = NA
    ) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_x_continuous(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
    labs(
      title = paste0(country, " pre-normalized FreqSegall_exp by BudgetInitial in ", plot_year),
      subtitle = "Values from AnalysisExpenditureMerged.gdx; BudgetInitial from Fooddem_Agg_seg.gdx",
      x = "BudgetInitial",
      y = "Frequency share before normalization",
      fill = NULL
    ) +
    theme_report(base_size = 10)

  ggsave(
    file.path(target_dir, paste0("freq_segall_exp_pre_normalized_2050_", country, ".png")),
    p,
    width = 11,
    height = 7.2,
    dpi = 300,
    bg = "white"
  )
}

cat("Output directory:", normalizePath(out_dir, winslash = "\\", mustWork = FALSE), "\n")
cat("Pre-normalized FreqSegall_exp plots:", length(list.files(country_out_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
