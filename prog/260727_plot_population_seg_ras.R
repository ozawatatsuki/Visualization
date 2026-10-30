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

input_ras_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "input_RAS.gdx"))
if (length(input_ras_candidates) != 1) {
  stop("Expected exactly one input_RAS.gdx under data/260727_FDM*, found: ", length(input_ras_candidates))
}
input_ras_path <- input_ras_candidates[[1]]

price_effect_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "Fooddem_PriceEffect.gdx"))
if (length(price_effect_candidates) != 1) {
  stop("Expected exactly one Fooddem_PriceEffect.gdx under data/260727_FDM*, found: ", length(price_effect_candidates))
}
price_effect_path <- price_effect_candidates[[1]]

map_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "region_iso_R17PHI.map"))
if (length(map_candidates) != 1) {
  stop("Expected exactly one region_iso_R17PHI.map under data/260727_FDM*, found: ", length(map_candidates))
}
region_map_path <- map_candidates[[1]]

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update", "population_seg_ras")
country_out_dir <- file.path(out_dir, "country_by_r17")
dir.create(country_out_dir, recursive = TRUE, showWarnings = FALSE)

plot_year <- 2050L
scenario <- "SSP2_400C_2025CP_NoCC_No"
baseline_ref <- "SSP2_BaU_NoCC_No"
population_display_cutoff <- 1

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

budget_ten_unit_bounds <- function(budget, target_width = 10) {
  bounds <- unique(budget[is.finite(BudgetInitial) & BudgetInitial > 0, .(seg, BudgetInitial)])
  setorder(bounds, BudgetInitial)
  if (nrow(bounds) == 1L) {
    bounds[, `:=`(
      hierarchy_width = target_width,
      subdivision_count = 1L,
      bar_xmin = BudgetInitial - target_width,
      bar_xmax = BudgetInitial
    )]
    return(bounds[, .(seg, hierarchy_width, subdivision_count, bar_xmin, bar_xmax)])
  }

  budget_values <- bounds$BudgetInitial
  hierarchy_width <- c(budget_values[2L] - budget_values[1L], diff(budget_values))
  width_ratio <- hierarchy_width / target_width
  if (any(hierarchy_width < target_width) || any(abs(width_ratio - round(width_ratio)) > 1e-9)) {
    stop("BudgetInitial widths must be positive multiples of ", target_width, ".")
  }

  bounds[, `:=`(
    hierarchy_width = hierarchy_width,
    subdivision_count = as.integer(round(width_ratio)),
    bar_xmin = BudgetInitial - hierarchy_width,
    bar_xmax = BudgetInitial
  )]
  bounds[, .(seg, hierarchy_width, subdivision_count, bar_xmin, bar_xmax)]
}

cat("Reading BudgetInitial...\n")
budget <- as.data.table(rgdx.param(price_effect_path, "BudgetInitial"))
setnames(budget, "seg2", "seg")
budget[, `:=`(
  seg = as.character(seg),
  BudgetInitial = as.numeric(BudgetInitial)
)]
bar_bounds <- budget_ten_unit_bounds(budget)

cat("Reading Population_seg_RAS...\n")
population_seg_ras <- as.data.table(rgdx.param(input_ras_path, "Population_seg_RAS"))
setnames(population_seg_ras, "seg2", "seg")
population_seg_ras <- population_seg_ras[
  Ref %in% c(baseline_ref, scenario) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(Population_seg_RAS),
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    population = as.numeric(Population_seg_RAS)
  )
]

population_seg_ras <- merge(population_seg_ras, budget, by = "seg", all.x = TRUE)
population_seg_ras <- merge(population_seg_ras, bar_bounds, by = "seg", all.x = TRUE)
if (
  anyNA(population_seg_ras$BudgetInitial) ||
    anyNA(population_seg_ras$hierarchy_width) ||
    anyNA(population_seg_ras$subdivision_count) ||
    anyNA(population_seg_ras$bar_xmin) ||
    anyNA(population_seg_ras$bar_xmax)
) {
  missing_seg <- unique(
    population_seg_ras[
      is.na(BudgetInitial) |
        is.na(hierarchy_width) |
        is.na(subdivision_count) |
        is.na(bar_xmin) |
        is.na(bar_xmax),
      seg
    ]
  )
  stop("Missing BudgetInitial for Population_seg_RAS seg: ", paste(head(missing_seg, 20), collapse = ", "))
}
population_seg_ras[, population_per_10 := population / subdivision_count]

allocation_check <- population_seg_ras[
  ,
  .(
    original_population = sum(population, na.rm = TRUE),
    allocated_population = sum(population_per_10 * subdivision_count, na.rm = TRUE)
  ),
  by = .(R, Ref, year)
]
allocation_check[, allocation_error := allocated_population - original_population]
if (max(abs(allocation_check$allocation_error), na.rm = TRUE) > 1e-8) {
  stop("Population allocation to 10-unit BudgetInitial bins did not preserve totals.")
}

region_map <- read_region_map(region_map_path)
country_map <- merge(
  data.table(R = sort(unique(population_seg_ras$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_map[is.na(R17), R17 := "_unmapped"]
unmapped <- country_map[R17 == "_unmapped", R]
if (length(unmapped) > 0) {
  fwrite(data.table(R = unmapped), file.path(country_out_dir, "unmapped_population_seg_ras_iso3.csv"))
}

population_seg_ras <- merge(population_seg_ras, country_map, by = "R", all.x = TRUE)
population_seg_ras[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]

fwrite(
  population_seg_ras[order(R17, R, Ref, BudgetInitial)],
  file.path(out_dir, "population_seg_ras_2050_country.csv")
)

clear_pngs(country_out_dir)
fill_values <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)

cat("Plotting country Population_seg_RAS figures...\n")
for (country in sort(unique(population_seg_ras$R))) {
  r17 <- country_map[R == country, R17][[1]]
  target_dir <- file.path(country_out_dir, r17)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  country_population <- population_seg_ras[R == country]
  country_envelope <- country_population[
    ,
    .(
      population_max = max(population_per_10, na.rm = TRUE),
      bar_xmin = min(bar_xmin),
      bar_xmax = max(bar_xmax)
    ),
    by = BudgetInitial
  ][order(BudgetInitial)]
  peak_idx <- which.max(country_envelope$population_max)
  cutoff_idx <- which(
    seq_len(nrow(country_envelope)) >= peak_idx &
      country_envelope$population_max <= population_display_cutoff
  )[1]
  country_xmax <- if (is.na(cutoff_idx)) {
    max(country_envelope$bar_xmax)
  } else {
    country_envelope$bar_xmin[cutoff_idx]
  }

  p <- ggplot(country_population) +
    geom_rect(
      aes(
        xmin = bar_xmin,
        xmax = bar_xmax,
        ymin = 0,
        ymax = population_per_10,
        fill = Ref
      ),
      alpha = 0.46,
      color = NA
    ) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_x_continuous(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    coord_cartesian(xlim = c(0, country_xmax), expand = FALSE) +
    labs(
      title = paste0(country, " Population_seg_RAS by BudgetInitial in ", plot_year),
      subtitle = "10-unit bins; x-axis ends when both scenarios fall to population <= 1",
      x = "BudgetInitial",
      y = "Population per 10 BudgetInitial units",
      fill = NULL
    ) +
    theme_report(base_size = 10)

  ggsave(
    file.path(target_dir, paste0("population_seg_ras_2050_", country, ".png")),
    p,
    width = 11,
    height = 7.2,
    dpi = 300,
    bg = "white"
  )
}

cat("Output directory:", normalizePath(out_dir, winslash = "\\", mustWork = FALSE), "\n")
cat("Population_seg_RAS plots:", length(list.files(country_out_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
