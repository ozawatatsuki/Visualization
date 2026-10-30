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

gdx_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "Fooddem_PriceEffect.gdx"))
if (length(gdx_candidates) != 1) {
  stop("Expected exactly one Fooddem_PriceEffect.gdx under data/260727_FDM*, found: ", length(gdx_candidates))
}
gdx_path <- gdx_candidates[[1]]

map_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "region_iso_R17PHI.map"))
if (length(map_candidates) != 1) {
  stop("Expected exactly one region_iso_R17PHI.map under data/260727_FDM*, found: ", length(map_candidates))
}
region_map_path <- map_candidates[[1]]

plot_year <- 2050L
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
ref_levels <- c(baseline_ref, scenario_ref)

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update", "fooddem_peffect_2050_budget_bar")
country_out_dir <- file.path(out_dir, "country_by_r17")
dir.create(country_out_dir, recursive = TRUE, showWarnings = FALSE)

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
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey30"),
      strip.text = element_text(face = "bold", color = "grey10"),
      strip.background = element_rect(fill = "grey96", color = NA),
      plot.margin = margin(8, 10, 8, 8)
    )
}

clear_generated_files <- function(path) {
  if (!dir.exists(path)) {
    return(invisible(NULL))
  }
  files <- list.files(
    path,
    pattern = "\\.(png|csv)$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(files) > 0) {
    unlink(files, force = TRUE)
  }
  invisible(NULL)
}

budget_rect_bounds <- function(budget_dt) {
  bounds <- unique(budget_dt[is.finite(BudgetInitial) & BudgetInitial > 0, .(seg, BudgetInitial)])
  setorder(bounds, BudgetInitial)
  if (nrow(bounds) == 0L) {
    stop("BudgetInitial has no positive finite rows.")
  }
  if (nrow(bounds) == 1L) {
    bounds[, `:=`(
      bar_xmin = pmax(.Machine$double.eps, BudgetInitial * 0.9),
      bar_xmax = BudgetInitial
    )]
    return(bounds)
  }

  budget_values <- bounds$BudgetInitial
  widths <- c(budget_values[2L] - budget_values[1L], diff(budget_values))
  if (any(!is.finite(widths)) || any(widths <= 0)) {
    stop("BudgetInitial must be strictly increasing by segment.")
  }
  bounds[, `:=`(
    bar_xmin = pmax(.Machine$double.eps, BudgetInitial - widths),
    bar_xmax = BudgetInitial
  )]
  bounds
}

cat("Reading BudgetInitial...\n")
budget <- as.data.table(rgdx.param(gdx_path, "BudgetInitial"))
setnames(budget, "seg2", "seg")
budget[, `:=`(
  seg = as.character(seg),
  BudgetInitial = as.numeric(BudgetInitial)
)]
budget_bounds <- budget_rect_bounds(budget)

region_map <- read_region_map(region_map_path)

clear_generated_files(country_out_dir)

cat("Reading FoodDem_peffect...\n")
food_peffect <- as.data.table(rgdx.param(gdx_path, "FoodDem_peffect"))
food_peffect <- food_peffect[
  Ref %in% ref_levels &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(FoodDem_peffect),
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    var = as.character(var),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    FoodDem_peffect = as.numeric(FoodDem_peffect)
  )
]
gc()

present_refs <- sort(unique(food_peffect$Ref))
missing_refs <- setdiff(ref_levels, present_refs)
if (length(missing_refs) > 0) {
  stop("Missing expected Ref in FoodDem_peffect: ", paste(missing_refs, collapse = ", "))
}

food_peffect <- merge(food_peffect, budget_bounds, by = "seg", all.x = TRUE)
if (anyNA(food_peffect$BudgetInitial) || anyNA(food_peffect$bar_xmin) || anyNA(food_peffect$bar_xmax)) {
  missing_seg <- unique(food_peffect[
    is.na(BudgetInitial) | is.na(bar_xmin) | is.na(bar_xmax),
    seg
  ])
  stop("Missing BudgetInitial bounds for seg: ", paste(head(missing_seg, 20), collapse = ", "))
}

country_map <- merge(
  data.table(R = sort(unique(food_peffect$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_map[is.na(R17), R17 := "_unmapped"]
unmapped_country <- country_map[R17 == "_unmapped", R]
unmapped_country_file <- file.path(country_out_dir, "unmapped_fooddem_peffect_iso3.csv")
if (length(unmapped_country) > 0) {
  fwrite(data.table(R = unmapped_country), unmapped_country_file)
} else if (file.exists(unmapped_country_file)) {
  unlink(unmapped_country_file)
}

food_peffect <- merge(food_peffect, country_map, by = "R", all.x = TRUE)
food_peffect[, Ref := factor(Ref, levels = ref_levels)]
food_peffect[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]
food_peffect[, `:=`(
  bar_ymin = pmin(FoodDem_peffect, 0),
  bar_ymax = pmax(FoodDem_peffect, 0)
)]
setorder(food_peffect, R17, R, var, Ref, BudgetInitial)

csv_path <- file.path(country_out_dir, "fooddem_peffect_2050_country_facet.csv")
fwrite(
  food_peffect[
    ,
    .(R17, R, Ref, var, year, seg, BudgetInitial, bar_xmin, bar_xmax, FoodDem_peffect)
  ],
  csv_path
)

ref_fills <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)

cat("Plotting country FoodDem_peffect figures...\n")
for (country in sort(unique(food_peffect$R))) {
  r17 <- country_map[R == country, R17][[1]]
  target_dir <- file.path(country_out_dir, r17)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  country_plot <- ggplot(food_peffect[R == country]) +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_rect(
      aes(
        xmin = bar_xmin,
        xmax = bar_xmax,
        ymin = bar_ymin,
        ymax = bar_ymax,
        fill = Ref
      ),
      alpha = 0.42,
      color = NA
    ) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = ref_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(country, " FoodDem_peffect by var in ", plot_year),
      subtitle = "Ref values overlaid with alpha; data from Fooddem_PriceEffect.gdx",
      x = "BudgetInitial (log scale)",
      y = "FoodDem_peffect (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 10)

  ggsave(
    file.path(target_dir, paste0("fooddem_peffect_2050_", country, ".png")),
    country_plot,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("Done. Wrote CSV and country plots under: ", country_out_dir, "\n", sep = "")
