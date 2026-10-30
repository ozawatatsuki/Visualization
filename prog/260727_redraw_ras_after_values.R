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

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update", "food_consumption_change_2050_ras_after")
total_dir <- file.path(out_dir, "total")
facet_dir <- file.path(out_dir, "facet")
country_dir <- file.path(out_dir, "country_facet_by_r17")
dir.create(total_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(facet_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)

scenario <- "SSP2_400C_2025CP_NoCC_No"
baseline_ref <- "SSP2_BaU_NoCC_No"
plot_year <- 2050L
r17_regions <- c(
  "WLD",
  "R17XSA",
  "R17XER",
  "R17XNF",
  "R17XAF",
  "R17XLM",
  "R17CIS",
  "R17XOC",
  "R17XE25",
  "R17XME",
  "R17BRA",
  "R17XSE",
  "R17CAN",
  "R17CHN",
  "R17IND",
  "R17JPN",
  "R17TUR",
  "R17USA"
)

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
  out[nzchar(R) & nzchar(R17)]
}

theme_report <- function(base_size = 11) {
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

clear_pngs <- function(path) {
  png_files <- list.files(path, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
  if (length(png_files) > 0) {
    unlink(png_files, force = TRUE)
  }
}

budget <- as.data.table(rgdx.param(gdx_path, "BudgetInitial"))
setnames(budget, "seg2", "seg")
budget[, seg := as.character(seg)]

region_map <- read_region_map(region_map_path)

cat("Reading FoodDemIn...\n")
food_income <- as.data.table(rgdx.param(gdx_path, "FoodDemIn"))
food_income <- food_income[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(FoodDemIn),
  .(
    Ref = as.character(Ref),
    var = as.character(var),
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    consumption = as.numeric(FoodDemIn)
  )
]

cat("Reading Population_seg...\n")
population_seg <- as.data.table(rgdx.param(gdx_path, "Population_seg"))
setnames(population_seg, "seg2", "seg")
population_seg <- population_seg[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL",
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    seg = as.character(seg),
    population = as.numeric(Population_seg)
  )
]

aggregate_country_consumption <- function(consumption_dt) {
  value_dt <- merge(
    consumption_dt,
    population_seg[, .(Ref, R, Y, seg, population)],
    by = c("Ref", "R", "Y", "seg"),
    all = FALSE
  )
  value_dt <- value_dt[is.finite(consumption) & is.finite(population) & population > 0]

  wld <- value_dt[
    ,
    .(
      consumption = sum(consumption * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      country_count = uniqueN(R)
    ),
    by = .(Ref, var, Y, year, seg)
  ]
  wld[, R := "WLD"]
  wld <- wld[, .(Ref, var, R, Y, year, seg, consumption, population, country_count)]

  mapped <- merge(value_dt, region_map, by = "R", all = FALSE)
  r17 <- mapped[
    ,
    .(
      consumption = sum(consumption * population, na.rm = TRUE) / sum(population, na.rm = TRUE),
      population = sum(population, na.rm = TRUE),
      country_count = uniqueN(R)
    ),
    by = .(Ref, var, Y, year, seg, R = R17)
  ]

  rbindlist(list(wld, r17), use.names = TRUE)
}

clear_pngs(total_dir)
clear_pngs(facet_dir)
clear_pngs(country_dir)

fills <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)
var_levels <- c(sort(setdiff(unique(food_income$var), "total")), "total")

ras_after_values <- merge(
  aggregate_country_consumption(food_income),
  budget,
  by = "seg",
  all.x = TRUE
)[
  ,
  .(Ref, R, var, year, seg, BudgetInitial, consumption)
]
if (anyNA(ras_after_values$BudgetInitial)) {
  missing_seg <- unique(ras_after_values[is.na(BudgetInitial), seg])
  stop("Missing BudgetInitial for seg: ", paste(head(missing_seg, 20), collapse = ", "))
}
ras_after_values[, R := factor(R, levels = r17_regions)]
ras_after_values[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
ras_after_values[, var := factor(var, levels = var_levels)]

fwrite(
  ras_after_values[var == "total"][order(R, Ref, BudgetInitial)],
  file.path(total_dir, "food_consumption_values_2050_r17_wld_ras_after_total.csv")
)
fwrite(
  ras_after_values[order(R, var, Ref, BudgetInitial)],
  file.path(facet_dir, "food_consumption_values_2050_r17_wld_ras_after_facet.csv")
)

for (region in r17_regions) {
  total_plot <- ggplot(
    ras_after_values[R == region & var == "total"],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    scale_fill_manual(values = fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(region, " RAS-after food consumption in ", plot_year),
      subtitle = "FoodDemIn values; country consumption weighted by Population_seg",
      x = "BudgetInitial (log scale)",
      y = "Food consumption (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 11)

  ggsave(
    file.path(total_dir, paste0("food_consumption_value_2050_ras_after_", region, ".png")),
    total_plot,
    width = 9.5,
    height = 5.4,
    dpi = 300,
    bg = "white"
  )

  facet_plot <- ggplot(
    ras_after_values[R == region],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(region, " RAS-after food consumption by var in ", plot_year),
      subtitle = "FoodDemIn values; country consumption weighted by Population_seg",
      x = "BudgetInitial (log scale)",
      y = "Food consumption (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 11)

  ggsave(
    file.path(facet_dir, paste0("food_consumption_value_2050_ras_after_", region, ".png")),
    facet_plot,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

country_map <- merge(
  data.table(R = sort(unique(food_income$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_map[is.na(R17), R17 := "_unmapped"]
unmapped <- country_map[R17 == "_unmapped", R]
if (length(unmapped) > 0) {
  fwrite(data.table(R = unmapped), file.path(country_dir, "unmapped_country_iso3.csv"))
}

country_values <- merge(food_income, budget, by = "seg", all.x = TRUE)[
  ,
  .(Ref, R, var, year, seg, BudgetInitial, consumption)
]
if (anyNA(country_values$BudgetInitial)) {
  missing_seg <- unique(country_values[is.na(BudgetInitial), seg])
  stop("Missing BudgetInitial for seg: ", paste(head(missing_seg, 20), collapse = ", "))
}
country_values <- merge(country_values, country_map, by = "R", all.x = TRUE)
country_values[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
country_values[, var := factor(var, levels = var_levels)]

fwrite(
  country_values[order(R17, R, var, Ref, BudgetInitial)],
  file.path(country_dir, "food_consumption_values_2050_country_ras_after_facet.csv")
)

for (country in sort(unique(country_values$R))) {
  r17 <- country_map[R == country, R17][[1]]
  target_dir <- file.path(country_dir, r17)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  country_plot <- ggplot(
    country_values[R == country],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(country, " RAS-after food consumption by var in ", plot_year),
      subtitle = "FoodDemIn values from Fooddem_PriceEffect.gdx",
      x = "BudgetInitial (log scale)",
      y = "Food consumption (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 10)

  ggsave(
    file.path(target_dir, paste0("food_consumption_value_2050_ras_after_", country, ".png")),
    country_plot,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("RAS-after value area plots:", length(list.files(out_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
