suppressPackageStartupMessages({
  library(countrycode)
  library(data.table)
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(patchwork)
  library(rnaturalearth)
  library(readr)
  library(scales)
  library(sf)
  library(viridis)
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

input_ras_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "input_RAS.gdx"))
if (length(input_ras_candidates) != 1) {
  stop("Expected exactly one input_RAS.gdx under data/260727_FDM*, found: ", length(input_ras_candidates))
}
input_ras_path <- input_ras_candidates[[1]]

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update")
food_dir <- file.path(out_dir, "food_consumption_change_2050")
total_food_dir <- file.path(food_dir, "total")
facet_food_dir <- file.path(food_dir, "facet")
country_food_dir <- file.path(food_dir, "country_facet_by_r17")
ras_before_dir <- file.path(out_dir, "food_consumption_change_2050_ras_before")
ras_before_total_dir <- file.path(ras_before_dir, "total")
ras_before_facet_dir <- file.path(ras_before_dir, "facet")
ras_before_country_dir <- file.path(ras_before_dir, "country_facet_by_r17")
ras_after_dir <- file.path(out_dir, "food_consumption_change_2050_ras_after")
ras_after_total_dir <- file.path(ras_after_dir, "total")
ras_after_facet_dir <- file.path(ras_after_dir, "facet")
ras_after_country_dir <- file.path(ras_after_dir, "country_facet_by_r17")
ras_compare_dir <- file.path(out_dir, "ras_before_after_comparison")
ineq_dir <- file.path(out_dir, "food_consumption_inequality")
hunger_dir <- file.path(out_dir, "hunger_risk_population")
pop_ras_dir <- file.path(out_dir, "population_seg_ras")
pop_ras_country_dir <- file.path(pop_ras_dir, "country_by_r17")
dir.create(food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(total_food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(facet_food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(country_food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_before_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_before_total_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_before_facet_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_before_country_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_after_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_after_total_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_after_facet_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_after_country_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ras_compare_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(ineq_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(hunger_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pop_ras_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(pop_ras_country_dir, recursive = TRUE, showWarnings = FALSE)

scenario <- "SSP2_400C_2025CP_NoCC_No"
baseline_ref <- "SSP2_BaU_NoCC_No"
plot_year <- 2050L
population_display_cutoff <- 1

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

legacy_region_dirs <- file.path(food_dir, r17_regions)
legacy_region_dirs <- legacy_region_dirs[dir.exists(legacy_region_dirs)]
if (length(legacy_region_dirs) > 0) {
  unlink(legacy_region_dirs, recursive = TRUE, force = TRUE)
}
legacy_food_files <- c(
  file.path(food_dir, paste0("food_consumption_change_2050_", r17_regions, ".png")),
  file.path(food_dir, "food_consumption_effects_2050_r17_wld.csv")
)
legacy_food_files <- legacy_food_files[file.exists(legacy_food_files)]
if (length(legacy_food_files) > 0) {
  unlink(legacy_food_files, force = TRUE)
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

clear_pngs(total_food_dir)
clear_pngs(facet_food_dir)
clear_pngs(country_food_dir)
clear_pngs(ras_before_total_dir)
clear_pngs(ras_before_facet_dir)
clear_pngs(ras_before_country_dir)
clear_pngs(ras_after_total_dir)
clear_pngs(ras_after_facet_dir)
clear_pngs(ras_after_country_dir)
clear_pngs(ras_compare_dir)
clear_pngs(pop_ras_country_dir)

is_country_code <- function(x) {
  !x %in% "WLD" & !grepl("^R17", x)
}

num_chr <- function(x) {
  as.numeric(as.character(x))
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

weighted_gini <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]

  if (length(x) < 2) {
    return(NA_real_)
  }

  total_w <- sum(w)
  total_xw <- sum(x * w)
  if (!is.finite(total_w) || !is.finite(total_xw) || total_w <= 0 || total_xw <= 0) {
    return(NA_real_)
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- c(0, cumsum(w) / total_w)
  cum_xw <- c(0, cumsum(x * w) / total_xw)
  area <- sum(diff(cum_w) * (head(cum_xw, -1) + tail(cum_xw, -1)) / 2)
  max(0, min(1, 1 - 2 * area))
}

lorenz_curve <- function(x, w, points = 1000L) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]
  w <- w[ok]

  if (length(x) == 0 || sum(x * w) <= 0) {
    return(data.table(population_share = numeric(), consumption_share = numeric()))
  }

  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  cum_w <- c(0, cumsum(w) / sum(w))
  cum_xw <- c(0, cumsum(x * w) / sum(x * w))
  grid <- seq(0, 1, length.out = points + 1L)
  approx_y <- approx(cum_w, cum_xw, xout = grid, ties = "ordered", rule = 2)$y
  data.table(
    population_share = grid,
    consumption_share = pmin(1, pmax(0, approx_y))
  )
}

build_world_map <- function() {
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  if ("continent" %in% names(world)) {
    world <- world[world$continent != "Antarctica", ]
  }

  iso_cols <- intersect(c("iso_a3", "adm0_a3", "gu_a3", "sov_a3"), names(world))
  world$R <- NA_character_
  for (col in iso_cols) {
    values <- as.character(world[[col]])
    values[values == "-99"] <- NA_character_
    world$R <- ifelse(is.na(world$R), values, world$R)
  }

  world
}

budget <- as.data.table(rgdx.param(gdx_path, "BudgetInitial"))
setnames(budget, "seg2", "seg")
budget[, seg := as.character(seg)]
budget[, BudgetInitial := as.numeric(BudgetInitial)]

budget_ten_unit_bounds <- function(target_width = 10) {
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

region_map <- read_region_map(region_map_path)

cat("Reading FoodDem_peffect...\n")
food_price <- as.data.table(rgdx.param(gdx_path, "FoodDem_peffect"))
food_price <- food_price[
  Ref == scenario &
    is_country_code(as.character(R)) &
    seg != "ALL" &
    !is.na(FoodDem_peffect) &
    (Y == as.character(plot_year) | var == "total"),
  .(
    Ref = as.character(Ref),
    var = as.character(var),
    R = as.character(R),
    Y = as.character(Y),
    seg = as.character(seg),
    income_price = as.numeric(FoodDem_peffect)
  )
]
food_price[, year := as.integer(Y)]
gc()

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
    seg = as.character(seg),
    food_income = as.numeric(FoodDemIn)
  )
]
food_income[, year := as.integer(Y)]
gc()

cat("Reading Population_seg...\n")
population_seg <- as.data.table(rgdx.param(gdx_path, "Population_seg"))
setnames(population_seg, "seg2", "seg")
population_seg <- population_seg[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    seg != "ALL",
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    seg = as.character(seg),
    population = as.numeric(Population_seg)
  )
]
population_seg[, year := as.integer(Y)]

food_countries <- sort(unique(c(food_price$R, food_income$R)))
unmapped_food_countries <- setdiff(food_countries, region_map$R)
unmapped_food_file <- file.path(food_dir, "unmapped_food_consumption_region_iso3.csv")
if (length(unmapped_food_countries) > 0) {
  write_csv(
    data.frame(R = unmapped_food_countries),
    unmapped_food_file
  )
  warning(
    "Some FoodDem countries are missing in region_iso_R17PHI.map and are excluded from R17 aggregation: ",
    paste(head(unmapped_food_countries, 20), collapse = ", ")
  )
} else if (file.exists(unmapped_food_file)) {
  unlink(unmapped_food_file)
}

aggregate_country_consumption <- function(consumption_dt, value_col) {
  value_dt <- copy(consumption_dt)
  setnames(value_dt, value_col, "consumption")
  value_dt <- merge(
    value_dt,
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

food_income_agg <- aggregate_country_consumption(food_income, "food_income")
food_price_agg <- aggregate_country_consumption(food_price[year == plot_year], "income_price")

baseline_food <- food_income_agg[
  Ref == baseline_ref,
  .(R, var, year, seg, baseline = consumption)
]
income_food <- food_income_agg[
  Ref == scenario,
  .(R, var, year, seg, income = consumption)
]
income_price_food <- food_price_agg[
  Ref == scenario,
  .(R, var, year, seg, income_price = consumption)
]

food_effects_wide <- Reduce(
  function(x, y) merge(x, y, by = c("R", "var", "year", "seg"), all = FALSE),
  list(baseline_food, income_food, income_price_food)
)
food_effects_wide <- merge(food_effects_wide, budget, by = "seg", all.x = TRUE)
food_effects_wide[
  ,
  `:=`(
    income_effect = income - baseline,
    price_effect = income_price - income,
    total_effect = income_price - baseline
  )
]

if (anyNA(food_effects_wide$BudgetInitial)) {
  missing_seg <- unique(food_effects_wide[is.na(BudgetInitial), seg])
  stop("Missing BudgetInitial for seg: ", paste(head(missing_seg, 20), collapse = ", "))
}

food_effects <- melt(
  food_effects_wide,
  id.vars = c("R", "var", "year", "seg", "BudgetInitial", "total_effect"),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "effect",
  value.name = "change"
)
food_effects[
  ,
  effect := factor(
    effect,
    levels = c("income_effect", "price_effect"),
    labels = c("Income effect", "Price effect")
  )
]
food_effects[, R := factor(R, levels = r17_regions)]
food_effects_total <- food_effects[var == "total"]
fwrite(
  food_effects_total[order(R, BudgetInitial, effect)],
  file.path(total_food_dir, "food_consumption_effects_2050_r17_wld_total.csv")
)
fwrite(
  food_effects[order(R, var, BudgetInitial, effect)],
  file.path(facet_food_dir, "food_consumption_effects_2050_r17_wld_facet.csv")
)

food_stack_ribbons <- rbindlist(list(
  food_effects_wide[
    ,
    .(
      R,
      var,
      year,
      seg,
      BudgetInitial,
      effect = "Income effect",
      stack_order = 1L,
      change = income_effect
    )
  ],
  food_effects_wide[
    ,
    .(
      R,
      var,
      year,
      seg,
      BudgetInitial,
      effect = "Price effect",
      stack_order = 2L,
      change = price_effect
    )
  ]
))
food_stack_ribbons[, R := factor(R, levels = r17_regions)]
food_stack_ribbons[
  ,
  effect := factor(effect, levels = c("Income effect", "Price effect"))
]
setorder(food_stack_ribbons, R, var, BudgetInitial, stack_order)
food_stack_ribbons[
  ,
  `:=`(
    positive_change = pmax(change, 0),
    negative_change = pmin(change, 0)
  )
]
food_stack_ribbons[
  ,
  `:=`(
    positive_base = shift(cumsum(positive_change), fill = 0),
    negative_base = shift(cumsum(negative_change), fill = 0)
  ),
  by = .(R, var, BudgetInitial)
]
food_stack_ribbons[
  ,
  `:=`(
    ymin = fifelse(change >= 0, positive_base, negative_base + negative_change),
    ymax = fifelse(change >= 0, positive_base + positive_change, negative_base)
  )
]

effect_fills <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "RAS-before change" = "#009E73"
)

scenario_ref_fills <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)

make_effect_ribbons <- function(effect_wide) {
  out <- rbindlist(list(
    effect_wide[
      ,
      .(
        R,
        var,
        year,
        seg,
        BudgetInitial,
        effect = "Income effect",
        stack_order = 1L,
        change = income_effect
      )
    ],
    effect_wide[
      ,
      .(
        R,
        var,
        year,
        seg,
        BudgetInitial,
        effect = "Price effect",
        stack_order = 2L,
        change = price_effect
      )
    ]
  ))
  if (all(as.character(unique(out$R)) %in% r17_regions)) {
    out[, R := factor(R, levels = r17_regions)]
  }
  out[, effect := factor(effect, levels = c("Income effect", "Price effect"))]
  setorder(out, R, var, BudgetInitial, stack_order)
  out[
    ,
    `:=`(
      positive_change = pmax(change, 0),
      negative_change = pmin(change, 0)
    )
  ]
  out[
    ,
    `:=`(
      positive_base = shift(cumsum(positive_change), fill = 0),
      negative_base = shift(cumsum(negative_change), fill = 0)
    ),
    by = .(R, var, BudgetInitial)
  ]
  out[
    ,
    `:=`(
      ymin = fifelse(change >= 0, positive_base, negative_base + negative_change),
      ymax = fifelse(change >= 0, positive_base + positive_change, negative_base)
    )
  ]
  out
}

make_linear_bar_bounds <- function(budget) {
  bounds <- unique(budget[
    is.finite(BudgetInitial) & BudgetInitial > 0,
    .(seg, BudgetInitial)
  ])
  setorder(bounds, BudgetInitial)
  if (nrow(bounds) == 1L) {
    bounds[, `:=`(
      bar_xmin = max(0, BudgetInitial * 0.95),
      bar_xmax = BudgetInitial * 1.05
    )]
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

make_single_change_ribbons <- function(effect_wide, effect_label) {
  out <- effect_wide[
    ,
    .(
      R,
      var,
      year,
      seg,
      BudgetInitial,
      effect = effect_label,
      change = total_effect,
      ymin = pmin(total_effect, 0),
      ymax = pmax(total_effect, 0)
    )
  ]
  if (all(as.character(unique(out$R)) %in% r17_regions)) {
    out[, R := factor(R, levels = r17_regions)]
  }
  out[, effect := factor(effect, levels = effect_label)]
  out
}

plot_effect_change <- function(ribbons, line_data, title, subtitle, facet_vars = FALSE, base_size = 11) {
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_ribbon(
      data = ribbons,
      aes(x = BudgetInitial, ymin = ymin, ymax = ymax, fill = effect),
      alpha = 0.86,
      color = NA
    ) +
    geom_line(
      data = line_data,
      aes(x = BudgetInitial, y = total_effect),
      inherit.aes = FALSE,
      color = "grey10",
      linewidth = 0.45
    ) +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_x_continuous(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "BudgetInitial",
      y = "Change from baseline (kcal/day/capita)"
    ) +
    theme_report(base_size = base_size)

  if (facet_vars) {
    p <- p + facet_wrap(~ var, ncol = 2, scales = "free_y")
  }
  p
}

plot_effect_bars <- function(bars, line_data, title, subtitle, base_size = 10) {
  ggplot() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_rect(
      data = bars,
      aes(
        xmin = bar_xmin,
        xmax = bar_xmax,
        ymin = ymin,
        ymax = ymax,
        fill = effect
      ),
      alpha = 0.9,
      color = "white",
      linewidth = 0.06
    ) +
    geom_line(
      data = line_data,
      aes(x = BudgetInitial, y = total_effect),
      inherit.aes = FALSE,
      color = "grey10",
      linewidth = 0.5
    ) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_x_continuous(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "BudgetInitial",
      y = "Change from baseline (kcal/day/capita)"
    ) +
    theme_report(base_size = base_size)
}

linear_bar_bounds <- make_linear_bar_bounds(budget)

plot_ras_country_value_facets <- function(
    country_values,
    output_dir,
    csv_suffix,
    png_suffix,
    title_label,
    source_label,
    value_col = "before_ras",
    source_file_label = "Fooddem_Agg_seg.gdx") {
  clear_pngs(output_dir)

  country_plot_values <- merge(
    country_values,
    budget,
    by = "seg",
    all.x = TRUE
  )[
    ,
    .(Ref, R, var, year, seg, BudgetInitial, consumption = as.numeric(get(value_col)))
  ]
  if (anyNA(country_plot_values$BudgetInitial)) {
    missing_seg <- unique(country_plot_values[is.na(BudgetInitial), seg])
    stop("Missing BudgetInitial for seg: ", paste(head(missing_seg, 20), collapse = ", "))
  }

  country_map <- merge(
    data.table(R = sort(unique(country_plot_values$R))),
    region_map,
    by = "R",
    all.x = TRUE
  )
  country_map[is.na(R17), R17 := "_unmapped"]
  unmapped_country <- country_map[R17 == "_unmapped", R]
  unmapped_country_file <- file.path(output_dir, "unmapped_country_iso3.csv")
  if (length(unmapped_country) > 0) {
    fwrite(data.table(R = unmapped_country), unmapped_country_file)
  } else if (file.exists(unmapped_country_file)) {
    unlink(unmapped_country_file)
  }

  country_plot_values <- merge(country_plot_values, country_map, by = "R", all.x = TRUE)
  country_plot_values[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
  country_plot_values[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]

  fwrite(
    country_plot_values[order(R17, R, var, Ref, BudgetInitial)],
    file.path(output_dir, paste0("food_consumption_values_2050_country_", csv_suffix, "_facet.csv"))
  )

  for (country in sort(unique(country_plot_values$R))) {
    r17 <- country_map[R == country, R17][[1]]
    target_dir <- file.path(output_dir, r17)
    dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

    country_plot <- ggplot(
      country_plot_values[R == country],
      aes(x = BudgetInitial, y = consumption, fill = Ref)
    ) +
      geom_area(position = "identity", alpha = 0.42, color = NA) +
      facet_wrap(~ var, ncol = 2, scales = "free_y") +
      scale_fill_manual(values = ras_before_ref_fills, drop = FALSE) +
      scale_x_log10(labels = label_number(big.mark = ",")) +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      guides(fill = guide_legend(nrow = 2)) +
      labs(
        title = paste0(country, " ", title_label, " food consumption by var in ", plot_year),
        subtitle = paste0(source_label, " values from ", source_file_label),
        x = "BudgetInitial (log scale)",
        y = "Food consumption (kcal/day/capita)",
        fill = NULL
      ) +
      theme_report(base_size = 10)

    ggsave(
      file.path(output_dir, r17, paste0("food_consumption_value_2050_", png_suffix, "_", country, ".png")),
      country_plot,
      width = 12,
      height = 14,
      dpi = 300,
      bg = "white"
    )
  }
}

for (region in r17_regions) {
  region_ribbons_total <- food_stack_ribbons[R == region & var == "total"]
  region_total_line <- unique(food_effects_wide[R == region & var == "total", .(BudgetInitial, total_effect)])

  p_food_total <- ggplot() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_ribbon(
      data = region_ribbons_total,
      aes(x = BudgetInitial, ymin = ymin, ymax = ymax, fill = effect),
      alpha = 0.86,
      color = NA
    ) +
    geom_line(
      data = region_total_line,
      aes(x = BudgetInitial, y = total_effect),
      inherit.aes = FALSE,
      color = "grey10",
      linewidth = 0.45
    ) +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = paste0(region, " food consumption change in ", plot_year),
      subtitle = paste0(
        scenario,
        " minus ",
        baseline_ref,
        "; country var=total weighted by Population_seg"
      ),
      x = "BudgetInitial (log scale)",
      y = "Change from baseline (kcal/day/capita)"
    ) +
    theme_report(base_size = 11)

  ggsave(
    file.path(total_food_dir, paste0("food_consumption_change_2050_", region, ".png")),
    p_food_total,
    width = 9.5,
    height = 5.4,
    dpi = 300,
    bg = "white"
  )

  region_ribbons_facet <- food_stack_ribbons[R == region]
  region_total_facet <- unique(food_effects_wide[R == region, .(var, BudgetInitial, total_effect)])

  p_food_facet <- ggplot() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_ribbon(
      data = region_ribbons_facet,
      aes(x = BudgetInitial, ymin = ymin, ymax = ymax, fill = effect),
      alpha = 0.86,
      color = NA
    ) +
    geom_line(
      data = region_total_facet,
      aes(x = BudgetInitial, y = total_effect),
      inherit.aes = FALSE,
      color = "grey10",
      linewidth = 0.45
    ) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = paste0(region, " food consumption change by var in ", plot_year),
      subtitle = paste0(
        scenario,
        " minus ",
        baseline_ref,
        "; country consumption weighted by Population_seg"
      ),
      x = "BudgetInitial (log scale)",
      y = "Change from baseline (kcal/day/capita)"
    ) +
    theme_report(base_size = 11)

  ggsave(
    file.path(facet_food_dir, paste0("food_consumption_change_2050_", region, ".png")),
    p_food_facet,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("Plotting country FoodDem_PriceEffect figures...\n")
country_baseline_food <- food_income[
  Ref == baseline_ref,
  .(R, var, year, seg, baseline = food_income)
]
country_income_food <- food_income[
  Ref == scenario,
  .(R, var, year, seg, income = food_income)
]
country_income_price_food <- food_price[
  Ref == scenario & year == plot_year,
  .(R, var, year, seg, income_price)
]
country_effects_wide <- Reduce(
  function(x, y) merge(x, y, by = c("R", "var", "year", "seg"), all = FALSE),
  list(country_baseline_food, country_income_food, country_income_price_food)
)
country_effects_wide <- merge(country_effects_wide, budget, by = "seg", all.x = TRUE)
country_effects_wide[
  ,
  `:=`(
    income_effect = income - baseline,
    price_effect = income_price - income,
    total_effect = income_price - baseline
  )
]
country_bars <- merge(
  make_effect_ribbons(country_effects_wide),
  linear_bar_bounds,
  by = "seg",
  all.x = TRUE
)
if (anyNA(country_bars$bar_xmin) || anyNA(country_bars$bar_xmax)) {
  stop("Missing bar bounds for country food-consumption effects.")
}
country_region_map <- merge(
  data.table(R = sort(unique(country_effects_wide$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_region_map[is.na(R17), R17 := "_unmapped"]
country_unmapped <- country_region_map[R17 == "_unmapped", R]
country_unmapped_file <- file.path(country_food_dir, "unmapped_country_facet_iso3.csv")
if (length(country_unmapped) > 0) {
  write_csv(data.frame(R = country_unmapped), country_unmapped_file)
} else if (file.exists(country_unmapped_file)) {
  unlink(country_unmapped_file)
}

for (country in country_region_map$R) {
  country_region <- country_region_map[R == country, R17][1]
  country_dir <- file.path(country_food_dir, country_region)
  dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)

  country_plot <- plot_effect_bars(
    bars = country_bars[R == country],
    line_data = unique(country_effects_wide[R == country, .(var, BudgetInitial, total_effect)]),
    title = paste0(country, " food consumption change by var in ", plot_year),
    subtitle = paste0(scenario, " minus ", baseline_ref, "; FoodDem_PriceEffect.gdx"),
    base_size = 10
  )

  ggsave(
    file.path(country_dir, paste0("food_consumption_change_2050_", country, ".png")),
    country_plot,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

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
population_seg_ras <- merge(population_seg_ras, budget_ten_unit_bounds(), by = "seg", all.x = TRUE)
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

population_allocation_check <- population_seg_ras[
  ,
  .(
    original_population = sum(population, na.rm = TRUE),
    allocated_population = sum(population_per_10 * subdivision_count, na.rm = TRUE)
  ),
  by = .(R, Ref, year)
]
population_allocation_check[, allocation_error := allocated_population - original_population]
if (max(abs(population_allocation_check$allocation_error), na.rm = TRUE) > 1e-8) {
  stop("Population allocation to 10-unit BudgetInitial bins did not preserve totals.")
}

population_country_map <- merge(
  data.table(R = sort(unique(population_seg_ras$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
population_country_map[is.na(R17), R17 := "_unmapped"]
population_unmapped <- population_country_map[R17 == "_unmapped", R]
population_unmapped_file <- file.path(pop_ras_country_dir, "unmapped_population_seg_ras_iso3.csv")
if (length(population_unmapped) > 0) {
  fwrite(data.table(R = population_unmapped), population_unmapped_file)
} else if (file.exists(population_unmapped_file)) {
  unlink(population_unmapped_file)
}

population_seg_ras <- merge(population_seg_ras, population_country_map, by = "R", all.x = TRUE)
population_seg_ras[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
fwrite(
  population_seg_ras[order(R17, R, Ref, BudgetInitial)],
  file.path(pop_ras_dir, "population_seg_ras_2050_country.csv")
)

for (country in sort(unique(population_seg_ras$R))) {
  r17 <- population_country_map[R == country, R17][[1]]
  country_dir <- file.path(pop_ras_country_dir, r17)
  dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)

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

  pop_plot <- ggplot(country_population) +
    geom_rect(
      aes(
        xmin = bar_xmin,
        xmax = bar_xmax,
        ymin = 0,
        ymax = population_per_10,
        fill = Ref
      ),
      color = NA,
      alpha = 0.46
    ) +
    scale_fill_manual(values = scenario_ref_fills, drop = FALSE) +
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
    file.path(country_dir, paste0("population_seg_ras_2050_", country, ".png")),
    pop_plot,
    width = 11,
    height = 7.2,
    dpi = 300,
    bg = "white"
  )
}

cat("Reading RAS-before food demand...\n")
ras_before_vars <- as.data.table(rgdx.param(agg_gdx_path, "fooddemUSDA_calib"))
ras_before_vars <- ras_before_vars[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(fooddemUSDA_calib),
  .(
    Ref = as.character(Ref),
    var = as.character(CUSDA),
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    before_ras = as.numeric(fooddemUSDA_calib)
  )
]
ras_before_total_from_vars <- ras_before_vars[
  ,
  .(before_ras = sum(before_ras, na.rm = TRUE)),
  by = .(Ref, R, Y, year, seg)
]
ras_before_total_from_vars[, var := "total"]
ras_before_total_from_vars <- ras_before_total_from_vars[
  ,
  .(Ref, var, R, Y, year, seg, before_ras)
]
ras_before_change_country <- rbindlist(
  list(ras_before_vars, ras_before_total_from_vars),
  use.names = TRUE
)
ras_before_change_agg <- aggregate_country_consumption(ras_before_change_country, "before_ras")
ras_before_values <- merge(ras_before_change_agg, budget, by = "seg", all.x = TRUE)[
  ,
  .(Ref, R, var, year, seg, BudgetInitial, consumption)
]
ras_before_values[, R := factor(R, levels = r17_regions)]
ras_before_values[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]

old_ras_before_csv <- c(
  file.path(ras_before_total_dir, "food_consumption_effects_2050_r17_wld_ras_before_total.csv"),
  file.path(ras_before_facet_dir, "food_consumption_effects_2050_r17_wld_ras_before_facet.csv")
)
old_ras_before_csv <- old_ras_before_csv[file.exists(old_ras_before_csv)]
if (length(old_ras_before_csv) > 0) {
  unlink(old_ras_before_csv, force = TRUE)
}
fwrite(
  ras_before_values[var == "total"][order(R, Ref, BudgetInitial)],
  file.path(ras_before_total_dir, "food_consumption_values_2050_r17_wld_ras_before_total.csv")
)
fwrite(
  ras_before_values[order(R, var, Ref, BudgetInitial)],
  file.path(ras_before_facet_dir, "food_consumption_values_2050_r17_wld_ras_before_facet.csv")
)

ras_before_ref_fills <- scenario_ref_fills
plot_ras_country_value_facets(
  ras_before_change_country,
  ras_before_country_dir,
  "ras_before",
  "ras_before",
  "RAS-before",
  "fooddemUSDA_calib"
)
for (region in r17_regions) {
  ras_before_plot_total <- ggplot(
    ras_before_values[R == region & var == "total"],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    scale_fill_manual(values = ras_before_ref_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(region, " RAS-before food consumption in ", plot_year),
      subtitle = "fooddemUSDA_calib values; country consumption weighted by Population_seg; total is summed across var",
      x = "BudgetInitial (log scale)",
      y = "Food consumption (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 11)
  ggsave(
    file.path(ras_before_total_dir, paste0("food_consumption_value_2050_ras_before_", region, ".png")),
    ras_before_plot_total,
    width = 9.5,
    height = 5.4,
    dpi = 300,
    bg = "white"
  )

  ras_before_plot_facet <- ggplot(
    ras_before_values[R == region],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = ras_before_ref_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 2)) +
    labs(
      title = paste0(region, " RAS-before food consumption by var in ", plot_year),
      subtitle = "fooddemUSDA_calib values; country consumption weighted by Population_seg",
      x = "BudgetInitial (log scale)",
      y = "Food consumption (kcal/day/capita)",
      fill = NULL
    ) +
    theme_report(base_size = 11)
  ggsave(
    file.path(ras_before_facet_dir, paste0("food_consumption_value_2050_ras_before_", region, ".png")),
    ras_before_plot_facet,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("Building RAS-after FoodDemIn figures...\n")
ras_after_values <- merge(food_income_agg, budget, by = "seg", all.x = TRUE)[
  ,
  .(Ref, R, var, year, seg, BudgetInitial, consumption)
]
ras_after_values[, R := factor(R, levels = r17_regions)]
ras_after_values[, Ref := factor(Ref, levels = c(baseline_ref, scenario))]
ras_after_values[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]

fwrite(
  ras_after_values[var == "total"][order(R, Ref, BudgetInitial)],
  file.path(ras_after_total_dir, "food_consumption_values_2050_r17_wld_ras_after_total.csv")
)
fwrite(
  ras_after_values[order(R, var, Ref, BudgetInitial)],
  file.path(ras_after_facet_dir, "food_consumption_values_2050_r17_wld_ras_after_facet.csv")
)

plot_ras_country_value_facets(
  food_income,
  ras_after_country_dir,
  "ras_after",
  "ras_after",
  "RAS-after",
  "FoodDemIn",
  value_col = "food_income",
  source_file_label = "Fooddem_PriceEffect.gdx"
)

for (region in r17_regions) {
  ras_after_plot_total <- ggplot(
    ras_after_values[R == region & var == "total"],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    scale_fill_manual(values = ras_before_ref_fills, drop = FALSE) +
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
    file.path(ras_after_total_dir, paste0("food_consumption_value_2050_ras_after_", region, ".png")),
    ras_after_plot_total,
    width = 9.5,
    height = 5.4,
    dpi = 300,
    bg = "white"
  )

  ras_after_plot_facet <- ggplot(
    ras_after_values[R == region],
    aes(x = BudgetInitial, y = consumption, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    facet_wrap(~ var, ncol = 2, scales = "free_y") +
    scale_fill_manual(values = ras_before_ref_fills, drop = FALSE) +
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
    file.path(ras_after_facet_dir, paste0("food_consumption_value_2050_ras_after_", region, ".png")),
    ras_after_plot_facet,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("Building RAS before/after comparison figures...\n")
ras_before_all <- as.data.table(rgdx.param(agg_gdx_path, "fooddemUSDA_ALL_calib"))
ras_before_all <- ras_before_all[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(fooddemUSDA_ALL_calib),
  .(
    Ref = as.character(Ref),
    var = "total",
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    seg = as.character(seg),
    before_ras = as.numeric(fooddemUSDA_ALL_calib)
  )
]
ras_before_compare_country <- rbindlist(
  list(ras_before_vars, ras_before_all),
  use.names = TRUE
)
ras_before_compare_agg <- aggregate_country_consumption(ras_before_compare_country, "before_ras")
ras_compare_before <- merge(ras_before_compare_agg, budget, by = "seg", all.x = TRUE)[
  ,
  .(
    Ref,
    R,
    var,
    year,
    seg,
    BudgetInitial,
    method = "Before RAS",
    consumption
  )
]
ras_compare_after <- merge(food_income_agg, budget, by = "seg", all.x = TRUE)[
  ,
  .(
    Ref,
    R,
    var,
    year,
    seg,
    BudgetInitial,
    method = "After RAS",
    consumption
  )
]
ras_compare <- rbindlist(list(ras_compare_before, ras_compare_after), use.names = TRUE)
ras_compare[, R := factor(R, levels = r17_regions)]
ras_compare[, method := factor(method, levels = c("Before RAS", "After RAS"))]
write_csv(
  ras_compare[order(Ref, R, var, seg, method)],
  file.path(ras_compare_dir, "ras_before_after_2050_r17_wld.csv")
)

compare_colors <- c("Before RAS" = "#009E73", "After RAS" = "#CC79A7")
for (ref_id in c(baseline_ref, scenario)) {
  ref_dir <- file.path(ras_compare_dir, ref_id)
  dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
  for (region in r17_regions) {
    compare_region <- ras_compare[Ref == ref_id & R == region]
    compare_plot <- ggplot(compare_region, aes(x = BudgetInitial, y = consumption, fill = method)) +
      geom_area(position = "identity", alpha = 0.42, color = NA) +
      facet_wrap(~ var, ncol = 2, scales = "free_y") +
      scale_fill_manual(values = compare_colors, drop = FALSE) +
      scale_x_log10(labels = label_number(big.mark = ",")) +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(
        title = paste0(region, " RAS before/after comparison in ", plot_year),
        subtitle = paste0("Ref = ", ref_id, "; country consumption weighted by Population_seg"),
        x = "BudgetInitial (log scale)",
        y = "Food consumption (kcal/day/capita)",
        fill = NULL
      ) +
      theme_report(base_size = 11)

    ggsave(
      file.path(ref_dir, paste0("ras_before_after_2050_", ref_id, "_", region, ".png")),
      compare_plot,
      width = 12,
      height = 14,
      dpi = 300,
      bg = "white"
    )
  }
}

food_country <- merge(
  food_price[var == "total" & is_country_code(R), .(Ref, R, Y, year, seg, consumption = income_price)],
  population_seg[Ref == scenario & is_country_code(R), .(Ref, R, Y, seg, population)],
  by = c("Ref", "R", "Y", "seg"),
  all = FALSE
)
food_country <- food_country[is.finite(consumption) & is.finite(population) & population > 0]

country_gini <- food_country[
  ,
  .(
    gini = weighted_gini(consumption, population),
    population = sum(population, na.rm = TRUE),
    segment_count = .N
  ),
  by = .(R, year)
]
country_gini_2050 <- country_gini[year == plot_year][order(R)]
fwrite(country_gini_2050, file.path(ineq_dir, "country_food_consumption_gini_2050.csv"))

world_gini <- food_country[
  ,
  .(
    gini = weighted_gini(consumption, population),
    population = sum(population, na.rm = TRUE),
    segment_count = .N
  ),
  by = year
][order(year)]
fwrite(world_gini, file.path(ineq_dir, "global_food_consumption_gini_trend.csv"))

lorenz_data <- food_country[
  ,
  lorenz_curve(consumption, population),
  by = year
][order(year, population_share)]

p_lorenz <- ggplot(lorenz_data, aes(x = population_share, y = consumption_share, color = factor(year))) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.45) +
  geom_line(linewidth = 0.75, alpha = 0.9) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_viridis_d(option = "C", end = 0.9) +
  coord_equal() +
  labs(
    title = "Global Lorenz curves",
    subtitle = paste0("All country segments, total food consumption; Ref = ", scenario),
    x = "Cumulative population share",
    y = "Cumulative food consumption share"
  ) +
  theme_report(base_size = 10)

p_world_gini <- ggplot(world_gini, aes(x = year, y = gini)) +
  geom_line(color = "#0072B2", linewidth = 0.9) +
  geom_point(color = "#0072B2", size = 2) +
  scale_x_continuous(breaks = sort(unique(world_gini$year))) +
  scale_y_continuous(labels = label_number(accuracy = 0.001), limits = c(0, NA)) +
  labs(
    title = "Global Gini trend",
    subtitle = "Population-weighted over all country segments",
    x = "Year",
    y = "Gini coefficient"
  ) +
  theme_report(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

combined_ineq <- p_lorenz + p_world_gini + plot_layout(widths = c(1.05, 0.95))
ggsave(
  file.path(ineq_dir, "global_food_consumption_lorenz_and_gini_trend.png"),
  combined_ineq,
  width = 13.2,
  height = 6.4,
  dpi = 300,
  bg = "white"
)

world <- build_world_map()
world_gini_map <- world |>
  left_join(as.data.frame(country_gini_2050), by = "R")

unmatched_gini <- setdiff(country_gini_2050$R, world$R)
unmatched_gini_file <- file.path(ineq_dir, "unmatched_country_gini_iso3.csv")
if (length(unmatched_gini) > 0) {
  write_csv(tibble::tibble(R = unmatched_gini), unmatched_gini_file)
} else if (file.exists(unmatched_gini_file)) {
  unlink(unmatched_gini_file)
}

p_gini_map <- ggplot(world_gini_map) +
  geom_sf(aes(fill = gini), color = "grey70", linewidth = 0.08) +
  scale_fill_viridis_c(
    option = "C",
    direction = -1,
    na.value = "grey92",
    labels = label_number(accuracy = 0.01)
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = paste0("Country food consumption Gini in ", plot_year),
    subtitle = paste0("Population-weighted by seg; total food consumption; Ref = ", scenario),
    fill = "Gini"
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", color = "grey10"),
    plot.subtitle = element_text(color = "grey30"),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  file.path(ineq_dir, "country_food_consumption_gini_2050_map.png"),
  p_gini_map,
  width = 12.8,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

cat("Reading PoU R17 data...\n")
pou_income_ragg <- as.data.table(rgdx.param(gdx_path, "PoU_In_Ragg_calc"))
setnames(pou_income_ragg, "R17", "R")
pou_income_ragg <- pou_income_ragg[
  R %in% r17_regions,
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    year = as.integer(as.character(Y)),
    value = as.numeric(PoU_In_Ragg_calc)
  )
]
pou_base <- pou_income_ragg[
  Ref == baseline_ref,
  .(R, year, baseline = value)
]
pou_income_only <- pou_income_ragg[
  Ref == scenario,
  .(R, year, income_only = value)
]

pou_income_price <- as.data.table(rgdx.param(gdx_path, "PoU_In_Pr_Ragg_calc"))
setnames(pou_income_price, "R17", "R")
pou_income_price <- pou_income_price[
  Ref == scenario & R %in% r17_regions,
  .(
    R = as.character(R),
    year = as.integer(as.character(Y)),
    income_price = as.numeric(PoU_In_Pr_Ragg_calc)
  )
]

pou_ragg <- Reduce(
  function(x, y) merge(x, y, by = c("R", "year"), all = FALSE),
  list(pou_base, pou_income_only, pou_income_price)
)
pou_ragg[
  ,
  `:=`(
    baseline_million = baseline / 1e6,
    income_only_million = income_only / 1e6,
    income_price_million = income_price / 1e6,
    income_effect_million = (income_only - baseline) / 1e6,
    price_effect_million = (income_price - income_only) / 1e6
  )
]
pou_ragg[, R := factor(R, levels = r17_regions)]
fwrite(
  pou_ragg[order(R, year)],
  file.path(hunger_dir, "hunger_risk_population_r17_wld.csv")
)

p_hunger <- ggplot(pou_ragg, aes(x = year)) +
  geom_ribbon(
    aes(
      ymin = pmin(baseline_million, income_only_million),
      ymax = pmax(baseline_million, income_only_million),
      fill = "Income effect"
    ),
    alpha = 0.48
  ) +
  geom_ribbon(
    aes(
      ymin = pmin(income_only_million, income_price_million),
      ymax = pmax(income_only_million, income_price_million),
      fill = "Price effect"
    ),
    alpha = 0.48
  ) +
  geom_line(aes(y = baseline_million, color = "Baseline"), linewidth = 0.45) +
  geom_line(aes(y = income_only_million, color = "Income only"), linewidth = 0.55, linetype = "dashed") +
  geom_line(aes(y = income_price_million, color = "Income + price"), linewidth = 0.65) +
  facet_wrap(~ R, ncol = 6, scales = "free_y") +
  scale_fill_manual(values = effect_fills, drop = FALSE) +
  scale_color_manual(
    values = c(
      "Baseline" = "grey30",
      "Income only" = "#D55E00",
      "Income + price" = "#0072B2"
    ),
    drop = FALSE
  ) +
  scale_x_continuous(breaks = sort(unique(pou_ragg$year))) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  labs(
    title = "Population at risk of hunger by R17/WLD",
    subtitle = paste0("Ref = ", scenario, "; ribbons show income and price effects relative to ", baseline_ref),
    x = "Year",
    y = "Population at risk of hunger (million people)",
    fill = "Effect",
    color = "Series"
  ) +
  theme_report(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.box = "vertical"
  )

ggsave(
  file.path(hunger_dir, "hunger_risk_population_r17_wld_panels.png"),
  p_hunger,
  width = 18,
  height = 9.6,
  dpi = 300,
  bg = "white"
)

cat("Reading country PoU_In_Pr...\n")
pou_country <- as.data.table(rgdx.param(gdx_path, "PoU_In_Pr"))
pou_country <- pou_country[
  Ref == scenario & Y == as.character(plot_year) & is_country_code(as.character(R)),
  .(
    R = as.character(R),
    year = as.integer(as.character(Y)),
    hunger_risk_population = as.numeric(PoU_In_Pr),
    hunger_risk_million = as.numeric(PoU_In_Pr) / 1e6
  )
][order(R)]
fwrite(pou_country, file.path(hunger_dir, "country_pou_in_pr_2050.csv"))

world_pou_map <- world |>
  left_join(as.data.frame(pou_country), by = "R")

unmatched_pou <- setdiff(pou_country$R, world$R)
unmatched_pou_file <- file.path(hunger_dir, "unmatched_country_pou_iso3.csv")
if (length(unmatched_pou) > 0) {
  write_csv(tibble::tibble(R = unmatched_pou), unmatched_pou_file)
} else if (file.exists(unmatched_pou_file)) {
  unlink(unmatched_pou_file)
}

p_pou_map <- ggplot(world_pou_map) +
  geom_sf(aes(fill = hunger_risk_million), color = "grey70", linewidth = 0.08) +
  scale_fill_viridis_c(
    option = "A",
    trans = "sqrt",
    na.value = "grey92",
    labels = label_number(big.mark = ",", accuracy = 0.1)
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = paste0("Country PoU_In_Pr in ", plot_year),
    subtitle = paste0("Ref = ", scenario),
    fill = "Million people"
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", color = "grey10"),
    plot.subtitle = element_text(color = "grey30"),
    plot.margin = margin(8, 8, 8, 8)
  )

ggsave(
  file.path(hunger_dir, "country_pou_in_pr_2050_map.png"),
  p_pou_map,
  width = 12.8,
  height = 6.8,
  dpi = 300,
  bg = "white"
)

cat("Output directory:", normalizePath(out_dir, winslash = "\\", mustWork = FALSE), "\n")
cat("Food consumption plots:", length(list.files(food_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
cat("Population_seg_RAS plots:", length(list.files(pop_ras_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
cat("Inequality plots:", length(list.files(ineq_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
cat("Hunger plots:", length(list.files(hunger_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
