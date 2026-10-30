suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(readr)
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

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update")
food_dir <- file.path(out_dir, "food_consumption_change_2050")
total_food_dir <- file.path(food_dir, "total")
facet_food_dir <- file.path(food_dir, "facet")
country_food_dir <- file.path(food_dir, "country_facet_by_r17")
dir.create(total_food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(facet_food_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(country_food_dir, recursive = TRUE, showWarnings = FALSE)

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

effect_fills <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

plot_effect_change <- function(ribbons, line_data, title, subtitle, facet_vars = FALSE, base_size = 11) {
  p <- ggplot() +
    geom_hline(yintercept = 0, color = "grey35", linewidth = 0.35) +
    geom_ribbon(
      data = ribbons,
      aes(x = BudgetInitial, ymin = ymin, ymax = ymax, fill = effect),
      alpha = 0.72
    ) +
    geom_line(
      data = line_data,
      aes(x = BudgetInitial, y = total_effect),
      color = "grey10",
      linewidth = 0.55
    ) +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_x_log10(labels = label_number(big.mark = ",")) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "BudgetInitial (log scale)",
      y = "Change from baseline (kcal/day/capita)",
      fill = "Effect"
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
      y = "Change from baseline (kcal/day/capita)",
      fill = "Effect"
    ) +
    theme_report(base_size = base_size)
}

clear_generated_files(total_food_dir)
clear_generated_files(facet_food_dir)
clear_generated_files(country_food_dir)
legacy_region_dirs <- file.path(food_dir, r17_regions)
legacy_region_dirs <- legacy_region_dirs[dir.exists(legacy_region_dirs)]
if (length(legacy_region_dirs) > 0) {
  unlink(legacy_region_dirs, recursive = TRUE, force = TRUE)
}
legacy_files <- c(
  file.path(food_dir, paste0("food_consumption_change_2050_", r17_regions, ".png")),
  file.path(food_dir, "food_consumption_effects_2050_r17_wld.csv"),
  file.path(food_dir, "unmapped_food_consumption_region_iso3.csv")
)
legacy_files <- legacy_files[file.exists(legacy_files)]
if (length(legacy_files) > 0) {
  unlink(legacy_files, force = TRUE)
}

cat("Reading BudgetInitial...\n")
budget <- as.data.table(rgdx.param(gdx_path, "BudgetInitial"))
setnames(budget, "seg2", "seg")
budget[, `:=`(
  seg = as.character(seg),
  BudgetInitial = as.numeric(BudgetInitial)
)]
linear_bar_bounds <- make_linear_bar_bounds(budget)

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
    seg = as.character(seg),
    food_income = as.numeric(FoodDemIn)
  )
]
food_income[, year := as.integer(Y)]
gc()

cat("Reading FoodDem_peffect...\n")
food_price <- as.data.table(rgdx.param(gdx_path, "FoodDem_peffect"))
food_price <- food_price[
  Ref == scenario &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(FoodDem_peffect),
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

cat("Reading Population_seg...\n")
population_seg <- as.data.table(rgdx.param(gdx_path, "Population_seg"))
setnames(population_seg, "seg2", "seg")
population_seg <- population_seg[
  Ref %in% c(scenario, baseline_ref) &
    is_country_code(as.character(R)) &
    Y == as.character(plot_year) &
    seg != "ALL" &
    !is.na(Population_seg),
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    seg = as.character(seg),
    population = as.numeric(Population_seg)
  )
]
population_seg[, year := as.integer(Y)]

food_countries <- sort(unique(c(food_income$R, food_price$R)))
unmapped_food_countries <- setdiff(food_countries, region_map$R)
unmapped_food_file <- file.path(food_dir, "unmapped_food_consumption_region_iso3.csv")
if (length(unmapped_food_countries) > 0) {
  write_csv(data.frame(R = unmapped_food_countries), unmapped_food_file)
  warning(
    "Some FoodDem countries are missing in region_iso_R17PHI.map and are excluded from R17 aggregation: ",
    paste(head(unmapped_food_countries, 20), collapse = ", ")
  )
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

cat("Aggregating R17/WLD...\n")
food_income_agg <- aggregate_country_consumption(food_income, "food_income")
food_price_agg <- aggregate_country_consumption(food_price, "income_price")

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
food_effects_wide[, R := factor(R, levels = r17_regions)]
food_effects_wide[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]

food_effects <- melt(
  food_effects_wide,
  id.vars = c("R", "var", "year", "seg", "BudgetInitial", "baseline", "income", "income_price", "total_effect"),
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
fwrite(
  food_effects[var == "total"][order(R, BudgetInitial, effect)],
  file.path(total_food_dir, "food_consumption_effects_2050_r17_wld_total.csv")
)
fwrite(
  food_effects[order(R, var, BudgetInitial, effect)],
  file.path(facet_food_dir, "food_consumption_effects_2050_r17_wld_facet.csv")
)

food_stack_ribbons <- make_effect_ribbons(food_effects_wide)

cat("Plotting R17/WLD total and facet figures...\n")
for (region in r17_regions) {
  region_total_line <- unique(food_effects_wide[R == region & var == "total", .(BudgetInitial, total_effect)])
  total_plot <- plot_effect_change(
    ribbons = food_stack_ribbons[R == region & var == "total"],
    line_data = region_total_line,
    title = paste0(region, " total food consumption change in ", plot_year),
    subtitle = paste0(
      "Income = FoodDemIn(", scenario, ") - FoodDemIn(", baseline_ref,
      "); Price = FoodDem_peffect(", scenario, ") - FoodDemIn(", scenario, ")"
    ),
    facet_vars = FALSE,
    base_size = 11
  )
  ggsave(
    file.path(total_food_dir, paste0("food_consumption_change_2050_", region, ".png")),
    total_plot,
    width = 9.5,
    height = 5.4,
    dpi = 300,
    bg = "white"
  )

  region_total_facet <- unique(food_effects_wide[R == region, .(var, BudgetInitial, total_effect)])
  facet_plot <- plot_effect_change(
    ribbons = food_stack_ribbons[R == region],
    line_data = region_total_facet,
    title = paste0(region, " food consumption change by var in ", plot_year),
    subtitle = paste0(
      "Income = FoodDemIn(", scenario, ") - FoodDemIn(", baseline_ref,
      "); Price = FoodDem_peffect(", scenario, ") - FoodDemIn(", scenario, ")"
    ),
    facet_vars = TRUE,
    base_size = 11
  )
  ggsave(
    file.path(facet_food_dir, paste0("food_consumption_change_2050_", region, ".png")),
    facet_plot,
    width = 12,
    height = 14,
    dpi = 300,
    bg = "white"
  )
}

cat("Building country effects...\n")
country_baseline_food <- food_income[
  Ref == baseline_ref,
  .(R, var, year, seg, baseline = food_income)
]
country_income_food <- food_income[
  Ref == scenario,
  .(R, var, year, seg, income = food_income)
]
country_income_price_food <- food_price[
  Ref == scenario,
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
if (anyNA(country_effects_wide$BudgetInitial)) {
  missing_seg <- unique(country_effects_wide[is.na(BudgetInitial), seg])
  stop("Missing BudgetInitial for country seg: ", paste(head(missing_seg, 20), collapse = ", "))
}

country_region_map <- merge(
  data.table(R = sort(unique(country_effects_wide$R))),
  region_map,
  by = "R",
  all.x = TRUE
)
country_region_map[is.na(R17), R17 := "_unmapped"]
country_unmapped <- country_region_map[R17 == "_unmapped", R]
if (length(country_unmapped) > 0) {
  write_csv(data.frame(R = country_unmapped), file.path(country_food_dir, "unmapped_country_facet_iso3.csv"))
}

country_effects_wide <- merge(country_effects_wide, country_region_map, by = "R", all.x = TRUE)
country_effects_wide[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]
fwrite(
  country_effects_wide[order(R17, R, var, BudgetInitial)],
  file.path(country_food_dir, "food_consumption_effects_2050_country_facet_wide.csv")
)
country_effects_long <- melt(
  country_effects_wide,
  id.vars = c("R17", "R", "var", "year", "seg", "BudgetInitial", "baseline", "income", "income_price", "total_effect"),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "effect",
  value.name = "change"
)
country_effects_long[
  ,
  effect := factor(
    effect,
    levels = c("income_effect", "price_effect"),
    labels = c("Income effect", "Price effect")
  )
]
fwrite(
  country_effects_long[order(R17, R, var, BudgetInitial, effect)],
  file.path(country_food_dir, "food_consumption_effects_2050_country_facet_long.csv")
)
country_bars <- merge(
  make_effect_ribbons(country_effects_wide),
  linear_bar_bounds,
  by = "seg",
  all.x = TRUE
)
if (anyNA(country_bars$bar_xmin) || anyNA(country_bars$bar_xmax)) {
  stop("Missing bar bounds for country food-consumption effects.")
}

cat("Plotting country facet figures...\n")
for (country in country_region_map$R) {
  country_region <- country_region_map[R == country, R17][1]
  country_dir <- file.path(country_food_dir, country_region)
  dir.create(country_dir, recursive = TRUE, showWarnings = FALSE)

  country_plot <- plot_effect_bars(
    bars = country_bars[R == country],
    line_data = unique(country_effects_wide[R == country, .(var, BudgetInitial, total_effect)]),
    title = paste0(country, " food consumption change by var in ", plot_year),
    subtitle = paste0(
      "Income = FoodDemIn(", scenario, ") - FoodDemIn(", baseline_ref,
      "); Price = FoodDem_peffect(", scenario, ") - FoodDemIn(", scenario, ")"
    ),
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

cat("Output directory:", normalizePath(food_dir, winslash = "\\", mustWork = FALSE), "\n")
cat("Total plots:", length(list.files(total_food_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
cat("Facet plots:", length(list.files(facet_food_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
cat("Country facet plots:", length(list.files(country_food_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
