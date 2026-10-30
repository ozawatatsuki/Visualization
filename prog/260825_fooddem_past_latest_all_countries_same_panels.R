suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect_old.gdx")) &
    file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected one data/260807_FDM* directory containing the past and current GDX files; found: ", length(data_candidates))
}
data_dir <- data_candidates[[1L]]

old_gdx <- file.path(data_dir, "Fooddem_PriceEffect_old.gdx")
current_gdx <- file.path(data_dir, "Fooddem_Quantile_10.gdx")
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
scenario_label <- "400C / 2025CP"
target_year <- 2050L

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "past_current_fooddem_all_countries_2025CP_same_panels"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_filtered_parameter <- function(gdx_path, symbol, domains, filters = list()) {
  universe <- rgdx(gdx_path)$uels
  requested_uels <- lapply(domains, function(domain_name) {
    if (domain_name %in% names(filters)) as.character(filters[[domain_name]]) else universe
  })
  raw <- rgdx(gdx_path, list(name = symbol, form = "sparse", uels = requested_uels))
  if (!identical(raw$dim, as.integer(length(domains)))) {
    stop("Unexpected dimension for ", symbol, " in ", basename(gdx_path), ".")
  }
  if (is.null(raw$val) || nrow(raw$val) == 0L) {
    stop("No filtered rows for ", symbol, " in ", basename(gdx_path), ".")
  }
  out <- as.data.table(setNames(
    lapply(seq_along(domains), function(j) raw$uels[[j]][raw$val[, j]]),
    domains
  ))
  out[[symbol]] <- as.numeric(raw$val[, length(domains) + 1L])
  out
}

complete_sparse <- function(sparse_dt, grid, keys, value_name) {
  out <- merge(grid, sparse_dt, by = keys, all.x = TRUE, sort = FALSE)
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

prepare_all_effects <- function(
  gdx_path,
  version_label,
  income_symbol,
  price_symbol,
  segment_domain,
  allowed_regions
) {
  filters <- list(
    Ref = c(baseline_ref, scenario_ref),
    Y = as.character(target_year)
  )
  domains <- c("Ref", "var", "R", "Y", segment_domain)
  income <- read_filtered_parameter(gdx_path, income_symbol, domains, filters)
  price <- read_filtered_parameter(gdx_path, price_symbol, domains, filters)
  setnames(income, c(segment_domain, income_symbol), c("quantile", "income_value"))
  setnames(price, c(segment_domain, price_symbol), c("quantile", "price_value"))
  for (col in c("Ref", "var", "R", "Y", "quantile")) {
    set(income, j = col, value = as.character(income[[col]]))
    set(price, j = col, value = as.character(price[[col]]))
  }

  keys <- c("Ref", "var", "R", "Y", "quantile")
  if (anyDuplicated(income, by = keys) || anyDuplicated(price, by = keys)) {
    stop("Duplicate FoodDem keys in ", version_label, ".")
  }
  grid <- CJ(
    Ref = c(baseline_ref, scenario_ref),
    var = sort(unique(c(income$var, price$var))),
    R = sort(unique(allowed_regions)),
    Y = as.character(target_year),
    quantile = unique(c(income$quantile, price$quantile)),
    unique = TRUE
  )
  income <- complete_sparse(income, grid, keys, "income_value")
  price <- complete_sparse(price, grid, keys, "price_value")
  combined <- merge(income, price, by = keys, all = FALSE, sort = FALSE)

  bau <- combined[Ref == baseline_ref, .(var, R, Y, quantile, bau_value = income_value)]
  scenario <- merge(
    combined[Ref == scenario_ref],
    bau,
    by = c("var", "R", "Y", "quantile"),
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(scenario$bau_value)) {
    stop("Missing BaU values in ", version_label, ".")
  }
  scenario[, `:=`(
    income_effect = income_value - bau_value,
    price_effect = price_value - income_value,
    total_effect = price_value - bau_value,
    Version = version_label
  )]
  gap <- max(abs(scenario$income_effect + scenario$price_effect - scenario$total_effect))
  if (!is.finite(gap) || gap > 1e-8) {
    stop("FoodDem decomposition failed in ", version_label, ": ", gap)
  }
  scenario
}

read_all_country_codes <- function(gdx_path, symbol) {
  raw <- as.data.table(rgdx.param(gdx_path, symbol))
  if (!"R" %in% names(raw)) {
    stop("Missing R domain in ", symbol, " from ", basename(gdx_path), ".")
  }
  codes <- sort(unique(as.character(raw$R)))
  codes[grepl("^[A-Z]{3}$", codes) & codes != "WLD"]
}

past_country_universe <- read_all_country_codes(old_gdx, "FoodDemIn")
current_country_universe <- read_all_country_codes(current_gdx, "FoodDemIn_Quantile")
comparable_country_codes <- sort(intersect(past_country_universe, current_country_universe))
if (length(comparable_country_codes) == 0L) {
  stop("No comparable country codes were found in the past/current GDX files.")
}

cat("Reading all-country FoodDem data...\n")
past <- prepare_all_effects(
  old_gdx,
  "Past version",
  "FoodDemIn",
  "FoodDem_peffect",
  "seg",
  comparable_country_codes
)
current <- prepare_all_effects(
  current_gdx,
  "Current version",
  "FoodDemIn_Quantile",
  "FoodDem_peffect_Quantile",
  "quantile",
  comparable_country_codes
)

country_codes <- comparable_country_codes
past <- past[R %in% country_codes]
current <- current[R %in% country_codes]

if (!identical(sort(unique(past$var)), sort(unique(current$var)))) {
  stop("Past/current FoodDem category sets differ.")
}
if (!setequal(unique(past$quantile), unique(current$quantile))) {
  stop("Past/current FoodDem quantile sets differ.")
}

fooddem <- rbindlist(list(past, current), use.names = TRUE)
fooddem[, quantile_num := suppressWarnings(as.integer(quantile))]
if (fooddem[quantile != "ALL" & is.na(quantile_num), .N] > 0L) {
  stop("Unexpected non-numeric quantile label.")
}
n_quantiles <- max(fooddem[quantile != "ALL", quantile_num])
if (!identical(sort(unique(fooddem[quantile != "ALL", quantile_num])), seq_len(n_quantiles))) {
  stop("Quantiles are not consecutive from 1 to ", n_quantiles, ".")
}
all_position <- n_quantiles + 2L
fooddem[quantile == "ALL", quantile_num := all_position]
fooddem[, Version := factor(Version, levels = c("Past version", "Current version"))]

var_levels <- c(sort(setdiff(unique(fooddem$var), "total")), intersect("total", unique(fooddem$var)))
fooddem[, var := factor(var, levels = var_levels)]

effects <- melt(
  fooddem,
  id.vars = c("Version", "Ref", "var", "R", "Y", "quantile", "quantile_num", "total_effect"),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "Effect",
  value.name = "Value"
)
effects[, Effect := factor(
  Effect,
  levels = c("income_effect", "price_effect"),
  labels = c("Income effect", "Price effect")
)]
setorder(effects, R, var, Version, quantile_num, Effect)

var_labels <- c(
  "Cereals" = "Cereals",
  "Dairy" = "Dairy",
  "Fish" = "Fish",
  "Food_Other" = "Other foods",
  "Fruits_and_Veg" = "Fruits & vegetables",
  "Meats" = "Meat",
  "Oils_and_Fats" = "Oils & fats",
  "total" = "Total"
)
effect_colors <- c("Income effect" = "#D55E00", "Price effect" = "#0072B2")
version_linetype <- c("Past version" = "22", "Current version" = "solid")
version_shape <- c("Past version" = 1, "Current version" = 16)

subtitle_text <- paste(
  strwrap(
    paste0(
      scenario_label,
      ". Past and current versions are overlaid within each food-category panel. ",
      "Pale yellow marks a category with at least one positive past-version income effect."
    ),
    width = 155
  ),
  collapse = "\n"
)
caption_text <- paste(
  strwrap(
    paste0(
      "Past: Fooddem_PriceEffect_old.gdx; Current: Fooddem_Quantile_10.gdx. ",
      "Income = FoodDemIn(scenario) - FoodDemIn(BaU); ",
      "Price = FoodDem_peffect(scenario) - FoodDemIn(scenario). Cross-price effects are excluded."
    ),
    width = 190
  ),
  collapse = "\n"
)

make_country_plot <- function(country) {
  country_data <- effects[R == country]
  positive_vars <- country_data[
    Version == "Past version" & Effect == "Income effect" & Value > 1e-10,
    sort(unique(as.character(var)))
  ]
  highlight_panels <- data.table(
    var = factor(positive_vars, levels = var_levels),
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf
  )

  ggplot(
    country_data,
    aes(
      x = quantile_num,
      y = Value,
      color = Effect,
      linetype = Version,
      shape = Version,
      group = interaction(Effect, Version)
    )
  ) +
    geom_rect(
      data = highlight_panels,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
      inherit.aes = FALSE,
      fill = "#FFF0BF",
      alpha = 0.72
    ) +
    geom_hline(yintercept = 0, color = "grey37", linewidth = 0.65) +
    geom_line(data = country_data[quantile != "ALL"], linewidth = 1.05) +
    geom_point(size = 2.35, stroke = 0.7) +
    geom_vline(
      xintercept = n_quantiles + 1L,
      color = "grey48",
      linewidth = 0.5,
      linetype = "dotted"
    ) +
    facet_wrap(
      vars(var),
      ncol = 4,
      scales = "free_y",
      labeller = labeller(var = as_labeller(var_labels))
    ) +
    scale_color_manual(values = effect_colors, drop = FALSE) +
    scale_linetype_manual(values = version_linetype, drop = FALSE) +
    scale_shape_manual(values = version_shape, drop = FALSE) +
    scale_x_continuous(
      breaks = c(seq_len(n_quantiles), all_position),
      labels = c(as.character(seq_len(n_quantiles)), "ALL"),
      limits = c(0.5, all_position + 0.6),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = label_number()) +
    guides(
      color = guide_legend(order = 1, nrow = 1, override.aes = list(linewidth = 1.4)),
      linetype = guide_legend(order = 2, nrow = 1),
      shape = guide_legend(order = 2, nrow = 1)
    ) +
    labs(
      title = paste0(country, " FoodDem effects in ", target_year, ": past vs current"),
      subtitle = subtitle_text,
      x = "Income decile",
      y = "Effect on FoodDem (kcal/day/capita)",
      color = "Effect",
      linetype = "Data version",
      shape = "Data version",
      caption = caption_text
    ) +
    theme_minimal(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey87", linewidth = 0.35),
      panel.grid.major.y = element_line(color = "grey82", linewidth = 0.4),
      panel.border = element_rect(color = "grey65", fill = NA, linewidth = 0.5),
      axis.text = element_text(color = "grey20", size = 11.2),
      axis.title = element_text(color = "grey10", size = 13),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 11.2),
      legend.title = element_text(size = 11.2, face = "bold"),
      plot.title = element_text(face = "bold", color = "grey8", size = 19),
      plot.subtitle = element_text(color = "grey25", size = 12, margin = margin(b = 8)),
      plot.caption = element_text(color = "grey32", hjust = 0, size = 9.2),
      strip.text = element_text(face = "bold", color = "grey8", size = 12.5),
      strip.background = element_rect(fill = "grey94", color = NA),
      plot.margin = margin(12, 14, 12, 12)
    )
}

manifest <- vector("list", length(country_codes))
cat("Creating ", length(country_codes), " country PNG files...\n", sep = "")
for (i in seq_along(country_codes)) {
  country <- country_codes[[i]]
  png_path <- file.path(out_dir, paste0(country, "_FoodDem_income_price_effects_past_vs_current_2050.png"))
  p <- make_country_plot(country)
  ggsave(png_path, p, width = 16, height = 9.6, units = "in", dpi = 300, bg = "white")
  positive_panel_count <- effects[
    R == country & Version == "Past version" & Effect == "Income effect" & Value > 1e-10,
    uniqueN(var)
  ]
  manifest[[i]] <- data.table(
    R = country,
    png = normalizePath(png_path, winslash = "/", mustWork = TRUE),
    positive_past_income_panels = positive_panel_count
  )
  if (i %% 25L == 0L || i == length(country_codes)) {
    cat("  completed ", i, "/", length(country_codes), "\n", sep = "")
  }
}

manifest <- rbindlist(manifest)
manifest_path <- file.path(out_dir, "FoodDem_past_current_all_countries_manifest.csv")
data_path <- file.path(out_dir, "FoodDem_past_current_all_countries_2050_data.csv")
coverage_path <- file.path(out_dir, "FoodDem_past_current_country_coverage.csv")
fwrite(manifest, manifest_path)
fwrite(
  effects[, .(
    Version = as.character(Version), Ref, var = as.character(var), R, Y,
    quantile, Effect = as.character(Effect), Value, total_effect
  )],
  data_path
)
coverage <- data.table(R = sort(union(past_country_universe, current_country_universe)))
coverage[, `:=`(
  present_in_past = R %in% past_country_universe,
  present_in_current = R %in% current_country_universe,
  comparable_and_plotted = R %in% country_codes
)]
fwrite(coverage, coverage_path)

if (manifest[, uniqueN(R)] != length(country_codes)) {
  stop("Country manifest count does not match the intended country count.")
}
cat("Created country plots:", length(country_codes), "\n")
cat("Current-only countries without a past-version counterpart:",
    paste(setdiff(current_country_universe, past_country_universe), collapse = ", "), "\n")
cat("Output:", normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n")
