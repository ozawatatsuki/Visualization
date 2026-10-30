# Figure 2: Distributional mechanisms behind changes in food consumption.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "tidyr", "ggplot2", "patchwork", "scales", "ragg"
)
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

source(file.path("prog", "figure_utils.R"))

library(dplyr)
library(tidyr)
library(ggplot2)
library(patchwork)

analysis_path <- file.path("data", "PHI", "AnalysisExpenditure.gdx")
input_path <- file.path("data", "PHI", "Inputdata.gdx")
fdm_variant <- Sys.getenv("FDM_VARIANT", unset = "ave")
fdm_folder <- switch(
  fdm_variant,
  ave = "ave修正後",
  detailed = "詳細修正後",
  freqseg = "freqseg変更後",
  mder = "MDER更新後",
  stop(
    "FDM_VARIANT must be 'ave', 'detailed', 'freqseg', or 'mder'.",
    call. = FALSE
  )
)
food_price_path <- file.path(
  "data", "FDM", fdm_folder, "Fooddem_PriceEffect.gdx"
)
quantile_path <- file.path(
  "data", "FDM", fdm_folder, "Fooddem_Quantile_10.gdx"
)

for (path in c(analysis_path, input_path, food_price_path, quantile_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

scenario_codes <- c(
  Baseline = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)
scenario_labels <- setNames(names(scenario_codes), unname(scenario_codes))
target_years <- 2050L

scenario_colors <- c(
  Baseline = "#555555",
  `400C` = "#B2182B"
)
direction_colors <- c(
  Decrease = "#4477AA",
  Increase = "#B2182B"
)
effect_labels <- c(
  income = "Income",
  price = "Own-price",
  cross_price = "Cross-price"
)
effect_colors <- c(
  Income = "#4477AA",
  `Own-price` = "#EE7733",
  `Cross-price` = "#009988"
)

analysis <- gamstransfer::Container$new()
analysis$read(analysis_path, symbols = c("Mu_exp", "sigma_exp"))

input <- gamstransfer::Container$new()
input$read(input_path, symbols = c("Population", "PriceChange"))

food_price <- gamstransfer::Container$new()
food_price$read(food_price_path, symbols = "FPchange")

quantile_data <- gamstransfer::Container$new()
quantile_data$read(
  quantile_path,
  symbols = c(
    "FoodDem_effect_diff", "FoodDem_effect_ratio",
    "FoodDem_peffect_CR_Quantile"
  )
)

read_country_parameter <- function(symbol, value_name) {
  analysis[symbol]$records |>
    tibble::as_tibble() |>
    transmute(
      region = as.character(.data[["R"]]),
      scenario_code = as.character(.data[["Ref"]]),
      year = as.integer(as.character(.data[["Y"]])),
      !!value_name := .data[["value"]]
    ) |>
    filter(
      scenario_code %in% unname(scenario_codes),
      year %in% target_years
    )
}

income_parameters <- read_country_parameter("Mu_exp", "mu") |>
  inner_join(
    read_country_parameter("sigma_exp", "sigma"),
    by = c("region", "scenario_code", "year")
  )

population <- input["Population"]$records |>
  tibble::as_tibble() |>
  transmute(
    region = as.character(.data[["R"]]),
    scenario_code = as.character(.data[["Ref"]]),
    year = as.integer(as.character(.data[["Y"]])),
    population = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year %in% target_years
  )

income_parameters <- income_parameters |>
  inner_join(
    population,
    by = c("region", "scenario_code", "year")
  )

income_check <- income_parameters |>
  count(scenario_code, year, name = "countries")
if (
  nrow(income_check) != length(scenario_codes) * length(target_years) ||
    any(income_check$countries < 175) ||
    anyNA(income_parameters)
) {
  stop("Country income parameters or population weights are incomplete.", call. = FALSE)
}

income_grid <- exp(seq(log(100), log(3e5), length.out = 700))

make_global_income_density <- function(data) {
  weights <- data$population / sum(data$population)
  density <- vapply(
    income_grid,
    function(income) {
      sum(weights * stats::dlnorm(income, meanlog = data$mu, sdlog = data$sigma))
    },
    numeric(1)
  )

  tibble(income = income_grid, density = density)
}

income_density <- income_parameters |>
  group_by(scenario_code, year) |>
  group_modify(~ make_global_income_density(.x)) |>
  ungroup() |>
  mutate(
    scenario = factor(
      unname(scenario_labels[scenario_code]),
      levels = names(scenario_codes)
    )
  )

food_item_labels <- c(
  Cereals = "Cereals",
  Meats = "Meat",
  Fish = "Fish",
  Dairy = "Dairy",
  Oils_and_Fats = "Oils & fats",
  Fruits_and_Veg = "Fruit & vegetables",
  Food_Other = "Other food"
)
commodity_colors <- c(
  Cereals = "#FFE4B5",
  Meat = "#FA8072",
  Fish = "#8DB6CD",
  Dairy = "#EED2EE",
  `Oils & fats` = "#F4A460",
  `Fruit & vegetables` = "#BCEE68",
  `Other food` = "#BEBEBE"
)

food_price_index <- food_price["FPchange"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    item_code = as.character(.data[["var2"]]),
    region = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    value = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    region == "WLD",
    year %in% target_years,
    item_code %in% names(food_item_labels)
  ) |>
  pivot_wider(names_from = scenario_code, values_from = value) |>
  transmute(
    year,
    item = unname(food_item_labels[item_code]),
    index = .data[[scenario_codes[["400C"]]]] /
      .data[[scenario_codes[["Baseline"]]]],
    direction = if_else(index >= 1, "Increase", "Decrease")
  )

nonfood_item_labels <- c(
  "Alcoholic beverages, tobacco, and narcotics" = "Alcohol & tobacco",
  "Clothing and footwear" = "Clothing & footwear",
  "Housing, water, electricity, gas and other fuels" = "Housing & utilities",
  "Furnishings, household equipment and maintenance" = "Furnishings",
  "Health" = "Health",
  "Transport" = "Transport",
  "Communication" = "Communication",
  "Recreation and culture" = "Recreation & culture",
  "Education" = "Education",
  "Restaurants and hotels" = "Restaurants & hotels",
  "Miscellaneous goods and services" = "Miscellaneous"
)

nonfood_price_index <- input["PriceChange"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    item_code = as.character(.data[["I"]]),
    region = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    value = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    region == "WLD",
    year %in% target_years,
    item_code %in% names(nonfood_item_labels)
  ) |>
  pivot_wider(names_from = scenario_code, values_from = value) |>
  transmute(
    year,
    item = unname(nonfood_item_labels[item_code]),
    index = .data[[scenario_codes[["400C"]]]] /
      .data[[scenario_codes[["Baseline"]]]],
    direction = if_else(index >= 1, "Increase", "Decrease")
  )

if (
  nrow(food_price_index) != length(food_item_labels) * length(target_years) ||
    nrow(nonfood_price_index) != length(nonfood_item_labels) * length(target_years) ||
    anyNA(food_price_index) ||
    anyNA(nonfood_price_index)
) {
  stop("Price-index data are incomplete after Baseline normalization.", call. = FALSE)
}

read_quantile_effect <- function(symbol, multiplier = 1) {
  quantile_data[symbol]$records |>
    tibble::as_tibble() |>
    filter(
      as.character(.data[["Ref"]]) == scenario_codes[["400C"]],
      as.character(.data[["var"]]) == "total",
      as.character(.data[["R"]]) == "WLD",
      as.integer(as.character(.data[["Y"]])) %in% target_years
    ) |>
    transmute(
      year = as.integer(as.character(.data[["Y"]])),
      quantile_code = as.character(.data[["quantile"]]),
      effect = factor(
        unname(effect_labels[as.character(.data[["effect"]])]),
        levels = unname(effect_labels)
      ),
      value = .data[["value"]] * multiplier
    ) |>
    mutate(
      quantile_number = match(
        quantile_code,
        c(as.character(1:10), "ALL")
      ),
      quantile = factor(
        quantile_number,
        levels = 1:11,
        labels = c(as.character(1:10), "Ave")
      )
    ) |>
    arrange(year, quantile_number, effect)
}

food_effect_diff <- read_quantile_effect("FoodDem_effect_diff")
food_effect_ratio <- read_quantile_effect("FoodDem_effect_ratio", multiplier = 100)

expected_effect_rows <- length(target_years) * 11 * length(effect_labels)
if (
  nrow(food_effect_diff) != expected_effect_rows ||
    nrow(food_effect_ratio) != expected_effect_rows ||
    anyNA(food_effect_diff) ||
    anyNA(food_effect_ratio)
) {
  stop("Quantile effect data are incomplete.", call. = FALSE)
}

food_consumption_wide <- quantile_data["FoodDem_peffect_CR_Quantile"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    item_code = as.character(.data[["var"]]),
    region = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    quantile_code = as.character(.data[["quantile"]]),
    value = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    item_code %in% c(names(food_item_labels), "total"),
    region == "WLD",
    year %in% target_years
  ) |>
  select(-region) |>
  pivot_wider(names_from = scenario_code, values_from = value)

baseline_total <- food_consumption_wide |>
  filter(item_code == "total") |>
  transmute(
    year,
    quantile_code,
    baseline_total = .data[[scenario_codes[["Baseline"]]]],
    total_diff = .data[[scenario_codes[["400C"]]]] - baseline_total
  )

food_commodity_stack <- food_consumption_wide |>
  filter(item_code %in% names(food_item_labels)) |>
  inner_join(baseline_total, by = c("year", "quantile_code")) |>
  transmute(
    year,
    quantile_code,
    commodity = factor(
      unname(food_item_labels[item_code]),
      levels = unname(food_item_labels)
    ),
    difference = .data[[scenario_codes[["400C"]]]] -
      .data[[scenario_codes[["Baseline"]]]],
    ratio = 100 * difference / baseline_total,
    total_diff,
    baseline_total
  ) |>
  mutate(
    quantile_number = match(
      quantile_code,
      c(as.character(1:10), "ALL")
    ),
    quantile = factor(
      quantile_number,
      levels = 1:11,
      labels = c(as.character(1:10), "Ave")
    )
  ) |>
  arrange(year, quantile_number, commodity)

commodity_stack_check <- food_commodity_stack |>
  group_by(year, quantile_code) |>
  summarise(
    difference_error = sum(difference) - first(total_diff),
    ratio_error = sum(ratio) - 100 * first(total_diff) / first(baseline_total),
    .groups = "drop"
  )

expected_commodity_rows <- length(target_years) * 11L * length(food_item_labels)
if (
  nrow(food_commodity_stack) != expected_commodity_rows ||
    anyNA(food_commodity_stack) ||
    max(abs(commodity_stack_check$difference_error)) > 1e-6 ||
    max(abs(commodity_stack_check$ratio_error)) > 1e-8
) {
  stop("Quantile commodity data do not sum to total food consumption.", call. = FALSE)
}

food_commodity_diff <- food_commodity_stack |>
  transmute(year, quantile, commodity, value = difference)
food_commodity_ratio <- food_commodity_stack |>
  transmute(year, quantile, commodity, value = ratio)

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 8.5, colour = "black"),
  plot.tag.position = c(0, 1)
)

make_price_panel <- function(
    data,
    tag,
    limits,
    breaks,
    base_size = 6.1,
    bar_linewidth = 3.0,
    show_values = TRUE) {
  ordered <- data |>
    arrange(index) |>
    mutate(
      item = factor(item, levels = item),
      value_label = sprintf("%.2f", index),
      label_hjust = -0.18
    )

  panel <- ggplot(ordered, aes(y = item)) +
    geom_vline(
      xintercept = 1,
      linewidth = 0.25,
      colour = "#555555",
      linetype = "22"
    ) +
    geom_segment(
      aes(x = 1, xend = index, yend = item, colour = direction),
      linewidth = bar_linewidth,
      lineend = "butt",
      alpha = 0.84
    ) +
    scale_colour_manual(values = direction_colors, guide = "none") +
    scale_x_continuous(
      limits = limits,
      breaks = breaks,
      labels = scales::label_number(accuracy = 0.01),
      expand = expansion(mult = 0)
    ) +
    labs(
      tag = tag,
      x = "Change in price index\n(Baseline = 1)",
      y = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_interim_nature(base_size = base_size, grid = "x", legend_position = "none") +
    panel_tag_theme +
    theme(
      axis.title = element_text(face = "plain", size = base_size),
      axis.text.y = element_text(size = base_size - 0.7),
      axis.text.x = element_text(size = base_size - 0.8),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      plot.margin = margin(1, 1.5, 0.8, 1.5, "mm")
    )

  if (show_values) {
    panel <- panel +
      geom_text(
        aes(x = index, label = value_label, hjust = label_hjust),
        size = 1.65,
        colour = "#333333"
      )
  }

  panel
}

make_stack_panel <- function(
    data,
    tag,
    y_label,
    show_legend,
    fill_variable,
    fill_values,
    legend_nrow = 1) {
  ggplot(
    data,
    aes(x = quantile, y = value, fill = .data[[fill_variable]])
  ) +
    geom_hline(yintercept = 0, linewidth = 0.25, colour = "#333333") +
    geom_col(
      width = 0.72,
      colour = "white",
      linewidth = 0.12
    ) +
    geom_vline(
      xintercept = 10.5,
      linewidth = 0.25,
      colour = "#777777",
      linetype = "22"
    ) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1),
      expand = expansion(mult = c(0.08, 0.08))
    ) +
    labs(
      tag = tag,
      x = "Income quantile",
      y = y_label,
      fill = NULL
    ) +
    theme_interim_nature(
      base_size = 7.0,
      grid = "y",
      legend_position = if (show_legend) "bottom" else "none"
    ) +
    panel_tag_theme +
    theme(
      axis.title = element_text(face = "plain"),
      axis.title.y = element_text(size = 6.1, margin = margin(r = 2)),
      axis.text.x = element_text(size = 6.2),
      axis.ticks.x = element_blank(),
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.6, "mm"),
      plot.tag.position = c(0.16, 0.98),
      plot.margin = margin(1.5, 1.5, 1.2, 4, "mm")
    ) +
    guides(
      fill = guide_legend(
        nrow = legend_nrow,
        byrow = TRUE,
        override.aes = list(colour = NA)
      )
    )
}

find_density_quantile <- function(data, probability = 0.995) {
  ordered <- data |>
    arrange(income)
  interval_area <- diff(ordered$income) *
    (head(ordered$density, -1) + tail(ordered$density, -1)) / 2
  cumulative_area <- c(0, cumsum(interval_area))

  stats::approx(
    cumulative_area / max(cumulative_area),
    ordered$income,
    xout = probability,
    ties = "ordered"
  )$y
}

make_figure <- function(
    target_year,
    stack_by = c("effect", "commodity"),
    income_scale = c("log", "linear")) {
  stack_by <- match.arg(stack_by)
  income_scale <- match.arg(income_scale)
  density_year <- income_density |>
    filter(year == target_year)

  if (stack_by == "effect") {
    stack_diff <- food_effect_diff
    stack_ratio <- food_effect_ratio
    fill_variable <- "effect"
    fill_values <- effect_colors
  } else {
    stack_diff <- food_commodity_diff
    stack_ratio <- food_commodity_ratio
    fill_variable <- "commodity"
    fill_values <- commodity_colors
  }

  panel_a <- ggplot(
    density_year,
    aes(x = income, y = density, colour = scenario, fill = scenario)
  ) +
    geom_area(position = "identity", alpha = 0.07, colour = NA) +
    geom_line(linewidth = 0.48, alpha = 0.88) +
    scale_colour_manual(values = scenario_colors, drop = FALSE) +
    scale_fill_manual(values = scenario_colors, drop = FALSE, guide = "none")

  if (income_scale == "log") {
    panel_a <- panel_a + scale_x_log10(
      breaks = c(100, 1000, 10000, 100000),
      labels = c("100", "1k", "10k", "100k"),
      limits = range(income_grid),
      expand = expansion(mult = 0)
    )
    income_axis_label <- "Income per capita (log scale)"
  } else {
    linear_upper <- density_year |>
      group_by(scenario) |>
      group_modify(~ tibble(upper = find_density_quantile(.x))) |>
      ungroup() |>
      summarise(upper = max(upper), .groups = "drop") |>
      pull(upper)
    linear_upper <- ceiling(linear_upper / 10000) * 10000

    panel_a <- panel_a +
      scale_x_continuous(
        breaks = scales::breaks_pretty(n = 4),
        labels = scales::label_number(scale_cut = scales::cut_short_scale()),
        expand = expansion(mult = 0)
      ) +
      coord_cartesian(xlim = c(0, linear_upper), expand = FALSE)
    income_axis_label <- "Income per capita"
  }

  panel_a <- panel_a +
    scale_y_continuous(
      labels = scales::label_scientific(digits = 1),
      expand = expansion(mult = c(0, 0.08))
    ) +
    labs(
      tag = "a",
      x = income_axis_label,
      y = "Density",
      colour = NULL
    ) +
    theme_interim_nature(base_size = 6.2, grid = "y", legend_position = "top") +
    panel_tag_theme +
    theme(
      axis.title = element_text(face = "plain", size = 6.0),
      axis.text = element_text(size = 5.4),
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.3, "mm"),
      plot.margin = margin(1, 1.5, 0.8, 2, "mm")
    ) +
    guides(
      colour = guide_legend(
        nrow = 1,
        override.aes = list(linewidth = 0.55, alpha = 0.9)
      )
    )

  panel_b <- make_price_panel(
    filter(food_price_index, year == target_year),
    tag = "b",
    limits = c(0.70, 1.95),
    breaks = c(0.75, 1.00, 1.25, 1.50, 1.75),
    base_size = 6.0,
    show_values = FALSE
  )

  panel_c <- make_price_panel(
    filter(nonfood_price_index, year == target_year),
    tag = "c",
    limits = c(0.90, 1.06),
    breaks = c(0.90, 0.95, 1.00, 1.05),
    base_size = 5.6,
    bar_linewidth = 1.65,
    show_values = FALSE
  )

  panel_d <- make_stack_panel(
    filter(stack_diff, year == target_year),
    tag = "d",
    y_label = "Change in food consumption\n(kcal per person per day)",
    show_legend = TRUE,
    fill_variable = fill_variable,
    fill_values = fill_values
  )

  panel_e <- make_stack_panel(
    filter(stack_ratio, year == target_year),
    tag = "e",
    y_label = "Change in food consumption\nfrom Baseline (%)",
    show_legend = stack_by == "commodity",
    fill_variable = fill_variable,
    fill_values = fill_values
  )

  top_row <- (panel_a | panel_b | panel_c) +
    plot_layout(widths = c(1.05, 0.95, 1.20))
  if (stack_by == "commodity") {
    bottom_row <- (panel_d | panel_e) +
      plot_layout(widths = c(1, 1), guides = "collect") &
      theme(legend.position = "bottom", legend.justification = "center")
  } else {
    bottom_row <- (panel_d | panel_e) +
      plot_layout(widths = c(1, 1))
  }

  (top_row / bottom_row) +
    plot_layout(heights = c(0.39, 0.61))
}

if (!isTRUE(getOption("fig2.build_only", FALSE))) {
output_dir <- "output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
existing_png <- list.files(
  output_dir,
  pattern = "^fig2_.*\\.png$",
  full.names = TRUE
)
if (length(existing_png) > 0) {
  file.remove(existing_png)
}

figure <- make_figure(target_years, stack_by = "effect")
output_path <- file.path(
  output_dir,
  paste0("fig2_food_consumption_2050_effect_", fdm_variant, ".png")
)

save_interim_plot(
  figure,
  output_path,
  height_mm = 90,
  dpi = 600,
  device = ragg::agg_png
)

message("Created ", output_path)
}
