# Figure 1: Global hunger risk under the 400C mitigation scenario.

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
fdm_path <- file.path("data", "FDM", fdm_folder, "Fooddem_PriceEffect.gdx")
hunger_tool_path <- file.path("data", "HungerTool", "AIMHub.gdx")

for (path in c(fdm_path, hunger_tool_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

scenario_codes <- c(
  Baseline = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)
scenario_labels <- setNames(names(scenario_codes), unname(scenario_codes))

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
scenario_colors <- c(
  Baseline = "#4D4D4D",
  `400C` = "#B2182B"
)

fdm_symbols <- c(
  full = "PoU_In_Pr_CR_Ragg_calc",
  income_price = "PoU_In_Pr_Ragg_calc",
  income = "PoU_In_Ragg_calc"
)

fdm <- gamstransfer::Container$new()
fdm$read(fdm_path, symbols = unname(fdm_symbols))

read_fdm_symbol <- function(metric, symbol) {
  fdm[symbol]$records |>
    tibble::as_tibble() |>
    filter(
      as.character(.data[["Ref"]]) %in% unname(scenario_codes)
    ) |>
    transmute(
      scenario_code = as.character(.data[["Ref"]]),
      region = as.character(.data[["R17"]]),
      year = as.integer(as.character(.data[["Y"]])),
      metric = metric,
      value = .data[["value"]] / 1e6
    )
}

current_long <- dplyr::bind_rows(
  Map(read_fdm_symbol, names(fdm_symbols), unname(fdm_symbols))
)

current_wide <- current_long |>
  pivot_wider(names_from = metric, values_from = value) |>
  arrange(scenario_code, region, year)

expected_region_count <- 18L
expected_current_rows <- length(scenario_codes) * expected_region_count * 9
if (nrow(current_wide) != expected_current_rows || anyNA(current_wide)) {
  stop(
    "Unexpected records in the FDM data; expected ",
    expected_current_rows, " complete scenario-region-year rows.",
    call. = FALSE
  )
}

baseline_current <- current_wide |>
  filter(
    .data[["scenario_code"]] == scenario_codes[["Baseline"]],
    region == "WLD"
  ) |>
  select(-scenario_code, -region)

policy_current <- current_wide |>
  filter(
    .data[["scenario_code"]] == scenario_codes[["400C"]],
    region == "WLD"
  ) |>
  select(-scenario_code, -region)

effects <- policy_current |>
  inner_join(baseline_current, by = "year", suffix = c("_400c", "_baseline")) |>
  transmute(
    year,
    income = income_400c - income_baseline,
    price = (income_price_400c - income_price_baseline) - income,
    cross_price = (full_400c - full_baseline) -
      (income_price_400c - income_price_baseline),
    total = full_400c - full_baseline
  )

stack_error <- with(
  effects,
  max(abs(income + price + cross_price - total))
)
if (stack_error > 1e-8) {
  stop("Effect decomposition does not sum to the total effect.", call. = FALSE)
}

effects_long <- effects |>
  select(year, all_of(names(effect_labels))) |>
  pivot_longer(-year, names_to = "effect", values_to = "value") |>
  mutate(
    effect = factor(
      unname(effect_labels[effect]),
      levels = unname(effect_labels)
    )
  )

current_totals <- current_wide |>
  filter(region == "WLD") |>
  transmute(
    scenario = factor(
      unname(scenario_labels[scenario_code]),
      levels = names(scenario_codes)
    ),
    year,
    value = full
  )

current_region_totals <- current_wide |>
  transmute(
    scenario = factor(
      unname(scenario_labels[scenario_code]),
      levels = names(scenario_codes)
    ),
    region,
    year,
    value = full
  )

hunger_tool <- gamstransfer::Container$new()
hunger_tool$read(hunger_tool_path, symbols = c("Risk_agg", "Risk_org"))

traditional_totals <- hunger_tool["Risk_agg"]$records |>
  tibble::as_tibble() |>
  filter(
    as.character(.data[["U"]]) == "fao_based",
    as.character(.data[["R_agg"]]) == "WLD",
    as.character(.data[["S"]]) %in% unname(scenario_codes)
  ) |>
  transmute(
    scenario = factor(
      unname(scenario_labels[as.character(.data[["S"]])]),
      levels = names(scenario_codes)
    ),
    year = as.integer(as.character(.data[["T"]])),
    value = .data[["value"]]
  ) |>
  filter(year >= min(effects$year), year <= max(effects$year)) |>
  arrange(scenario, year)

traditional_region_totals <- hunger_tool["Risk_org"]$records |>
  tibble::as_tibble() |>
  filter(
    as.character(.data[["U"]]) == "fao_based",
    as.character(.data[["S"]]) %in% unname(scenario_codes)
  ) |>
  transmute(
    scenario = factor(
      unname(scenario_labels[as.character(.data[["S"]])]),
      levels = names(scenario_codes)
    ),
    region = if_else(
      as.character(.data[["R_org"]]) == "World",
      "WLD",
      paste0("R17", as.character(.data[["R_org"]]))
    ),
    year = as.integer(as.character(.data[["T"]])),
    value = .data[["value"]]
  ) |>
  filter(year >= min(effects$year), year <= max(effects$year)) |>
  arrange(scenario, region, year)

traditional_baseline <- traditional_totals |>
  filter(scenario == "Baseline") |>
  select(year, baseline = value)

traditional_400c <- traditional_totals |>
  filter(scenario == "400C") |>
  select(year, policy = value)

traditional_effect <- traditional_400c |>
  inner_join(traditional_baseline, by = "year") |>
  transmute(year, total = policy - baseline)

peak <- effects |>
  slice_max(total, n = 1, with_ties = FALSE)
peak_year <- peak$year[[1]]
peak_value <- peak$total[[1]]

comparison_years <- sort(unique(c(2030L, 2050L, peak_year, 2100L)))

comparison_data <- current_region_totals |>
  rename(this_study = value) |>
  inner_join(
    traditional_region_totals |> rename(conventional_method = value),
    by = c("scenario", "region", "year")
  ) |>
  filter(year %in% comparison_years) |>
  mutate(
    year = factor(
      year,
      levels = comparison_years,
      labels = as.character(comparison_years)
    ),
    geography = if_else(region == "WLD", "World", "R17 region")
  )

expected_comparison_rows <-
  expected_region_count * length(scenario_codes) * length(comparison_years)
if (nrow(comparison_data) != expected_comparison_rows || anyNA(comparison_data)) {
  stop(
    "Regional comparison is incomplete; expected ",
    expected_comparison_rows, " matched records.",
    call. = FALSE
  )
}

ribbon_data <- current_totals |>
  pivot_wider(names_from = scenario, values_from = value) |>
  mutate(
    ymin = pmin(.data[["Baseline"]], .data[["400C"]]),
    ymax = pmax(.data[["Baseline"]], .data[["400C"]])
  ) |>
  arrange(year)

line_end_a <- bind_rows(
  effects |>
    filter(year == max(year)) |>
    transmute(year, total, method = "This study"),
  traditional_effect |>
    filter(year == max(year)) |>
    transmute(year, total, method = "Conventional method")
)

method_levels <- c("This study", "Conventional method")
panel_b_lines <- bind_rows(
  current_totals |>
    mutate(method = factor("This study", levels = method_levels)),
  traditional_totals |>
    mutate(method = factor("Conventional method", levels = method_levels))
)

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 9, colour = "black"),
  plot.tag.position = c(0, 1)
)

panel_a <- ggplot() +
  geom_hline(yintercept = 0, linewidth = 0.25, colour = "black") +
  geom_col(
    data = effects_long,
    aes(x = year, y = value, fill = effect),
    width = 6.4,
    colour = "white",
    linewidth = 0.12,
    alpha = 1
  ) +
  geom_line(
    data = effects,
    aes(x = year, y = total),
    linewidth = 0.50,
    colour = "black",
    alpha = 0.82,
    lineend = "round"
  ) +
  geom_line(
    data = traditional_effect,
    aes(x = year, y = total),
    linewidth = 0.30,
    linetype = "22",
    colour = "#555555",
    alpha = 0.68,
    lineend = "round"
  ) +
  geom_text(
    data = line_end_a,
    aes(x = year + 2.5, y = total, label = method, colour = method),
    hjust = 0,
    size = 2.0,
    alpha = 0.94,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = effect_colors, drop = FALSE) +
  scale_colour_manual(
    values = c(`This study` = "black", `Conventional method` = "#3B3B3B")
  ) +
  scale_x_continuous(
    breaks = seq(2020, 2100, 20),
    limits = c(2016.5, 2127),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(100),
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    expand = expansion(mult = c(0.07, 0.14))
  ) +
  labs(
    tag = "a",
    x = "Year",
    y = "Additional population at risk of hunger (million)",
    fill = NULL
  ) +
  guides(
    fill = guide_legend(
      nrow = 1,
      byrow = TRUE,
      override.aes = list(colour = NA)
    )
  ) +
  coord_cartesian(clip = "off") +
  theme_interim_nature(base_size = 7.2, grid = "y", legend_position = "top") +
  panel_tag_theme +
  theme(
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(0.5, "mm"),
    axis.title.y = element_text(face = "plain"),
    plot.margin = margin(3, 2, 3, 4, "mm")
  )

panel_b <- ggplot() +
  geom_ribbon(
    data = ribbon_data,
    aes(x = year, ymin = ymin, ymax = ymax),
    fill = scenario_colors[["400C"]],
    alpha = 0.09
  ) +
  geom_line(
    data = panel_b_lines,
    aes(
      x = year,
      y = value,
      colour = scenario,
      linetype = method,
      linewidth = method,
      group = interaction(scenario, method)
    ),
    alpha = 0.94,
    lineend = "round"
  ) +
  scale_colour_manual(values = scenario_colors, drop = FALSE) +
  scale_linetype_manual(
    values = c(`This study` = "solid", `Conventional method` = "22"),
    drop = FALSE
  ) +
  scale_linewidth_manual(
    values = c(`This study` = 0.50, `Conventional method` = 0.34),
    guide = "none"
  ) +
  scale_x_continuous(
    breaks = seq(2020, 2100, 20),
    limits = c(2017, 2104),
    expand = expansion(mult = 0)
  ) +
  scale_y_continuous(
    breaks = scales::breaks_width(300),
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    limits = c(0, NA),
    expand = expansion(mult = c(0.03, 0.08))
  ) +
  labs(
    tag = "b",
    x = NULL,
    y = "Population at risk of hunger\n(million)",
    colour = NULL,
    linetype = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_interim_nature(base_size = 6.5, grid = "y", legend_position = "top") +
  panel_tag_theme +
  theme(
    legend.box = "vertical",
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.spacing.y = grid::unit(-0.5, "mm"),
    legend.box.spacing = grid::unit(0.4, "mm"),
    legend.key.width = grid::unit(5.0, "mm"),
    axis.title.y = element_text(face = "plain"),
    plot.margin = margin(3, 4, 1, 2, "mm"),
    axis.text.x = element_text(size = 5.6)
  ) +
  guides(
    colour = guide_legend(
      order = 1,
      nrow = 1,
      override.aes = list(linetype = "solid", linewidth = 0.50, alpha = 0.8)
    ),
    linetype = guide_legend(
      order = 2,
      nrow = 1,
      override.aes = list(colour = "#444444", linewidth = 0.42, alpha = 0.8)
    )
  )

year_shapes <- setNames(c(16, 17, 15, 18), levels(comparison_data$year))
regional_comparison_data <- comparison_data |>
  filter(region != "WLD")

panel_c <- ggplot() +
  geom_abline(
    slope = 1,
    intercept = 0,
    linewidth = 0.25,
    linetype = "22",
    colour = "#666666",
    alpha = 0.55
  ) +
  geom_point(
    data = regional_comparison_data,
    aes(
      x = conventional_method,
      y = this_study,
      colour = scenario,
      shape = year
    ),
    size = 1.15,
    stroke = 0.2,
    alpha = 0.42
  ) +
  scale_colour_manual(values = scenario_colors, drop = FALSE) +
  scale_shape_manual(values = year_shapes, drop = FALSE) +
  scale_x_continuous(
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.04))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_pretty(n = 4),
    labels = scales::label_number(accuracy = 1, big.mark = ","),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    tag = "c",
    x = "Conventional method\n(million people)",
    y = "This study\n(million people)",
    colour = NULL,
    shape = NULL
  ) +
  coord_cartesian(clip = "off") +
  theme_interim_nature(base_size = 6.1, grid = "both", legend_position = "top") +
  panel_tag_theme +
  theme(
    panel.grid.major = element_line(colour = "#E2E2E2", linewidth = 0.18),
    axis.title = element_text(size = 6.0, face = "plain", lineheight = 0.9),
    axis.text = element_text(size = 5.3),
    legend.direction = "horizontal",
    legend.justification = "left",
    legend.box.just = "left",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(0.3, "mm"),
    legend.key.width = grid::unit(3.6, "mm"),
    plot.margin = margin(1, 4, 3, 2, "mm")
  ) +
  guides(
    colour = "none",
    shape = guide_legend(
      nrow = 1,
      override.aes = list(colour = "#555555", size = 1.4, alpha = 0.7)
    )
  )

right_column <- (panel_b / panel_c) +
  plot_layout(heights = c(1.04, 0.96))

fig1 <- (panel_a | right_column) +
  plot_layout(widths = c(2, 1))

if (!isTRUE(getOption("fig1.build_only", FALSE))) {
output_dir <- "output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
existing_png <- list.files(
  output_dir,
  pattern = "^fig1_.*\\.png$",
  full.names = TRUE
)
if (length(existing_png) > 0) {
  file.remove(existing_png)
}

png_path <- file.path(
  output_dir,
  paste0("fig1_global_hunger_", fdm_variant, ".png")
)

save_interim_plot(
  fig1,
  png_path,
  height_mm = 85,
  dpi = 600,
  device = ragg::agg_png
)

world_comparison_summary <- comparison_data |>
  filter(region == "WLD") |>
  select(scenario, year, this_study, conventional_method) |>
  arrange(year, scenario)

message(
  sprintf(
    "Figure 1 created. Peak additional risk: %.1f million people in %d.",
    peak_value,
    peak_year
  )
)
print(world_comparison_summary)
}
