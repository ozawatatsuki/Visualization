# Compare Figure 1 panel b and country contributions to the 2020 gap.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
  "ragg", "rnaturalearth", "sf"
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

scenario_codes <- c(
  Baseline = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)
scenario_labels <- setNames(names(scenario_codes), unname(scenario_codes))
scenario_colors <- c(Baseline = "#4D4D4D", `400C` = "#B2182B")
method_levels <- c("This study", "Conventional method")

variants <- tibble::tribble(
  ~variant, ~folder,
  "prev", "詳細修正後",
  "new", "ave修正後"
)

hunger_tool_path <- file.path("data", "HungerTool", "AIMHub.gdx")
country_tool_path <- file.path("data", "HungerTool", "country", "AIMHub.gdx")
mapping_path <- file.path("data", "HungerTool", "country", "FAOISO.map")

for (path in c(hunger_tool_path, country_tool_path, mapping_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

hunger_tool <- gamstransfer::Container$new()
hunger_tool$read(hunger_tool_path, symbols = "Risk_agg")
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
    value = .data[["value"]],
    method = factor("Conventional method", levels = method_levels)
  ) |>
  filter(year >= 2020, year <= 2100) |>
  arrange(scenario, year)

fao_iso <- read.delim(
  mapping_path,
  header = FALSE,
  sep = "\t",
  quote = "\"",
  col.names = c("iso3", "separator", "fao_id"),
  stringsAsFactors = FALSE,
  strip.white = TRUE
) |>
  transmute(
    iso3 = if_else(as.character(iso3) == "F351", "CHN", as.character(iso3)),
    fao_id = as.character(fao_id)
  )

country_tool <- gamstransfer::Container$new()
country_tool$read(country_tool_path, symbols = "Risk")
traditional_country <- country_tool["Risk"]$records |>
  tibble::as_tibble() |>
  transmute(
    method = as.character(.data[["U"]]),
    scenario_code = as.character(.data[["S"]]),
    fao_id = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["T"]])),
    hunger_tool = .data[["value"]]
  ) |>
  filter(
    method == "fao_based",
    scenario_code == scenario_codes[["Baseline"]],
    year == 2020L
  ) |>
  left_join(fao_iso, by = "fao_id") |>
  select(iso3, hunger_tool)

if (anyNA(traditional_country$iso3)) {
  stop("One or more HungerTool country records could not be mapped to ISO3.", call. = FALSE)
}

country_names <- rnaturalearth::ne_countries(
  scale = "medium",
  returnclass = "sf"
) |>
  sf::st_drop_geometry() |>
  transmute(
    iso3 = as.character(adm0_a3),
    country = as.character(name_long)
  ) |>
  filter(iso3 != "-99") |>
  distinct(iso3, .keep_all = TRUE)

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 8.5, colour = "black"),
  plot.tag.position = c(0, 1)
)

make_time_series_panel <- function(current_totals) {
  ribbon_data <- current_totals |>
    select(-method) |>
    pivot_wider(names_from = scenario, values_from = value) |>
    mutate(
      ymin = pmin(.data[["Baseline"]], .data[["400C"]]),
      ymax = pmax(.data[["Baseline"]], .data[["400C"]])
    ) |>
    arrange(year)

  panel_lines <- bind_rows(current_totals, traditional_totals)

  ggplot() +
    geom_ribbon(
      data = ribbon_data,
      aes(x = year, ymin = ymin, ymax = ymax),
      fill = scenario_colors[["400C"]],
      alpha = 0.09
    ) +
    geom_line(
      data = panel_lines,
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
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(accuracy = 1, big.mark = ","),
      limits = c(0, NA),
      expand = expansion(mult = c(0.03, 0.08))
    ) +
    labs(
      tag = "a",
      title = "Global population at risk of hunger",
      x = "Year",
      y = "Population at risk of hunger\n(million)",
      colour = NULL,
      linetype = NULL
    ) +
    coord_cartesian(clip = "off") +
    theme_interim_nature(base_size = 6.5, grid = "y", legend_position = "top") +
    panel_tag_theme +
    theme(
      plot.title = element_text(face = "plain", size = 7.0, hjust = 0.5),
      legend.box = "vertical",
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.margin = margin(0, 0, 0, 0),
      legend.spacing.y = grid::unit(-0.5, "mm"),
      legend.box.spacing = grid::unit(0.4, "mm"),
      legend.key.width = grid::unit(5.0, "mm"),
      axis.title = element_text(face = "plain"),
      axis.text.x = element_text(size = 5.6),
      plot.margin = margin(2, 3, 2, 3, "mm")
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
}

make_country_panel <- function(study_country) {
  country_gap <- full_join(
    traditional_country,
    study_country,
    by = "iso3"
  ) |>
    mutate(
      hunger_tool = coalesce(hunger_tool, 0),
      this_study = coalesce(this_study, 0),
      difference = hunger_tool - this_study,
      absolute_difference = abs(difference),
      direction = if_else(
        difference >= 0,
        "HungerTool higher",
        "This study higher"
      )
    ) |>
    left_join(country_names, by = "iso3") |>
    mutate(country = coalesce(country, iso3)) |>
    slice_max(absolute_difference, n = 15, with_ties = FALSE) |>
    arrange(absolute_difference) |>
    mutate(country = factor(country, levels = country))

  ggplot(country_gap, aes(x = difference, y = country, fill = direction)) +
    geom_vline(xintercept = 0, linewidth = 0.28, colour = "#333333") +
    geom_col(width = 0.70, colour = "white", linewidth = 0.10) +
    scale_fill_manual(
      values = c(
        `HungerTool higher` = "#B2182B",
        `This study higher` = "#2166AC"
      ),
      breaks = c("HungerTool higher", "This study higher"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty(n = 5),
      labels = scales::label_number(accuracy = 1, big.mark = ","),
      expand = expansion(mult = c(0.08, 0.06))
    ) +
    labs(
      tag = "b",
      title = "Largest country contributions to the 2020 gap",
      x = "HungerTool - this study (million)",
      y = NULL,
      fill = NULL
    ) +
    theme_interim_nature(base_size = 6.5, grid = "x", legend_position = "top") +
    panel_tag_theme +
    theme(
      plot.title = element_text(face = "plain", size = 7.0, hjust = 0.5),
      axis.title = element_text(face = "plain"),
      axis.text.y = element_text(size = 5.3),
      axis.text.x = element_text(size = 5.4),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      legend.direction = "horizontal",
      legend.justification = "left",
      legend.box.just = "left",
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      legend.key.width = grid::unit(4.5, "mm"),
      plot.margin = margin(2, 3, 2, 3, "mm")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

output_dir <- file.path("output", "others")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (index in seq_len(nrow(variants))) {
  variant <- variants$variant[[index]]
  fdm_path <- file.path(
    "data", "FDM", variants$folder[[index]], "Fooddem_PriceEffect.gdx"
  )
  if (!file.exists(fdm_path)) {
    stop("Input file not found: ", fdm_path, call. = FALSE)
  }

  fdm <- gamstransfer::Container$new()
  fdm$read(
    fdm_path,
    symbols = c("PoU_In_Pr_CR_Ragg_calc", "PoU_In_Pr_CR")
  )

  current_totals <- fdm["PoU_In_Pr_CR_Ragg_calc"]$records |>
    tibble::as_tibble() |>
    filter(
      as.character(.data[["Ref"]]) %in% unname(scenario_codes),
      as.character(.data[["R17"]]) == "WLD"
    ) |>
    transmute(
      scenario = factor(
        unname(scenario_labels[as.character(.data[["Ref"]])]),
        levels = names(scenario_codes)
      ),
      year = as.integer(as.character(.data[["Y"]])),
      value = .data[["value"]] / 1e6,
      method = factor("This study", levels = method_levels)
    ) |>
    filter(year >= 2020, year <= 2100) |>
    arrange(scenario, year)

  study_country <- fdm["PoU_In_Pr_CR"]$records |>
    tibble::as_tibble() |>
    transmute(
      scenario_code = as.character(.data[["Ref"]]),
      iso3 = as.character(.data[["R"]]),
      year = as.integer(as.character(.data[["Y"]])),
      this_study = .data[["value"]] / 1e6
    ) |>
    filter(
      scenario_code == scenario_codes[["Baseline"]],
      year == 2020L
    ) |>
    select(iso3, this_study)

  combined_figure <- (
    make_time_series_panel(current_totals) |
      make_country_panel(study_country)
  ) +
    plot_layout(widths = c(1.05, 1)) +
    plot_annotation(title = variant)

  output_path <- file.path(
    output_dir,
    paste0("fig1_panel_b_country_gap_", variant, ".png")
  )
  save_interim_plot(
    combined_figure,
    output_path,
    height_mm = 85,
    dpi = 600,
    device = ragg::agg_png
  )
  message("Created ", output_path)
}
