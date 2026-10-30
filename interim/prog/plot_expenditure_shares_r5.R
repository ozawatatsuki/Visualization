# Plot expenditure composition by decile for R5 regions and the world.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "tidyr", "ggplot2", "scales", "ragg"
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

input_path <- file.path("data", "PHI", "DecileResults_region_10.gdx")
output_dir <- file.path("output", "others")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path, call. = FALSE)
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

scenario_codes <- c(
  BaU = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)
target_years <- c(2020L, 2050L)

region_labels <- c(
  `R5OECD90+EU` = "OECD90 + EU",
  R5ASIA = "Asia",
  R5LAM = "Latin America",
  R5REF = "REs",
  R5MAF = "Middle East & Africa",
  WLD = "World"
)

item_labels <- c(
  "Food and nonalcoholic beverages" = "Food",
  "Alcoholic beverages, tobacco, and narcotics" = "Alcohol & tobacco",
  "Clothing and footwear" = "Clothing & footwear",
  Housing = "Housing",
  Water = "Water",
  Energy = "Energy",
  "Furnishings, household equipment and maintenance" = "Furnishings",
  Health = "Health",
  Transport = "Transport",
  Communication = "Communication",
  "Recreation and culture" = "Recreation",
  Education = "Education",
  "Restaurants and hotels" = "Restaurants & hotels",
  "Miscellaneous goods and services" = "Miscellaneous"
)

# Distinct print-safe colors, ordered to preserve the source category order.
item_colors <- c(
  Food = "#E3B341",
  "Alcohol & tobacco" = "#D95F59",
  "Clothing & footwear" = "#6BAED6",
  Housing = "#F39C55",
  Water = "#57B7B0",
  Energy = "#737373",
  Furnishings = "#B59BCB",
  Health = "#C7B85B",
  Transport = "#8FCB4A",
  Communication = "#3E78B2",
  Recreation = "#C96BAA",
  Education = "#5AA469",
  "Restaurants & hotels" = "#F29AAA",
  Miscellaneous = "#7D6B8F"
)

decile_levels <- c(as.character(1:10), "Ave")
region_levels <- unname(region_labels)
item_levels <- unname(item_labels)

container <- gamstransfer::Container$new()
container$read(input_path, symbols = "Exp_share")

expenditure_share <- container["Exp_share"]$records |>
  tibble::as_tibble() |>
  transmute(
    region_code = as.character(.data[["R"]]),
    scenario_code = as.character(.data[["Ref"]]),
    year = as.integer(as.character(.data[["Y"]])),
    item_code = as.character(.data[["I"]]),
    decile = as.character(.data[["DEC"]]),
    share = .data[["value"]]
  ) |>
  filter(
    region_code %in% names(region_labels),
    scenario_code %in% unname(scenario_codes),
    year %in% target_years,
    item_code %in% names(item_labels),
    decile %in% decile_levels
  ) |>
  mutate(
    scenario = factor(
      names(scenario_codes)[match(scenario_code, scenario_codes)],
      levels = names(scenario_codes)
    ),
    region = factor(
      unname(region_labels[region_code]),
      levels = region_levels
    ),
    item = factor(
      unname(item_labels[item_code]),
      levels = item_levels
    ),
    decile = factor(decile, levels = decile_levels)
  ) |>
  select(scenario, year, region, decile, item, share) |>
  complete(
    scenario,
    year,
    region,
    decile,
    item,
    fill = list(share = 0)
  ) |>
  group_by(scenario, year, region, decile) |>
  mutate(share = share / sum(share, na.rm = TRUE)) |>
  ungroup()

if (anyNA(expenditure_share$share) || any(!is.finite(expenditure_share$share))) {
  stop("Exp_share contains incomplete or invalid region-decile totals.", call. = FALSE)
}

share_totals <- expenditure_share |>
  summarise(total = sum(share), .by = c(scenario, year, region, decile))
if (any(abs(share_totals$total - 1) > 1e-8)) {
  stop("Normalized expenditure shares do not sum to one.", call. = FALSE)
}

make_expenditure_plot <- function(data, scenario_name, year_value) {
  plot_data <- data |>
    filter(scenario == scenario_name, year == year_value)

  ggplot(plot_data, aes(x = decile, y = share, fill = item)) +
    geom_col(width = 0.82, colour = "white", linewidth = 0.08) +
    geom_vline(
      xintercept = 10.5,
      linewidth = 0.28,
      linetype = "22",
      colour = "#5F6265"
    ) +
    facet_wrap(~region, ncol = 3) +
    scale_fill_manual(
      values = item_colors,
      breaks = item_levels,
      drop = FALSE
    ) +
    scale_y_continuous(
      breaks = seq(0, 1, by = 0.25),
      labels = scales::label_percent(accuracy = 1),
      expand = expansion(mult = c(0, 0.015))
    ) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste0(scenario_name, " | ", year_value),
      x = "Income decile",
      y = "Expenditure share",
      fill = NULL
    ) +
    theme_interim_nature(
      base_size = 7.3,
      grid = "y",
      legend_position = "bottom"
    ) +
    theme(
      axis.title = element_text(face = "plain"),
      axis.text.x = element_text(size = 6.2),
      axis.text.y = element_text(size = 6.4),
      panel.grid.major.y = element_line(colour = "#D6D8DA", linewidth = 0.24),
      strip.background = element_rect(fill = "#F1F2F3", colour = NA),
      strip.text = element_text(size = 7.5, face = "bold", margin = margin(2.2, 0, 2.2, 0, "mm")),
      panel.spacing.x = grid::unit(3.2, "mm"),
      panel.spacing.y = grid::unit(4.0, "mm"),
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.text = element_text(size = 6.2),
      legend.key.height = grid::unit(2.8, "mm"),
      legend.key.width = grid::unit(3.4, "mm"),
      legend.spacing.x = grid::unit(1.2, "mm"),
      legend.margin = margin(t = 2, unit = "mm"),
      plot.title = element_text(size = 9, face = "bold", margin = margin(b = 3)),
      plot.margin = margin(2.5, 3, 2, 2.5, "mm")
    ) +
    guides(
      fill = guide_legend(
        nrow = 3,
        byrow = TRUE,
        override.aes = list(colour = NA)
      )
    )
}

output_paths <- character()
for (scenario_name in names(scenario_codes)) {
  for (year_value in target_years) {
    plot <- make_expenditure_plot(
      expenditure_share,
      scenario_name,
      year_value
    )
    output_path <- file.path(
      output_dir,
      sprintf(
        "expenditure_share_by_decile_%s_%d.png",
        tolower(scenario_name),
        year_value
      )
    )
    save_interim_plot(
      plot,
      output_path,
      height_mm = 124,
      dpi = 450,
      device = ragg::agg_png
    )
    output_paths <- c(output_paths, output_path)
  }
}

message("Created:\n", paste(output_paths, collapse = "\n"))
