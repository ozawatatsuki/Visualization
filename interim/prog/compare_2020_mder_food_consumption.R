# Compare country-level MDER and mean food consumption in 2020.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "ggplot2", "ggrepel", "patchwork", "scales",
  "ragg"
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
library(ggplot2)
library(ggrepel)
library(patchwork)

scenario_code <- "SSP2_BaU_NoCC_No"
target_year <- 2020L

fdm_path <- file.path(
  "data", "FDM", "MDER更新前", "Fooddem_PriceEffect.gdx"
)
hunger_tool_path <- file.path("data", "HungerTool", "country", "AIMHub.gdx")
hunger_input_path <- file.path("data", "HungerTool", "input_parameters.gdx")
mapping_path <- file.path("data", "HungerTool", "country", "FAOISO.map")
output_dir <- file.path("output", "others")
output_path <- file.path(
  output_dir,
  "fdm_hungertool_mder_food_consumption_2020.png"
)

for (path in c(fdm_path, hunger_tool_path, hunger_input_path, mapping_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

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
  ) |>
  distinct(fao_id, .keep_all = TRUE)

hunger_tool <- gamstransfer::Container$new()
hunger_tool$read(
  hunger_tool_path,
  symbols = "PCAL"
)

hunger_input <- gamstransfer::Container$new()
hunger_input$read(hunger_input_path, symbols = "MDER_fao")

read_hunger_parameter <- function(symbol, value_name) {
  data <- hunger_tool[symbol]$records |>
    tibble::as_tibble()

  data |>
    filter(
      as.character(.data[["U"]]) == "fao_based",
      as.character(.data[["S"]]) == scenario_code,
      as.integer(as.character(.data[["T"]])) == target_year
    ) |>
    transmute(
      fao_id = as.character(.data[["R"]]),
      !!value_name := .data[["value"]]
    )
}

hunger_mder <- hunger_input["MDER_fao"]$records |>
  tibble::as_tibble() |>
  filter(as.integer(as.character(.data[["T"]])) == target_year) |>
  transmute(
    fao_id = as.character(.data[["R"]]),
    mder_hungertool = .data[["value"]]
  )

hunger_country <- hunger_mder |>
  inner_join(
    read_hunger_parameter("PCAL", "food_hungertool"),
    by = "fao_id"
  ) |>
  inner_join(fao_iso, by = "fao_id") |>
  select(iso3, mder_hungertool, food_hungertool)

fdm <- gamstransfer::Container$new()
fdm$read(fdm_path, symbols = c("MDER", "FoodDem_peffect_CR"))

fdm_mder <- fdm["MDER"]$records |>
  tibble::as_tibble() |>
  filter(
    as.character(.data[["Ref"]]) == scenario_code,
    as.integer(as.character(.data[["Y"]])) == target_year
  ) |>
  transmute(
    iso3 = as.character(.data[["R"]]),
    mder_fdm = .data[["value"]]
  )

fdm_food <- fdm["FoodDem_peffect_CR"]$records |>
  tibble::as_tibble() |>
  filter(
    as.character(.data[["Ref"]]) == scenario_code,
    as.integer(as.character(.data[["Y"]])) == target_year,
    as.character(.data[["var"]]) == "total",
    as.character(.data[["seg"]]) == "ALL"
  ) |>
  transmute(
    iso3 = as.character(.data[["R"]]),
    food_fdm = .data[["value"]]
  )

comparison <- fdm_mder |>
  inner_join(fdm_food, by = "iso3") |>
  inner_join(hunger_country, by = "iso3") |>
  mutate(
    highlight = case_when(
      iso3 == "IND" ~ "IND",
      iso3 == "CHN" ~ "CHN",
      TRUE ~ "Other countries"
    ),
    highlight = factor(
      highlight,
      levels = c("Other countries", "IND", "CHN")
    )
  )

if (!all(c("IND", "CHN") %in% comparison$iso3)) {
  stop("IND or CHN is missing after joining the country data.", call. = FALSE)
}
if (nrow(comparison) < 100) {
  stop("Too few matched countries: ", nrow(comparison), call. = FALSE)
}

point_colors <- c(
  `Other countries` = "#6F7478",
  IND = "#C43C39",
  CHN = "#2878B5"
)

comparison_long <- bind_rows(
  comparison |>
    transmute(
      iso3,
      highlight,
      metric = "MDER",
      hunger_tool = mder_hungertool,
      fdm = mder_fdm
    ),
  comparison |>
    transmute(
      iso3,
      highlight,
      metric = "Mean food consumption",
      hunger_tool = food_hungertool,
      fdm = food_fdm
    )
)

make_scatter_panel <- function(data, title, axis_breaks) {
  plot_range <- range(c(data$hunger_tool, data$fdm), na.rm = TRUE)
  padding <- diff(plot_range) * 0.055
  limits <- c(plot_range[1] - padding, plot_range[2] + padding)
  metrics <- data |>
    summarise(
      mean_difference = mean(fdm - hunger_tool),
      rmse = sqrt(mean((fdm - hunger_tool)^2)),
      .groups = "drop"
    )
  metric_label <- sprintf(
    "Mean difference: %+.0f\nRMSE: %.0f",
    metrics$mean_difference,
    metrics$rmse
  )

  ggplot(data, aes(x = hunger_tool, y = fdm)) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linewidth = 0.45,
      colour = "#202020",
      linetype = "22"
    ) +
    geom_point(
      data = filter(data, highlight == "Other countries"),
      colour = point_colors[["Other countries"]],
      size = 1.65,
      alpha = 0.62
    ) +
    geom_point(
      data = filter(data, highlight != "Other countries"),
      aes(colour = highlight),
      size = 3.4,
      alpha = 1,
      stroke = 0
    ) +
    geom_text_repel(
      data = filter(data, highlight != "Other countries"),
      aes(label = iso3, colour = highlight),
      size = 2.8,
      family = "Arial",
      fontface = "bold",
      box.padding = 0.35,
      point.padding = 0.3,
      min.segment.length = 0,
      segment.size = 0.32,
      segment.colour = "#454545",
      seed = 42,
      show.legend = FALSE
    ) +
    annotate(
      "text",
      x = limits[1] + diff(limits) * 0.035,
      y = limits[2] - diff(limits) * 0.04,
      label = metric_label,
      hjust = 0,
      vjust = 1,
      size = 2.35,
      family = "Arial",
      colour = "#353535",
      lineheight = 1.08
    ) +
    scale_colour_manual(values = point_colors, drop = FALSE) +
    scale_x_continuous(
      limits = limits,
      breaks = axis_breaks,
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = 0)
    ) +
    scale_y_continuous(
      limits = limits,
      breaks = axis_breaks,
      labels = scales::label_number(big.mark = ",", accuracy = 1),
      expand = expansion(mult = 0)
    ) +
    coord_equal(clip = "off") +
    labs(
      title = title,
      x = "HungerTool (kcal person⁻¹ day⁻¹)",
      y = "FDM (kcal person⁻¹ day⁻¹)"
    ) +
    theme_interim_nature(
      base_size = 8,
      grid = "both",
      legend_position = "none"
    ) +
    theme(
      axis.title = element_text(face = "plain"),
      panel.grid.major = element_line(colour = "#E1E3E5", linewidth = 0.25),
      plot.title = element_text(size = 9, margin = margin(b = 3)),
      plot.margin = margin(2.5, 4.5, 2.5, 3, "mm")
    )
}

mder_data <- comparison_long |>
  filter(metric == "MDER")
food_data <- comparison_long |>
  filter(metric == "Mean food consumption")

panel_mder <- make_scatter_panel(
  mder_data,
  "a   MDER",
  axis_breaks = seq(1600, 2100, by = 100)
)
panel_food <- make_scatter_panel(
  food_data,
  "b   Mean food consumption",
  axis_breaks = seq(1500, 4500, by = 500)
)

figure <- panel_mder + panel_food +
  plot_layout(widths = c(1, 1))

save_interim_plot(
  figure,
  output_path,
  height_mm = 82,
  dpi = 450,
  device = ragg::agg_png
)

message("Matched countries: ", nrow(comparison))
message(
  "Highlighted values:\n",
  paste(
    capture.output(
      print(
        comparison |>
          filter(iso3 %in% c("IND", "CHN")) |>
          select(
            iso3,
            mder_hungertool,
            mder_fdm,
            food_hungertool,
            food_fdm
          )
      )
    ),
    collapse = "\n"
  )
)
message("Created ", output_path)
