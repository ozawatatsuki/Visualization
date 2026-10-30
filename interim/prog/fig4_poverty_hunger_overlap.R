# Figure 4: Overlap between poverty and hunger risk in 2050.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
  "ragg", "sf", "rnaturalearth", "rnaturalearthdata", "ggtext"
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
library(sf)

analysis_path <- file.path("data", "PHI", "AnalysisExpenditure.gdx")
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
food_path <- file.path("data", "FDM", fdm_folder, "Fooddem_PriceEffect.gdx")
r5_map_path <- file.path("data", "R17toR5.map")

for (path in c(analysis_path, food_path, r5_map_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

target_year <- 2050L
poverty_threshold <- 3.00 * 365
scenario_codes <- c(
  Baseline = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)

category_levels <- c(
  "Neither poverty nor hunger",
  "Poverty only",
  "Hunger only",
  "Poverty and hunger"
)
category_colors <- c(
  `Neither poverty nor hunger` = "#C7D6D9",
  `Poverty only` = "#E6A23C",
  `Hunger only` = "#4A90B8",
  `Poverty and hunger` = "#C34A5F"
)

r5_codes <- c("R5OECD90+EU", "R5ASIA", "R5LAM", "R5REF", "R5MAF")
r5_labels <- c(
  `R5OECD90+EU` = "OECD90 + EU",
  R5ASIA = "Asia",
  R5LAM = "Latin America",
  R5REF = "Reforming economies",
  R5MAF = "Middle East & Africa"
)
r5_colors <- c(
  `OECD90 + EU` = "#3B4992",
  Asia = "#A56B00",
  `Latin America` = "#008B6E",
  `Reforming economies` = "#8E5EA2",
  `Middle East & Africa` = "#8C564B"
)
composition_strip_labels <- c(
  World = "<b>World</b>",
  `OECD90 + EU` = paste0(
    "<span style='color:", r5_colors[["OECD90 + EU"]],
    "'><b>OECD90 + EU</b></span>"
  ),
  Asia = paste0(
    "<span style='color:", r5_colors[["Asia"]],
    "'><b>Asia</b></span>"
  ),
  `Latin America` = paste0(
    "<span style='color:", r5_colors[["Latin America"]],
    "'><b>Latin America</b></span>"
  ),
  REF = paste0(
    "<span style='color:", r5_colors[["Reforming economies"]],
    "'><b>REF</b></span>"
  ),
  `Middle East &\nAfrica` = paste0(
    "<span style='color:", r5_colors[["Middle East & Africa"]],
    "'><b>Middle East &amp;<br>Africa</b></span>"
  )
)
r5_panel_labels <- c(
  WLD = "World",
  `R5OECD90+EU` = "OECD90 + EU",
  R5ASIA = "Asia",
  R5LAM = "Latin America",
  R5REF = "REF",
  R5MAF = "Middle East &\nAfrica"
)

region_members <- c(
  R17BRA = "BRA",
  R17CAN = "CAN",
  R17CHN = "CHN HKG",
  R17CIS = "ARM AZE BLR GEO KAZ KGZ MDA RUS TJK TKM UKR UZB",
  R17IND = "IND",
  R17JPN = "JPN",
  R17TUR = "TUR",
  R17USA = "USA",
  R17XAF = "AGO BEN BWA BFA BDI CPV CMR CAF TCD COM COD COG CIV DJI GNQ ERI SWZ ETH GAB GMB GHA GIN GNB KEN LSO LBR MDG MWI MLI MRT MUS MOZ NAM NER NGA RWA STP SEN SYC SLE SOM ZAF SSD SDN TZA TGO UGA ZMB ZWE",
  R17XE25 = "AUT BEL CYP CZE DNK EST FIN FRA DEU GRC HUN IRL ITA LVA LTU LUX MLT NLD POL PRT SVK SVN ESP SWE GBR",
  R17XER = "ALB AND BIH BGR CHI HRV FRO GIB ISL IMN XKX LIE MCO MNE MKD NOR ROU SMR SRB CHE",
  R17XLM = "ATG ARG ABW BHS BRB BLZ BMU BOL VGB CYM CHL COL CRI CUB CUW DMA DOM ECU SLV GRL GRD GTM GUY HTI HND JAM MEX NIC PAN PRY PER PRI SXM KNA LCA MAF VCT SUR TTO TCA URY VEN VIR ANG MON",
  R17XME = "BHR IRN IRQ ISR JOR KWT LBN OMN QAT SAU SYR ARE PSE YEM PLS",
  R17XNF = "DZA EGY LBY MAR TUN",
  R17XOC = "AUS NZL",
  R17XSA = "AFG ASM BGD BTN BRN FJI PYF GUM KIR MAC MDV MHL FSM NRU NPL NCL MNP PAK PLW PNG WSM SLB LKA TON TUV VUT",
  R17XSE = "KHM IDN PRK KOR LAO MYS MNG MMR PHL SGP THA TLS VNM"
)
country_to_r17 <- stack(strsplit(region_members, " ", fixed = TRUE)) |>
  transmute(
    country_code = as.character(values),
    r17 = as.character(ind)
  )
r17_to_r5 <- read.delim(
  r5_map_path,
  header = FALSE,
  sep = "\t",
  quote = "\"",
  col.names = c("r17", "separator", "r5"),
  stringsAsFactors = FALSE,
  strip.white = TRUE
) |>
  select(r17, r5)
country_to_r5 <- country_to_r17 |>
  inner_join(r17_to_r5, by = "r17")

analysis <- gamstransfer::Container$new()
analysis$read(analysis_path, symbols = "EV")

food <- gamstransfer::Container$new()
food$read(
  food_path,
  symbols = c("BudgetInitial", "FoodDem_peffect_CR", "Population_seg", "MDER")
)

budget <- food["BudgetInitial"]$records |>
  tibble::as_tibble() |>
  transmute(
    segment = as.character(.data[["seg2"]]),
    initial_budget = .data[["value"]]
  )

equivalent_variation <- analysis["EV"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    segment = as.character(.data[["Seg"]]),
    year = as.integer(as.character(.data[["Y"]])),
    ev = .data[["value"]]
  ) |>
  filter(
    scenario_code == scenario_codes[["400C"]],
    year == target_year
  )

food_demand <- food["FoodDem_peffect_CR"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    segment = as.character(.data[["seg"]]),
    variable = as.character(.data[["var"]]),
    food_demand = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year == target_year,
    segment != "ALL",
    variable == "total"
  ) |>
  select(-variable)

segment_population <- food["Population_seg"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    segment = as.character(.data[["seg2"]]),
    segment_population = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year == target_year,
    segment != "ALL",
    is.finite(segment_population),
    segment_population > 0
  )

mder <- food["MDER"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    mder = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year == target_year
  )

segment_status <- food_demand |>
  inner_join(
    segment_population,
    by = c("scenario_code", "country_code", "year", "segment")
  ) |>
  inner_join(budget, by = "segment") |>
  inner_join(mder, by = c("scenario_code", "country_code", "year")) |>
  left_join(
    equivalent_variation,
    by = c("scenario_code", "country_code", "year", "segment")
  ) |>
  mutate(
    ev = case_when(
      scenario_code == scenario_codes[["Baseline"]] ~ 0,
      TRUE ~ ev
    )
  )

missing_policy_ev <- segment_status |>
  filter(scenario_code == scenario_codes[["400C"]], is.na(ev))
if (nrow(missing_policy_ev) > 0) {
  stop("EV is missing for one or more 400C country-segment records.", call. = FALSE)
}
if (anyNA(segment_status) || any(!is.finite(segment_status$food_demand))) {
  stop("Country-segment classification data are incomplete.", call. = FALSE)
}

segment_status <- segment_status |>
  mutate(
    real_expenditure = initial_budget * (1 + ev),
    poverty = real_expenditure <= poverty_threshold,
    hunger = food_demand <= mder,
    category = case_when(
      poverty & hunger ~ "Poverty and hunger",
      poverty ~ "Poverty only",
      hunger ~ "Hunger only",
      TRUE ~ "Neither poverty nor hunger"
    ),
    category = factor(category, levels = category_levels)
  )

country_summary <- segment_status |>
  group_by(scenario_code, country_code, year) |>
  summarise(
    population = sum(segment_population),
    poverty_population = sum(segment_population[poverty]),
    hunger_population = sum(segment_population[hunger]),
    overlap_population = sum(segment_population[poverty & hunger]),
    .groups = "drop"
  ) |>
  mutate(
    poverty_share = 100 * poverty_population / population,
    hunger_share = 100 * hunger_population / population,
    overlap_share = 100 * overlap_population / population,
    hunger_only_share = 100 *
      (hunger_population - overlap_population) / population,
    overlap_among_poor_share = if_else(
      poverty_population > 0,
      100 * overlap_population / poverty_population,
      NA_real_
    ),
    overlap_among_hungry_share = if_else(
      hunger_population > 0,
      100 * overlap_population / hunger_population,
      NA_real_
    )
  )

country_category <- segment_status |>
  group_by(scenario_code, country_code, year, category) |>
  summarise(population = sum(segment_population), .groups = "drop") |>
  complete(
    scenario_code,
    country_code,
    year,
    category = factor(category_levels, levels = category_levels),
    fill = list(population = 0)
  )

regional_category_r5 <- country_category |>
  inner_join(country_to_r5, by = "country_code") |>
  group_by(scenario_code, r5, category) |>
  summarise(population = sum(population), .groups = "drop") |>
  group_by(scenario_code, r5) |>
  mutate(share = 100 * population / sum(population)) |>
  ungroup()

world_category <- country_category |>
  group_by(scenario_code, category) |>
  summarise(population = sum(population), .groups = "drop") |>
  group_by(scenario_code) |>
  mutate(
    r5 = "WLD",
    share = 100 * population / sum(population)
  ) |>
  ungroup()

regional_category_all <- bind_rows(world_category, regional_category_r5) |>
  mutate(
    scenario = factor(
      scenario_code,
      levels = unname(scenario_codes),
      labels = names(scenario_codes)
    ),
    region = factor(
      unname(r5_panel_labels[r5]),
      levels = unname(r5_panel_labels[c("WLD", r5_codes)])
    ),
    category = factor(category, levels = category_levels)
  )

regional_check <- regional_category_all |>
  group_by(scenario, region) |>
  summarise(total_share = sum(share), .groups = "drop")
if (
  nrow(regional_check) != length(scenario_codes) * (length(r5_codes) + 1L) ||
    max(abs(regional_check$total_share - 100)) > 1e-8
) {
  stop("Regional category shares do not sum to 100%.", call. = FALSE)
}

regional_category <- regional_category_all |>
  filter(category != "Neither poverty nor hunger") |>
  droplevels()

population_status_country <- country_summary |>
  transmute(
    Ref = scenario_code,
    Region = country_code,
    Y = as.character(year),
    HungerRiskPopulation = hunger_population,
    PovertyPopulation = poverty_population,
    TotalPopulation = population
  )

population_status_r5 <- country_summary |>
  inner_join(country_to_r5, by = "country_code") |>
  group_by(scenario_code, r5, year) |>
  summarise(
    HungerRiskPopulation = sum(hunger_population),
    PovertyPopulation = sum(poverty_population),
    TotalPopulation = sum(population),
    .groups = "drop"
  ) |>
  transmute(
    Ref = scenario_code,
    Region = r5,
    Y = as.character(year),
    HungerRiskPopulation,
    PovertyPopulation,
    TotalPopulation
  )

population_status_world <- country_summary |>
  group_by(scenario_code, year) |>
  summarise(
    HungerRiskPopulation = sum(hunger_population),
    PovertyPopulation = sum(poverty_population),
    TotalPopulation = sum(population),
    .groups = "drop"
  ) |>
  transmute(
    Ref = scenario_code,
    Region = "WLD",
    Y = as.character(year),
    HungerRiskPopulation,
    PovertyPopulation,
    TotalPopulation
  )

population_status <- bind_rows(
  population_status_country,
  population_status_r5,
  population_status_world
) |>
  pivot_longer(
    cols = c(HungerRiskPopulation, PovertyPopulation, TotalPopulation),
    names_to = "Status",
    values_to = "value"
  )

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica") |>
  mutate(country_code = as.character(adm0_a3)) |>
  left_join(country_to_r5, by = "country_code") |>
  mutate(
    r5_label = factor(
      unname(r5_labels[r5]),
      levels = unname(r5_labels)
    )
  ) |>
  st_make_valid() |>
  st_wrap_dateline(
    options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"),
    quiet = TRUE
  ) |>
  st_transform("+proj=robin")

map_values <- country_summary |>
  filter(scenario_code == scenario_codes[["400C"]])

map_upper <- function(values) {
  upper <- as.numeric(stats::quantile(values, 0.98, na.rm = TRUE))
  min(100, max(10, ceiling(upper / 10) * 10))
}

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 8.5, colour = "black"),
  plot.tag.position = c(0, 1)
)

make_map_panel <- function(
    metric,
    title,
    tag,
    colours,
    legend_title = "Population share (%)",
    fill_limits = NULL,
    fill_values = NULL,
    fill_breaks = NULL) {
  if (is.null(fill_limits)) {
    limit <- map_upper(map_values[[metric]])
    fill_limits <- c(0, limit)
    fill_values <- limit * c(0, 0.05, 0.20, 0.50, 1)
    fill_breaks <- pretty(fill_limits, n = 4)
  }
  map_data <- world |>
    left_join(
      select(map_values, country_code, value = all_of(metric)),
      by = "country_code"
    )

  ggplot(map_data) +
    geom_sf(
      aes(fill = value),
      colour = "#686868",
      linewidth = 0.07,
      linejoin = "round"
    ) +
    geom_sf(
      aes(colour = r5_label),
      fill = NA,
      linewidth = 0.17,
      alpha = 0.92,
      key_glyph = draw_key_path
    ) +
    scale_fill_gradientn(
      colours = colours,
      values = scales::rescale(fill_values, from = fill_limits),
      limits = fill_limits,
      breaks = fill_breaks,
      labels = scales::label_number(accuracy = 1),
      oob = scales::squish,
      na.value = "#E3E5E6",
      name = legend_title
    ) +
    scale_colour_manual(
      values = r5_colors,
      breaks = unname(r5_labels),
      drop = FALSE,
      name = "R5 region outlines"
    ) +
    coord_sf(datum = NA, expand = FALSE, clip = "off") +
    labs(tag = tag, title = title) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        direction = "horizontal",
        barwidth = grid::unit(46, "mm"),
        barheight = grid::unit(2.3, "mm"),
        ticks.colour = "#333333",
        frame.colour = "#555555"
      ),
      colour = "none"
    ) +
    theme_void(base_size = 6.6, base_family = "Arial") +
    panel_tag_theme +
    theme(
      plot.title = element_text(face = "plain", size = 7.0, hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(face = "plain", size = 6.0),
      legend.text = element_text(size = 5.4),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.5, "mm"),
      plot.margin = margin(0.5, 1, 0, 1, "mm")
    )
}

make_composition_panel <- function(tag = "c") {
  ggplot(
    regional_category,
    aes(x = scenario, y = share, fill = category)
  ) +
    geom_col(
      width = 0.64,
      colour = "white",
      linewidth = 0.18,
      position = position_stack(reverse = TRUE)
    ) +
    facet_wrap(
      vars(region),
      nrow = 1,
      scales = "free_y",
      axes = "all_y",
      axis.labels = "all_y",
      labeller = as_labeller(composition_strip_labels)
    ) +
    scale_fill_manual(values = category_colors, drop = FALSE) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 4),
      labels = scales::label_number(suffix = "%", accuracy = 0.1),
      expand = expansion(mult = c(0, 0.06))
    ) +
    labs(
      tag = tag,
      x = NULL,
      y = "Share of population",
      fill = NULL
    ) +
    theme_interim_nature(
      base_size = 6.5,
      grid = "y",
      legend_position = "bottom"
    ) +
    panel_tag_theme +
    theme(
      axis.title = element_text(face = "plain", size = 6.2),
      axis.text.x = element_text(size = 5.7),
      axis.text.y = element_text(size = 5.4),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      panel.spacing = grid::unit(3.0, "mm"),
      strip.text = ggtext::element_markdown(size = 6.2, lineheight = 0.95),
      legend.text = element_text(size = 5.7),
      legend.key.width = grid::unit(4.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.5, "mm"),
      plot.margin = margin(1, 2, 1.5, 3, "mm")
    ) +
    guides(fill = guide_legend(nrow = 1, byrow = TRUE))
}

poverty_map <- make_map_panel(
  metric = "poverty_share",
  title = "Population in poverty",
  tag = "a",
  colours = c("#FFFFFF", "#FFF0C7", "#F4B860", "#D67538", "#8F2D2D")
)
overlap_map <- make_map_panel(
  metric = "overlap_among_hungry_share",
  title = "Poverty among population at risk of hunger",
  tag = "b",
  colours = c("#2166AC", "#F7F7F7", "#B2182B"),
  legend_title = "Share among population at risk of hunger (%)",
  fill_limits = c(0, 100),
  fill_values = c(0, 50, 100),
  fill_breaks = seq(0, 100, by = 25)
)
overlap_total_map <- make_map_panel(
  metric = "overlap_share",
  title = "Population in poverty and at risk of hunger",
  tag = "c",
  colours = c("#FFFFFF", "#FEE5D9", "#FCAE91", "#FB6A4A", "#CB181D"),
  legend_title = "Population share (%)"
)
hunger_only_total_map <- make_map_panel(
  metric = "hunger_only_share",
  title = "Population at risk of hunger without poverty",
  tag = "d",
  colours = c("#FFFFFF", "#FFF9C4", "#FFF176", "#FDD835", "#E6AB02"),
  legend_title = "Population share (%)"
)

outline_legend_data <- tidyr::crossing(
  region = factor(unname(r5_labels), levels = unname(r5_labels)),
  x = 1:2
) |>
  mutate(y = 1)

outline_legend <- ggplot(
  outline_legend_data,
  aes(x = x, y = y, colour = region, group = region)
) +
  geom_line(linewidth = 0.8, alpha = 0) +
  scale_colour_manual(values = r5_colors, drop = FALSE) +
  guides(
    colour = guide_legend(
      title = "R5 region outlines",
      title.position = "top",
      title.hjust = 0.5,
      nrow = 1,
      byrow = TRUE,
      override.aes = list(linewidth = 1.2, alpha = 1)
    )
  ) +
  theme_void(base_size = 6.2, base_family = "Arial") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "plain", size = 5.8),
    legend.text = element_text(size = 5.3),
    legend.key.width = grid::unit(9, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    plot.margin = margin(0, 0, 0, 0)
  )

make_figure <- function(panel_b) {
  map_row <- (poverty_map | panel_b) +
    plot_layout(widths = c(1, 1))

  (map_row / outline_legend / make_composition_panel()) +
    plot_layout(heights = c(1.62, 0.16, 1))
}

make_extended_figure <- function(panel_b) {
  top_map_row <- (poverty_map | panel_b) +
    plot_layout(widths = c(1, 1))
  middle_map_row <- (overlap_total_map | hunger_only_total_map) +
    plot_layout(widths = c(1, 1))

  (top_map_row / plot_spacer() / middle_map_row / outline_legend /
    make_composition_panel(tag = "e")) +
    plot_layout(heights = c(1.52, 0.10, 1.52, 0.16, 1))
}

figure <- make_figure(overlap_map)
extended_figure <- make_extended_figure(overlap_map)

if (!isTRUE(getOption("fig4.build_only", FALSE))) {
output_dir <- "output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
existing_png <- list.files(
  output_dir,
  pattern = "^fig4_.*\\.png$",
  full.names = TRUE
)
if (length(existing_png) > 0) {
  file.remove(existing_png)
}
output_path <- file.path(
  output_dir,
  paste0("fig4_poverty_hunger_overlap_2050_", fdm_variant, ".png")
)
if (file.exists(output_path)) {
  file.remove(output_path)
}

save_interim_plot(
  figure,
  output_path,
  height_mm = 130,
  dpi = 600,
  device = ragg::agg_png
)

extended_output_path <- file.path(
  output_dir,
  paste0(
    "fig4_poverty_hunger_overlap_extended_maps_2050_",
    fdm_variant,
    ".png"
  )
)
save_interim_plot(
  extended_figure,
  extended_output_path,
  height_mm = 170,
  dpi = 600,
  device = ragg::agg_png
)

segment_export <- segment_status |>
  transmute(
    Ref = scenario_code,
    R = country_code,
    Y = as.character(year),
    seg = segment,
    FoodDemand = food_demand,
    RealExpenditure = real_expenditure,
    SegmentPopulation = segment_population,
    PovertyThreshold = poverty_threshold,
    MDERThreshold = mder
  )

data_output_dir <- file.path("output", "data")
dir.create(data_output_dir, recursive = TRUE, showWarnings = FALSE)
gdx_path <- file.path(
  data_output_dir,
  paste0("fig4_poverty_hunger_data_2050_", fdm_variant, ".gdx")
)
if (file.exists(gdx_path)) {
  file.remove(gdx_path)
}

fig4_data <- gamstransfer::Container$new()
fig4_data$addSet("Ref", records = unique(segment_export$Ref))
fig4_data$addSet("R", records = unique(segment_export$R))
fig4_data$addSet("Y", records = unique(segment_export$Y))
fig4_data$addSet("seg", records = unique(segment_export$seg))
fig4_data$addSet("Region", records = unique(population_status$Region))
fig4_data$addSet("Status", records = unique(population_status$Status))

add_segment_parameter <- function(name, description) {
  records <- segment_export |>
    transmute(
      Ref,
      R,
      Y,
      seg,
      value = .data[[name]]
    )
  fig4_data$addParameter(
    name,
    domain = c("Ref", "R", "Y", "seg"),
    records = records,
    description = description
  )
}

add_segment_parameter(
  "FoodDemand",
  "Food demand by country and income segment"
)
add_segment_parameter(
  "RealExpenditure",
  "Initial budget multiplied by one plus equivalent variation"
)
add_segment_parameter(
  "SegmentPopulation",
  "Population by country and income segment"
)
add_segment_parameter(
  "PovertyThreshold",
  "Annual real expenditure threshold: 3.00 multiplied by 365"
)
add_segment_parameter(
  "MDERThreshold",
  "Minimum dietary energy requirement used as hunger-risk threshold"
)
fig4_data$addParameter(
  "PopulationStatus",
  domain = c("Ref", "Region", "Y", "Status"),
  records = population_status,
  description = paste(
    "Hunger-risk, poverty, and total populations for countries,",
    "R5 regions, and the world"
  )
)
fig4_data$write(gdx_path)

message("Created ", output_path)
message("Created ", extended_output_path)
message("Created ", gdx_path)
message(
  "Country coverage: ", n_distinct(segment_status$country_code),
  "; poverty threshold: ", poverty_threshold,
  "; scenarios: ", paste(names(scenario_codes), collapse = ", "), "."
)
}
