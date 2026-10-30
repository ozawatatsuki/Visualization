# Appendix figures: population-weighted global elasticity profiles.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "ggplot2", "scales", "ragg"
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

fdm_variant <- Sys.getenv("FDM_VARIANT", unset = "freqseg")
fdm_folder <- switch(
  fdm_variant,
  freqseg = "freqseg変更後",
  mder = "MDER更新後",
  stop("FDM_VARIANT must be either 'freqseg' or 'mder'.", call. = FALSE)
)

fdm_path <- file.path(
  "data", "FDM", fdm_folder, "Fooddem_PriceEffect.gdx"
)
population_path <- file.path("data", "PHI", "Inputdata.gdx")
for (path in c(fdm_path, population_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

target_years <- c(2050L, 2100L)
scenario_labels <- c(
  SSP2_400C_2030CP_NoCC_No = "2030CP",
  SSP2_BaU_NoCC_No = "BaU"
)
scenario_colors <- c(
  `2030CP` = "#168F96",
  BaU = "#A8D600"
)

item_labels <- c(
  "Alcoholic beverages, tobacco, and narcotics" =
    "Alcoholic beverages,\ntobacco, and narcotics",
  "Clothing and footwear" = "Clothing and\nfootwear",
  Communication = "Communication",
  Education = "Education",
  Energy = "Energy",
  "Furnishings, household equipment and maintenance" =
    "Furnishings, household\nequipment and maintenance",
  Health = "Health",
  Housing = "Housing",
  "Miscellaneous goods and services" =
    "Miscellaneous goods\nand services",
  "Recreation and culture" = "Recreation and\nculture",
  "Restaurants and hotels" = "Restaurants and\nhotels",
  Transport = "Transport",
  Water = "Water"
)
food_labels <- c(
  Cereals = "Cereals",
  Meats = "Meat",
  Fish = "Fish",
  Dairy = "Dairy",
  Oils_and_Fats = "Oils & fats",
  Fruits_and_Veg = "Fruit & vegetables",
  Food_Other = "Other food"
)
food_colors <- c(
  Cereals = "#D8C500",
  Meat = "#58B7AA",
  Fish = "#E5A2C2",
  Dairy = "#9B83C1",
  `Oils & fats` = "#EF6758",
  `Fruit & vegetables` = "#5D9FC6",
  `Other food` = "#F39A3C"
)
region_order <- c(
  "R17XSA", "R17XER", "R17XNF", "R17XAF", "R17XLM", "R17CIS",
  "R17XOC", "R17XE25", "R17XME", "R17BRA", "R17XSE", "R17CAN",
  "R17CHN", "R17IND", "R17JPN", "R17TUR", "R17USA"
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
region_map <- stack(strsplit(region_members, " ", fixed = TRUE)) |>
  transmute(
    country = as.character(values),
    region_code = as.character(ind)
  )

fdm <- gamstransfer::Container$new()
fdm$read(
  fdm_path,
  symbols = c(
    "CrossPriceEla_Food", "priceEla", "Population_seg", "budgetinitial"
  )
)

input <- gamstransfer::Container$new()
input$read(population_path, symbols = "Population")

budget <- fdm["budgetinitial"]$records |>
  tibble::as_tibble() |>
  transmute(
    segment = as.character(.data[["seg2"]]),
    budget = .data[["value"]]
  )

population <- input["Population"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    region = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    population = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% names(scenario_labels),
    year %in% target_years,
    is.finite(population),
    population > 0
  )

cross_price_wld <- fdm["CrossPriceEla_Food"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    region = as.character(.data[["R"]]),
    segment = as.character(.data[["seg2"]]),
    year = as.integer(as.character(.data[["Y"]])),
    item_code = as.character(.data[["I"]]),
    elasticity = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% names(scenario_labels),
    year %in% target_years,
    item_code %in% names(item_labels),
    is.finite(elasticity)
  ) |>
  inner_join(
    population,
    by = c("scenario_code", "region", "year")
  ) |>
  group_by(scenario_code, year, segment, item_code) |>
  summarise(
    elasticity = weighted.mean(elasticity, population),
    countries = n_distinct(region),
    .groups = "drop"
  ) |>
  inner_join(budget, by = "segment") |>
  mutate(
    scenario = factor(
      unname(scenario_labels[scenario_code]),
      levels = unname(scenario_labels)
    ),
    item = factor(
      unname(item_labels[item_code]),
      levels = unname(item_labels)
    )
  ) |>
  arrange(year, item, scenario, budget)

segment_population <- fdm["Population_seg"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    segment = as.character(.data[["seg2"]]),
    segment_population = .data[["value"]]
  ) |>
  filter(
    scenario_code == "SSP2_400C_2030CP_NoCC_No",
    year %in% target_years,
    segment != "ALL",
    is.finite(segment_population),
    segment_population > 0
  ) |>
  inner_join(region_map, by = "country")

region_segment_population <- segment_population |>
  group_by(region_code, year, segment) |>
  summarise(
    segment_population = sum(segment_population),
    .groups = "drop"
  )

price_region <- fdm["priceEla"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    variable_code = as.character(.data[["var"]]),
    country = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    segment = as.character(.data[["seg"]]),
    elasticity = .data[["value"]]
  ) |>
  filter(
    scenario_code == "SSP2_BaU_NoCC_No",
    year %in% target_years,
    segment != "ALL",
    variable_code %in% names(food_labels),
    is.finite(elasticity)
  ) |>
  inner_join(
    select(
      segment_population,
      country, year, segment, region_code, segment_population
    ),
    by = c("country", "year", "segment")
  ) |>
  group_by(region_code, year, segment, variable_code) |>
  summarise(
    elasticity = weighted.mean(elasticity, segment_population),
    countries = n_distinct(country),
    .groups = "drop"
  ) |>
  inner_join(
    region_segment_population,
    by = c("region_code", "year", "segment")
  ) |>
  inner_join(budget, by = "segment") |>
  mutate(
    variable = factor(
      unname(food_labels[variable_code]),
      levels = unname(food_labels)
    ),
    region = factor(region_code, levels = region_order)
  ) |>
  group_by(region, year, variable) |>
  arrange(budget, .by_group = TRUE) |>
  mutate(
    cumulative_probability = 100 * (
      cumsum(segment_population) - 0.5 * segment_population
    ) / sum(segment_population)
  ) |>
  ungroup()

if (nrow(cross_price_wld) == 0 || nrow(price_region) == 0) {
  stop("No elasticity records remained after population weighting.", call. = FALSE)
}
if (anyNA(cross_price_wld) || anyNA(price_region)) {
  stop("Population-weighted elasticity data contain missing values.", call. = FALSE)
}

budget_breaks <- c(100, 1000, 10000, 100000, 1000000)
budget_labels <- c("100", "1k", "10k", "100k", "1m")

appendix_theme <- theme_bw(base_size = 6.8, base_family = "Arial") +
  theme(
    text = element_text(colour = "#202020"),
    panel.border = element_rect(colour = "#B8B8B8", linewidth = 0.22),
    panel.grid.major = element_line(colour = "#D8D8D8", linewidth = 0.20),
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "#F0F0F0", colour = NA),
    strip.text = element_text(
      face = "bold",
      size = 6.3,
      lineheight = 0.95,
      margin = margin(1.2, 1, 1.2, 1, "mm")
    ),
    axis.title = element_text(face = "plain", size = 6.5),
    axis.text = element_text(size = 5.6),
    axis.ticks = element_line(linewidth = 0.22),
    axis.ticks.length = grid::unit(1.2, "mm"),
    legend.position = "bottom",
    legend.title = element_text(size = 6.0),
    legend.text = element_text(size = 6.0),
    legend.key.width = grid::unit(8, "mm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.box.spacing = grid::unit(0.5, "mm"),
    panel.spacing = grid::unit(2.2, "mm"),
    plot.margin = margin(2, 2, 2, 2, "mm")
  )

make_cross_price_plot <- function(target_year) {
  ggplot(
    filter(cross_price_wld, year == target_year),
    aes(x = budget, y = elasticity, colour = scenario, group = scenario)
  ) +
    geom_hline(yintercept = 0, linewidth = 0.22, colour = "#777777") +
    geom_line(linewidth = 0.42, alpha = 0.88) +
    facet_wrap(
      vars(item),
      ncol = 3,
      scales = "free_y",
      axes = "all_x",
      axis.labels = "all_x"
    ) +
    scale_colour_manual(values = scenario_colors, drop = FALSE) +
    scale_x_log10(
      breaks = budget_breaks,
      labels = budget_labels,
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    labs(
      x = "Initial budget (log scale)",
      y = "Cross-price elasticity",
      colour = "Scenario"
    ) +
    appendix_theme
}

make_price_plot <- function(target_year) {
  ggplot(
    filter(price_region, year == target_year),
    aes(
      x = cumulative_probability,
      y = elasticity,
      colour = variable,
      group = variable
    )
  ) +
    geom_hline(
      yintercept = 0,
      linewidth = 0.24,
      colour = "#343434",
      linetype = "22"
    ) +
    geom_line(linewidth = 0.42, alpha = 0.92) +
    facet_wrap(
      vars(region),
      ncol = 6,
      axes = "all_x",
      axis.labels = "all_x"
    ) +
    scale_colour_manual(values = food_colors, drop = FALSE) +
    scale_x_continuous(
      limits = c(0, 100),
      breaks = c(0, 25, 50, 75, 100),
      expand = expansion(mult = c(0, 0))
    ) +
    labs(
      x = "Cumulative population share (%)",
      y = "Own-price elasticity",
      colour = NULL
    ) +
    appendix_theme +
    guides(colour = guide_legend(nrow = 1, byrow = TRUE)) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.key.width = grid::unit(5.5, "mm")
    )
}

output_dir <- file.path("output", "appendix")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

for (target_year in target_years) {
  cross_path <- file.path(
    output_dir,
    paste0("appendix_cross_price_elasticity_", target_year, "_", fdm_variant, ".png")
  )
  price_path <- file.path(
    output_dir,
    paste0("appendix_price_elasticity_", target_year, "_", fdm_variant, ".png")
  )

  for (path in c(cross_path, price_path)) {
    if (file.exists(path)) {
      file.remove(path)
    }
  }

  save_interim_plot(
    make_cross_price_plot(target_year),
    cross_path,
    height_mm = 165,
    dpi = 600,
    device = ragg::agg_png
  )
  save_interim_plot(
    make_price_plot(target_year),
    price_path,
    height_mm = 125,
    dpi = 600,
    device = ragg::agg_png
  )

  message("Created ", cross_path)
  message("Created ", price_path)
}

message(
  "Population coverage: CrossPriceEla_Food ",
  min(cross_price_wld$countries), "-", max(cross_price_wld$countries),
  " countries per curve; regional priceEla ",
  min(price_region$countries), "-", max(price_region$countries),
  " countries per region/segment curve."
)
