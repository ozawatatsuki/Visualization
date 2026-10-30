# Figure 3: Regional heterogeneity in hunger risk and food consumption.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c(
  "gamstransfer", "dplyr", "tidyr", "ggplot2", "patchwork", "scales",
  "ragg", "sf", "rnaturalearth", "rnaturalearthdata"
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
price_effect_path <- file.path(
  "data", "FDM", fdm_folder, "Fooddem_PriceEffect.gdx"
)
quantile_path <- file.path(
  "data", "FDM", fdm_folder, "Fooddem_Quantile_10.gdx"
)
r5_map_path <- file.path("data", "R17toR5.map")

for (path in c(price_effect_path, quantile_path, r5_map_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

scenario_codes <- c(
  Baseline = "SSP2_BaU_NoCC_No",
  `400C` = "SSP2_400C_2030CP_NoCC_No"
)
target_years <- 2050L

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
  Baseline = "#666666",
  `400C` = "#B2182B"
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

r5_codes <- c(
  "R5OECD90+EU", "R5ASIA", "R5LAM", "R5REF", "R5MAF"
)
r5_labels <- c(
  `R5OECD90+EU` = "OECD90 + EU",
  R5ASIA = "Asia",
  R5LAM = "Latin America",
  R5REF = "Reforming economies",
  R5MAF = "Middle East & Africa"
)
r5_panel_labels <- c(
  `R5OECD90+EU` = "OECD90 + EU",
  R5ASIA = "Asia",
  R5LAM = "Latin America",
  R5REF = "REs",
  R5MAF = "Middle East & Africa"
)
r5_colors <- c(
  `OECD90 + EU` = "#3B4992",
  Asia = "#A56B00",
  `Latin America` = "#008B6E",
  `Reforming economies` = "#8E5EA2",
  `Middle East & Africa` = "#8C564B"
)

price_effect <- gamstransfer::Container$new()
hunger_population_symbols <- c(
  income = "PoU_In_Ragg_calc",
  income_price = "PoU_In_Pr_Ragg_calc",
  full = "PoU_In_Pr_CR_Ragg_calc"
)
price_effect$read(
  price_effect_path,
  symbols = c(
    "PoUrate_In_Pr_CR", "PoU_In_Pr_CR", "Population_valid", "Population_seg",
    "FoodDem_peffect_CR", "MDER",
    unname(hunger_population_symbols)
  )
)

quantile_data <- gamstransfer::Container$new()
quantile_symbols <- c(
  income = "FoodDemIn_Quantile",
  income_price = "FoodDem_peffect_Quantile",
  full = "FoodDem_peffect_CR_Quantile"
)
quantile_data$read(quantile_path, symbols = unname(quantile_symbols))

hunger_change <- price_effect["PoUrate_In_Pr_CR"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    value = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year %in% target_years
  ) |>
  mutate(
    scenario = factor(
      scenario_code,
      levels = unname(scenario_codes),
      labels = names(scenario_codes)
    )
  ) |>
  select(-scenario_code) |>
  pivot_wider(names_from = scenario, values_from = value) |>
  transmute(
    country_code,
    year,
    change_pp = 100 * (.data[["400C"]] - .data[["Baseline"]])
  )

hunger_absolute <- price_effect["PoU_In_Pr_CR"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    value = .data[["value"]] / 1e6
  ) |>
  filter(
    scenario_code == scenario_codes[["400C"]],
    year %in% target_years
  ) |>
  select(-scenario_code)

expected_country_years <- length(target_years) *
  n_distinct(hunger_change$country_code)
if (nrow(hunger_change) != expected_country_years || anyNA(hunger_change)) {
  stop("Country hunger-risk data are incomplete.", call. = FALSE)
}

read_quantile_symbol <- function(metric, symbol) {
  quantile_data[symbol]$records |>
    tibble::as_tibble() |>
    transmute(
      scenario_code = as.character(.data[["Ref"]]),
      region = as.character(.data[["R"]]),
      year = as.integer(as.character(.data[["Y"]])),
      quantile = as.character(.data[["quantile"]]),
      variable = as.character(.data[["var"]]),
      metric = metric,
      value = .data[["value"]]
    ) |>
    filter(
      scenario_code %in% unname(scenario_codes),
      region %in% r5_codes,
      year %in% target_years,
      variable == "total"
    ) |>
    select(-variable)
}

quantile_wide <- bind_rows(
  Map(read_quantile_symbol, names(quantile_symbols), unname(quantile_symbols))
) |>
  pivot_wider(names_from = metric, values_from = value)

quantile_baseline <- quantile_wide |>
  filter(scenario_code == scenario_codes[["Baseline"]]) |>
  select(-scenario_code)

quantile_400c <- quantile_wide |>
  filter(scenario_code == scenario_codes[["400C"]]) |>
  select(-scenario_code)

food_effects_wide <- inner_join(
  quantile_400c,
  quantile_baseline,
  by = c("region", "year", "quantile"),
  suffix = c("_400c", "_baseline")
) |>
  mutate(
    income_change = income_400c - income_baseline,
    income_price_change = income_price_400c - income_price_baseline,
    full_change = full_400c - full_baseline
  ) |>
  transmute(
    region,
    year,
    quantile,
    income = 100 * income_change / full_baseline,
    price = 100 * (income_price_change - income_change) / full_baseline,
    cross_price = 100 * (full_change - income_price_change) / full_baseline,
    total = 100 * full_change / full_baseline
  )

stack_error <- with(
  food_effects_wide,
  max(abs(income + price + cross_price - total))
)
if (stack_error > 1e-8) {
  stop("Food-consumption effects do not sum to the total change.", call. = FALSE)
}

food_effects <- food_effects_wide |>
  select(-total) |>
  pivot_longer(
    cols = all_of(names(effect_labels)),
    names_to = "effect_code",
    values_to = "value"
  ) |>
  mutate(
    effect = factor(
      unname(effect_labels[effect_code]),
      levels = unname(effect_labels)
    ),
    quantile = factor(
      quantile,
      levels = c(as.character(1:10), "ALL"),
      labels = c(as.character(1:10), "Ave")
    )
  )

expected_effect_rows <- length(r5_codes) * length(target_years) * 11L * 3L
if (nrow(food_effects) != expected_effect_rows || anyNA(food_effects)) {
  stop("R5 quantile effects are incomplete.", call. = FALSE)
}

food_consumption_wide <- quantile_data[quantile_symbols[["full"]]]$records |>
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
    region %in% r5_codes,
    year %in% target_years
  ) |>
  pivot_wider(names_from = scenario_code, values_from = value)

baseline_total <- food_consumption_wide |>
  filter(item_code == "total") |>
  transmute(
    region,
    year,
    quantile_code,
    baseline_total = .data[[scenario_codes[["Baseline"]]]],
    total_diff = .data[[scenario_codes[["400C"]]]] - baseline_total
  )

food_commodities <- food_consumption_wide |>
  filter(item_code %in% names(food_item_labels)) |>
  inner_join(
    baseline_total,
    by = c("region", "year", "quantile_code")
  ) |>
  transmute(
    region,
    year,
    quantile_code,
    commodity = factor(
      unname(food_item_labels[item_code]),
      levels = unname(food_item_labels)
    ),
    value = (
      .data[[scenario_codes[["400C"]]]] -
        .data[[scenario_codes[["Baseline"]]]]
    ),
    total_difference = total_diff
  ) |>
  mutate(
    quantile = factor(
      quantile_code,
      levels = c(as.character(1:10), "ALL"),
      labels = c(as.character(1:10), "Ave")
    )
  ) |>
  arrange(region, year, quantile, commodity)

food_consumption_400c <- food_consumption_wide |>
  filter(item_code %in% names(food_item_labels)) |>
  transmute(
    region,
    year,
    quantile_code,
    commodity = factor(
      unname(food_item_labels[item_code]),
      levels = unname(food_item_labels)
    ),
    value = .data[[scenario_codes[["400C"]]]]
  ) |>
  mutate(
    quantile = factor(
      quantile_code,
      levels = c(as.character(1:10), "ALL"),
      labels = c(as.character(1:10), "Ave")
    )
  ) |>
  arrange(region, year, quantile, commodity)

food_consumption_400c_check <- food_consumption_400c |>
  group_by(region, year, quantile_code) |>
  summarise(stacked_total = sum(value), .groups = "drop") |>
  inner_join(
    food_consumption_wide |>
      filter(item_code == "total") |>
      transmute(
        region,
        year,
        quantile_code,
        reported_total = .data[[scenario_codes[["400C"]]]]
      ),
    by = c("region", "year", "quantile_code")
  )

if (
  nrow(food_consumption_400c_check) !=
    length(r5_codes) * length(target_years) * 11L ||
    max(abs(
      food_consumption_400c_check$stacked_total -
        food_consumption_400c_check$reported_total
    )) > 1e-6
) {
  stop("400C commodity totals do not match reported food consumption.", call. = FALSE)
}

commodity_stack_check <- food_commodities |>
  group_by(region, year, quantile_code) |>
  summarise(
    stack_error = sum(value) - first(total_difference),
    .groups = "drop"
  )

expected_commodity_rows <-
  length(r5_codes) * length(target_years) * 11L * length(food_item_labels)
if (
  nrow(food_commodities) != expected_commodity_rows ||
    anyNA(food_commodities) ||
    max(abs(commodity_stack_check$stack_error)) > 1e-8
) {
  stop("R5 commodity contributions do not sum to the total change.", call. = FALSE)
}

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

hunger_population_r5 <- price_effect["PoU_In_Pr_CR_Ragg_calc"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    region = as.character(.data[["R17"]]),
    year = as.integer(as.character(.data[["Y"]])),
    value = .data[["value"]] / 1e6
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year %in% target_years
  ) |>
  inner_join(r17_to_r5, by = c("region" = "r17")) |>
  group_by(scenario_code, r5, year) |>
  summarise(value = sum(value), .groups = "drop") |>
  mutate(
    scenario = factor(
      scenario_code,
      levels = unname(scenario_codes),
      labels = names(scenario_codes)
    ),
    region_label = factor(
      unname(r5_labels[r5]),
      levels = rev(unname(r5_labels))
    )
  )

expected_hunger_population_rows <-
  length(scenario_codes) * length(r5_codes) * length(target_years)
if (
  nrow(hunger_population_r5) != expected_hunger_population_rows ||
    anyNA(hunger_population_r5)
) {
  stop("R5 hunger-risk population data are incomplete.", call. = FALSE)
}

read_hunger_population_symbol <- function(metric, symbol) {
  price_effect[symbol]$records |>
    tibble::as_tibble() |>
    transmute(
      scenario_code = as.character(.data[["Ref"]]),
      region = as.character(.data[["R17"]]),
      year = as.integer(as.character(.data[["Y"]])),
      metric = metric,
      value = .data[["value"]] / 1e6
    ) |>
    filter(
      scenario_code %in% unname(scenario_codes),
      year %in% target_years
    ) |>
    inner_join(r17_to_r5, by = c("region" = "r17")) |>
    group_by(scenario_code, r5, year, metric) |>
    summarise(value = sum(value), .groups = "drop")
}

hunger_population_wide <- bind_rows(
  Map(
    read_hunger_population_symbol,
    names(hunger_population_symbols),
    unname(hunger_population_symbols)
  )
) |>
  pivot_wider(names_from = metric, values_from = value)

hunger_population_baseline <- hunger_population_wide |>
  filter(scenario_code == scenario_codes[["Baseline"]]) |>
  select(-scenario_code)

hunger_population_400c <- hunger_population_wide |>
  filter(scenario_code == scenario_codes[["400C"]]) |>
  select(-scenario_code)

hunger_population_effects <- inner_join(
  hunger_population_400c,
  hunger_population_baseline,
  by = c("r5", "year"),
  suffix = c("_400c", "_baseline")
) |>
  transmute(
    r5,
    year,
    region_label = factor(
      unname(r5_labels[r5]),
      levels = rev(unname(r5_labels))
    ),
    baseline = full_baseline,
    income = income_400c - income_baseline,
    price = (income_price_400c - income_price_baseline) - income,
    cross_price = (full_400c - full_baseline) -
      (income_price_400c - income_price_baseline),
    total_400c = full_400c
  )

hunger_population_stack_error <- with(
  hunger_population_effects,
  max(abs(baseline + income + price + cross_price - total_400c))
)
if (hunger_population_stack_error > 1e-8) {
  stop("R5 hunger-risk effects do not sum to the 400C total.", call. = FALSE)
}

cis_codes <- c(
  "ARM", "AZE", "BLR", "GEO", "KAZ", "KGZ", "MDA", "RUS", "TJK",
  "TKM", "UKR", "UZB"
)
eu25_codes <- c(
  "AUT", "BEL", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "DEU",
  "GRC", "HUN", "IRL", "ITA", "LVA", "LTU", "LUX", "MLT", "NLD",
  "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "GBR"
)
south_asia_codes <- c("AFG", "BGD", "BTN", "MDV", "NPL", "PAK", "LKA")
middle_east_codes <- c(
  "BHR", "IRN", "IRQ", "ISR", "JOR", "KWT", "LBN", "OMN", "PSE",
  "QAT", "SAU", "SYR", "ARE", "YEM"
)

world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  filter(continent != "Antarctica") |>
  mutate(
    country_code = as.character(adm0_a3),
    r17 = case_when(
      country_code == "USA" ~ "R17USA",
      country_code == "CAN" ~ "R17CAN",
      country_code == "BRA" ~ "R17BRA",
      country_code == "CHN" ~ "R17CHN",
      country_code == "IND" ~ "R17IND",
      country_code == "JPN" ~ "R17JPN",
      country_code == "TUR" ~ "R17TUR",
      country_code %in% cis_codes ~ "R17CIS",
      country_code %in% eu25_codes ~ "R17XE25",
      country_code %in% c("GRL", "FRO") ~ "R17XER",
      country_code %in% c("SGS", "IOT", "ATF", "HMD") ~ "R17XOC",
      country_code %in% c("SHN", "SYC", "MUS") ~ "R17XAF",
      continent == "Europe" ~ "R17XER",
      continent == "Oceania" ~ "R17XOC",
      continent == "South America" ~ "R17XLM",
      continent == "North America" ~ "R17XLM",
      continent == "Africa" & subregion == "Northern Africa" ~ "R17XNF",
      continent == "Africa" ~ "R17XAF",
      country_code %in% middle_east_codes ~ "R17XME",
      country_code %in% south_asia_codes ~ "R17XSA",
      continent == "Asia" ~ "R17XSE",
      TRUE ~ NA_character_
    )
  ) |>
  left_join(r17_to_r5, by = "r17") |>
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

if (anyNA(world$r5_label)) {
  warning("Some map geometries could not be assigned to an R5 region.")
}

country_to_r5 <- world |>
  st_drop_geometry() |>
  select(country_code, r5) |>
  filter(!is.na(country_code), !is.na(r5)) |>
  distinct()

population_r5_scenario <- price_effect["Population_valid"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    population = .data[["value"]] / 1e6
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year %in% target_years
  ) |>
  inner_join(country_to_r5, by = "country_code") |>
  group_by(scenario_code, r5, year) |>
  summarise(population = sum(population), .groups = "drop")

population_r5 <- population_r5_scenario |>
  filter(scenario_code == scenario_codes[["400C"]]) |>
  select(-scenario_code)

if (
  nrow(population_r5_scenario) !=
    length(scenario_codes) * length(r5_codes) * length(target_years)
) {
  stop("R5 population denominators are incomplete.", call. = FALSE)
}

mder_r5 <- price_effect["MDER"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    mder = .data[["value"]]
  ) |>
  filter(
    scenario_code == scenario_codes[["400C"]],
    year %in% target_years
  ) |>
  inner_join(
    price_effect["Population_valid"]$records |>
      tibble::as_tibble() |>
      transmute(
        scenario_code = as.character(.data[["Ref"]]),
        country_code = as.character(.data[["R"]]),
        year = as.integer(as.character(.data[["Y"]])),
        population = .data[["value"]]
      ) |>
      filter(
        scenario_code == scenario_codes[["400C"]],
        year %in% target_years
      ),
    by = c("scenario_code", "country_code", "year")
  ) |>
  inner_join(country_to_r5, by = "country_code") |>
  group_by(r5, year) |>
  summarise(
    mder = weighted.mean(mder, population),
    .groups = "drop"
  )

if (
  nrow(mder_r5) != length(r5_codes) * length(target_years) ||
    anyNA(mder_r5)
) {
  stop("Population-weighted R5 MDER values are incomplete.", call. = FALSE)
}

food_distribution_r5 <- price_effect["FoodDem_peffect_CR"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    country_code = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    segment = as.character(.data[["seg"]]),
    variable = as.character(.data[["var"]]),
    food_consumption = .data[["value"]]
  ) |>
  filter(
    scenario_code %in% unname(scenario_codes),
    year %in% target_years,
    segment != "ALL",
    variable == "total",
    is.finite(food_consumption)
  ) |>
  select(-variable) |>
  inner_join(
    price_effect["Population_seg"]$records |>
      tibble::as_tibble() |>
      transmute(
        scenario_code = as.character(.data[["Ref"]]),
        country_code = as.character(.data[["R"]]),
        year = as.integer(as.character(.data[["Y"]])),
        segment = as.character(.data[["seg2"]]),
        population = .data[["value"]]
      ) |>
      filter(
        scenario_code %in% unname(scenario_codes),
        year %in% target_years,
        segment != "ALL",
        is.finite(population),
        population > 0
      ),
    by = c("scenario_code", "country_code", "year", "segment")
  ) |>
  inner_join(country_to_r5, by = "country_code") |>
  group_by(scenario_code, r5, year, food_consumption) |>
  summarise(population = sum(population), .groups = "drop") |>
  arrange(scenario_code, r5, year, food_consumption) |>
  group_by(scenario_code, r5, year) |>
  mutate(
    cumulative_population = cumsum(population) / 1e6,
    cumulative_population_share = 100 * cumsum(population) / sum(population)
  ) |>
  ungroup() |>
  mutate(
    scenario = factor(
      scenario_code,
      levels = unname(scenario_codes),
      labels = names(scenario_codes)
    )
  )

distribution_population_check <- food_distribution_r5 |>
  group_by(scenario_code, r5, year) |>
  summarise(population = max(cumulative_population), .groups = "drop") |>
  inner_join(
    population_r5_scenario,
    by = c("scenario_code", "r5", "year")
  )

if (
  nrow(distribution_population_check) !=
    length(scenario_codes) * length(r5_codes) * length(target_years) ||
    max(abs(
      distribution_population_check$population.x -
        distribution_population_check$population.y
    )) > 1e-5
) {
  stop(
    "Cumulative population totals do not match R5 population denominators.",
    call. = FALSE
  )
}

hunger_rate_effects <- hunger_population_effects |>
  inner_join(population_r5, by = c("r5", "year")) |>
  transmute(
    r5,
    year,
    region_label,
    baseline = 100 * baseline / population,
    income = 100 * income / population,
    price = 100 * price / population,
    cross_price = 100 * cross_price / population,
    total_400c = 100 * total_400c / population
  )

map_limit <- ceiling(max(hunger_change$change_pp, na.rm = TRUE) / 4) * 4

stack_bounds <- food_effects |>
  group_by(region, year, quantile) |>
  summarise(
    negative = sum(value[value < 0]),
    positive = sum(value[value > 0]),
    .groups = "drop"
  )
effect_limits <- c(
  floor(min(stack_bounds$negative) / 5) * 5,
  ceiling(max(stack_bounds$positive) / 5) * 5
)
effect_breaks <- pretty(effect_limits, n = 4)

commodity_stack_bounds <- food_commodities |>
  group_by(region, year, quantile) |>
  summarise(
    negative = sum(value[value < 0]),
    positive = sum(value[value > 0]),
    .groups = "drop"
  )
commodity_limits <- c(
  floor(min(commodity_stack_bounds$negative) / 5) * 5,
  ceiling(max(commodity_stack_bounds$positive) / 5) * 5
)
commodity_breaks <- pretty(commodity_limits, n = 4)

consumption_upper <- ceiling(
  max(
    food_consumption_400c_check$stacked_total,
    mder_r5$mder,
    na.rm = TRUE
  ) / 500
) * 500
consumption_breaks <- seq(0, consumption_upper, by = 500)
consumption_minor_breaks <- seq(0, consumption_upper, by = 100)

distribution_x_upper <- 3000
distribution_y_upper <- ceiling(
  1.05 * max(
    food_distribution_r5$cumulative_population_share[
      food_distribution_r5$food_consumption <= distribution_x_upper
    ],
    na.rm = TRUE
  ) / 10
) * 10
distribution_x_breaks <- seq(0, distribution_x_upper, by = 1000)
distribution_y_breaks <- pretty(c(0, distribution_y_upper), n = 5)

panel_tag_theme <- theme(
  plot.tag = element_text(face = "bold", size = 8.5, colour = "black"),
  plot.tag.position = c(0, 1)
)

make_map_panel <- function(target_year) {
  map_data <- world |>
    left_join(
      filter(hunger_change, year == target_year),
      by = "country_code"
    )

  ggplot() +
    geom_sf(
      data = map_data,
      aes(fill = change_pp),
      colour = "#F7F7F7",
      linewidth = 0.04
    ) +
    geom_sf(
      data = map_data,
      aes(colour = r5_label),
      fill = NA,
      linewidth = 0.16,
      alpha = 0.90,
      key_glyph = draw_key_path
    ) +
    scale_fill_gradientn(
      colours = c(
        "#FFFFFF", "#FEEBE2", "#FCAE91", "#FB6A4A", "#CB181D", "#7F0000"
      ),
      values = c(0, 0.04, 0.15, 0.35, 0.65, 1),
      limits = c(0, map_limit),
      breaks = seq(0, map_limit, length.out = 5),
      labels = scales::label_number(accuracy = 1),
      oob = scales::squish,
      na.value = "#E5E5E5",
      name = "Change in population at risk of hunger (percentage points)"
    ) +
    scale_colour_manual(
      values = r5_colors,
      breaks = unname(r5_labels),
      drop = FALSE,
      name = "R5 region outlines"
    ) +
    coord_sf(datum = NA, expand = FALSE, clip = "off") +
    labs(tag = "a") +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        direction = "horizontal",
        barwidth = grid::unit(62, "mm"),
        barheight = grid::unit(2.3, "mm"),
        ticks.colour = "#333333",
        frame.colour = "#555555",
        order = 1
      ),
      colour = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.0, alpha = 1),
        order = 2
      )
    ) +
    theme_void(base_size = 6.4, base_family = "Arial") +
    panel_tag_theme +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "center",
      legend.title = element_text(face = "plain", size = 6.0),
      legend.text = element_text(size = 5.3, colour = "black"),
      legend.key.height = grid::unit(2.7, "mm"),
      legend.key.width = grid::unit(9, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.3, "mm"),
      plot.margin = margin(2, 2, 0.5, 2, "mm")
    )
}

make_absolute_map_panel <- function(target_year) {
  map_data <- world |>
    left_join(
      filter(hunger_absolute, year == target_year),
      by = "country_code"
    )
  upper <- ceiling(max(map_data$value, na.rm = TRUE) / 25) * 25

  ggplot() +
    geom_sf(
      data = map_data,
      aes(fill = value),
      colour = "#F7F7F7",
      linewidth = 0.04
    ) +
    geom_sf(
      data = map_data,
      aes(colour = r5_label),
      fill = NA,
      linewidth = 0.16,
      alpha = 0.90,
      key_glyph = draw_key_path
    ) +
    scale_fill_gradientn(
      colours = c(
        "#FFFFFF", "#FEEBE2", "#FCAE91", "#FB6A4A", "#CB181D", "#7F0000"
      ),
      limits = c(0, upper),
      breaks = pretty(c(0, upper), n = 5),
      labels = scales::label_number(accuracy = 1),
      trans = "sqrt",
      oob = scales::squish,
      na.value = "#E5E5E5",
      name = "Population at risk of hunger (million)"
    ) +
    scale_colour_manual(
      values = r5_colors,
      breaks = unname(r5_labels),
      drop = FALSE,
      name = "R5 region outlines"
    ) +
    coord_sf(datum = NA, expand = FALSE, clip = "off") +
    labs(tag = "a") +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        direction = "horizontal",
        barwidth = grid::unit(62, "mm"),
        barheight = grid::unit(2.3, "mm"),
        ticks.colour = "#333333",
        frame.colour = "#555555",
        order = 1
      ),
      colour = guide_legend(
        title.position = "top",
        title.hjust = 0.5,
        nrow = 1,
        byrow = TRUE,
        override.aes = list(linewidth = 1.0, alpha = 1),
        order = 2
      )
    ) +
    theme_void(base_size = 6.4, base_family = "Arial") +
    panel_tag_theme +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.just = "center",
      legend.title = element_text(face = "plain", size = 6.0),
      legend.text = element_text(size = 5.3, colour = "black"),
      legend.key.height = grid::unit(2.7, "mm"),
      legend.key.width = grid::unit(9, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.3, "mm"),
      plot.margin = margin(2, 2, 0.5, 2, "mm")
    )
}

make_hunger_rate_panel <- function(target_year) {
  panel_data <- hunger_rate_effects |>
    filter(year == target_year) |>
    mutate(
      region_x = match(as.character(region_label), unname(r5_labels)),
      after_income = baseline + income,
      after_price = after_income + price
    )

  stacked_segments <- bind_rows(
    panel_data |>
      transmute(
        region_x,
        x_offset = -0.18,
        component = "Baseline",
        y_start = 0,
        y_end = baseline
      ),
    panel_data |>
      transmute(
        region_x,
        x_offset = -0.06,
        component = "Income",
        y_start = baseline,
        y_end = after_income
      ),
    panel_data |>
      transmute(
        region_x,
        x_offset = 0.06,
        component = "Own-price",
        y_start = after_income,
        y_end = after_price
      ),
    panel_data |>
      transmute(
        region_x,
        x_offset = 0.18,
        component = "Cross-price",
        y_start = after_price,
        y_end = total_400c
      )
  ) |>
    mutate(
      component = factor(
        component,
        levels = c("Baseline", unname(effect_labels))
      ),
      x = region_x + x_offset,
      y_min = pmin(y_start, y_end),
      y_max = pmax(y_start, y_end)
    )

  component_colors <- c(Baseline = scenario_colors[["Baseline"]], effect_colors)

  ggplot() +
    geom_rect(
      data = stacked_segments,
      aes(
        xmin = x - 0.065,
        xmax = x + 0.065,
        ymin = y_min,
        ymax = y_max,
        fill = component
      ),
      colour = "white",
      linewidth = 0.12
    ) +
    geom_point(
      data = panel_data,
      aes(x = region_x + 0.18, y = total_400c),
      shape = 23,
      size = 2.1,
      stroke = 0.25,
      fill = scenario_colors[["400C"]],
      colour = "white"
    ) +
    geom_text(
      data = panel_data,
      aes(
        x = region_x + 0.18,
        y = total_400c,
        label = scales::number(total_400c, accuracy = 0.1)
      ),
      vjust = -0.65,
      size = 1.45,
      colour = scenario_colors[["400C"]]
    ) +
    scale_fill_manual(values = component_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = 1:5,
      labels = c(
        "OECD90\n+ EU", "Asia", "Latin\nAmerica", "REs",
        "Middle East\n& Africa"
      ),
      expand = expansion(add = 0.52)
    ) +
    scale_y_continuous(
      breaks = scales::breaks_pretty(n = 5),
      labels = scales::label_number(accuracy = 0.1),
      expand = expansion(mult = c(0, 0.18))
    ) +
    labs(
      tag = "b",
      title = "Population at risk of hunger",
      x = NULL,
      y = "Population at risk of hunger\n(% of population)",
      fill = NULL
    ) +
    theme_interim_nature(base_size = 5.3, grid = "y", legend_position = "bottom") +
    panel_tag_theme +
    theme(
      plot.title = element_text(face = "plain", size = 6.0, hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 7.2),
      plot.tag.position = c(0, 1),
      axis.title = element_text(face = "plain", size = 5.1),
      axis.text.x = element_text(size = 4.0, lineheight = 0.9),
      axis.text.y = element_text(size = 4.5),
      axis.ticks.x = element_blank(),
      axis.ticks.length = grid::unit(1, "mm"),
      axis.line.x = element_blank(),
      legend.text = element_text(size = 4.8),
      legend.key.width = grid::unit(6.5, "mm"),
      legend.key.height = grid::unit(2.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      plot.margin = margin(1.5, 1, 1, 1.5, "mm")
    ) +
    guides(
      fill = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(colour = NA)
      )
    )
}

make_region_panel <- function(
    target_year,
    region_code,
    tag,
    show_y_axis,
    show_y_title = show_y_axis,
    stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)
  region_label <- unname(r5_labels[[region_code]])
  region_title <- unname(r5_panel_labels[[region_code]])

  if (stack_by == "effect") {
    panel_data <- food_effects |>
      filter(year == target_year, region == region_code)
    fill_variable <- "effect"
    fill_values <- effect_colors
    y_limits <- effect_limits
    y_breaks <- effect_breaks
  } else {
    panel_data <- food_commodities |>
      filter(year == target_year, region == region_code)
    fill_variable <- "commodity"
    fill_values <- commodity_colors
    y_limits <- commodity_limits
    y_breaks <- commodity_breaks
  }

  panel <- ggplot(
    panel_data,
    aes(x = quantile, y = value, fill = .data[[fill_variable]])
  ) +
    geom_hline(yintercept = 0, linewidth = 0.22, colour = "#333333") +
    geom_col(width = 0.72, colour = "white", linewidth = 0.08) +
    geom_vline(
      xintercept = 10.5,
      linewidth = 0.22,
      colour = "#777777",
      linetype = "22"
    ) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_y_continuous(
      limits = y_limits,
      breaks = y_breaks,
      labels = scales::label_number(accuracy = 1, scale_cut = scales::cut_short_scale()),
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    labs(
      tag = tag,
      title = region_title,
      x = "Income quantile",
      y = if (show_y_title) {
        "Change in food consumption\n(kcal per person per day)"
      } else {
        NULL
      },
      fill = NULL
    ) +
    theme_interim_nature(base_size = 5.3, grid = "y", legend_position = "bottom") +
    panel_tag_theme +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 5.5,
        colour = unname(r5_colors[[region_label]]),
        hjust = 0.5,
        margin = margin(b = 0.5, unit = "mm")
      ),
      plot.tag = element_text(face = "bold", size = 7.2),
      plot.tag.position = c(0.10, 0.98),
      axis.title = element_text(face = "plain", size = 5.1),
      axis.title.y = element_text(margin = margin(r = 1.5)),
      axis.text.x = element_text(size = 4.2),
      axis.text.y = element_text(size = 4.5),
      axis.ticks.x = element_blank(),
      axis.ticks.length = grid::unit(1, "mm"),
      legend.text = element_text(size = 5.0),
      legend.key.size = grid::unit(2.8, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      plot.margin = margin(1.5, 1, 1, if (show_y_axis) 3 else 1, "mm")
    ) +
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(colour = NA)
      )
    )

  if (!show_y_axis) {
    panel <- panel +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )
  }

  panel
}

make_consumption_mder_panel <- function(
    target_year,
    region_code,
    tag = NULL,
    show_y_axis = FALSE,
    show_y_title = FALSE) {
  region_label <- unname(r5_labels[[region_code]])
  region_title <- unname(r5_panel_labels[[region_code]])
  panel_data <- food_consumption_400c |>
    filter(year == target_year, region == region_code)
  mder_data <- mder_r5 |>
    filter(year == target_year, r5 == region_code)

  panel <- ggplot(
    panel_data,
    aes(x = quantile, y = value, fill = commodity)
  ) +
    geom_col(width = 0.72, colour = "white", linewidth = 0.08) +
    geom_vline(
      xintercept = 10.5,
      linewidth = 0.22,
      colour = "#777777",
      linetype = "22"
    ) +
    geom_hline(
      data = mder_data,
      aes(yintercept = mder, linetype = "Population-weighted MDER"),
      colour = "#222222",
      linewidth = 0.38,
      inherit.aes = FALSE
    ) +
    scale_fill_manual(values = commodity_colors, drop = FALSE) +
    scale_linetype_manual(
      values = c(`Population-weighted MDER` = "22"),
      breaks = "Population-weighted MDER"
    ) +
    scale_y_continuous(
      limits = c(0, consumption_upper),
      breaks = consumption_breaks,
      minor_breaks = consumption_minor_breaks,
      labels = scales::label_number(accuracy = 1, big.mark = ","),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      tag = tag,
      title = region_title,
      x = "Income quantile",
      y = if (show_y_title) {
        "Food consumption\n(kcal per person per day)"
      } else {
        NULL
      },
      fill = NULL,
      linetype = NULL
    ) +
    theme_interim_nature(base_size = 5.3, grid = "y", legend_position = "bottom") +
    panel_tag_theme +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 5.5,
        colour = unname(r5_colors[[region_label]]),
        hjust = 0.5,
        margin = margin(b = 0.5, unit = "mm")
      ),
      plot.tag = element_text(face = "bold", size = 7.2),
      plot.tag.position = c(0.10, 0.98),
      axis.title = element_text(face = "plain", size = 5.1),
      axis.title.y = if (show_y_title) {
        element_text(margin = margin(r = 1.5))
      } else {
        element_blank()
      },
      axis.text.x = element_text(size = 4.2),
      axis.text.y = element_text(size = 4.5),
      axis.ticks.x = element_blank(),
      axis.ticks.length = grid::unit(1, "mm"),
      panel.grid.major.y = element_line(
        colour = "#A0A0A0",
        linewidth = 0.20,
        linetype = "22"
      ),
      panel.grid.minor.y = element_line(
        colour = "#D0D0D0",
        linewidth = 0.12,
        linetype = "22"
      ),
      legend.text = element_text(size = 5.0),
      legend.key.width = grid::unit(8, "mm"),
      legend.key.height = grid::unit(2.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      plot.margin = margin(1.0, 0.7, 0.5, 0.7, "mm")
    ) +
    guides(
      fill = "none",
      linetype = guide_legend(
        override.aes = list(colour = "#222222", linewidth = 0.5)
      )
    )

  if (!show_y_axis) {
    panel <- panel +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )
  }

  panel
}

make_cumulative_population_panel <- function(
    target_year,
    region_code,
    tag = NULL,
    show_y_axis = FALSE,
    show_y_title = FALSE) {
  region_label <- unname(r5_labels[[region_code]])
  region_title <- unname(r5_panel_labels[[region_code]])
  panel_data <- food_distribution_r5 |>
    filter(
      year == target_year,
      r5 == region_code,
      food_consumption <= distribution_x_upper
    )
  mder_data <- mder_r5 |>
    filter(year == target_year, r5 == region_code)

  panel <- ggplot(
    panel_data,
    aes(
      x = food_consumption,
      y = cumulative_population_share,
      linetype = scenario
    )
  ) +
    geom_vline(
      data = mder_data,
      aes(xintercept = mder, linetype = "MDER"),
      colour = "#222222",
      linewidth = 0.38,
      inherit.aes = FALSE
    ) +
    geom_line(
      colour = unname(r5_colors[[region_label]]),
      linewidth = 0.46,
      lineend = "round"
    ) +
    scale_linetype_manual(
      values = c(Baseline = "42", `400C` = "solid", MDER = "13"),
      breaks = c("Baseline", "400C", "MDER")
    ) +
    scale_x_continuous(
      breaks = distribution_x_breaks,
      labels = scales::label_number(accuracy = 1, big.mark = ""),
      expand = expansion(mult = c(0, 0.01))
    ) +
    scale_y_continuous(
      breaks = distribution_y_breaks,
      labels = scales::label_number(accuracy = 1, big.mark = ","),
      expand = expansion(mult = c(0, 0.02))
    ) +
    coord_cartesian(
      xlim = c(0, distribution_x_upper),
      ylim = c(0, distribution_y_upper)
    ) +
    labs(
      tag = tag,
      title = region_title,
      x = "Food consumption\n(kcal per person per day)",
      y = if (show_y_title) {
        "Cumulative population\n(% of regional population)"
      } else {
        NULL
      },
      linetype = NULL
    ) +
    theme_interim_nature(base_size = 5.3, grid = "both", legend_position = "bottom") +
    panel_tag_theme +
    theme(
      plot.title = element_text(
        face = "bold",
        size = 5.5,
        colour = unname(r5_colors[[region_label]]),
        hjust = 0.5,
        margin = margin(b = 0.5, unit = "mm")
      ),
      plot.tag = element_text(face = "bold", size = 7.2),
      plot.tag.position = c(0.10, 0.98),
      axis.title = element_text(face = "plain", size = 5.1),
      axis.title.y = if (show_y_title) {
        element_text(margin = margin(r = 1.5))
      } else {
        element_blank()
      },
      axis.text.x = element_text(size = 4.2),
      axis.text.y = element_text(size = 4.5),
      axis.ticks.length = grid::unit(1, "mm"),
      panel.grid.major = element_line(
        colour = "#C8C8C8",
        linewidth = 0.16,
        linetype = "22"
      ),
      legend.text = element_text(size = 5.0),
      legend.key.width = grid::unit(8, "mm"),
      legend.key.height = grid::unit(2.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      plot.margin = margin(1.0, 0.7, 0.5, 0.7, "mm")
    ) +
    guides(
      linetype = guide_legend(
        override.aes = list(colour = "#222222", linewidth = 0.46)
      )
    )

  if (!show_y_axis) {
    panel <- panel +
      theme(
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank()
      )
  }

  panel
}

make_hunger_population_panel <- function(target_year) {
  panel_data <- hunger_population_effects |>
    filter(year == target_year) |>
    mutate(
      region_x = match(
        as.character(region_label),
        unname(r5_labels)
      ),
      after_income = baseline + income,
      after_price = after_income + price,
      x_baseline = region_x - 0.33,
      x_income = region_x - 0.11,
      x_price = region_x + 0.11,
      x_cross = region_x + 0.33
    )

  baseline_segments <- panel_data |>
    transmute(
      region_x,
      x = x_baseline,
      component = factor(
        "Baseline",
        levels = c("Baseline", unname(effect_labels))
      ),
      y_start = 0,
      y_end = baseline
    )

  effect_segments <- bind_rows(
    panel_data |>
      transmute(
        region_x,
        x = x_income,
        component = "Income",
        y_start = baseline,
        y_end = after_income
      ),
    panel_data |>
      transmute(
        region_x,
        x = x_price,
        component = "Own-price",
        y_start = after_income,
        y_end = after_price
      ),
    panel_data |>
      transmute(
        region_x,
        x = x_cross,
        component = "Cross-price",
        y_start = after_price,
        y_end = total_400c
      )
  ) |>
    mutate(
      component = factor(
        component,
        levels = c("Baseline", unname(effect_labels))
      )
    )

  connector_segments <- bind_rows(
    panel_data |>
      transmute(y = baseline, x_start = x_baseline, x_end = x_income),
    panel_data |>
      transmute(y = after_income, x_start = x_income, x_end = x_price),
    panel_data |>
      transmute(y = after_price, x_start = x_price, x_end = x_cross)
  )

  component_colors <- c(Baseline = scenario_colors[["Baseline"]], effect_colors)

  ggplot() +
    geom_segment(
      data = connector_segments,
      aes(x = x_start, xend = x_end, y = y, yend = y),
      linewidth = 0.22,
      colour = "#777777",
      linetype = "22"
    ) +
    geom_segment(
      data = baseline_segments,
      aes(
        x = x,
        xend = x,
        y = y_start,
        yend = y_end,
        colour = component
      ),
      linewidth = 1.45,
      lineend = "butt"
    ) +
    geom_segment(
      data = effect_segments,
      aes(
        x = x,
        xend = x,
        y = y_start,
        yend = y_end,
        colour = component
      ),
      linewidth = 1.45,
      lineend = "butt"
    ) +
    geom_point(
      data = panel_data,
      aes(x = x_cross, y = total_400c),
      shape = 18,
      size = 1.8,
      colour = scenario_colors[["400C"]]
    ) +
    geom_text(
      data = panel_data,
      aes(
        x = x_cross,
        y = total_400c,
        label = scales::number(total_400c, accuracy = 1)
      ),
      vjust = -0.55,
      size = 1.45,
      colour = scenario_colors[["400C"]]
    ) +
    scale_colour_manual(values = component_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = 1:5,
      labels = c(
        "OECD90\n+ EU", "Asia", "Latin\nAmerica", "REs",
        "Middle East\n& Africa"
      ),
      expand = expansion(add = 0.52)
    ) +
    scale_y_continuous(
      labels = scales::label_number(accuracy = 1),
      expand = expansion(mult = c(0, 0.12))
    ) +
    labs(
      tag = "b",
      title = "Population at risk of hunger",
      x = NULL,
      y = "Population at risk of hunger\n(million)",
      colour = NULL
    ) +
    theme_interim_nature(base_size = 5.3, grid = "y", legend_position = "bottom") +
    panel_tag_theme +
    theme(
      plot.title = element_text(face = "plain", size = 6.0, hjust = 0.5),
      plot.tag = element_text(face = "bold", size = 7.2),
      plot.tag.position = c(0, 1),
      axis.title = element_text(face = "plain", size = 5.1),
      axis.text.x = element_text(size = 4.0, lineheight = 0.9),
      axis.text.y = element_text(size = 4.5),
      axis.ticks.x = element_blank(),
      axis.ticks.length = grid::unit(1, "mm"),
      axis.line.x = element_blank(),
      legend.text = element_text(size = 4.8),
      legend.key.width = grid::unit(7, "mm"),
      legend.key.height = grid::unit(2.5, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.box.spacing = grid::unit(0.4, "mm"),
      plot.margin = margin(1.5, 1, 1, 1.5, "mm")
    ) +
    guides(
      colour = guide_legend(
        nrow = 2,
        byrow = TRUE,
        override.aes = list(linewidth = 1.8)
      )
    )
}

make_figure <- function(target_year, stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)
  region_panels <- lapply(seq_along(r5_codes), function(index) {
    make_region_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "c" else NULL,
      show_y_axis = index == 1L,
      stack_by = stack_by
    )
  })

  region_row <- wrap_plots(
    region_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(legend.position = "bottom")

  top_row <- (
    make_map_panel(target_year) |
      make_hunger_population_panel(target_year)
  ) +
    plot_layout(widths = c(3.35, 1))

  (top_row / region_row) +
    plot_layout(heights = c(1.65, 1), guides = "keep")
}

make_inset_absolute_rate_figure <- function(
    target_year,
    stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)
  make_compact_region_panel <- function(
      region_code,
      tag = NULL,
      show_y_title = FALSE) {
    make_region_panel(
      target_year = target_year,
      region_code = region_code,
      tag = tag,
      show_y_axis = TRUE,
      show_y_title = show_y_title,
      stack_by = stack_by
    ) +
      theme(
        legend.position = "none",
        plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        plot.title = element_text(size = 5.1),
        axis.title.y = if (show_y_title) {
          element_text(size = 4.5, margin = margin(r = 1.0))
        } else {
          element_blank()
        },
        axis.text.y = element_text(size = 3.9),
        axis.text.x = element_text(size = 3.9),
        plot.margin = margin(0.8, 0.8, 0.8, 0.8, "mm")
      )
  }

  legend_data <- tibble::tibble(
    commodity = factor(
      unname(food_item_labels),
      levels = unname(food_item_labels)
    ),
    x = seq_along(food_item_labels),
    y = 1
  )
  commodity_legend <- ggplot(
    legend_data,
    aes(x = x, y = y, fill = commodity)
  ) +
    geom_point(shape = 22, size = 0, alpha = 0) +
    scale_fill_manual(values = commodity_colors, drop = FALSE) +
    guides(
      fill = guide_legend(
        nrow = 1,
        byrow = TRUE,
        override.aes = list(size = 3.2, alpha = 1, colour = NA)
      )
    ) +
    theme_void(base_size = 5.5, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 5.2),
      legend.key.width = grid::unit(4.4, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, 0, 0, 0)
    )

  map_upper <- ceiling(
    max(
      hunger_absolute$value[hunger_absolute$year == target_year],
      na.rm = TRUE
    ) / 25
  ) * 25
  fill_legend_data <- tibble::tibble(
    x = c(1, 2),
    y = 1,
    value = c(0, map_upper)
  )
  fill_legend <- ggplot(
    fill_legend_data,
    aes(x = x, y = y, fill = value)
  ) +
    geom_point(shape = 22, size = 0, alpha = 0) +
    scale_fill_gradientn(
      colours = c(
        "#FFFFFF", "#FEEBE2", "#FCAE91", "#FB6A4A", "#CB181D", "#7F0000"
      ),
      limits = c(0, map_upper),
      breaks = pretty(c(0, map_upper), n = 5),
      labels = scales::label_number(accuracy = 1),
      trans = "sqrt",
      oob = scales::squish,
      name = "Population at risk of hunger (million)"
    ) +
    guides(
      fill = guide_colorbar(
        title.position = "top",
        title.hjust = 0.5,
        direction = "horizontal",
        barwidth = grid::unit(66, "mm"),
        barheight = grid::unit(2.3, "mm"),
        ticks.colour = "#333333",
        frame.colour = "#555555"
      )
    ) +
    theme_void(base_size = 5.5, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "plain", size = 5.8),
      legend.text = element_text(size = 5.1),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, 0, 0, 0)
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
        nrow = 2,
        byrow = TRUE,
        override.aes = list(linewidth = 1.2, alpha = 1)
      )
    ) +
    theme_void(base_size = 5.5, base_family = "Arial") +
    theme(
      legend.position = "bottom",
      legend.title = element_text(face = "plain", size = 5.8),
      legend.text = element_text(size = 5.1),
      legend.key.width = grid::unit(8, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      plot.margin = margin(0, 0, 0, 0)
    )

  map_canvas <- make_absolute_map_panel(target_year) +
    theme(
      legend.position = "none",
      plot.tag.position = c(0.25, 0.985),
      plot.margin = margin(0, 0, 0, 0, "mm")
    )

  map_canvas <- map_canvas + inset_element(
    make_compact_region_panel(
      "R5OECD90+EU",
      tag = "c",
      show_y_title = TRUE
    ),
    left = 0.005, bottom = 0.690, right = 0.230, top = 0.985,
    align_to = "full", clip = TRUE
  )
  map_canvas <- map_canvas + inset_element(
    make_compact_region_panel("R5REF"),
    left = 0.770, bottom = 0.690, right = 0.995, top = 0.985,
    align_to = "full", clip = TRUE
  )
  map_canvas <- map_canvas + inset_element(
    make_compact_region_panel("R5LAM"),
    left = 0.005, bottom = 0.370, right = 0.230, top = 0.665,
    align_to = "full", clip = TRUE
  )
  map_canvas <- map_canvas + inset_element(
    make_compact_region_panel("R5ASIA"),
    left = 0.770, bottom = 0.370, right = 0.995, top = 0.665,
    align_to = "full", clip = TRUE
  )
  map_canvas <- map_canvas + inset_element(
    make_compact_region_panel("R5MAF"),
    left = 0.005, bottom = 0.050, right = 0.230, top = 0.345,
    align_to = "full", clip = TRUE
  )
  map_canvas <- map_canvas + inset_element(
    make_hunger_rate_panel(target_year),
    left = 0.755, bottom = 0.015, right = 0.995, top = 0.350,
    align_to = "full", clip = TRUE
  )

  legend_stack <- wrap_plots(
    fill_legend,
    outline_legend,
    commodity_legend,
    ncol = 1,
    heights = c(0.95, 1.40, 0.85)
  )
  map_canvas <- map_canvas + inset_element(
    legend_stack,
    left = 0.245, bottom = 0.005, right = 0.745, top = 0.315,
    align_to = "full", clip = FALSE
  )

  map_canvas
}

make_absolute_rate_figure <- function(
    target_year,
    stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)

  region_panels <- lapply(seq_along(r5_codes), function(index) {
    make_region_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "c" else NULL,
      show_y_axis = index == 1L,
      stack_by = stack_by
    )
  })

  region_row <- wrap_plots(
    region_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom",
      plot.margin = margin(0.5, 0.7, 0.5, 0.7, "mm")
    )

  map_panel <- make_absolute_map_panel(target_year) +
    theme(plot.margin = margin(0.5, 1.5, 0.5, 0.5, "mm"))
  rate_panel <- make_hunger_rate_panel(target_year) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "mm"))

  top_row <- (map_panel | rate_panel) +
    plot_layout(widths = c(3.45, 1), guides = "keep")

  (top_row / region_row) +
    plot_layout(heights = c(1.70, 1), guides = "keep")
}

make_absolute_rate_mder_figure <- function(
    target_year,
    stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)

  consumption_panels <- lapply(seq_along(r5_codes), function(index) {
    make_consumption_mder_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "c" else NULL,
      show_y_axis = index == 1L,
      show_y_title = index == 1L
    )
  })
  consumption_row <- wrap_plots(
    consumption_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom",
      plot.margin = margin(0.5, 0.7, 0.5, 0.7, "mm")
    )

  change_panels <- lapply(seq_along(r5_codes), function(index) {
    make_region_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "d" else NULL,
      show_y_axis = index == 1L,
      stack_by = stack_by
    )
  })
  change_row <- wrap_plots(
    change_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom",
      plot.margin = margin(0.5, 0.7, 0.5, 0.7, "mm")
    )

  map_panel <- make_absolute_map_panel(target_year) +
    theme(plot.margin = margin(0.5, 1.5, 0.5, 0.5, "mm"))
  rate_panel <- make_hunger_rate_panel(target_year) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "mm"))
  top_row <- (map_panel | rate_panel) +
    plot_layout(widths = c(3.45, 1), guides = "keep")

  (top_row / consumption_row / change_row) +
    plot_layout(heights = c(1.70, 0.95, 1), guides = "keep")
}

make_absolute_rate_distribution_figure <- function(
    target_year,
    stack_by = c("effect", "commodity")) {
  stack_by <- match.arg(stack_by)

  distribution_panels <- lapply(seq_along(r5_codes), function(index) {
    make_cumulative_population_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "d" else NULL,
      show_y_axis = index == 1L,
      show_y_title = index == 1L
    )
  })
  distribution_row <- wrap_plots(
    distribution_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom",
      plot.margin = margin(0.5, 1.5, 0.5, 1.5, "mm")
    )

  change_panels <- lapply(seq_along(r5_codes), function(index) {
    make_region_panel(
      target_year = target_year,
      region_code = r5_codes[[index]],
      tag = if (index == 1L) "c" else NULL,
      show_y_axis = index == 1L,
      stack_by = stack_by
    )
  })
  change_row <- wrap_plots(
    change_panels,
    nrow = 1,
    guides = "collect"
  ) &
    theme(
      legend.position = "bottom",
      plot.margin = margin(0.5, 0.7, 0.5, 0.7, "mm")
    )

  map_panel <- make_absolute_map_panel(target_year) +
    theme(plot.margin = margin(0.5, 1.5, 0.5, 0.5, "mm"))
  rate_panel <- make_hunger_rate_panel(target_year) +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 1.5, "mm"))
  top_row <- (map_panel | rate_panel) +
    plot_layout(widths = c(3.45, 1), guides = "keep")

  (top_row / change_row / distribution_row) +
    plot_layout(heights = c(1.70, 1, 0.95), guides = "keep")
}

if (!isTRUE(getOption("fig3.build_only", FALSE))) {
output_dir <- "output"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
existing_png <- list.files(
  output_dir,
  pattern = "^fig3_.*\\.png$",
  full.names = TRUE
)
if (length(existing_png) > 0) {
  file.remove(existing_png)
}

absolute_rate_figure <- make_absolute_rate_figure(
  target_years,
  stack_by = "commodity"
)
absolute_rate_output_path <- file.path(
  output_dir,
  paste0("fig3_regional_hunger_2050_", fdm_variant, ".png")
)

save_interim_plot(
  absolute_rate_figure,
  absolute_rate_output_path,
  height_mm = 125,
  dpi = 600,
  device = ragg::agg_png
)

message("Created ", absolute_rate_output_path)

absolute_rate_mder_figure <- make_absolute_rate_mder_figure(
  target_years,
  stack_by = "commodity"
)
absolute_rate_mder_output_path <- file.path(
  output_dir,
  paste0("fig3_regional_hunger_2050_with_food_mder_", fdm_variant, ".png")
)

save_interim_plot(
  absolute_rate_mder_figure,
  absolute_rate_mder_output_path,
  height_mm = 180,
  dpi = 600,
  device = ragg::agg_png,
  allow_oversize = TRUE
)

message("Created ", absolute_rate_mder_output_path)

absolute_rate_distribution_figure <- make_absolute_rate_distribution_figure(
  target_years,
  stack_by = "commodity"
)
absolute_rate_distribution_output_path <- file.path(
  output_dir,
  paste0(
    "fig3_regional_hunger_2050_with_food_distribution_",
    fdm_variant,
    ".png"
  )
)

save_interim_plot(
  absolute_rate_distribution_figure,
  absolute_rate_distribution_output_path,
  height_mm = 180,
  dpi = 600,
  device = ragg::agg_png,
  allow_oversize = TRUE
)

message("Created ", absolute_rate_distribution_output_path)
}
