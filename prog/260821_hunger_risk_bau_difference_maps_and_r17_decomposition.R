suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(rnaturalearth)
  library(scales)
  library(sf)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx"))]
if (length(data_candidates) != 1L) {
  stop(
    "Expected exactly one data/260807_FDM* directory containing Fooddem_PriceEffect.gdx; found: ",
    length(data_candidates)
  )
}

source_gdx <- file.path(data_candidates[[1]], "Fooddem_PriceEffect.gdx")
out_root <- file.path(project_dir, "output", "260807_FDM_scenario_quantile")
map_dir <- file.path(out_root, "hunger_risk_population_world_maps_bau_difference")
r17_dir <- file.path(out_root, "hunger_risk_population_R17_WLD_decomposition")
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(r17_dir, recursive = TRUE, showWarnings = FALSE)

target_years <- c(2020L, 2050L, 2100L)
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2030CP_NoCC_No"
scenario_label <- "400C / 2030CP"

read_country_symbol <- function(symbol, value_name) {
  out <- as.data.table(rgdx.param(source_gdx, symbol))
  required <- c("Ref", "R", "Y", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    as.character(Ref) %in% c(baseline_ref, scenario_ref),
    .(
      Ref = as.character(Ref),
      R = as.character(R),
      Year = as.integer(as.character(Y)),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_name)
  out
}

standard_sparse <- read_country_symbol("PoU_In_Pr", "standard_value")
cr_sparse <- read_country_symbol("PoU_In_Pr_CR", "cr_value")
model_countries <- sort(unique(standard_sparse[Ref == baseline_ref, R]))
all_years <- sort(unique(standard_sparse$Year))
country_grid <- CJ(
  Ref = c(baseline_ref, scenario_ref),
  R = model_countries,
  Year = all_years,
  unique = TRUE
)

complete_country_symbol <- function(sparse_data, value_name) {
  out <- merge(
    country_grid,
    sparse_data,
    by = c("Ref", "R", "Year"),
    all.x = TRUE,
    sort = FALSE
  )
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

standard <- complete_country_symbol(standard_sparse, "standard_value")
cr <- complete_country_symbol(cr_sparse, "cr_value")
country_values <- merge(
  standard,
  cr,
  by = c("Ref", "R", "Year"),
  all = FALSE,
  sort = FALSE
)
bau <- country_values[
  Ref == baseline_ref,
  .(R, Year, bau_standard = standard_value, bau_cr = cr_value)
]
bau_gap <- max(abs(bau$bau_standard - bau$bau_cr))
if (!is.finite(bau_gap) || bau_gap > 1e-6) {
  stop("PoU_In_Pr and PoU_In_Pr_CR differ under BaU. Maximum gap: ", bau_gap)
}

map_wide <- merge(
  country_values[Ref == scenario_ref],
  bau[, .(R, Year, bau_value = bau_standard)],
  by = c("R", "Year"),
  all.x = TRUE,
  sort = FALSE
)
map_wide[, `:=`(
  In_Pr = (standard_value - bau_value) / 1e6,
  In_Pr_CR = (cr_value - bau_value) / 1e6
)]
map_data <- melt(
  map_wide[Year %in% target_years],
  id.vars = c("R", "Year"),
  measure.vars = c("In_Pr", "In_Pr_CR"),
  variable.name = "Panel",
  value.name = "bau_difference_million"
)
map_data[, Panel := factor(
  Panel,
  levels = c("In_Pr", "In_Pr_CR"),
  labels = c("PoU_In_Pr - BaU", "PoU_In_Pr_CR - BaU")
)]

read_ragg_symbol <- function(symbol, value_name) {
  out <- as.data.table(rgdx.param(source_gdx, symbol))
  required <- c("Ref", "R17", "Y", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    as.character(Ref) %in% c(baseline_ref, scenario_ref),
    .(
      Ref = as.character(Ref),
      R17 = as.character(R17),
      Year = as.integer(as.character(Y)),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_name)
  out
}

ragg_standard <- read_ragg_symbol("PoU_In_Pr_Ragg_calc", "ragg_standard")
ragg_cr <- read_ragg_symbol("PoU_In_Pr_CR_Ragg_calc", "ragg_cr")
ragg_check <- merge(
  ragg_standard,
  ragg_cr,
  by = c("Ref", "R17", "Year"),
  all = FALSE
)
ragg_bau <- ragg_check[
  Ref == baseline_ref & R17 == "WLD",
  .(Year, bau_standard = ragg_standard, bau_cr = ragg_cr)
]
if (max(abs(ragg_bau$bau_standard - ragg_bau$bau_cr)) > 1e-6) {
  stop("R17 BaU differs between PoU_In_Pr and PoU_In_Pr_CR.")
}

country_sums <- map_data[
  , .(country_difference = sum(bau_difference_million) * 1e6),
  by = .(Year, Panel)
]
ragg_scenario <- ragg_check[Ref == scenario_ref & R17 == "WLD"]
ragg_bau_base <- ragg_check[Ref == baseline_ref & R17 == "WLD"]
ragg_differences <- merge(
  ragg_scenario,
  ragg_bau_base[, .(Year, bau_value = ragg_standard)],
  by = "Year",
  all.x = TRUE
)
ragg_differences <- rbind(
  ragg_differences[, .(
    Year,
    Panel = factor("PoU_In_Pr - BaU", levels = levels(map_data$Panel)),
    WLD_difference = ragg_standard - bau_value
  )],
  ragg_differences[, .(
    Year,
    Panel = factor("PoU_In_Pr_CR - BaU", levels = levels(map_data$Panel)),
    WLD_difference = ragg_cr - bau_value
  )]
)
map_sum_check <- merge(
  country_sums,
  ragg_differences[Year %in% target_years],
  by = c("Year", "Panel"),
  all.x = TRUE
)
if (map_sum_check[
  abs(country_difference - WLD_difference) > pmax(1, abs(WLD_difference) * 1e-8),
  .N
] > 0L) {
  stop("Country-level BaU differences do not sum to WLD differences.")
}

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$admin != "Antarctica", ]
candidate_fields <- c("iso_a3_eh", "iso_a3", "adm0_a3")
candidate_fields <- candidate_fields[candidate_fields %in% names(world)]
world$map_code <- NA_character_
for (field in candidate_fields) {
  candidate <- as.character(world[[field]])
  use <- is.na(world$map_code) & candidate %in% model_countries
  world$map_code[use] <- candidate[use]
}
mapped_codes <- unique(stats::na.omit(world$map_code))
unmatched_codes <- setdiff(model_countries, mapped_codes)
if (length(unmatched_codes) > 0L) {
  stop("Natural Earth geometry is missing country codes: ", paste(unmatched_codes, collapse = ", "))
}

shared_abs_limit <- max(abs(map_data$bau_difference_million))
candidate_breaks <- c(-300, -200, -100, -50, -25, -10, -5, -1, 0, 1, 5, 10, 25, 50, 100, 200, 300)
legend_breaks <- candidate_breaks[abs(candidate_breaks) <= shared_abs_limit]
if (!0 %in% legend_breaks) {
  legend_breaks <- sort(c(legend_breaks, 0))
}
if (max(abs(legend_breaks)) < shared_abs_limit * 0.75) {
  legend_breaks <- sort(c(legend_breaks, -shared_abs_limit, shared_abs_limit))
}

make_sf_map_data <- function(year_value) {
  pieces <- lapply(levels(map_data$Panel), function(panel_value) {
    values <- map_data[Year == year_value & as.character(Panel) == panel_value]
    map_piece <- world
    idx <- match(map_piece$map_code, values$R)
    map_piece$bau_difference_million <- values$bau_difference_million[idx]
    map_piece$Panel <- factor(panel_value, levels = levels(map_data$Panel))
    map_piece
  })
  do.call(rbind, pieces)
}

map_manifest <- vector("list", length(target_years))
for (i in seq_along(target_years)) {
  year_value <- target_years[[i]]
  sf_data <- make_sf_map_data(year_value)
  p_map <- ggplot(sf_data) +
    geom_sf(
      aes(fill = bau_difference_million),
      colour = "white",
      linewidth = 0.08
    ) +
    facet_wrap(vars(Panel), nrow = 1) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "#F7F7F7",
      high = "#B2182B",
      midpoint = 0,
      limits = c(-shared_abs_limit, shared_abs_limit),
      breaks = legend_breaks,
      trans = pseudo_log_trans(sigma = 1),
      labels = label_number(accuracy = 1),
      na.value = "#D9D9D9",
      name = "Difference from BaU (million people)"
    ) +
    coord_sf(crs = "ESRI:54030", datum = NA, expand = FALSE) +
    guides(fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(11, "cm"),
      barheight = grid::unit(0.45, "cm"),
      ticks.colour = "#444444",
      frame.colour = "#777777"
    )) +
    labs(
      title = paste0("Change in population at risk of hunger from BaU in ", year_value),
      subtitle = paste0(scenario_label, "; red = increase, blue = decrease"),
      caption = "Source: Fooddem_PriceEffect.gdx; common symmetric pseudo-log scale across years and panels."
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#EAF2F5", colour = NA),
      strip.background = element_rect(fill = "#F2F2F2", colour = "#D0D0D0", linewidth = 0.4),
      strip.text = element_text(face = "bold", colour = "#222222", size = 12, margin = margin(7, 6, 7, 6)),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, colour = "#555555", hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(size = 8.5, colour = "#666666", hjust = 1, margin = margin(t = 7)),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.margin = margin(t = 2),
      panel.spacing = grid::unit(0.25, "cm"),
      plot.margin = margin(14, 16, 10, 16)
    )

  filename <- sprintf("hunger_risk_population_world_map_bau_difference_%d.png", year_value)
  ggsave(
    file.path(map_dir, filename),
    p_map,
    width = 14,
    height = 7.2,
    units = "in",
    dpi = 300,
    bg = "white"
  )
  map_manifest[[i]] <- data.table(
    year = year_value,
    scenario = scenario_label,
    panels = "PoU_In_Pr - BaU | PoU_In_Pr_CR - BaU",
    file = filename,
    model_country_count = length(model_countries),
    mapped_country_count = length(mapped_codes),
    shared_abs_limit_million = shared_abs_limit
  )
}

fwrite(
  map_data[, .(
    R,
    Year,
    Panel = as.character(Panel),
    bau_difference_million
  )],
  file.path(map_dir, "hunger_risk_population_world_map_bau_difference_data.csv")
)
fwrite(
  rbindlist(map_manifest),
  file.path(map_dir, "hunger_risk_population_world_map_bau_difference_manifest.csv")
)
fwrite(
  data.table(R = unmatched_codes),
  file.path(map_dir, "unmatched_model_country_codes.csv")
)

# R17 + WLD decomposition: Baseline + Income + Price + Cross price = PoU_In_Pr_CR.
income_ragg <- read_ragg_symbol("PoU_In_Ragg_calc", "income_absolute")
price_ragg <- read_ragg_symbol("PoU_In_Pr_Ragg_calc", "price_absolute")
cross_ragg <- read_ragg_symbol("PoU_In_Pr_CR_Ragg_calc", "cross_absolute")
ragg_all <- Reduce(
  function(x, y) merge(x, y, by = c("Ref", "R17", "Year"), all = TRUE, sort = FALSE),
  list(income_ragg, price_ragg, cross_ragg)
)
baseline_ragg <- ragg_all[
  Ref == baseline_ref,
  .(R17, Year, Baseline = income_absolute)
]
decomp_wide <- merge(
  ragg_all[Ref == scenario_ref],
  baseline_ragg,
  by = c("R17", "Year"),
  all.x = TRUE,
  sort = FALSE
)
decomp_wide[, `:=`(
  `Income effect` = income_absolute - Baseline,
  `Price effect` = price_absolute - income_absolute,
  `Cross price effect` = cross_absolute - price_absolute,
  Total = cross_absolute
)]
decomp_gap <- max(abs(
  decomp_wide$Baseline +
    decomp_wide[["Income effect"]] +
    decomp_wide[["Price effect"]] +
    decomp_wide[["Cross price effect"]] -
    decomp_wide$Total
))
if (!is.finite(decomp_gap) || decomp_gap > 1e-6) {
  stop("R17 decomposition failed. Maximum gap: ", decomp_gap)
}

r17_order <- c(
  "WLD", "R17XSA", "R17XER", "R17XNF", "R17XAF", "R17XLM",
  "R17CIS", "R17XOC", "R17XE25", "R17XME", "R17BRA", "R17XSE",
  "R17CAN", "R17CHN", "R17IND", "R17JPN", "R17TUR", "R17USA"
)
missing_r17 <- setdiff(r17_order, unique(decomp_wide$R17))
if (length(missing_r17) > 0L) {
  stop("Missing expected R17 regions: ", paste(missing_r17, collapse = ", "))
}
decomp_wide[, `:=`(
  R17 = factor(R17, levels = r17_order),
  Year_factor = factor(Year, levels = sort(unique(Year))),
  Total_million = Total / 1e6
)]
decomp_long <- melt(
  decomp_wide,
  id.vars = c("R17", "Year", "Year_factor", "Total_million"),
  measure.vars = c("Baseline", "Income effect", "Price effect", "Cross price effect"),
  variable.name = "Component",
  value.name = "Value"
)
decomp_long[, `:=`(
  Component = factor(
    Component,
    levels = c("Baseline", "Income effect", "Price effect", "Cross price effect")
  ),
  Value_million = Value / 1e6
)]

component_colors <- c(
  "Baseline" = "#7F7F7F",
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "Cross price effect" = "#009E73"
)
year_breaks <- as.character(c(2020, 2040, 2060, 2080, 2100))
p_r17 <- ggplot(
  decomp_long,
  aes(x = Year_factor, y = Value_million, fill = Component, group = Component)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.78,
    alpha = 0.9,
    colour = "grey30",
    linewidth = 0.08
  ) +
  geom_hline(yintercept = 0, colour = "grey35", linewidth = 0.3) +
  geom_line(
    data = unique(decomp_wide[, .(R17, Year_factor, Total_million)]),
    aes(x = Year_factor, y = Total_million, group = 1),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 0.45
  ) +
  geom_point(
    data = unique(decomp_wide[, .(R17, Year_factor, Total_million)]),
    aes(x = Year_factor, y = Total_million),
    inherit.aes = FALSE,
    shape = 21,
    size = 1.3,
    stroke = 0.35,
    fill = "white",
    colour = "black"
  ) +
  facet_wrap(vars(R17), ncol = 6, scales = "free_y", drop = FALSE) +
  scale_fill_manual(values = component_colors, drop = FALSE) +
  scale_x_discrete(breaks = year_breaks, drop = FALSE) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(
    title = "Population at risk of hunger: Baseline and effect decomposition",
    subtitle = paste0(
      scenario_label,
      "; black line = resulting PoU_In_Pr_CR. Negative effects are stacked below zero."
    ),
    x = "Year",
    y = "Population at risk of hunger / effect (million people)",
    fill = NULL,
    caption = "Source: Fooddem_PriceEffect.gdx"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey87", linewidth = 0.25),
    axis.text = element_text(colour = "grey25"),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    axis.title = element_text(colour = "grey10"),
    legend.position = "bottom",
    legend.title = element_blank(),
    plot.title = element_text(face = "bold", size = 17, hjust = 0.5),
    plot.subtitle = element_text(colour = "grey30", size = 10.5, hjust = 0.5),
    plot.caption = element_text(colour = "grey40", size = 8.5, hjust = 1),
    strip.text = element_text(face = "bold", colour = "grey10", size = 10),
    strip.background = element_rect(fill = "grey95", colour = "grey80", linewidth = 0.3),
    panel.spacing = grid::unit(0.35, "cm"),
    plot.margin = margin(10, 12, 8, 10)
  )

r17_filename <- "hunger_risk_population_R17_WLD_baseline_effects_2030CP.png"
ggsave(
  file.path(r17_dir, r17_filename),
  p_r17,
  width = 18,
  height = 11.5,
  units = "in",
  dpi = 300,
  bg = "white"
)
fwrite(
  decomp_long[, .(
    R17 = as.character(R17),
    Year,
    Component = as.character(Component),
    Value_million,
    Total_million
  )],
  file.path(r17_dir, "hunger_risk_population_R17_WLD_baseline_effects_2030CP_data.csv")
)
fwrite(
  data.table(
    scenario = scenario_label,
    regions = length(r17_order),
    years = length(unique(decomp_wide$Year)),
    components = "Baseline | Income effect | Price effect | Cross price effect",
    file = r17_filename,
    maximum_decomposition_gap_people = decomp_gap
  ),
  file.path(r17_dir, "hunger_risk_population_R17_WLD_baseline_effects_2030CP_manifest.csv")
)

cat("Created 3 BaU-difference map PNGs in:\n")
cat(normalizePath(map_dir, winslash = "/", mustWork = TRUE), "\n")
cat("Created the R17 + WLD decomposition PNG in:\n")
cat(normalizePath(r17_dir, winslash = "/", mustWork = TRUE), "\n")
