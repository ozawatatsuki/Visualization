# Run from the project root: Rscript prog/fig3_presentation.R
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.build_only = TRUE)
source("prog/fig3_regional_hunger.R", encoding = "UTF-8")
options(fig3.build_only = NULL)
output_dir <- file.path("output", "presentation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
save_slide <- function(plot, name, height = 5.625) {
  if (isTRUE(getOption("fig3.presentation_data_only", FALSE))) return(invisible(NULL))
  ggsave(file.path(output_dir, paste0(name, ".png")), plot,
    width = 40/3, height = height, units = "in", dpi = 300,
    device = ragg::agg_png, bg = "white")
}
slide_theme <- function(size = 11) {
  theme(axis.title = element_text(size = size, face = "plain"),
    axis.title.y = element_text(size = size),
    axis.text.x = element_text(size = size - 1),
    axis.text.y = element_text(size = size - 1),
    plot.title = element_text(size = size + 1),
    plot.tag = element_text(size = 17, face = "bold"),
    legend.text = element_text(size = size - 1),
    legend.title = element_text(size = size),
    legend.key.width = grid::unit(6, "mm"),
    plot.margin = margin(3, 3, 3, 3, "mm"),
    panel.grid.major = element_line(colour = "#E4E4E4", linewidth = 0.2))
}
map_panel <- make_absolute_map_panel(2050) +
  theme(plot.tag = element_text(size = 18, face = "bold"),
    legend.text = element_text(size = 10), legend.title = element_text(size = 11),
    legend.key.width = grid::unit(6, "mm"),
    plot.margin = margin(3, 3, 3, 3, "mm")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE,
    title.position = "top", override.aes = list(linewidth = 1)))
rate_panel <- make_hunger_rate_panel(2050) + slide_theme(11)
for (i in seq_along(rate_panel$layers)) {
  if (inherits(rate_panel$layers[[i]]$geom, "GeomText"))
    rate_panel$layers[[i]]$aes_params$size <- 3
}
save_slide((map_panel | rate_panel) + plot_layout(widths = c(1.65, 1)),
           "fig3_1_panels_ab_2050")

# Use the model's country mapping for newly aggregated explanatory data.
model_map <- read.table(file.path("data", "FDM", fdm_folder,
  "region_iso_R17toR.map"), col.names = c("r17", "country")) |>
  mutate(r17 = sub("\\.$", "", r17)) |>
  left_join(r17_to_r5, by = "r17") |>
  select(country, r5)
# Six modeled countries omitted from the supplied map; memberships follow
# the country groups documented in prog/appendix_elasticities.R.
model_map <- bind_rows(model_map, tibble::tribble(
  ~country, ~r5,
  "ATG", "R5LAM", "GRD", "R5LAM", "KIR", "R5ASIA",
  "FSM", "R5ASIA", "SYC", "R5MAF", "SDN", "R5MAF"
))
stopifnot(!anyDuplicated(model_map$country), !anyNA(model_map))
price_effect$read(price_effect_path, symbols = c("FPchange", "priceEla"))
population <- price_effect["Population_seg"]$records |>
  tibble::as_tibble() |>
  transmute(scenario_code = as.character(Ref), country = as.character(R),
    year = as.integer(as.character(Y)), segment = as.character(seg2), population = value) |>
  filter(year == 2050, scenario_code == scenario_codes[["Baseline"]],
    segment != "ALL", population > 0) |>
  left_join(model_map, by = "country")
stopifnot(!anyNA(population))
country_population <- population |>
  group_by(country, r5) |> summarise(population = sum(population), .groups = "drop")
prices <- price_effect["FPchange"]$records |>
  tibble::as_tibble() |>
  transmute(scenario_code = as.character(Ref), country = as.character(R),
    year = as.integer(as.character(Y)), item = as.character(var2), value) |>
  filter(year == 2050, scenario_code %in% unname(scenario_codes)) |>
  inner_join(country_population, by = "country") |>
  pivot_wider(names_from = scenario_code, values_from = value) |>
  mutate(change = 100 * (.data[[scenario_codes[["400C"]]]] /
    .data[[scenario_codes[["Baseline"]]]] - 1)) |>
  group_by(r5, item) |>
  summarise(value = weighted.mean(change, population), countries = n(), .groups = "drop")
elasticity <- price_effect["priceEla"]$records |>
  tibble::as_tibble() |>
  transmute(scenario_code = as.character(Ref), country = as.character(R),
    year = as.integer(as.character(Y)), segment = as.character(seg),
    item = as.character(var), elasticity = value) |>
  filter(year == 2050, scenario_code == scenario_codes[["Baseline"]],
    segment != "ALL", item %in% names(food_item_labels))
# All positive-population country/segment cells must be represented for every food.
expected_elasticity <- tidyr::crossing(
  population |> select(country, segment), item = names(food_item_labels))
stopifnot(nrow(anti_join(expected_elasticity, elasticity,
  by = c("country", "segment", "item"))) == 0)
elasticity <- elasticity |>
  inner_join(population, by = c("scenario_code", "country", "year", "segment")) |>
  group_by(r5, item) |>
  summarise(value = weighted.mean(elasticity, population), .groups = "drop")
stopifnot(nrow(prices) == 35, nrow(elasticity) == 35,
  !anyNA(prices), !anyNA(elasticity))

# Population-weighted mixtures of country lognormal income distributions,
# consistent with Figure 2; display Baseline and 400C in each R5 region.
phi <- gamstransfer::Container$new()
phi$read("data/PHI/AnalysisExpenditure.gdx", symbols = c("Mu_exp", "sigma_exp"))
inp <- gamstransfer::Container$new()
inp$read("data/PHI/Inputdata.gdx", symbols = "Population")
read_phi <- function(container, symbol, name) {
  container[symbol]$records |> tibble::as_tibble() |>
    transmute(country = as.character(R), scenario_code = as.character(Ref),
      year = as.integer(as.character(Y)), !!name := value) |>
    filter(year == 2050, scenario_code %in% unname(scenario_codes))
}
parameters <- read_phi(phi, "Mu_exp", "mu") |>
  inner_join(read_phi(phi, "sigma_exp", "sigma"),
    by = c("country", "scenario_code", "year")) |>
  inner_join(read_phi(inp, "Population", "population"),
    by = c("country", "scenario_code", "year")) |>
  left_join(model_map, by = "country")
stopifnot(!anyNA(parameters), all(parameters$sigma > 0))
income_grid <- exp(seq(log(100), log(3e5), length.out = 500))
income_density <- parameters |>
  group_by(r5, scenario_code) |>
  group_modify(function(data, key) {
    tibble(income = income_grid, density = vapply(income_grid, function(x)
      weighted.mean(dlnorm(x, data$mu, data$sigma), data$population), numeric(1)))
  }) |> ungroup() |>
  mutate(scenario = factor(scenario_code, levels = unname(scenario_codes),
    labels = names(scenario_codes)))
stopifnot(nrow(income_density) == 5000, !anyNA(income_density))

small_theme <- theme_interim_nature(base_size = 10, grid = "y", legend_position = "bottom") +
  slide_theme(10) + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5),
    plot.margin = margin(2, 2, 2, 2, "mm"))
changes <- lapply(seq_along(r5_codes), function(i) {
  p <- make_region_panel(2050, r5_codes[i], if (i == 1) "c" else NULL,
    show_y_axis = i == 1, stack_by = "commodity") + slide_theme(10) +
    theme(axis.text.x = element_text(size = 8),
      plot.title = element_text(size = 11, hjust = 0.5))
  if (i != 1) p <- p + theme(axis.text.y = element_blank())
  p
})
change_row <- wrap_plots(changes, nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")
heatmap <- function(data, title, digits, legend_title) {
  data <- data |> mutate(region = factor(r5, levels = r5_codes),
    item_label = factor(unname(food_item_labels[item]), levels = rev(unname(food_item_labels))))
  ggplot(data, aes(region, item_label, fill = value)) +
    geom_tile(colour = "white", linewidth = 0.7) +
    geom_text(aes(label = sprintf(paste0("%.", digits, "f"), value)), size = 3) +
    scale_x_discrete(labels = c("OECD90\n+ EU", "Asia", "Latin\nAmerica", "REs", "Middle East\n& Africa")) +
    labs(title = title, x = NULL, y = NULL, fill = legend_title) +
    theme_minimal(base_size = 11, base_family = "Arial") +
    theme(panel.grid = element_blank(), axis.text = element_text(colour = "black", size = 10),
      plot.title = element_text(size = 12, face = "bold"), legend.position = "none")
}
price_panel <- heatmap(prices, "Price change (%) | 400C vs Baseline", 1, "%") +
  scale_fill_gradient2(low = "#9AC6DE", mid = "#FFFFFF", high = "#ECA1A9", midpoint = 0)
elasticity_panel <- heatmap(elasticity, "Own-price elasticity | Baseline", 2, NULL) +
  scale_fill_gradient(low = "#95BCD9", high = "#F5F8FA")
income_panels <- lapply(seq_along(r5_codes), function(i) {
  ggplot(filter(income_density, r5 == r5_codes[i]),
    aes(income, density, colour = scenario)) +
    geom_line(linewidth = 0.65) +
    scale_colour_manual(values = scenario_colors) +
    scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = c("100", "1k", "10k", "100k")) +
    scale_y_continuous(labels = scales::label_scientific(digits = 1),
      breaks = scales::breaks_pretty(3)) +
    labs(title = unname(r5_panel_labels[r5_codes[i]]), x = "Income per capita (log)",
      y = if (i == 1) "Income density" else NULL, colour = NULL) + small_theme
})
income_row <- wrap_plots(income_panels, nrow = 1, guides = "collect") &
  theme(legend.position = "bottom")
figure2 <- (change_row / (price_panel | elasticity_panel) / income_row) +
  plot_layout(heights = c(1.1, 1.05, 1)) +
  plot_annotation(caption = paste("2050 | Price changes: country population-weighted means.",
    "Elasticities: country and income-segment population-weighted means (Baseline)."),
    theme = theme(plot.caption = element_text(size = 9, hjust = 0)))
save_slide(figure2, "fig3_2_panel_c_and_drivers_2050", height = 7.5)

distribution_panels <- lapply(seq_along(r5_codes), function(i) {
  p <- make_cumulative_population_panel(2050, r5_codes[i],
    tag = if (i == 1) "d" else NULL, show_y_axis = i == 1, show_y_title = i == 1) +
    slide_theme(11) + theme(plot.title = element_text(size = 11, hjust = 0.5),
      axis.text.x = element_text(size = 9))
  if (i != 1) p <- p + theme(axis.text.y = element_blank(), axis.title.y = element_blank())
  p
})
save_slide(wrap_plots(distribution_panels, nrow = 1, guides = "collect") &
    theme(legend.position = "bottom"),
  "fig3_3_panel_d_appendix_2050")
message("Created three Figure 3 presentation PNGs.")
