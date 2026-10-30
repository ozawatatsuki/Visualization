# Three geographic inset maps, exported as a single PNG.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.presentation_data_only = TRUE)
source("prog/fig3_presentation.R", encoding = "UTF-8")
options(fig3.presentation_data_only = NULL)
library(cowplot)
price_effect$read(price_effect_path, symbols = "budgetinitial")
budgets <- price_effect["budgetinitial"]$records |> tibble::as_tibble() |>
  transmute(segment = as.character(seg2), budget = value)
segment_weights <- population |> group_by(r5, segment) |>
  summarise(population = sum(population), .groups = "drop") |>
  left_join(budgets, by = "segment") |> group_by(r5) |>
  arrange(budget, .by_group = TRUE) |>
  mutate(cumulative_population = 100 * (cumsum(population) - 0.5 * population) / sum(population)) |>
  ungroup()
profiles <- price_effect["priceEla"]$records |> tibble::as_tibble() |>
  transmute(scenario_code = as.character(Ref), country = as.character(R),
    year = as.integer(as.character(Y)), segment = as.character(seg),
    item = as.character(var), value) |>
  filter(year == 2050, scenario_code == scenario_codes[["Baseline"]],
    segment != "ALL", item %in% names(food_item_labels)) |>
  inner_join(population, by = c("scenario_code", "country", "year", "segment")) |>
  group_by(r5, segment, item) |>
  summarise(value = weighted.mean(value, population), .groups = "drop") |>
  left_join(segment_weights, by = c("r5", "segment")) |>
  mutate(food = factor(unname(food_item_labels[item]), levels = unname(food_item_labels)))
stopifnot(!anyNA(profiles), n_distinct(profiles$r5) == 5,
  all(profiles$cumulative_population >= 0 & profiles$cumulative_population <= 100))
food_line_colors <- setNames(c("#B38C00", "#D55E00", "#0072B2", "#9B59B6",
  "#E69F00", "#009E73", "#555555"), unname(food_item_labels))
map_colors <- setNames(c("#B4C5E1", "#F0D399", "#A7D7C4", "#D2B7E1", "#DFC1B1"), r5_codes)
map_world <- world |> left_join(rename(model_map, model_r5 = r5),
  by = c("country_code" = "country")) |> mutate(map_region = coalesce(model_r5, r5))
background_map <- ggplot(map_world) +
  geom_sf(aes(fill = map_region), colour = "white", linewidth = 0.1) +
  scale_fill_manual(values = map_colors, guide = "none", na.value = "#EEEEEE") +
  coord_sf(datum = NA, expand = FALSE) + theme_void() +
  theme(plot.background = element_rect(fill = "#F5F7F8", colour = NA))

card_theme <- function(region) {
  theme_classic(base_size = 8, base_family = "Arial") +
    theme(plot.background = element_rect(fill = scales::alpha("white", 0.90),
        colour = unname(map_colors[region]), linewidth = 0.7),
      panel.background = element_rect(fill = "transparent", colour = NA),
      plot.title = element_text(size = 10, face = "bold", hjust = 0,
        colour = unname(r5_colors[r5_labels[region]])),
      axis.title = element_text(size = 8), axis.text = element_text(size = 7.5, colour = "#333333"),
      axis.line = element_line(linewidth = 0.25),
      axis.ticks = element_line(linewidth = 0.25),
      panel.grid.major.y = element_line(colour = "#EAEAEA", linewidth = 0.18),
      legend.position = "none", plot.margin = margin(4, 5, 4, 5))
}
make_card <- function(region, kind) {
  title <- unname(r5_panel_labels[region])
  if (kind == "income") {
    p <- ggplot(filter(income_density, r5 == region), aes(income, density, colour = scenario)) +
      geom_line(linewidth = 0.6) + scale_colour_manual(values = scenario_colors) +
      scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = c("100", "1k", "10k", "100k")) +
      scale_y_continuous(breaks = scales::breaks_pretty(3), labels = scales::label_scientific(digits = 1)) +
      labs(x = "Income per capita (log scale)", y = "Density")
  } else if (kind == "price") {
    data <- filter(prices, r5 == region) |>
      mutate(food = factor(unname(food_item_labels[item]), levels = rev(unname(food_item_labels))))
    p <- ggplot(data, aes(value, food, fill = food)) +
      geom_vline(xintercept = 0, colour = "grey45", linewidth = 0.25) +
      geom_col(width = 0.66) + scale_fill_manual(values = food_line_colors) +
      scale_x_continuous(limits = c(-25, 180), breaks = c(0, 50, 100, 150)) +
      labs(x = "Price change (%)", y = NULL)
  } else {
    p <- ggplot(filter(profiles, r5 == region), aes(cumulative_population, value, colour = food)) +
      geom_line(linewidth = 0.5) + scale_colour_manual(values = food_line_colors) +
      scale_x_continuous(limits = c(0, 100), breaks = c(0, 50, 100), expand = c(0.01, 0)) +
      coord_cartesian(ylim = range(profiles$value)) +
      labs(x = "Cumulative population (%)", y = "Own-price elasticity")
  }
  p + labs(title = title) + card_theme(region)
}
# Geographic anchors: OECD over North America, REF over northern Eurasia,
# Latin America in the southwest, MAF in the centre, Asia in the east.
positions <- tibble::tribble(~region, ~x, ~y, ~w, ~h,
  "R5OECD90+EU", .015, .58, .32, .30,
  "R5REF", .48, .65, .32, .30,
  "R5LAM", .06, .12, .32, .30,
  "R5MAF", .39, .25, .32, .30,
  "R5ASIA", .675, .015, .32, .30)
make_map <- function(kind, title, subtitle) {
  plot <- ggdraw() + draw_plot(background_map, x = 0, y = .11, width = 1, height = .77)
  for (i in seq_len(nrow(positions))) {
    q <- positions[i, ]
    plot <- plot + draw_plot(make_card(q$region, kind), q$x, .11 + .77 * q$y,
      q$w, .77 * q$h)
  }
  plot <- plot + draw_label(title, x = .02, y = .97, hjust = 0, size = 16, fontface = "bold") +
    draw_label(subtitle, x = .02, y = .925, hjust = 0, size = 10)
  if (kind == "income") {
    legend <- get_legend(ggplot(income_density, aes(income, density, colour = scenario)) +
      geom_line() + scale_colour_manual(values = scenario_colors) + theme_void() +
      theme(legend.position = "bottom", legend.text = element_text(size = 9)) + labs(colour = NULL))
  } else {
    legend <- get_legend(ggplot(profiles, aes(cumulative_population, value, colour = food)) +
      geom_line() + scale_colour_manual(values = food_line_colors) + theme_void() +
      theme(legend.position = "bottom", legend.text = element_text(size = 8),
        legend.key.width = grid::unit(4, "mm")) + labs(colour = NULL) +
      guides(colour = guide_legend(nrow = 2, byrow = TRUE)))
  }
  plot + draw_plot(legend, x = 0, y = .005, width = 1, height = .1)
}
positions <- tibble::tribble(~region, ~x, ~y, ~w, ~h,
  "R5OECD90+EU", .025, .54, .29, .39,
  "R5REF", .59, .54, .29, .39,
  "R5LAM", .045, .085, .29, .39,
  "R5MAF", .365, .085, .29, .39,
  "R5ASIA", .685, .085, .29, .39)
make_group <- function(region) {
  cards <- lapply(c("income", "price", "elasticity"), function(kind) {
    metric_title <- c(income = "1  Income distribution", price = "2  Food price change",
      elasticity = "3  Own-price elasticity")[[kind]]
    make_card(region, kind) + labs(title = metric_title) +
      theme(plot.title = element_text(size = 11, face = "bold"),
        axis.text = element_text(size = 9), axis.title = element_text(size = 9))
  })
  plots <- plot_grid(plotlist = cards, ncol = 1, align = "v")
  ggdraw() + draw_plot(plots, y = 0, height = .94) +
    draw_label(unname(r5_labels[region]), x = .02, y = .97,
      hjust = 0, size = 15, fontface = "bold", colour = unname(r5_colors[r5_labels[region]]))
}
combined <- ggdraw() + draw_plot(background_map, x = 0, y = .05, width = 1, height = .90)
for (i in seq_len(nrow(positions))) {
  q <- positions[i, ]
  combined <- combined + draw_plot(make_group(q$region), q$x, q$y, q$w, q$h)
}
food_legend_plot <- ggplot(profiles, aes(cumulative_population, value, colour = food)) +
  geom_line() + scale_colour_manual(values = food_line_colors) + theme_void() +
  theme(legend.position = "bottom", legend.text = element_text(size = 12),
    legend.key.width = grid::unit(7, "mm")) + labs(colour = NULL) +
  guides(colour = guide_legend(nrow = 1))
legend_gtable <- ggplotGrob(food_legend_plot)
food_legend <- legend_gtable$grobs[[which(legend_gtable$layout$name == "guide-box-bottom")]]
combined <- combined +
  draw_label("Regional drivers of food consumption change | 2050", x = .02, y = .98,
    hjust = 0, size = 22, fontface = "bold") +
  draw_label("Income: Baseline (grey) and 400C (red)  |  Prices: 400C vs Baseline (%)  |  Elasticities: Baseline, cumulative population (%)",
    x = .02, y = .952, hjust = 0, size = 12) +
  draw_plot(food_legend, x = .05, y = 0, width = .9, height = .055)
ggsave(file.path(output_dir, "fig3_2_panel_c_and_drivers_2050.png"), combined,
  width = 24, height = 14, units = "in", dpi = 300,
  device = ragg::agg_png, bg = "white")
