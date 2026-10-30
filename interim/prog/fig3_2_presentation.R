# Final Figure 3_2: four mechanisms by five regions, no geographic background.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.presentation_data_only = TRUE)
source("prog/fig3_presentation.R", encoding = "UTF-8")
options(fig3.presentation_data_only = NULL)
price_effect$read(price_effect_path, symbols = "budgetinitial")
budgets <- price_effect["budgetinitial"]$records |> tibble::as_tibble() |>
  transmute(segment = as.character(seg2), budget = value)
weights <- population |> group_by(r5, segment) |>
  summarise(population = sum(population), .groups = "drop") |>
  left_join(budgets, by = "segment") |> group_by(r5) |>
  arrange(budget, .by_group = TRUE) |>
  mutate(cumulative_population = 100 * (cumsum(population) - .5 * population) / sum(population)) |>
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
  left_join(weights, by = c("r5", "segment")) |>
  mutate(food = factor(unname(food_item_labels[item]), levels = unname(food_item_labels)))
stopifnot(!anyNA(profiles))
food_colors <- setNames(c("#B38C00", "#D55E00", "#0072B2", "#9B59B6",
  "#E69F00", "#009E73", "#777777"), unname(food_item_labels))
price_data <- prices |> mutate(food = factor(unname(food_item_labels[item]),
  levels = rev(unname(food_item_labels))))
consumption <- food_commodities |> filter(year == 2050, quantile_code != "ALL")
bounds <- consumption |> group_by(region, quantile) |>
  summarise(lo = sum(pmin(value, 0)), hi = sum(pmax(value, 0)), .groups = "drop")
consumption_limits <- range(c(bounds$lo, bounds$hi, 0)) + c(-25, 15)
income_limit <- max(income_density$density) * 1.08
theme_grid <- theme_interim_nature(base_size = 11, grid = "y", legend_position = "none") +
  theme(plot.title = element_text(size = 12, face = "bold", hjust = .5),
    axis.title = element_text(size = 10, face = "plain"),
    axis.text = element_text(size = 9),
    panel.grid.major = element_line(colour = "#E5E5E5", linewidth = .22),
    plot.margin = margin(3, 3, 3, 3, "mm"))
row_plots <- lapply(1:4, function(row) {
  panels <- lapply(seq_along(r5_codes), function(i) {
    region <- r5_codes[i]
    if (row == 1) {
      p <- ggplot(filter(price_data, r5 == region), aes(value, food, fill = food)) +
        geom_vline(xintercept = 0, linewidth = .3, colour = "grey40") +
        geom_col(width = .65) + scale_fill_manual(values = food_colors) +
        scale_x_continuous(limits = c(-25, 180), breaks = c(0, 50, 100, 150)) +
        labs(x = "Price change (%)", y = NULL,
          title = unname(r5_panel_labels[region]))
    } else if (row == 2) {
      p <- ggplot(filter(profiles, r5 == region), aes(cumulative_population, value, colour = food)) +
        geom_line(linewidth = .65) + scale_colour_manual(values = food_colors) +
        scale_x_continuous(limits = c(0, 100), breaks = c(0, 25, 50, 75, 100), expand = c(.01, 0)) +
        coord_cartesian(ylim = range(profiles$value)) +
        labs(x = "Cumulative population (%)", y = if (i == 1) "Own-price elasticity" else NULL)
    } else if (row == 3) {
      p <- ggplot(filter(consumption, .data$region == .env$region), aes(quantile, value, fill = commodity)) +
        geom_hline(yintercept = 0, colour = "grey40", linewidth = .3) +
        geom_col(width = .73, colour = "white", linewidth = .12) +
        scale_fill_manual(values = food_colors) +
        scale_y_continuous(limits = consumption_limits, breaks = scales::breaks_pretty(4)) +
        scale_x_discrete(drop = TRUE) +
        labs(x = "Income decile", y = if (i == 1) "Food consumption change\n(kcal/person/day)" else NULL)
    } else {
      p <- ggplot(filter(income_density, r5 == region), aes(income, density, colour = scenario)) +
        geom_line(linewidth = .7) + scale_colour_manual(values = scenario_colors) +
        scale_x_log10(breaks = c(100, 1000, 10000, 100000), labels = c("100", "1k", "10k", "100k")) +
        scale_y_continuous(limits = c(0, income_limit), breaks = scales::breaks_pretty(3),
          labels = scales::label_scientific(digits = 1)) +
        labs(x = "Income per capita (log)", y = if (i == 1) "Income density" else NULL)
    }
    p <- p + theme_grid
    if (row == 1) p <- p + theme(plot.title = element_text(colour = unname(r5_colors[r5_labels[region]])))
    if (i != 1) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    p
  })
  wrap_plots(panels, nrow = 1, widths = rep(1, 5))
})
legend_plot <- ggplot(profiles, aes(cumulative_population, value, colour = food)) +
  geom_line() + scale_colour_manual(values = food_colors) + labs(colour = NULL) +
  theme_void() + theme(legend.position = "bottom", legend.text = element_text(size = 11)) +
  guides(colour = guide_legend(nrow = 1))
legend_table <- ggplotGrob(legend_plot)
legend <- legend_table$grobs[[which(legend_table$layout$name == "guide-box-bottom")]]
figure <- (row_plots[[1]] / row_plots[[2]] / row_plots[[3]] / row_plots[[4]] /
    wrap_elements(legend)) + plot_layout(heights = c(1.05, 1, 1, 1, .15)) +
  plot_annotation(title = "Regional drivers of food consumption change | 2050",
    subtitle = "Prices and consumption: 400C − Baseline | Elasticities: Baseline | Income: Baseline (grey), 400C (red)",
    caption = "Prices: population-weighted means. Elasticities: country-weighted curves by income segment; x = cumulative Baseline population. Common scales within each row.",
    theme = theme(plot.title = element_text(size = 18, face = "bold"),
      plot.subtitle = element_text(size = 10), plot.caption = element_text(size = 9, hjust = 0)))
ggsave("output/presentation/fig3_2_panel_c_and_drivers_2050.png", figure,
  width = 16, height = 11, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
