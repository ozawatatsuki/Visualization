# Regional comparison replacing the map-centred Figure 3 presentation.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.build_only = TRUE)
source("prog/fig3_regional_hunger.R", encoding = "UTF-8")
options(fig3.build_only = NULL)
region_names <- c("OECD90 + EU", "Asia", "Latin America", "Reforming economies", "Middle East & Africa")
short_names <- c("OECD90 + EU", "Asia", "Latin America", "Reforming economies", "Middle East & Africa")
regional <- hunger_population_effects |>
  filter(year == 2050) |>
  left_join(select(hunger_rate_effects, r5, year, rate_baseline = baseline, rate_400c = total_400c),
    by = c("r5", "year")) |>
  mutate(region = factor(unname(r5_labels[r5]), levels = rev(region_names)))
common_theme <- theme_interim_nature(base_size = 13, grid = "x", legend_position = "bottom") +
  theme(axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 11),
    plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11),
    plot.tag = element_text(size = 18, face = "bold"),
    panel.grid.major = element_line(colour = "#E6E6E6", linewidth = .25),
    legend.text = element_text(size = 11), legend.key.width = grid::unit(7, "mm"),
    plot.margin = margin(4, 6, 3, 4, "mm"))
make_comparison <- function(rate = TRUE) {
  data <- regional |> transmute(region,
    Baseline = if (rate) rate_baseline else baseline,
    `400C` = if (rate) rate_400c else total_400c)
  long <- data |> pivot_longer(c(Baseline, `400C`), names_to = "scenario", values_to = "value") |>
    mutate(scenario = factor(scenario, levels = c("Baseline", "400C")))
  ggplot(data, aes(y = region)) +
    geom_segment(aes(x = Baseline, xend = `400C`, yend = region), colour = "#BBBBBB", linewidth = 1.3) +
    geom_point(data = long, aes(x = value, colour = scenario, shape = scenario), size = 3.5) +
    geom_text(aes(x = `400C`, label = if (rate) sprintf("%.1f%%", `400C`) else sprintf("%.1f", `400C`)),
      nudge_y = .24, size = 3.5, colour = "#B2182B") +
    scale_colour_manual(values = c(Baseline = "#666666", `400C` = "#B2182B")) +
    scale_shape_manual(values = c(Baseline = 1, `400C` = 16)) +
    scale_x_continuous(limits = c(0, NA), expand = expansion(mult = c(.015, .13)),
      breaks = scales::breaks_pretty(5)) +
    labs(tag = if (rate) "a" else "b",
      title = if (rate) "How widespread is hunger risk?" else "How many people are at risk?",
      subtitle = "Baseline → 400C",
      x = if (rate) "Share of regional population (%)" else "Population at risk of hunger (million)",
      y = NULL, colour = NULL, shape = NULL) + common_theme
}
top <- (make_comparison(TRUE) | make_comparison(FALSE)) +
  plot_layout(guides = "collect") & theme(legend.position = "bottom")

# Use relative consumption changes and a common scale across all regions.
limits <- range(c(0, food_effects |> group_by(region, quantile) |>
  summarise(lo = sum(pmin(value, 0)), hi = sum(pmax(value, 0)), .groups = "drop") |>
  select(lo, hi) |> unlist()))
bottom_panels <- lapply(seq_along(r5_codes), function(i) {
  region_code <- r5_codes[i]
  p <- ggplot(filter(food_effects, year == 2050, region == region_code),
      aes(quantile, value, fill = effect)) +
    geom_hline(yintercept = 0, colour = "grey35", linewidth = .35) +
    geom_col(width = .73, colour = "white", linewidth = .12) +
    geom_vline(xintercept = 10.5, linetype = "22", colour = "grey50", linewidth = .3) +
    scale_fill_manual(values = effect_colors) +
    scale_y_continuous(limits = limits + c(-1, 1), breaks = scales::breaks_pretty(5)) +
    labs(tag = if (i == 1) "c" else NULL,
      title = unname(r5_panel_labels[region_code]),
      x = "Income decile", y = if (i == 1) "Food consumption change (%)" else NULL, fill = NULL) +
    theme_interim_nature(base_size = 11, grid = "y", legend_position = "bottom") +
    theme(plot.title = element_text(size = 11, face = "bold", hjust = .5,
        colour = unname(r5_colors[r5_labels[region_code]])),
      plot.tag = element_text(size = 18, face = "bold"),
      axis.title = element_text(size = 11, face = "plain"), axis.text.x = element_text(size = 9),
      panel.grid.major = element_line(colour = "#E6E6E6", linewidth = .25),
      legend.text = element_text(size = 11),
      plot.margin = margin(3, 3, 3, 3, "mm"))
  if (i != 1) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  p
})
bottom <- wrap_plots(bottom_panels, nrow = 1, guides = "collect") & theme(legend.position = "bottom")
figure <- (top / bottom) + plot_layout(heights = c(1.12, 1)) +
  plot_annotation(title = "Regional differences in hunger risk and food consumption | 2050",
    caption = "Top: hunger-risk levels in Baseline and 400C. Bottom: 400C − Baseline relative to Baseline; decile 1 = lowest income, Ave = regional average.",
    theme = theme(plot.title = element_text(size = 18, face = "bold"),
      plot.caption = element_text(size = 10, hjust = 0)))
ggsave("output/presentation/fig3_1_panels_ab_2050.png", figure,
  width = 40/3, height = 8, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
