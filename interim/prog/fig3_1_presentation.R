# Final Figure 3_1: original map/rate layout and regional effect decomposition.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.build_only = TRUE)
source("prog/fig3_regional_hunger.R", encoding = "UTF-8")
options(fig3.build_only = NULL)
map_panel <- make_absolute_map_panel(2050) +
  theme(plot.tag = element_text(size = 18, face = "bold"),
    legend.title = element_text(size = 10), legend.text = element_text(size = 9),
    legend.key.width = grid::unit(6, "mm")) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE, title.position = "top",
    override.aes = list(linewidth = 1)))
rate_data <- hunger_rate_effects |> filter(year == 2050) |>
  mutate(region = factor(r5, levels = r5_codes))
rate_long <- rate_data |>
  select(region, baseline, income, price, cross_price) |>
  pivot_longer(-region, names_to = "component", values_to = "value") |>
  mutate(component = factor(component,
    levels = c("baseline", "income", "price", "cross_price"),
    labels = c("Baseline", "Income", "Own-price", "Cross-price")))
rate_panel <- ggplot(rate_long, aes(region, value, fill = component)) +
  geom_hline(yintercept = 0, colour = "grey45", linewidth = .3) +
  geom_col(width = .58, position = position_stack(reverse = TRUE),
    colour = "white", linewidth = .15) +
  geom_point(data = rate_data, aes(region, total_400c), inherit.aes = FALSE,
    shape = 23, size = 3, fill = "#B2182B", colour = "white", stroke = .35) +
  geom_text(data = rate_data,
    aes(region, total_400c, label = sprintf("%.1f", total_400c)),
    inherit.aes = FALSE, nudge_x = .27, hjust = 0, size = 3.2, colour = "#B2182B") +
  scale_fill_manual(values = c(Baseline = "#666666", effect_colors)) +
  scale_x_discrete(labels = c("OECD90\n+ EU", "Asia", "Latin\nAmerica", "REs", "Middle East\n& Africa"),
    expand = expansion(add = c(.5, .9))) +
  scale_y_continuous(expand = expansion(mult = c(.05, .10))) +
  labs(tag = "b", title = "Population at risk of hunger",
    y = "Share of regional population (%)", x = NULL, fill = NULL) +
  theme_interim_nature(base_size = 11, grid = "y", legend_position = "bottom") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  theme(plot.tag = element_text(size = 18, face = "bold"),
    plot.title = element_text(size = 13), axis.title = element_text(size = 11),
    axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 10), legend.key.width = grid::unit(7, "mm"),
    panel.grid.major = element_line(colour = "#E5E5E5", linewidth = .25),
    plot.margin = margin(3, 5, 3, 4, "mm"))
regions <- lapply(seq_along(r5_codes), function(i) {
  p <- make_region_panel(2050, r5_codes[i], if (i == 1) "c" else NULL,
    show_y_axis = i == 1, stack_by = "effect") +
    labs(y = if (i == 1) "Food consumption change\nfrom Baseline (%)" else NULL,
      x = "Income decile") +
    theme(plot.tag = element_text(size = 18, face = "bold"),
      plot.title = element_text(size = 11), axis.title = element_text(size = 11),
      axis.text.x = element_text(size = 9), legend.text = element_text(size = 11),
      legend.key.width = grid::unit(7, "mm"),
      panel.grid.major = element_line(colour = "#E5E5E5", linewidth = .25),
      plot.margin = margin(3, 3, 3, 3, "mm"))
  if (i == 1) p <- p + theme(axis.text.y = element_text(size = 10))
  p
})
top <- (map_panel | rate_panel) + plot_layout(widths = c(1.9, 1))
bottom <- wrap_plots(regions, nrow = 1, guides = "collect") & theme(legend.position = "bottom")
figure <- (top / bottom) + plot_layout(heights = c(1.45, 1)) +
  plot_annotation(caption = "2050 | a: 400C. b: Baseline plus income, own-price and cross-price effects; red diamonds = 400C. c: 400C − Baseline; Ave = regional average.",
    theme = theme(plot.caption = element_text(size = 9, hjust = 0)))
ggsave("output/presentation/fig3_1_panels_ab_2050.png", figure,
  width = 40/3, height = 6.5, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
