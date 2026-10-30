# Supplement for Figure 3a,b: additional people and their geographic sources.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig3.build_only = TRUE)
source("prog/fig3_regional_hunger.R", encoding = "UTF-8")
options(fig3.build_only = NULL)
regional <- hunger_population_effects |>
  filter(year == 2050) |>
  mutate(additional = total_400c - baseline,
    region_name = factor(unname(r5_labels[r5]), levels = rev(unname(r5_labels))))
regional_long <- regional |>
  select(r5, region_name, income, price, cross_price) |>
  pivot_longer(c(income, price, cross_price), names_to = "effect", values_to = "value") |>
  mutate(effect = factor(unname(effect_labels[effect]), levels = unname(effect_labels)))
country <- price_effect["PoU_In_Pr_CR"]$records |> tibble::as_tibble() |>
  transmute(scenario = as.character(Ref), country_code = as.character(R),
    year = as.integer(as.character(Y)), value = value / 1e6) |>
  filter(year == 2050, scenario %in% unname(scenario_codes)) |>
  select(-year) |> pivot_wider(names_from = scenario, values_from = value, values_fill = 0) |>
  mutate(additional = .data[[scenario_codes[["400C"]]]] - .data[[scenario_codes[["Baseline"]]]]) |>
  left_join(world |> sf::st_drop_geometry() |> select(country_code, name_long, r5) |>
    distinct(country_code, .keep_all = TRUE), by = "country_code") |>
  arrange(desc(additional)) |> slice_head(n = 15) |>
  mutate(country = factor(coalesce(name_long, country_code), levels = rev(coalesce(name_long, country_code))),
    region = factor(unname(r5_labels[r5]), levels = unname(r5_labels)))
stopifnot(nrow(regional) == 5, nrow(country) == 15, !anyNA(country$region),
  max(abs(regional$income + regional$price + regional$cross_price - regional$additional)) < 1e-8)
base_theme <- theme_interim_nature(base_size = 13, grid = "x", legend_position = "bottom") +
  theme(axis.title = element_text(size = 12, face = "plain"),
    axis.text = element_text(size = 11), plot.title = element_text(size = 15, face = "bold"),
    plot.subtitle = element_text(size = 11), plot.tag = element_text(size = 19, face = "bold"),
    panel.grid.major = element_line(colour = "#E5E5E5", linewidth = .25),
    legend.text = element_text(size = 11), legend.key.width = grid::unit(6, "mm"),
    plot.margin = margin(4, 7, 4, 4, "mm"))
p1 <- ggplot(regional_long, aes(value, region_name, fill = effect)) +
  geom_vline(xintercept = 0, linewidth = .4, colour = "grey35") +
  geom_col(width = .58) +
  geom_point(data = regional, aes(additional, region_name), inherit.aes = FALSE,
    shape = 18, size = 3.7) +
  scale_fill_manual(values = effect_colors) +
  scale_x_continuous(expand = expansion(mult = c(.08, .10))) +
  labs(tag = "a", title = "Which regions drive the increase?",
    subtitle = "Income, own-price and cross-price effects; diamonds show net change",
    x = "Additional population at risk of hunger (million)", y = NULL, fill = NULL) +
  base_theme
p2 <- ggplot(country, aes(additional, country, fill = region)) +
  geom_col(width = .65) +
  geom_text(aes(label = sprintf("%.1f", additional)), hjust = -.2, size = 3.5) +
  scale_fill_manual(values = r5_colors, drop = FALSE) +
  scale_x_continuous(expand = expansion(mult = c(0, .16))) +
  labs(tag = "b", title = "Which countries contribute most?",
    subtitle = "15 countries with the largest increases",
    x = "Additional population at risk of hunger (million)", y = NULL, fill = NULL) +
  base_theme + guides(fill = guide_legend(nrow = 3, byrow = TRUE))
figure <- (p1 | p2) + plot_layout(widths = c(1.15, 1)) +
  plot_annotation(title = "Explaining regional hunger risk in 2050",
    subtitle = "400C − Baseline | Complement to Figure 3a,b",
    caption = "These panels show changes in people; Figure 3a maps 400C levels, while Figure 3b shows population shares.",
    theme = theme(plot.title = element_text(size = 20, face = "bold"),
      plot.subtitle = element_text(size = 13), plot.caption = element_text(size = 10, hjust = 0)))
ggsave("output/presentation/fig3_1_explanation_2050.png", figure,
  width = 40/3, height = 7.5, units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
print(regional |> select(r5, baseline, total_400c, additional, income, price, cross_price))
