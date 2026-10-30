# Run from the project root: Rscript prog/fig1_presentation.R
# Reuse the report's data and panels without exporting report files.
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig1.build_only = TRUE)
source(file.path("prog", "fig1_global_hunger.R"), encoding = "UTF-8")
options(fig1.build_only = NULL)
stopifnot(requireNamespace("ggrepel", quietly = TRUE))

fdm$read(fdm_path, symbols = "PoU_In_Pr_CR")
study_country <- fdm["PoU_In_Pr_CR"]$records |>
  tibble::as_tibble() |>
  transmute(scenario_code = as.character(Ref), iso3 = as.character(R),
            year = as.integer(as.character(Y)), value = value / 1e6) |>
  filter(year == 2050, scenario_code %in% unname(scenario_codes))

mapping <- read.delim("data/HungerTool/country/FAOISO.map", header = FALSE,
  sep = "\t", quote = "\"", strip.white = TRUE,
  col.names = c("iso3", "separator", "fao_id")) |>
  transmute(iso3 = if_else(iso3 == "F351", "CHN", as.character(iso3)),
            fao_id = as.character(fao_id))
country_tool <- gamstransfer::Container$new()
country_tool$read("data/HungerTool/country/AIMHub.gdx", symbols = "Risk")
conventional_country <- country_tool["Risk"]$records |>
  tibble::as_tibble() |>
  transmute(method = as.character(U), scenario_code = as.character(S),
            fao_id = as.character(R), year = as.integer(as.character(T)), value) |>
  filter(method == "fao_based", year == 2050,
         scenario_code %in% unname(scenario_codes)) |>
  left_join(mapping, by = "fao_id")
stopifnot(!anyNA(conventional_country$iso3))

additional_risk <- function(data) {
  stopifnot(!anyDuplicated(data[c("iso3", "scenario_code")]))
  # GDX omits zero-valued parameter records. Complete scenarios only for
  # countries represented in this method; do not invent absent countries.
  data |>
    select(iso3, scenario_code, value) |>
    complete(iso3, scenario_code = unname(scenario_codes), fill = list(value = 0)) |>
    pivot_wider(names_from = scenario_code, values_from = value) |>
    transmute(iso3, additional = .data[[scenario_codes[["400C"]]]] -
                .data[[scenario_codes[["Baseline"]]]])
}
country_names <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") |>
  sf::st_drop_geometry() |>
  transmute(iso3 = as.character(adm0_a3), country = as.character(name_long)) |>
  distinct(iso3, .keep_all = TRUE)
study_delta <- additional_risk(study_country)
conventional_delta <- additional_risk(conventional_country)
country_comparison <- inner_join(
  rename(study_delta, this_study = additional),
  rename(conventional_delta, conventional_method = additional), by = "iso3") |>
  left_join(country_names, by = "iso3") |>
  mutate(country = coalesce(country, iso3),
         difference = this_study - conventional_method,
         distance_to_identity = abs(difference) / sqrt(2)) |>
  arrange(desc(distance_to_identity)) |>
  mutate(labelled = row_number() <= 8L)
stopifnot(nrow(country_comparison) > 100,
          all(is.finite(country_comparison$this_study)),
          all(is.finite(country_comparison$conventional_method)))
print(head(country_comparison, 12))
message("Matched countries: ", nrow(country_comparison),
        "; study only: ", nrow(anti_join(study_delta, conventional_delta, by = "iso3")),
        "; conventional only: ", nrow(anti_join(conventional_delta, study_delta, by = "iso3")))

limits <- range(c(0, country_comparison$this_study, country_comparison$conventional_method))
limits <- limits + c(-0.06, 0.16) * diff(limits)
panel_c <- ggplot(country_comparison, aes(conventional_method, this_study)) +
  geom_hline(yintercept = 0, linewidth = 0.3, colour = "grey80") +
  geom_vline(xintercept = 0, linewidth = 0.3, colour = "grey80") +
  geom_abline(slope = 1, intercept = 0, linetype = "22", linewidth = 0.6, colour = "grey45") +
  geom_point(size = 2.6, colour = "#4477AA", alpha = 0.65) +
  geom_point(data = filter(country_comparison, labelled), size = 3, colour = "#B2182B") +
  ggrepel::geom_text_repel(data = filter(country_comparison, labelled),
    aes(label = country), seed = 42, size = 4.5, family = "Arial",
    box.padding = 0.6, point.padding = 0.4, min.segment.length = 0,
    max.overlaps = Inf, max.iter = 20000, max.time = 3,
    segment.color = "grey50") +
  annotate("text", x = limits[2] * 0.86, y = limits[2] * 0.94,
           label = "1:1", size = 4.5, colour = "grey40") +
  scale_x_continuous(breaks = scales::breaks_pretty(5)) +
  scale_y_continuous(breaks = scales::breaks_pretty(5)) +
  coord_cartesian(xlim = limits, ylim = limits, expand = FALSE) +
  labs(tag = "c", title = "Additional hunger risk in 2050",
       subtitle = "400C − Baseline, by country",
       x = "HungerTool (million people)",
       y = "This study (million people)") +
  theme_interim_nature(base_size = 15, grid = "both", legend_position = "none") +
  theme(axis.title = element_text(face = "plain", size = 14),
        plot.tag = element_text(size = 19, face = "bold"),
        plot.title = element_text(size = 17),
        plot.subtitle = element_text(size = 14),
        panel.grid.major = element_line(colour = "#E4E4E4", linewidth = 0.25),
        plot.margin = margin(5, 5, 5, 5, "mm"))

# Preserve the report layout: main panel a on the left, b above c on the right.
slide_theme <- theme(text = element_text(size = 14),
  axis.title = element_text(size = 14, face = "plain"),
  axis.text = element_text(size = 12), legend.text = element_text(size = 12),
  axis.text.x = element_text(size = 12),
  panel.grid.major = element_line(colour = "#E4E4E4", linewidth = 0.25),
  legend.spacing.y = grid::unit(1, "mm"),
  plot.tag = element_text(size = 19, face = "bold"),
  legend.key.width = grid::unit(9, "mm"),
  plot.margin = margin(4, 4, 4, 4, "mm"))
panel_a <- panel_a + slide_theme +
  labs(y = "Additional hunger risk\n(million people)")
# Enlarge the direct method labels for projection.
for (i in seq_along(panel_a$layers)) {
  if (inherits(panel_a$layers[[i]]$geom, "GeomText")) {
    panel_a$layers[[i]]$aes_params$size <- 3.5
    panel_a$layers[[i]]$mapping$label <- aes(
      label = if_else(method == "Conventional method", "HungerTool", as.character(method))
    )$label
  }
}
panel_b <- panel_b + slide_theme + labs(x = NULL) +
  theme(axis.title = element_text(size = 11),
        axis.text = element_text(size = 10), axis.text.x = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.key.width = grid::unit(6, "mm"))
method_linetype_scale <- panel_b$scales$get_scales("linetype")
method_linetype_scale$labels <- c(
  "This study" = "This study", "Conventional method" = "HungerTool"
)
panel_c <- panel_c +
  labs(title = "Additional hunger risk, 2050",
       subtitle = "400C − Baseline",
       x = "HungerTool\n(million people)",
       y = "This study\n(million people)") +
  theme(axis.title = element_text(size = 10),
        axis.text = element_text(size = 9),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 10),
        plot.margin = margin(2, 4, 3, 2, "mm"))
for (i in seq_along(panel_c$layers)) {
  if (inherits(panel_c$layers[[i]]$geom, "GeomTextRepel") ||
      inherits(panel_c$layers[[i]]$geom, "GeomText")) {
    panel_c$layers[[i]]$aes_params$size <- 2.8
  }
}
right_column <- (panel_b / panel_c) + plot_layout(heights = c(1.04, 0.96))
fig1_presentation <- (panel_a | right_column) +
  plot_layout(widths = c(2, 1))
output_dir <- file.path("output", "presentation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
ggsave(file.path(output_dir, "fig1_global_hunger_ave.png"), fig1_presentation,
       width = 40 / 3, height = 7.5 * 3 / 4, units = "in", dpi = 300,
       device = ragg::agg_png, bg = "white")
