# Run from the project root: Rscript prog/fig4_presentation.R
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig4.build_only = TRUE)
source("prog/fig4_poverty_hunger_overlap.R", encoding = "UTF-8")
options(fig4.build_only = NULL)

presentation_theme <- theme(
  plot.tag = element_text(size = 17, face = "bold"),
  plot.title = element_text(size = 12),
  strip.text = ggtext::element_markdown(size = 11),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10),
  legend.key.width = grid::unit(7, "mm")
)
map_row <- ((poverty_map + presentation_theme) |
  (overlap_map + presentation_theme)) + plot_layout(widths = c(1, 1))
composition <- make_composition_panel() + presentation_theme + theme(
  axis.title = element_text(size = 11, face = "plain"),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  panel.grid.major = element_line(colour = "#E4E4E4", linewidth = 0.2))
figure <- (map_row / (outline_legend + presentation_theme) / composition) +
  plot_layout(heights = c(2.15, 0.16, 1))
output_dir <- file.path("output", "presentation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_path <- file.path(output_dir,
  paste0("fig4_poverty_hunger_overlap_2050_", fdm_variant, ".png"))
ggsave(output_path, figure, width = 40 / 3, height = 6.5,
       units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
message("Created ", output_path)
