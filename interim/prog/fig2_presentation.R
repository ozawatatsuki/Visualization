# Run from the project root: Rscript prog/fig2_presentation.R
Sys.setlocale("LC_CTYPE", ".UTF-8")
options(fig2.build_only = TRUE)
source(file.path("prog", "fig2_food_consumption.R"), encoding = "UTF-8")
options(fig2.build_only = NULL)

# Match the current Figure 1 slide dimensions, preserving all five panels.
figure <- make_figure(target_years, stack_by = "effect") &
  theme(
    text = element_text(size = 12),
    axis.title = element_text(size = 11, face = "plain"),
    axis.title.y = element_text(size = 11),
    axis.text = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    legend.text = element_text(size = 11),
    legend.key.width = grid::unit(7, "mm"),
    legend.key.height = grid::unit(4, "mm"),
    plot.tag = element_text(size = 17, face = "bold"),
    panel.grid.major = element_line(colour = "#E4E4E4", linewidth = 0.25),
    plot.margin = margin(2, 3, 2, 3, "mm")
  )
output_dir <- file.path("output", "presentation")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
output_path <- file.path(output_dir,
  paste0("fig2_food_consumption_2050_effect_", fdm_variant, ".png"))
ggsave(output_path, figure, width = 40 / 3, height = 7.5 * 3 / 4,
       units = "in", dpi = 300, device = ragg::agg_png, bg = "white")
message("Created ", output_path)
