# Figure size policy for interim report charts.
FIG_WIDTH_MM <- 180
FIG_HEIGHT_MIN_MM <- 42.5
FIG_HEIGHT_MAX_MM <- 170

# Colorblind-safe palette based on Okabe-Ito colors, tuned for print figures.
INTERIM_PALETTE <- c(
  blue = "#0072B2",
  vermillion = "#D55E00",
  green = "#009E73",
  purple = "#CC79A7",
  orange = "#E69F00",
  sky = "#56B4E9",
  black = "#000000",
  grey = "#999999"
)

mm_to_in <- function(mm) {
  mm / 25.4
}

figure_size <- function(
    height_mm = FIG_HEIGHT_MAX_MM,
    allow_oversize = FALSE) {
  if (!is.numeric(height_mm) || length(height_mm) != 1 || is.na(height_mm)) {
    stop("height_mm must be a single numeric value.", call. = FALSE)
  }
  if (
    height_mm < FIG_HEIGHT_MIN_MM ||
      (!allow_oversize && height_mm > FIG_HEIGHT_MAX_MM)
  ) {
    stop(
      sprintf(
        "height_mm must be between %.1f and %.1f mm.",
        FIG_HEIGHT_MIN_MM,
        FIG_HEIGHT_MAX_MM
      ),
      call. = FALSE
    )
  }

  list(
    width_mm = FIG_WIDTH_MM,
    height_mm = height_mm,
    width_in = mm_to_in(FIG_WIDTH_MM),
    height_in = mm_to_in(height_mm)
  )
}

save_interim_plot <- function(
    plot,
    filename,
    height_mm = FIG_HEIGHT_MAX_MM,
    dpi = 300,
    device = NULL,
    bg = "white",
    allow_oversize = FALSE,
    ...) {
  size <- figure_size(height_mm, allow_oversize = allow_oversize)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for save_interim_plot().", call. = FALSE)
  }

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = size$width_in,
    height = size$height_in,
    units = "in",
    dpi = dpi,
    device = device,
    bg = bg,
    ...
  )

  invisible(size)
}

theme_interim_nature <- function(
    base_size = 7.5,
    base_family = "Arial",
    grid = c("y", "none", "x", "both"),
    legend_position = "right") {
  grid <- match.arg(grid)

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for theme_interim_nature().", call. = FALSE)
  }

  major_x <- if (grid %in% c("x", "both")) ggplot2::element_line(linewidth = 0.18) else ggplot2::element_blank()
  major_y <- if (grid %in% c("y", "both")) ggplot2::element_line(linewidth = 0.18) else ggplot2::element_blank()

  ggplot2::theme_classic(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = "black"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 1.5, hjust = 0),
      plot.subtitle = ggplot2::element_text(size = base_size, margin = ggplot2::margin(b = 4)),
      plot.caption = ggplot2::element_text(size = base_size - 1, colour = "grey25", hjust = 1),
      axis.title = ggplot2::element_text(size = base_size, face = "bold"),
      axis.text = ggplot2::element_text(size = base_size - 0.5, colour = "black"),
      axis.line = ggplot2::element_line(linewidth = 0.25, colour = "black"),
      axis.ticks = ggplot2::element_line(linewidth = 0.25, colour = "black"),
      axis.ticks.length = grid::unit(1.6, "mm"),
      panel.grid.major.x = major_x,
      panel.grid.major.y = major_y,
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(4, "mm"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = base_size),
      legend.text = ggplot2::element_text(size = base_size - 0.5),
      legend.key.size = grid::unit(3.5, "mm"),
      legend.background = ggplot2::element_blank(),
      legend.box.background = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(3, 4, 3, 3, "mm")
    )
}

scale_colour_interim <- function(..., values = INTERIM_PALETTE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale_colour_interim().", call. = FALSE)
  }
  ggplot2::scale_colour_manual(..., values = values)
}

scale_color_interim <- scale_colour_interim

scale_fill_interim <- function(..., values = INTERIM_PALETTE) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required for scale_fill_interim().", call. = FALSE)
  }
  ggplot2::scale_fill_manual(..., values = values)
}
