suppressPackageStartupMessages({
  library(ggplot2)
  library(dplyr)
})

output_dir <- "output/260420"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
text_family <- "Yu Gothic"

# Schematic parameter settings for explanatory material
kappa <- 0
alpha <- 0.30
beta <- 0.85
gamma <- 35
VX <- 120
vx_minus_gamma <- VX - gamma
if (vx_minus_gamma <= 0) {
  stop("VX must be larger than gamma.")
}

# Finer utility grid for denser bar representation
u_values <- seq(-4, 4, by = 0.20)
phi_slope <- 1.10
phi_values <- alpha + (beta - alpha) / (1 + exp(-phi_slope * (u_values - kappa)))
excess_spending <- vx_minus_gamma * phi_values
total_spending <- gamma + excess_spending

plot_df <- tibble(
  U = u_values,
  phi = phi_values,
  excess = excess_spending,
  gamma = gamma,
  total = total_spending
)

alpha_line_total <- gamma + vx_minus_gamma * alpha
beta_line_total <- gamma + vx_minus_gamma * beta

alpha_char <- "\u03b1"
beta_char <- "\u03b2"
kappa_char <- "\u03ba"
sum_i_char <- "\u03a3"

alpha_color <- "#1f77b4"
beta_color <- "#d62728"
gamma_color <- "#4daf4a"
kappa_color <- "#6a3d9a"
vx_color <- "#444444"

y_max <- max(c(plot_df$total, VX)) * 1.08
y_breaks <- sort(unique(c(pretty(c(0, y_max), n = 7), alpha_line_total, beta_line_total)))
y_breaks <- y_breaks[abs(y_breaks - 60) > 1e-9]
y_labels <- vapply(
  y_breaks,
  function(y) {
    if (abs(y - alpha_line_total) < 1e-9) {
      return(paste0(sum_i_char, " (VX-gamma)*", alpha_char, "+gamma"))
    }
    if (abs(y - beta_line_total) < 1e-9) {
      return(paste0(sum_i_char, " (VX-gamma)*", beta_char, "+gamma"))
    }
    format(round(y, 1), trim = TRUE)
  },
  character(1)
)

gamma_label <- "gamma(\u6700\u4f4e\u652f\u51fa)"
excess_label <- "\u8d85\u904e\u652f\u51fa"
legend_df <- tibble(
  U = c(min(u_values), min(u_values)),
  y = c(0, 0),
  label = c(gamma_label, excess_label)
)

bar_width <- 0.17

p <- ggplot() +
  annotate(
    "rect",
    xmin = min(u_values) - 0.35,
    xmax = kappa,
    ymin = 0,
    ymax = y_max,
    fill = kappa_color,
    alpha = 0.05
  ) +
  geom_col(
    data = plot_df,
    aes(x = U, y = gamma),
    width = bar_width,
    fill = gamma_color,
    color = "grey25",
    linewidth = 0.18
  ) +
  geom_rect(
    data = plot_df,
    aes(
      xmin = U - bar_width / 2,
      xmax = U + bar_width / 2,
      ymin = gamma,
      ymax = total,
      fill = phi
    ),
    color = NA
  ) +
  geom_col(
    data = plot_df,
    aes(x = U, y = total),
    width = bar_width,
    fill = NA,
    color = "grey25",
    linewidth = 0.18
  ) +
  geom_hline(yintercept = alpha_line_total, color = alpha_color, linetype = "dashed", linewidth = 0.45) +
  geom_hline(yintercept = beta_line_total, color = beta_color, linetype = "dashed", linewidth = 0.45) +
  geom_hline(yintercept = VX, color = vx_color, linetype = "dotted", linewidth = 0.5) +
  geom_vline(xintercept = kappa, color = kappa_color, linetype = "dotdash", linewidth = 0.7) +
  annotate(
    "curve",
    x = min(u_values) + 0.9,
    y = alpha_line_total + 8,
    xend = min(u_values) + 0.1,
    yend = alpha_line_total + 0.6,
    curvature = 0.25,
    color = alpha_color,
    linewidth = 0.45,
    arrow = arrow(type = "closed", length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "label",
    x = -2.30,
    y = alpha_line_total + 6.8,
    label = "\u5c0f\u3055\u3044U\u3067\u03b1\u5074\u3078\u53ce\u675f",
    hjust = 0.5,
    color = alpha_color,
    fill = "white",
    linewidth = 0,
    size = 3.4,
    family = text_family,
    fontface = "bold",
    alpha = 0.88
  ) +
  annotate(
    "segment",
    x = max(u_values) + 0.42,
    xend = max(u_values) + 0.42,
    y = gamma,
    yend = VX,
    color = vx_color,
    linewidth = 0.6,
    arrow = arrow(ends = "both", type = "closed", length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "label",
    x = max(u_values) + 0.62,
    y = (gamma + VX) / 2,
    label = paste0(sum_i_char, "(VX-gamma)"),
    hjust = 0,
    color = vx_color,
    fill = "white",
    alpha = 0.88,
    linewidth = 0,
    size = 3.2,
    family = text_family,
    fontface = "bold"
  ) +
  annotate(
    "label",
    x = max(u_values) - 0.08,
    y = VX + 0.8,
    label = paste0(sum_i_char, " VX"),
    hjust = 1,
    color = vx_color,
    fill = "white",
    alpha = 0.88,
    linewidth = 0,
    size = 3.6,
    family = text_family,
    fontface = "bold"
  ) +
  annotate(
    "curve",
    x = 2.95,
    y = beta_line_total + 6.1,
    xend = max(u_values) - 0.1,
    yend = beta_line_total + 0.1,
    curvature = -0.12,
    color = beta_color,
    linewidth = 0.45,
    arrow = arrow(type = "closed", length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "label",
    x = 2.20,
    y = beta_line_total + 6.2,
    label = "\u5927\u304d\u3044U\u3067\u03b2\u5074\u3078\u53ce\u675f",
    hjust = 0.5,
    color = beta_color,
    fill = "white",
    linewidth = 0,
    size = 3.4,
    family = text_family,
    fontface = "bold",
    alpha = 0.88
  ) +
  annotate(
    "segment",
    x = min(u_values) - 0.25,
    xend = kappa,
    y = y_max * 0.95,
    yend = y_max * 0.95,
    color = kappa_color,
    linewidth = 0.6,
    arrow = arrow(ends = "last", type = "closed", length = grid::unit(0.10, "inches"))
  ) +
  annotate(
    "text",
    x = (min(u_values) + kappa) / 2 - 0.08,
    y = y_max * 0.985,
    label = paste0(kappa_char, ": \u52b9\u7528\u6c34\u6e96\u306e\u57fa\u6e96\u4f4d\u7f6e"),
    color = kappa_color,
    size = 3.6,
    fontface = "bold",
    family = text_family
  ) +
  annotate(
    "label",
    x = max(u_values) - 0.10,
    y = alpha_line_total,
    label = paste0(sum_i_char, " (VX-gamma)*", alpha_char, "+gamma"),
    hjust = 1,
    color = alpha_color,
    fill = "white",
    alpha = 0.88,
    linewidth = 0,
    size = 4.0,
    fontface = "bold",
    family = text_family
  ) +
  annotate(
    "label",
    x = max(u_values) - 0.10,
    y = beta_line_total + 0.7,
    label = paste0(sum_i_char, " (VX-gamma)*", beta_char, "+gamma"),
    hjust = 1,
    color = beta_color,
    fill = "white",
    alpha = 0.88,
    linewidth = 0,
    size = 4.0,
    fontface = "bold",
    family = text_family
  ) +
  annotate(
    "label",
    x = max(u_values) + 0.98,
    y = 74,
    label = paste0(sum_i_char, " VX: \u7dcf\u652f\u51fa\u984d\n", sum_i_char, " phi = 1"),
    hjust = 1,
    vjust = 1,
    color = vx_color,
    fill = "white",
    alpha = 0.90,
    linewidth = 0,
    size = 3.2,
    family = text_family,
    fontface = "bold"
  ) +
  geom_point(
    data = legend_df,
    aes(x = U, y = y, color = label),
    inherit.aes = FALSE,
    alpha = 0,
    size = 4,
    show.legend = TRUE
  ) +
  scale_fill_gradient(
    name = "\u8d85\u904e\u652f\u51fa\u306e\u8272",
    low = alpha_color,
    high = beta_color,
    limits = c(alpha, beta),
    breaks = c(alpha, (alpha + beta) / 2, beta),
    labels = c("\u03b1\u5074", "\u4e2d\u9593", "\u03b2\u5074"),
    oob = scales::squish
  ) +
  scale_color_manual(
    name = NULL,
    values = setNames(
      c(gamma_color, "#b45f6b"),
      c(gamma_label, excess_label)
    ),
    breaks = c(gamma_label, excess_label),
    drop = FALSE
  ) +
  scale_x_continuous(
    name = "\u52b9\u7528\u6c34\u6e96 U",
    breaks = seq(-4, 4, by = 1),
    minor_breaks = seq(-4, 4, by = 0.2),
    expand = expansion(mult = c(0.01, 0.13))
  ) +
  scale_y_continuous(
    name = "\u652f\u51fa\u984d",
    limits = c(0, y_max),
    breaks = y_breaks,
    labels = y_labels,
    expand = c(0, 0)
  ) +
  labs(
    title = NULL,
    subtitle = paste0("\u8d85\u904e\u652f\u51fa = phi(U)\u00d7", sum_i_char, "(VX-gamma), phi(U)\u306f\u5c0f\u3055\u3044U\u3067\u03b1\u3001\u5927\u304d\u3044U\u3067\u03b2\u3078\u53ce\u675f")
  ) +
  theme_bw(base_size = 12, base_family = text_family) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    plot.margin = margin(10, 40, 10, 36)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(alpha = 1, shape = 15, size = 7)
    ),
    fill = "none"
  )

p_no_labels <- ggplot() +
  annotate(
    "rect",
    xmin = min(u_values) - 0.35,
    xmax = kappa,
    ymin = 0,
    ymax = y_max,
    fill = kappa_color,
    alpha = 0.05
  ) +
  geom_col(
    data = plot_df,
    aes(x = U, y = gamma),
    width = bar_width,
    fill = gamma_color,
    color = "grey25",
    linewidth = 0.18
  ) +
  geom_rect(
    data = plot_df,
    aes(
      xmin = U - bar_width / 2,
      xmax = U + bar_width / 2,
      ymin = gamma,
      ymax = total,
      fill = phi
    ),
    color = NA
  ) +
  geom_col(
    data = plot_df,
    aes(x = U, y = total),
    width = bar_width,
    fill = NA,
    color = "grey25",
    linewidth = 0.18
  ) +
  geom_hline(yintercept = alpha_line_total, color = alpha_color, linetype = "dashed", linewidth = 0.45) +
  geom_hline(yintercept = beta_line_total, color = beta_color, linetype = "dashed", linewidth = 0.45) +
  geom_hline(yintercept = VX, color = vx_color, linetype = "dotted", linewidth = 0.5) +
  geom_vline(xintercept = kappa, color = kappa_color, linetype = "dotdash", linewidth = 0.7) +
  geom_point(
    data = legend_df,
    aes(x = U, y = y, color = label),
    inherit.aes = FALSE,
    alpha = 0,
    size = 4,
    show.legend = TRUE
  ) +
  scale_fill_gradient(
    name = "\u8d85\u904e\u652f\u51fa\u306e\u8272",
    low = alpha_color,
    high = beta_color,
    limits = c(alpha, beta),
    breaks = c(alpha, (alpha + beta) / 2, beta),
    labels = c("\u03b1\u5074", "\u4e2d\u9593", "\u03b2\u5074"),
    oob = scales::squish
  ) +
  scale_color_manual(
    name = NULL,
    values = setNames(
      c(gamma_color, "#b45f6b"),
      c(gamma_label, excess_label)
    ),
    breaks = c(gamma_label, excess_label),
    drop = FALSE
  ) +
  scale_x_continuous(
    name = "\u52b9\u7528\u6c34\u6e96 U",
    breaks = seq(-4, 4, by = 1),
    minor_breaks = seq(-4, 4, by = 0.2),
    expand = expansion(mult = c(0.01, 0.13))
  ) +
  scale_y_continuous(
    name = "\u652f\u51fa\u984d",
    limits = c(0, y_max),
    breaks = y_breaks,
    labels = y_labels,
    expand = c(0, 0)
  ) +
  labs(
    title = NULL,
    subtitle = paste0("\u8d85\u904e\u652f\u51fa = phi(U)\u00d7", sum_i_char, "(VX-gamma), phi(U)\u306f\u5c0f\u3055\u3044U\u3067\u03b1\u3001\u5927\u304d\u3044U\u3067\u03b2\u3078\u53ce\u675f")
  ) +
  theme_bw(base_size = 12, base_family = text_family) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 28),
    plot.subtitle = element_text(size = 14),
    legend.position = "bottom",
    legend.box = "horizontal",
    legend.title = element_blank(),
    plot.margin = margin(10, 40, 10, 36)
  ) +
  guides(
    color = guide_legend(
      order = 1,
      override.aes = list(alpha = 1, shape = 15, size = 7)
    ),
    fill = "none"
  )

ggsave(
  filename = file.path(output_dir, "03_AIDADS_parameter_schematic.png"),
  plot = p,
  width = 11,
  height = 6.5,
  dpi = 320
)

ggsave(
  filename = file.path(output_dir, "03_AIDADS_parameter_schematic.pdf"),
  plot = p,
  width = 11,
  height = 6.5,
  device = grDevices::cairo_pdf
)

ggsave(
  filename = file.path(output_dir, "03_AIDADS_parameter_schematic_no_labels.png"),
  plot = p_no_labels,
  width = 11,
  height = 6.5,
  dpi = 320
)

ggsave(
  filename = file.path(output_dir, "03_AIDADS_parameter_schematic_no_labels.pdf"),
  plot = p_no_labels,
  width = 11,
  height = 6.5,
  device = grDevices::cairo_pdf
)

cat("Saved:", file.path(output_dir, "03_AIDADS_parameter_schematic.png"), "\n")
cat("Saved:", file.path(output_dir, "03_AIDADS_parameter_schematic.pdf"), "\n")
cat("Saved:", file.path(output_dir, "03_AIDADS_parameter_schematic_no_labels.png"), "\n")
cat("Saved:", file.path(output_dir, "03_AIDADS_parameter_schematic_no_labels.pdf"), "\n")
