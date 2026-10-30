suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(ragg)
  library(scales)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = FALSE)))
  }
  if (interactive()) {
    if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      path <- rstudioapi::getActiveDocumentContext()$path
      if (nzchar(path)) {
        return(dirname(normalizePath(path, winslash = "/", mustWork = FALSE)))
      }
    }
  }
  getwd()
}

script_dir <- get_script_dir()

gdx_file <- file.path(script_dir, "..", "data", "AIDADSCalibCrossCountry_SSP.gdx")
output_dir <- file.path(script_dir, "..", "output")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_file)) {
  stop(sprintf("GDX file was not found: %s", normalizePath(gdx_file, winslash = "/", mustWork = FALSE)))
}

raw <- as_tibble(rgdx.param(gdx_file, "PoutputIncomeEla"))
if (nrow(raw) == 0) {
  stop("PoutputIncomeEla has no records.")
}
if (!"PoutputIncomeEla" %in% names(raw)) {
  stop("Unexpected schema: value column PoutputIncomeEla was not found.")
}

df <- raw %>%
  rename(Value = PoutputIncomeEla) %>%
  mutate(
    SSP = as.character(SSP),
    Seg = as.character(Seg),
    Y = as.numeric(as.character(Y)),
    R = as.character(R),
    I = as.character(I),
    Value = as.numeric(Value)
  ) %>%
  arrange(I, R, Y)

if (anyNA(df$Y)) {
  stop("Some Y values could not be converted to numeric.")
}

ssp_label <- paste(sort(unique(df$SSP)), collapse = ", ")
seg_label <- paste(sort(unique(df$Seg)), collapse = ", ")

country_levels <- sort(unique(df$R))
df <- df %>%
  mutate(
    R_factor = factor(R, levels = country_levels),
    R_index = as.numeric(R_factor)
  )

p <- ggplot(df, aes(x = Y, y = Value, group = R, colour = R_index)) +
  geom_line(alpha = 0.5, linewidth = 0.3) +
  geom_point(alpha = 0.9, size = 0.6) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(
    option = "C",
    breaks = NULL,
    guide = "none"
  ) +
  scale_x_continuous(breaks = sort(unique(df$Y))) +
  labs(
    title = "PoutputIncomeEla (by I, lines by country)",
    subtitle = sprintf("SSP: %s | Seg: %s", ssp_label, seg_label),
    x = "Y",
    y = "Value",
    colour = "Country"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "bottom"
  )

out_png <- file.path(output_dir, "PoutputIncomeEla_byI_Y_Value.png")
ggsave(
  filename = out_png,
  plot = p,
  width = 20,
  height = 13,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Presentation-sized scatter (x = Y; thicker lines / larger points; 2:1 aspect).

p_y_thick <- ggplot(df, aes(x = Y, y = Value, group = R, colour = R_index)) +
  geom_line(alpha = 0.55, linewidth = 0.9) +
  geom_point(alpha = 0.85, size = 1.6) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(option = "C", breaks = NULL, guide = "none") +
  scale_x_continuous(limits = range(df$Y), breaks = sort(unique(df$Y))) +
  labs(
    title = "PoutputIncomeEla (x = Y; thick lines & points)",
    subtitle = sprintf("SSP: %s | Seg: %s", ssp_label, seg_label),
    x = "Y",
    y = "Value"
  ) +
  theme_bw(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.25),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    strip.text = element_text(size = 12),
    legend.position = "none"
  )

out_png_y_thick <- file.path(output_dir, "PoutputIncomeEla_byI_Y_thick_2to1.png")
ggsave(
  filename = out_png_y_thick,
  plot = p_y_thick,
  width = 20,
  height = 10,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png_y_thick, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Alternative plot (aligned x across panels; country-only gradient colouring).

p_country_gradient <- ggplot(df, aes(x = Y, y = Value, group = R, colour = R_index)) +
  geom_line(alpha = 0.55, linewidth = 0.35) +
  geom_point(alpha = 0.85, size = 0.7) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(option = "C", breaks = NULL, guide = "none") +
  scale_x_continuous(limits = range(df$Y), breaks = sort(unique(df$Y))) +
  labs(
    title = "PoutputIncomeEla (aligned x; gradient by country)",
    subtitle = sprintf("SSP: %s | Seg: %s", ssp_label, seg_label),
    x = "Y",
    y = "Value"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "none"
  )

out_png_country_gradient <- file.path(output_dir, "PoutputIncomeEla_byI_Y_aligned_countryGradient.png")
ggsave(
  filename = out_png_country_gradient,
  plot = p_country_gradient,
  width = 20,
  height = 13,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png_country_gradient, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Alternative plot (drop Y dimension; aggregate over years).

df_country_mean <- df %>%
  group_by(SSP, Seg, R, I) %>%
  summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    R_factor = factor(R, levels = country_levels),
    R_index = as.numeric(R_factor)
  )

p_country_mean <- ggplot(df_country_mean, aes(x = R_factor, y = Value, colour = R_index)) +
  geom_point(alpha = 0.9, size = 0.9) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(option = "C", breaks = NULL, guide = "none") +
  labs(
    title = "PoutputIncomeEla (mean over Y; gradient by country)",
    subtitle = sprintf("SSP: %s | Seg: %s | Aggregated over Y", ssp_label, seg_label),
    x = "Country (R)",
    y = "Value (mean over Y)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "none"
  )

out_png_country_mean <- file.path(output_dir, "PoutputIncomeEla_byI_Country_meanOverY_countryGradient.png")
ggsave(
  filename = out_png_country_mean,
  plot = p_country_mean,
  width = 24,
  height = 13,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png_country_mean, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Alternative plot (no x dimension; countries distinguished only by colour).

df_country_strip <- df_country_mean %>%
  mutate(x_dummy = 1)

p_country_strip <- ggplot(df_country_strip, aes(x = x_dummy, y = Value, colour = R_index)) +
  geom_point(
    alpha = 0.9,
    size = 0.9
  ) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(option = "C", breaks = NULL, guide = "none") +
  scale_x_continuous(breaks = NULL, limits = c(0.95, 1.05)) +
  labs(
    title = "PoutputIncomeEla (mean over Y; no x-axis dimension; gradient by country)",
    subtitle = sprintf("SSP: %s | Seg: %s | Aggregated over Y", ssp_label, seg_label),
    x = NULL,
    y = "Value (mean over Y)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "none"
  )

out_png_country_strip <- file.path(output_dir, "PoutputIncomeEla_byI_noX_countryGradient_meanOverY.png")
ggsave(
  filename = out_png_country_strip,
  plot = p_country_strip,
  width = 24,
  height = 13,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png_country_strip, winslash = "/", mustWork = FALSE), "\n")

# ---------------------------------------------------------------------------
# Alternative plot:
# - X axis: country (R)
# - shape by year (Y)
# - colour: gradient by Value (clipped to a visible range), with outliers in red
# - no lines (points only)

df2 <- df %>%
  mutate(
    Y_factor = factor(Y, levels = sort(unique(Y)))
  ) %>%
  group_by(I) %>%
  mutate(
    q1 = quantile(Value, 0.25, na.rm = TRUE),
    q3 = quantile(Value, 0.75, na.rm = TRUE),
    iqr = q3 - q1,
    outlier = Value < (q1 - 1.5 * iqr) | Value > (q3 + 1.5 * iqr),
    clip_lo = quantile(Value[!outlier], 0.05, na.rm = TRUE),
    clip_hi = quantile(Value[!outlier], 0.95, na.rm = TRUE),
    Value_clip = pmin(pmax(Value, clip_lo), clip_hi)
  ) %>%
  ungroup()

year_levels <- levels(df2$Y_factor)
shape_values <- c(16, 17, 15, 18, 3, 4, 7, 8, 0, 1, 2, 5)
if (length(year_levels) > length(shape_values)) {
  stop(sprintf(
    "Too many years (%d) for the fixed shape palette (%d). Please extend shape_values.",
    length(year_levels), length(shape_values)
  ))
}
shape_map <- setNames(shape_values[seq_along(year_levels)], year_levels)

p_country <- ggplot() +
  geom_point(
    data = df2 %>% filter(!outlier),
    mapping = aes(x = R, y = Value, shape = Y_factor, colour = Value_clip),
    alpha = 0.85,
    size = 0.9,
    stroke = 0.25,
    position = position_jitter(width = 0.18, height = 0)
  ) +
  geom_point(
    data = df2 %>% filter(outlier),
    mapping = aes(x = R, y = Value, shape = Y_factor),
    colour = "red",
    alpha = 0.95,
    size = 1.1,
    stroke = 0.35,
    position = position_jitter(width = 0.18, height = 0),
    show.legend = FALSE
  ) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_colour_viridis_c(option = "C") +
  scale_shape_manual(values = shape_map, drop = FALSE) +
  labs(
    title = "PoutputIncomeEla (points only; x = country; shape = year)",
    subtitle = "Outliers are highlighted in red (I-wise IQR rule). Colour scale is clipped to 5–95% within each I.",
    x = "Country (R)",
    y = "Value",
    colour = "Value (clipped)",
    shape = "Year (Y)"
  ) +
  theme_bw(base_size = 10) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "bottom",
    legend.box = "vertical"
  )

out_png_country <- file.path(output_dir, "PoutputIncomeEla_byI_Country_shapeYear_outlierRed.png")
ggsave(
  filename = out_png_country,
  plot = p_country,
  width = 24,
  height = 13,
  dpi = 300,
  device = ragg::agg_png
)

cat("Saved:", normalizePath(out_png_country, winslash = "/", mustWork = FALSE), "\n")
