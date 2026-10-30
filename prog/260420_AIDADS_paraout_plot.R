suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

input_case <- "gamma罰則なし"
input_dir <- file.path("data/260420_AIDADS", input_case)
gdx_file <- file.path(input_dir, "AIDADSCalibCrossCountry_SSP.gdx")
output_root <- "output/260420"
dir.create(output_root, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_file)) {
  stop(sprintf("GDX file was not found: %s", gdx_file))
}

read_paraout <- function(path) {
  df <- as_tibble(rgdx.param(path, "ParaOut"))
  if (ncol(df) != 4) {
    stop(sprintf("ParaOut in %s is expected to have 4 columns", path))
  }
  names(df) <- c("para", "SSP", "I", "value")
  df %>%
    mutate(
      para = tolower(as.character(para)),
      SSP = as.character(SSP),
      I = as.character(I),
      value = as.numeric(value)
    )
}

read_uout <- function(path) {
  df <- as_tibble(rgdx.param(path, "Uout"))
  if (ncol(df) < 5) {
    stop(sprintf("Uout in %s is expected to have at least 5 columns", path))
  }
  first_col <- names(df)[1]
  last_col <- names(df)[ncol(df)]
  df %>%
    transmute(
      SSP = as.character(.data[[first_col]]),
      U = as.numeric(.data[[last_col]])
    )
}

paraout_raw <- read_paraout(gdx_file)
uout_raw <- read_uout(gdx_file)

if (nrow(paraout_raw) == 0) {
  stop("ParaOut has no records.")
}
if (nrow(uout_raw) == 0) {
  stop("Uout has no records.")
}

para_levels <- c("gamma", "alpha", "beta")
ssp_levels <- sort(unique(paraout_raw$SSP))
i_levels <- sort(unique(paraout_raw$I))

paraout <- paraout_raw %>%
  complete(para = para_levels, SSP = ssp_levels, I = i_levels, fill = list(value = 0))

base_palette <- c(
  "#1b9e77", "#d95f02", "#7570b3", "#e7298a",
  "#66a61e", "#e6ab02", "#a6761d", "#666666"
)
if (length(ssp_levels) > length(base_palette)) {
  stop("Too many SSP categories for the fixed palette.")
}
ssp_palette <- setNames(base_palette[seq_along(ssp_levels)], ssp_levels)

gamma_data <- paraout %>%
  filter(para == "gamma") %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    I = factor(I, levels = i_levels)
  )

alpha_beta <- paraout %>%
  filter(para %in% c("alpha", "beta")) %>%
  pivot_wider(names_from = para, values_from = value, values_fill = 0) %>%
  mutate(
    SSP = factor(SSP, levels = ssp_levels),
    I = factor(I, levels = i_levels)
  )

u_min_data <- min(uout_raw$U, na.rm = TRUE)
u_max_data <- max(uout_raw$U, na.rm = TRUE)
u_min <- floor(min(-8, u_min_data))
u_max <- ceiling(max(8, u_max_data))
u_seq <- seq(u_min, u_max, length.out = 400)

phi_data <- crossing(alpha_beta, U = u_seq) %>%
  mutate(phi = (alpha + exp(U) * beta) / (1 + exp(U)))

theme_plot <- theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "bottom"
  )

p_gamma <- ggplot(gamma_data, aes(x = SSP, y = value, fill = SSP)) +
  geom_col(width = 0.7, color = "grey20", linewidth = 0.2) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_fill_manual(values = ssp_palette, drop = FALSE) +
  labs(
    title = sprintf("gamma by SSP (%s)", input_case),
    x = "SSP",
    y = "gamma",
    fill = "SSP"
  ) +
  theme_plot

p_phi <- ggplot(phi_data, aes(x = U, y = phi, color = SSP)) +
  geom_hline(
    data = alpha_beta,
    aes(yintercept = beta, color = SSP),
    linetype = "dashed",
    linewidth = 0.4,
    alpha = 0.8,
    show.legend = FALSE
  ) +
  geom_line(linewidth = 0.9) +
  facet_wrap(~I, scales = "free_y", ncol = 4) +
  scale_color_manual(values = ssp_palette, drop = FALSE) +
  scale_x_continuous(limits = c(u_min, u_max), breaks = seq(u_min, u_max, by = 2)) +
  labs(
    title = sprintf("phi(U) by SSP (%s)", input_case),
    subtitle = sprintf(
      "U range: %.1f to %.1f (extended so phi approaches beta)",
      u_min, u_max
    ),
    x = "U",
    y = "phi",
    color = "SSP"
  ) +
  theme_plot

ggsave(
  filename = file.path(output_root, "01_gamma_bar.png"),
  plot = p_gamma,
  width = 20,
  height = 12,
  dpi = 300
)

ggsave(
  filename = file.path(output_root, "02_phi_line.png"),
  plot = p_phi,
  width = 20,
  height = 12,
  dpi = 300
)

manifest <- tibble(
  case = input_case,
  gdx_file = gdx_file,
  u_min_data = u_min_data,
  u_max_data = u_max_data,
  u_min = u_min,
  u_max = u_max,
  n_paraout_sparse = nrow(paraout_raw),
  n_paraout_completed = nrow(paraout),
  output_gamma = file.path(output_root, "01_gamma_bar.png"),
  output_phi = file.path(output_root, "02_phi_line.png")
)

write.csv(
  manifest,
  file.path(output_root, "plot_manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Completed for", input_case, "Outputs were saved under:", output_root, "\n")
