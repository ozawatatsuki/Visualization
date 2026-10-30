suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(tibble)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_dir <- file.path(project_dir, "data", "260715_HT_GDPcap\u5909\u66f4")
out_dir <- file.path(project_dir, "output", "260715_HT_GDPcap_change")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

scenarios <- c("SSP2_400C_2025CP_NoCC_No", "SSP2_BaU_NoCC_No")
scenario_labels <- c(
  "SSP2_400C_2025CP_NoCC_No" = "SSP2 400C 2025CP",
  "SSP2_BaU_NoCC_No" = "SSP2 BaU"
)
scenario_diff_label <- "SSP2 400C 2025CP - SSP2 BaU"
version_labels <- c(
  before = "Before update",
  after = "After update"
)

region_files <- c(
  before = file.path(data_dir, "\u66f4\u65b0\u524d_region.gdx"),
  after = file.path(data_dir, "\u66f4\u65b0\u5f8c_region.gdx")
)
country_files <- c(
  before = file.path(data_dir, "\u66f4\u65b0\u524d_country.gdx"),
  after = file.path(data_dir, "\u66f4\u65b0\u5f8c_country.gdx")
)

required_files <- c(region_files, country_files)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}

theme_publication <- function(base_size = 12) {
  theme_classic(base_size = base_size) +
    theme(
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey10"),
      legend.position = "bottom",
      legend.title = element_blank(),
      strip.background = element_rect(fill = "white", color = "grey40", linewidth = 0.4),
      strip.text = element_text(face = "bold", color = "grey10"),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey25"),
      plot.margin = margin(10, 12, 10, 10)
    )
}

range_with_padding <- function(values, include_zero = FALSE) {
  values <- values[is.finite(values)]
  if (length(values) == 0) {
    return(c(0, 1))
  }

  limits <- range(values, na.rm = TRUE)
  if (include_zero) {
    limits <- range(c(limits, 0), na.rm = TRUE)
  }

  if (isTRUE(all.equal(limits[1], limits[2]))) {
    padding <- max(abs(limits[1]), 1) * 0.05
  } else {
    padding <- diff(limits) * 0.05
  }

  c(limits[1] - padding, limits[2] + padding)
}

read_region_risk <- function(path, version_id) {
  as_tibble(rgdx.param(path, "Risk_agg")) |>
    mutate(across(c(U, S, R_agg, T), as.character)) |>
    filter(
      U == "fao_based",
      S %in% scenarios,
      R_agg == "WLD"
    ) |>
    transmute(
      version = version_labels[[version_id]],
      scenario = factor(S, levels = scenarios, labels = scenario_labels[scenarios]),
      T = suppressWarnings(as.integer(T)),
      value = Risk_agg
    ) |>
    filter(!is.na(T), is.finite(value))
}

read_country_risk <- function(path, value_name) {
  out <- as_tibble(rgdx.param(path, "Risk")) |>
    mutate(across(c(U, S, R, T), as.character)) |>
    filter(
      U == "fao_based",
      S %in% scenarios
    ) |>
    transmute(
      U,
      S,
      R,
      T = suppressWarnings(as.integer(T)),
      value = Risk
    ) |>
    filter(!is.na(T), is.finite(value))

  names(out)[names(out) == "value"] <- value_name
  out
}

region_plot_data <- bind_rows(
  read_region_risk(region_files[["before"]], "before"),
  read_region_risk(region_files[["after"]], "after")
) |>
  mutate(version = factor(version, levels = version_labels))

if (nrow(region_plot_data) == 0) {
  stop("No records were found for Risk_agg with U=fao_based, selected scenarios, and R_agg=WLD.")
}

version_colors <- c(
  "Before update" = "#2B6CB0",
  "After update" = "#D1495B"
)

p_region <- ggplot(region_plot_data, aes(x = T, y = value, color = version)) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 1.7, alpha = 0.86) +
  facet_wrap(~ scenario, nrow = 1) +
  scale_color_manual(values = version_colors, drop = FALSE) +
  scale_x_continuous(breaks = c(2001, seq(2010, 2100, by = 10))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Risk_agg annual change in WLD",
    subtitle = "U = fao_based, R_agg = WLD",
    x = "T",
    y = "value"
  ) +
  theme_publication(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

region_png <- file.path(out_dir, "risk_agg_wld_annual_change_before_after.png")
ggsave(region_png, p_region, width = 10.5, height = 5.2, dpi = 300, bg = "white")

region_diff_data <- inner_join(
  region_plot_data |>
    filter(scenario == scenario_labels[[scenarios[1]]]) |>
    transmute(version, T, value_400c = value),
  region_plot_data |>
    filter(scenario == scenario_labels[[scenarios[2]]]) |>
    transmute(version, T, value_bau = value),
  by = c("version", "T")
) |>
  mutate(
    version = factor(version, levels = version_labels),
    value = value_400c - value_bau
  )

if (nrow(region_diff_data) == 0) {
  stop("No records were found for the WLD scenario difference.")
}

p_region_diff <- ggplot(region_diff_data, aes(x = T, y = value, color = version)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.55) +
  geom_line(linewidth = 0.95) +
  geom_point(size = 1.7, alpha = 0.86) +
  scale_color_manual(values = version_colors, drop = FALSE) +
  scale_x_continuous(breaks = c(2001, seq(2010, 2100, by = 10))) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Risk_agg scenario difference in WLD",
    subtitle = paste0("U = fao_based, R_agg = WLD; ", scenario_diff_label),
    x = "T",
    y = "value difference"
  ) +
  theme_publication(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

region_diff_png <- file.path(out_dir, "risk_agg_wld_scenario_difference_before_after.png")
ggsave(region_diff_png, p_region_diff, width = 7.4, height = 5.2, dpi = 300, bg = "white")

old_country <- read_country_risk(country_files[["before"]], "before")
new_country <- read_country_risk(country_files[["after"]], "after")

country_plot_data <- inner_join(
  old_country,
  new_country,
  by = c("U", "S", "R", "T")
) |>
  mutate(
    scenario = factor(S, levels = scenarios, labels = scenario_labels[scenarios])
  ) |>
  filter(is.finite(before), is.finite(after))

if (nrow(country_plot_data) == 0) {
  stop("No common records were found for Risk in before/after country GDX files.")
}

country_colors <- c(
  "SSP2 400C 2025CP" = "#0072B2",
  "SSP2 BaU" = "#D55E00"
)

axis_limits <- range(c(country_plot_data$before, country_plot_data$after), na.rm = TRUE)
axis_limits <- c(0, max(axis_limits[2], 0))

p_country <- ggplot(country_plot_data, aes(x = before, y = after, color = scenario)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey45", linewidth = 0.65) +
  geom_point(alpha = 0.45, size = 1.55) +
  coord_equal(xlim = axis_limits, ylim = axis_limits, expand = TRUE) +
  scale_color_manual(values = country_colors, drop = FALSE) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Country Risk: before vs after update",
    subtitle = "U = fao_based",
    x = "Before update",
    y = "After update"
  ) +
  theme_publication(base_size = 12)

country_png <- file.path(out_dir, "risk_country_before_after_scatter.png")
ggsave(country_png, p_country, width = 7.4, height = 6.6, dpi = 300, bg = "white")

country_diff_data <- inner_join(
  country_plot_data |>
    filter(S == scenarios[1]) |>
    transmute(U, R, T, before_400c = before, after_400c = after),
  country_plot_data |>
    filter(S == scenarios[2]) |>
    transmute(U, R, T, before_bau = before, after_bau = after),
  by = c("U", "R", "T")
) |>
  mutate(
    before = before_400c - before_bau,
    after = after_400c - after_bau,
    abs_change = abs(after - before)
  ) |>
  filter(is.finite(before), is.finite(after))

if (nrow(country_diff_data) == 0) {
  stop("No common records were found for the country scenario difference.")
}

diff_axis_limits <- range_with_padding(c(country_diff_data$before, country_diff_data$after), include_zero = TRUE)

p_country_diff <- ggplot(country_diff_data, aes(x = before, y = after)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey45", linewidth = 0.65) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "grey70", linewidth = 0.45) +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey70", linewidth = 0.45) +
  geom_point(color = "#2F855A", alpha = 0.45, size = 1.55) +
  coord_equal(xlim = diff_axis_limits, ylim = diff_axis_limits, expand = FALSE) +
  scale_x_continuous(labels = scales::label_number(big.mark = ",")) +
  scale_y_continuous(labels = scales::label_number(big.mark = ",")) +
  labs(
    title = "Country Risk scenario difference: before vs after update",
    subtitle = paste0("U = fao_based; ", scenario_diff_label),
    x = "Before update difference",
    y = "After update difference"
  ) +
  theme_publication(base_size = 12)

country_diff_png <- file.path(out_dir, "risk_country_scenario_difference_before_after_scatter.png")
ggsave(country_diff_png, p_country_diff, width = 7.4, height = 6.6, dpi = 300, bg = "white")

summary_csv <- file.path(out_dir, "risk_country_before_after_summary.csv")
summary_data <- country_plot_data |>
  mutate(abs_diff = abs(after - before)) |>
  group_by(scenario) |>
  summarise(
    records = n(),
    countries = n_distinct(R),
    years = n_distinct(T),
    max_before = max(before, na.rm = TRUE),
    max_after = max(after, na.rm = TRUE),
    mean_abs_diff = mean(abs_diff, na.rm = TRUE),
    max_abs_diff = max(abs_diff, na.rm = TRUE),
    .groups = "drop"
  )
write.csv(summary_data, summary_csv, row.names = FALSE)

diff_summary_csv <- file.path(out_dir, "risk_scenario_difference_summary.csv")
diff_summary_data <- tibble(
  metric = c("region_records", "country_records", "country_count", "year_count", "country_mean_abs_update_change", "country_max_abs_update_change"),
  value = c(
    nrow(region_diff_data),
    nrow(country_diff_data),
    n_distinct(country_diff_data$R),
    n_distinct(country_diff_data$T),
    mean(country_diff_data$abs_change, na.rm = TRUE),
    max(country_diff_data$abs_change, na.rm = TRUE)
  )
)
write.csv(diff_summary_data, diff_summary_csv, row.names = FALSE)

message("Saved figure: ", region_png)
message("Saved figure: ", region_diff_png)
message("Saved figure: ", country_png)
message("Saved figure: ", country_diff_png)
message("Saved summary: ", summary_csv)
message("Saved summary: ", diff_summary_csv)
message("Region records: ", nrow(region_plot_data))
message("Region difference records: ", nrow(region_diff_data))
message("Country common records: ", nrow(country_plot_data))
message("Country difference records: ", nrow(country_diff_data))
