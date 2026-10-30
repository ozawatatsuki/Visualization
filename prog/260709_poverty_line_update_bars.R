suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(readr)
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

data_dir <- file.path(project_dir, "data", "260709_\u8ca7\u56f0\u30e9\u30a4\u30f3\u66f4\u65b0")
old_dir <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u524d")
new_dir <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u5f8c")
out_dir <- file.path(project_dir, "output", "260709_poverty_line_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_ref <- "SSP2_BaU_NoCC_No"
target_region <- "WLD"
target_year <- "2020"

line_map <- tribble(
  ~version,       ~line_group, ~line_order, ~th_label, ~th_gdx,
  "Old Version",  "low",       1,           "2.15",    "pop_2.15",
  "Old Version",  "middle",    2,           "3.65",    "pop_3.65",
  "Old Version",  "high",      3,           "6.85",    "pop_6.85",
  "New Version",  "low",       1,           "3.00",    "pop_3.00",
  "New Version",  "middle",    2,           "4.20",    "pop_4.20",
  "New Version",  "high",      3,           "8.30",    "pop_8.30"
)

obs_files <- tribble(
  ~version,       ~line_group, ~path,
  "Old Version",  "low",       file.path(old_dir, "povcalnet_region_oldlow.csv"),
  "Old Version",  "middle",    file.path(old_dir, "povcalnet_region_oldmiddle.csv"),
  "Old Version",  "high",      file.path(old_dir, "povcalnet_region_oldhigh.csv"),
  "New Version",  "low",       file.path(new_dir, "povcalnet_region_newlow.csv"),
  "New Version",  "middle",    file.path(new_dir, "povcalnet_region_newmiddle.csv"),
  "New Version",  "high",      file.path(new_dir, "povcalnet_region_newhigh.csv")
)

est_files <- tribble(
  ~version,       ~path,
  "Old Version",  file.path(old_dir, "AnalysisIncome.gdx"),
  "New Version",  file.path(new_dir, "AnalysisIncome.gdx")
)

required_files <- c(obs_files$path, est_files$path)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}

read_obs_one <- function(version, line_group, path) {
  read_csv(path, show_col_types = FALSE) |>
    filter(region_code == target_region, as.character(reporting_year) == target_year) |>
    select(poverty_line, headcount, pop_in_poverty) |>
    mutate(version = version, line_group = line_group) |>
    tidyr::pivot_longer(
      cols = c(headcount, pop_in_poverty),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      metric = recode(
        metric,
        headcount = "poverty_rate",
        pop_in_poverty = "poverty_population"
      ),
      source = "obs"
    ) |>
    select(version, line_group, source, metric, value)
}

read_est_metric <- function(path, symbol, value_col, metric_name) {
  as_tibble(rgdx.param(path, symbol)) |>
    filter(
      as.character(Ref) == target_ref,
      as.character(R) == target_region,
      as.character(Y) == target_year
    ) |>
    transmute(
      th_gdx = as.character(TH),
      metric = metric_name,
      value = .data[[value_col]]
    )
}

read_est_one <- function(version, path) {
  bind_rows(
    read_est_metric(path, "ROP", "RoP", "poverty_rate"),
    read_est_metric(path, "POV", "PoV", "poverty_population")
  ) |>
    mutate(version = version, source = "est") |>
    select(version, source, metric, th_gdx, value)
}

obs <- bind_rows(purrr::pmap(obs_files, read_obs_one)) |>
  left_join(line_map, by = c("version", "line_group"))

est <- bind_rows(purrr::pmap(est_files, read_est_one)) |>
  inner_join(line_map, by = c("version", "th_gdx")) |>
  select(version, line_group, source, metric, value, line_order, th_label, th_gdx)

plot_data <- bind_rows(
  obs |> select(version, line_group, source, metric, value, line_order, th_label, th_gdx),
  est
) |>
  mutate(
    version = factor(version, levels = c("Old Version", "New Version")),
    source = factor(source, levels = c("obs", "est")),
    th_label = factor(th_label, levels = c("2.15", "3.65", "6.85", "3.00", "4.20", "8.30"))
  )

expected_n <- 2 * 3 * 2 * 2
if (nrow(plot_data) != expected_n) {
  stop("Unexpected number of plot records: ", nrow(plot_data), " (expected ", expected_n, ")")
}

make_bar_plot <- function(metric_name, y_label, y_transform, y_labels, output_file) {
  df <- plot_data |>
    filter(metric == metric_name) |>
    mutate(plot_value = y_transform(value))

  p <- ggplot(df, aes(x = th_label, y = plot_value, fill = source)) +
    geom_col(
      position = position_dodge(width = 0.74),
      width = 0.68
    ) +
    facet_wrap(~version, nrow = 1, scales = "free_x") +
    scale_fill_manual(
      values = c(obs = "#F8766D", est = "#00BFC4"),
      breaks = c("obs", "est"),
      labels = c("obs", "est")
    ) +
    scale_y_continuous(
      labels = y_labels,
      expand = expansion(mult = c(0, 0.08))
    ) +
    scale_x_discrete(drop = TRUE) +
    labs(x = "Poverty Line", y = y_label, fill = NULL) +
    theme_classic(base_size = 16) +
    theme(
      axis.line = element_blank(),
      axis.text = element_text(color = "grey30"),
      axis.title = element_text(color = "grey10"),
      legend.position = "right",
      legend.key.size = unit(0.9, "lines"),
      panel.border = element_rect(color = "grey35", fill = NA, linewidth = 0.5),
      panel.spacing.x = unit(1.8, "lines"),
      strip.background = element_blank(),
      strip.text = element_text(hjust = 0, color = "grey10", size = 15),
      plot.margin = margin(8, 12, 8, 8)
    )

  ggsave(
    filename = file.path(out_dir, output_file),
    plot = p,
    width = 9,
    height = 5.2,
    dpi = 300,
    bg = "white"
  )
}

make_bar_plot(
  metric_name = "poverty_rate",
  y_label = "Poverty Rate (%)",
  y_transform = function(x) x * 100,
  y_labels = function(x) sprintf("%.0f", x),
  output_file = "poverty_rate_wld_2020.png"
)

make_bar_plot(
  metric_name = "poverty_population",
  y_label = "Poverty Headcount (billion)",
  y_transform = function(x) x / 1e9,
  y_labels = function(x) sprintf("%.1f", x),
  output_file = "poverty_population_wld_2020.png"
)

message("Saved figures to: ", out_dir)
