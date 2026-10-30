suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(tibble)
  library(tidyr)
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
old_gdx <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u524d", "AnalysisIncome.gdx")
new_gdx <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u5f8c", "AnalysisIncome.gdx")
out_dir <- file.path(project_dir, "output", "260709_poverty_line_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_ref <- "SSP2_BaU_NoCC_No"
target_year <- "2020"

aggregate_codes <- c(
  "WLD", "AFE", "AFW", "EAP", "EAS", "ECA", "ECS", "LAC", "LCN",
  "MEA", "MNA", "NAC", "OHI", "SAS", "SSF", "SSA"
)

is_country_code <- function(x) {
  grepl("^[A-Z]{3}$", x) & !(x %in% aggregate_codes)
}

read_distribution <- function(path, version) {
  mu <- as_tibble(rgdx.param(path, "Mu_Income")) |>
    filter(as.character(Ref) == target_ref, as.character(Y) == target_year) |>
    transmute(R = as.character(R), mu = Mu_Income)

  sigma <- as_tibble(rgdx.param(path, "Sigma_Income")) |>
    filter(as.character(Ref) == target_ref, as.character(Y) == target_year) |>
    transmute(R = as.character(R), sigma = Sigma_Income)

  inner_join(mu, sigma, by = "R") |>
    filter(is_country_code(R)) |>
    mutate(version = version)
}

read_symbol_count <- function(path, symbol, th) {
  as_tibble(rgdx.param(path, symbol)) |>
    filter(
      as.character(Ref) == target_ref,
      as.character(Y) == target_year,
      as.character(TH) == th
    ) |>
    transmute(R = as.character(R)) |>
    filter(is_country_code(R)) |>
    distinct(R) |>
    nrow()
}

old_dist <- read_distribution(old_gdx, "Old Version")
new_dist <- read_distribution(new_gdx, "New Version")

dist_compare <- old_dist |>
  inner_join(new_dist, by = "R", suffix = c("_old", "_new")) |>
  mutate(
    delta_mu = mu_new - mu_old,
    delta_sigma = sigma_new - sigma_old
  )

line_changes <- tribble(
  ~component,                         ~mean,
  "log poverty line\nlow: 3.00 / 2.15",    log(3.00 / 2.15),
  "log poverty line\nmiddle: 4.20 / 3.65", log(4.20 / 3.65),
  "log poverty line\nhigh: 8.30 / 6.85",   log(8.30 / 6.85)
)

change_summary <- bind_rows(
  line_changes |>
    mutate(range_min = NA_real_, range_max = NA_real_),
  dist_compare |>
    summarise(
      component = "mean delta\nMu_Income",
      mean = mean(delta_mu),
      range_min = min(delta_mu),
      range_max = max(delta_mu)
    ),
  dist_compare |>
    summarise(
      component = "mean delta\nSigma_Income",
      mean = mean(delta_sigma),
      range_min = min(delta_sigma),
      range_max = max(delta_sigma)
    )
) |>
  mutate(
    component = factor(component, levels = component),
    label = sprintf("%+.3f", mean)
  )

parameter_plot <- ggplot(change_summary, aes(x = component, y = mean, fill = component)) +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.4) +
  geom_col(width = 0.62, show.legend = FALSE) +
  geom_errorbar(
    aes(ymin = range_min, ymax = range_max),
    width = 0.16,
    linewidth = 0.5,
    na.rm = TRUE
  ) +
  geom_text(
    aes(label = label, vjust = ifelse(mean >= 0, -0.5, 1.25)),
    size = 4.6,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = c("#7A93B8", "#7A93B8", "#7A93B8", "#E07A5F", "#6A994E")) +
  scale_y_continuous(expand = expansion(mult = c(0.12, 0.18))) +
  labs(
    x = NULL,
    y = "Change in log-scale parameter"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(color = "grey20"),
    axis.text.y = element_text(color = "grey30"),
    axis.title.y = element_text(color = "grey10"),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  filename = file.path(out_dir, "est_parameter_shift_by_line_wld_2020.png"),
  plot = parameter_plot,
  width = 10.4,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

old_countries <- old_dist |> distinct(R) |> pull(R)
new_countries <- new_dist |> distinct(R) |> pull(R)
common_countries <- intersect(old_countries, new_countries)
old_only <- setdiff(old_countries, new_countries)
new_only <- setdiff(new_countries, old_countries)

count_data <- tribble(
  ~item,                 ~version,       ~count,
  "Mu & Sigma params",   "Old Version",  length(old_countries),
  "Mu & Sigma params",   "New Version",  length(new_countries),
  "ROP middle records",  "Old Version",  read_symbol_count(old_gdx, "ROP", "pop_3.65"),
  "ROP middle records",  "New Version",  read_symbol_count(new_gdx, "ROP", "pop_4.20"),
  "POV middle records",  "Old Version",  read_symbol_count(old_gdx, "POV", "pop_3.65"),
  "POV middle records",  "New Version",  read_symbol_count(new_gdx, "POV", "pop_4.20")
) |>
  mutate(
    item = factor(item, levels = c("Mu & Sigma params", "ROP middle records", "POV middle records")),
    version = factor(version, levels = c("Old Version", "New Version"))
  )

count_plot <- ggplot(count_data, aes(x = item, y = count, fill = version)) +
  geom_col(position = position_dodge(width = 0.72), width = 0.64) +
  geom_text(
    aes(label = count),
    position = position_dodge(width = 0.72),
    vjust = -0.45,
    size = 4.4
  ) +
  scale_fill_manual(values = c("Old Version" = "#F8766D", "New Version" = "#00BFC4")) +
  scale_y_continuous(limits = c(0, max(count_data$count) + 18), expand = expansion(mult = c(0, 0.02))) +
  labs(
    x = NULL,
    y = "Number of countries",
    fill = NULL
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text.x = element_text(color = "grey20"),
    axis.text.y = element_text(color = "grey30"),
    axis.title.y = element_text(color = "grey10"),
    legend.position = "right",
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  filename = file.path(out_dir, "est_country_count_comparison_wld_2020.png"),
  plot = count_plot,
  width = 8.2,
  height = 5.2,
  dpi = 300,
  bg = "white"
)

message("Saved diagnostic figures to: ", out_dir)
