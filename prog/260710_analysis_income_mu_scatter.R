suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
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

read_mu_income <- function(path) {
  as_tibble(rgdx.param(path, "Mu_Income")) |>
    filter(
      as.character(Ref) == target_ref,
      as.character(Y) == target_year
    ) |>
    transmute(
      R = as.character(R),
      mu_income = Mu_Income
    ) |>
    filter(is_country_code(R)) |>
    distinct(R, .keep_all = TRUE)
}

old_data <- read_mu_income(old_gdx) |>
  transmute(R, mu_old = mu_income)
new_data <- read_mu_income(new_gdx) |>
  transmute(R, mu_new = mu_income)

plot_data <- inner_join(old_data, new_data, by = "R") |>
  filter(is.finite(mu_old), is.finite(mu_new)) |>
  mutate(delta_mu = mu_new - mu_old)

if (nrow(plot_data) == 0) {
  stop("No common Mu_Income records were found for ", target_ref, " in ", target_year, ".")
}

label_data <- plot_data |>
  slice_max(abs(delta_mu), n = 10, with_ties = FALSE)

axis_min <- floor(min(c(plot_data$mu_old, plot_data$mu_new), na.rm = TRUE) * 2) / 2
axis_max <- ceiling(max(c(plot_data$mu_old, plot_data$mu_new), na.rm = TRUE) * 2) / 2

p <- ggplot(plot_data, aes(x = mu_old, y = mu_new)) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "grey45", linewidth = 0.6) +
  geom_point(color = "#2B6CB0", alpha = 0.72, size = 2.2) +
  geom_point(data = label_data, color = "#B23A48", alpha = 0.9, size = 2.6) +
  geom_text_repel(
    data = label_data,
    aes(label = R),
    color = "grey10",
    size = 4.0,
    min.segment.length = 0,
    box.padding = 0.35,
    point.padding = 0.25,
    max.overlaps = Inf,
    seed = 260710
  ) +
  coord_equal(xlim = c(axis_min, axis_max), ylim = c(axis_min, axis_max), expand = TRUE, clip = "off") +
  labs(
    title = "Mu_Income in 2020: Old vs New AnalysisIncome.gdx",
    x = "Old Version",
    y = "New Version"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey10"),
    plot.title = element_text(color = "grey10", hjust = 0.5),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  filename = file.path(out_dir, "analysis_income_mu_2020_old_vs_new.png"),
  plot = p,
  width = 7.2,
  height = 6.4,
  dpi = 300,
  bg = "white"
)

message("Saved figure to: ", file.path(out_dir, "analysis_income_mu_2020_old_vs_new.png"))
message(
  "Records: old=", nrow(old_data),
  ", new=", nrow(new_data),
  ", common=", nrow(plot_data),
  ", old_only=", length(setdiff(old_data[["R"]], new_data[["R"]])),
  ", new_only=", length(setdiff(new_data[["R"]], old_data[["R"]]))
)
