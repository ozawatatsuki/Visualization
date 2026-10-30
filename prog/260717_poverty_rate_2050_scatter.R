suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(ggrepel)
  library(scales)
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
target_year <- "2050"

aggregate_codes <- c(
  "WLD", "AFE", "AFW", "EAP", "EAS", "ECA", "ECS", "LAC", "LCN",
  "MEA", "MNA", "NAC", "OHI", "SAS", "SSF", "SSA"
)

is_country_code <- function(x) {
  grepl("^[A-Z]{3}$", x) & !(x %in% aggregate_codes)
}

read_rop <- function(path, th, value_name) {
  as_tibble(rgdx.param(path, "ROP")) |>
    filter(
      as.character(Ref) == target_ref,
      as.character(Y) == target_year,
      as.character(TH) == th
    ) |>
    transmute(
      R = as.character(R),
      value = RoP
    ) |>
    filter(is_country_code(R)) |>
    distinct(R, .keep_all = TRUE) |>
    rename("{value_name}" := value)
}

old_data <- read_rop(old_gdx, "pop_2.15", "rop_old")
new_data <- read_rop(new_gdx, "pop_3.00", "rop_new")

plot_data <- full_join(old_data, new_data, by = "R") |>
  mutate(
    rop_old = coalesce(rop_old, 0),
    rop_new = coalesce(rop_new, 0)
  ) |>
  filter(is.finite(rop_old), is.finite(rop_new)) |>
  mutate(
    rop_old_pct = rop_old * 100,
    rop_new_pct = rop_new * 100,
    absolute_gap_pct = abs(rop_new_pct - rop_old_pct)
  )

if (nrow(plot_data) == 0) {
  stop("No poverty rate records were found for ", target_ref, " in ", target_year, ".")
}

label_data <- plot_data |>
  slice_max(absolute_gap_pct, n = 12, with_ties = FALSE)

axis_max <- max(c(plot_data$rop_old_pct, plot_data$rop_new_pct), na.rm = TRUE)
axis_max <- ceiling(axis_max / 10) * 10

p <- ggplot(plot_data, aes(x = rop_old_pct, y = rop_new_pct)) +
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
    seed = 260717
  ) +
  coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max), expand = TRUE, clip = "off") +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Poverty Rate in 2050: Old $2.15 vs New $3.00",
    x = "Old Version, $2.15 line",
    y = "New Version, $3.00 line"
  ) +
  theme_classic(base_size = 15) +
  theme(
    axis.text = element_text(color = "grey30"),
    axis.title = element_text(color = "grey10"),
    plot.title = element_text(color = "grey10", hjust = 0.5),
    plot.margin = margin(10, 14, 10, 10)
  )

ggsave(
  filename = file.path(out_dir, "poverty_rate_2050_old215_vs_new300.png"),
  plot = p,
  width = 7.2,
  height = 6.4,
  dpi = 300,
  bg = "white"
)

message("Saved figure to: ", file.path(out_dir, "poverty_rate_2050_old215_vs_new300.png"))
message(
  "Records: old=", nrow(old_data),
  ", new=", nrow(new_data),
  ", common=", length(intersect(old_data[["R"]], new_data[["R"]])),
  ", plotted=", nrow(plot_data)
)
