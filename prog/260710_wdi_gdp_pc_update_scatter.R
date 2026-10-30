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
old_gdx <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u524d", "WDI_load.gdx")
new_gdx <- file.path(data_dir, "\u30e9\u30a4\u30f3\u66f4\u65b0\u5f8c", "WDI_load.gdx")
out_dir <- file.path(project_dir, "output", "260709_poverty_line_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

indicator_code <- "NY.GDP.PCAP.PP.KD"
target_year <- "2020"

required_files <- c(old_gdx, new_gdx)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}

read_wdi_gdp_pc <- function(path, version) {
  as_tibble(rgdx.param(path, "WDI_load")) |>
    transmute(
      R = as.character(i),
      indicator = as.character(j),
      Y = as.character(k),
      value = value
    ) |>
    filter(indicator == indicator_code, Y == target_year) |>
    distinct(R, .keep_all = TRUE) |>
    mutate(version = version)
}

old_data <- read_wdi_gdp_pc(old_gdx, "Old Version") |>
  transmute(R, gdp_pc_old = value)
new_data <- read_wdi_gdp_pc(new_gdx, "New Version") |>
  transmute(R, gdp_pc_new = value)

plot_data <- inner_join(old_data, new_data, by = "R") |>
  filter(is.finite(gdp_pc_old), is.finite(gdp_pc_new))

if (nrow(plot_data) == 0) {
  stop("No common records were found for ", indicator_code, " in ", target_year, ".")
}

max_value <- max(c(plot_data$gdp_pc_old, plot_data$gdp_pc_new), na.rm = TRUE)
axis_max <- ceiling(max_value / 10000) * 10000

wdi_aggregate_codes <- c(
  "AFE", "AFW", "ARB", "CEB", "CSS", "EAP", "EAR", "EAS", "ECA", "ECS",
  "EMU", "EUU", "FCS", "HIC", "HPC", "IBD", "IBT", "IDA", "IDB", "IDX",
  "INX", "LAC", "LCN", "LDC", "LIC", "LMC", "LMY", "LTE", "MEA", "MIC",
  "MNA", "NAC", "OED", "OSS", "PRE", "PSS", "PST", "SAS", "SSA", "SSF",
  "SST", "TEA", "TEC", "TLA", "TMN", "TSA", "TSS", "UMC", "WLD"
)

label_data <- plot_data |>
  filter(grepl("^[A-Z]{3}$", R), !(R %in% wdi_aggregate_codes)) |>
  mutate(deviation_from_identity = abs(log(gdp_pc_new / gdp_pc_old))) |>
  slice_max(deviation_from_identity, n = 8, with_ties = FALSE)

format_dollar <- function(x) {
  paste0("$", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
}

p <- ggplot(plot_data, aes(x = gdp_pc_old, y = gdp_pc_new)) +
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
  coord_equal(xlim = c(0, axis_max), ylim = c(0, axis_max), expand = TRUE, clip = "off") +
  scale_x_continuous(labels = format_dollar) +
  scale_y_continuous(labels = format_dollar) +
  labs(
    title = "GDP per capita in 2020: Old vs New WDI_load",
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
  filename = file.path(out_dir, "wdi_gdp_pc_ppp_2020_old_vs_new.png"),
  plot = p,
  width = 7.2,
  height = 6.4,
  dpi = 300,
  bg = "white"
)

message("Saved figure to: ", file.path(out_dir, "wdi_gdp_pc_ppp_2020_old_vs_new.png"))
message(
  "Records: old=", nrow(old_data),
  ", new=", nrow(new_data),
  ", common=", nrow(plot_data),
  ", old_only=", length(setdiff(old_data[["R"]], new_data[["R"]])),
  ", new_only=", length(setdiff(new_data[["R"]], old_data[["R"]]))
)
