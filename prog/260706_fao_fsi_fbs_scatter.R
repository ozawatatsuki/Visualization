suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(scales)
})

label_n <- 15
input_dir <- file.path("data", "FAOデータ比較")
output_dir <- file.path("output", "FAOデータ比較")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

fsi <- read_csv(
  file.path(input_dir, "FSI.csv"),
  col_types = cols(.default = col_character(), Year = col_integer(), Value = col_double())
)

fbs <- read_csv(
  file.path(input_dir, "FBS.csv"),
  col_types = cols(.default = col_character(), Year = col_integer(), Value = col_double())
)

fsi_cmp <- fsi |>
  transmute(
    area_code = `Area Code (M49)`,
    area_fsi = Area,
    year = Year,
    value_fsi = Value,
    item_fsi = Item,
    unit_fsi = Unit
  )

fbs_cmp <- fbs |>
  transmute(
    area_code = `Area Code (M49)`,
    area_fbs = Area,
    year = Year,
    value_fbs = Value,
    item_fbs = Item,
    unit_fbs = Unit
  )

matched <- fbs_cmp |>
  inner_join(fsi_cmp, by = c("area_code", "year")) |>
  filter(!is.na(value_fbs), !is.na(value_fsi)) |>
  mutate(
    diff_fsi_minus_fbs = value_fsi - value_fbs,
    abs_diff = abs(diff_fsi_minus_fbs),
    outlier_label = paste(area_fbs, year, sep = "\n")
  ) |>
  arrange(area_code, year)

if (nrow(matched) == 0) {
  stop("No matched country-year observations were found.")
}

write_csv(matched, file.path(output_dir, "fsi_fbs_matched_country_year.csv"))

label_points <- matched |>
  slice_max(abs_diff, n = label_n, with_ties = FALSE)

write_csv(label_points, file.path(output_dir, "fsi_fbs_labeled_large_diff.csv"))

plot_stats <- matched |>
  summarise(
    n = n(),
    countries = n_distinct(area_code),
    year_min = min(year),
    year_max = max(year),
    correlation = cor(value_fbs, value_fsi),
    mean_diff_fsi_minus_fbs = mean(value_fsi - value_fbs),
    rmse = sqrt(mean((value_fsi - value_fbs)^2))
  )

limits <- range(c(matched$value_fbs, matched$value_fsi), finite = TRUE)
padding <- diff(limits) * 0.04
limits <- c(
  floor((limits[1] - padding) / 100) * 100,
  ceiling((limits[2] + padding) / 100) * 100
)

subtitle_text <- sprintf(
  "Matched country-years: %s (%s countries, %s-%s); r = %.3f; mean FSI - FBS = %.1f kcal/cap/d",
  comma(plot_stats$n),
  plot_stats$countries,
  plot_stats$year_min,
  plot_stats$year_max,
  plot_stats$correlation,
  plot_stats$mean_diff_fsi_minus_fbs
)

p <- ggplot(matched, aes(x = value_fbs, y = value_fsi, colour = year)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey35", linewidth = 0.7) +
  geom_point(alpha = 0.58, size = 1.8) +
  coord_equal(xlim = limits, ylim = limits) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  geom_point(
    data = label_points,
    aes(x = value_fbs, y = value_fsi),
    inherit.aes = FALSE,
    shape = 21,
    fill = "white",
    colour = "black",
    size = 2.6,
    stroke = 0.7
  ) +
  geom_label_repel(
    data = label_points,
    aes(x = value_fbs, y = value_fsi, label = outlier_label),
    inherit.aes = FALSE,
    size = 3,
    label.size = 0.18,
    label.padding = unit(0.12, "lines"),
    min.segment.length = 0,
    max.overlaps = Inf,
    seed = 260706,
    colour = "black",
    fill = alpha("white", 0.86)
  ) +
  scale_colour_gradient(
    name = "Year",
    low = "#2b8cbe",
    high = "#e34a33",
    breaks = pretty_breaks(n = 5)
  ) +
  labs(
    title = "FAO FBS vs FSI, matched by country and year",
    subtitle = subtitle_text,
    x = "FBS: Food supply (kcal/capita/day)",
    y = "FSI: Dietary energy supply used for PoU estimation (kcal/cap/day)",
    caption = sprintf("Dashed line: x = y. Labels show the %s largest absolute differences.", label_n)
  ) +
  theme_bw(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(size = 10),
    legend.position = "right"
  )

plot_path <- file.path(output_dir, "fsi_fbs_scatter_xy.png")
ggsave(plot_path, p, width = 8.5, height = 7, dpi = 300, bg = "white")

print(plot_stats)
print(label_points |> select(area_fbs, year, value_fbs, value_fsi, diff_fsi_minus_fbs, abs_diff))
message("Saved plot: ", plot_path)
message("Saved matched data: ", file.path(output_dir, "fsi_fbs_matched_country_year.csv"))
message("Saved labeled points: ", file.path(output_dir, "fsi_fbs_labeled_large_diff.csv"))
