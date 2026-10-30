suppressPackageStartupMessages({
  library(dplyr)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
  library(tibble)
})

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

script_dir <- get_script_dir()
project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
data_dir <- file.path(project_dir, "data", "260623_金額比較")
output_dir <- file.path(project_dir, "output", "260623_金額比較")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

decile_country_gdx <- file.path(data_dir, "DecileResults_country_10.gdx")
fdm_gdx <- file.path(data_dir, "FDM2PHI_10.gdx")

required_files <- c(decile_country_gdx, fdm_gdx)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(sprintf("Missing input file(s): %s", paste(missing_files, collapse = ", ")))
}

ref_levels <- c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No")
seg_levels <- c(as.character(1:10), "Ave")
target_year <- "2050"
target_item <- "Food and nonalcoholic beverages"
target_var <- "total"

expenditure <- as_tibble(rgdx.param(decile_country_gdx, "Expenditure")) %>%
  filter(
    as.character(.data$i2) %in% ref_levels,
    as.character(.data$i3) == target_year,
    as.character(.data$i4) == target_item
  ) %>%
  transmute(
    R = as.character(.data$i1),
    Ref = as.character(.data$i2),
    seg = if_else(as.character(.data$i5) == "Ave", "Ave", as.character(.data$i5)),
    Expenditure = as.numeric(.data$value)
  )

food_dem <- as_tibble(rgdx.param(fdm_gdx, "FoodDem_2021PPP")) %>%
  filter(
    as.character(.data$Ref) %in% ref_levels,
    as.character(.data$Y) == target_year,
    as.character(.data$var) == target_var
  ) %>%
  transmute(
    R = as.character(.data$R),
    Ref = as.character(.data$Ref),
    seg = if_else(as.character(.data$seg) == "ALL", "Ave", as.character(.data$seg)),
    FoodDem_2021PPP = as.numeric(.data$FoodDem_2021PPP)
  )

paired <- inner_join(expenditure, food_dem, by = c("R", "Ref", "seg")) %>%
  mutate(diff = .data$Expenditure - .data$FoodDem_2021PPP)

expected_points <- length(ref_levels) * length(seg_levels)
complete_countries <- paired %>%
  count(.data$R, name = "n_points") %>%
  filter(.data$n_points == expected_points) %>%
  pull("R")

ranking <- paired %>%
  filter(.data$R %in% complete_countries) %>%
  group_by(.data$R) %>%
  summarise(
    mean_diff = mean(.data$diff),
    max_diff = max(.data$diff),
    min_diff = min(.data$diff),
    total_diff = sum(.data$diff),
    n_points = n(),
    .groups = "drop"
  )

positive_top5 <- ranking %>%
  filter(.data$mean_diff > 0) %>%
  arrange(desc(.data$mean_diff), .data$R) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 5)

negative_top5 <- ranking %>%
  filter(.data$mean_diff < 0) %>%
  arrange(.data$mean_diff, .data$R) %>%
  mutate(rank = row_number()) %>%
  slice_head(n = 5)

if (nrow(positive_top5) < 5) {
  stop("Fewer than five countries with positive mean differences were available for plotting.")
}
if (nrow(negative_top5) < 5) {
  stop("Fewer than five countries with negative mean differences were available for plotting.")
}

make_plot_data <- function(countries) {
  bind_rows(
    expenditure %>%
      filter(.data$R %in% countries) %>%
      rename(value = Expenditure) %>%
      mutate(series = "Expenditure"),
    food_dem %>%
      filter(.data$R %in% countries) %>%
      rename(value = FoodDem_2021PPP) %>%
      mutate(series = "FoodDem_2021PPP")
  ) %>%
    mutate(
      R = factor(.data$R, levels = countries),
      Ref = factor(.data$Ref, levels = ref_levels),
      seg = factor(.data$seg, levels = seg_levels),
      series = factor(.data$series, levels = c("Expenditure", "FoodDem_2021PPP"))
    ) %>%
    arrange(.data$R, .data$Ref, .data$seg, .data$series)
}

make_plot <- function(plot_data, title, subtitle) {
  expected_rows <- 5 * length(ref_levels) * length(seg_levels) * 2
  if (nrow(plot_data) != expected_rows) {
    stop(sprintf("Unexpected row count after filtering: %s, expected %s.", nrow(plot_data), expected_rows))
  }
  if (any(is.na(plot_data$value))) {
    stop("Filtered data contains NA values.")
  }

  ggplot(plot_data, aes(x = .data$seg, y = .data$value, fill = .data$series)) +
    geom_col(
      position = position_dodge(width = 0.78),
      width = 0.68,
      color = "grey25",
      linewidth = 0.15
    ) +
    facet_grid(rows = vars(.data$R), cols = vars(.data$Ref), scales = "free_y") +
    scale_fill_manual(
      breaks = c("Expenditure", "FoodDem_2021PPP"),
      labels = c("AIM-PHI", "FDM"),
      values = c("Expenditure" = "#2F6B9A", "FoodDem_2021PPP" = "#D9892B")
    ) +
    scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "seg",
      y = "value",
      fill = "series",
      caption = "Difference is AIM-PHI minus FDM. AIM-PHI is Expenditure from DecileResults_country_10.gdx. FDM is FoodDem_2021PPP. DEC=Ave is matched with seg=ALL."
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      strip.background = element_rect(fill = "white", color = "grey45"),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0)
    )
}

positive_countries <- positive_top5 %>% pull("R")
negative_countries <- negative_top5 %>% pull("R")

positive_plot <- make_plot(
  make_plot_data(positive_countries),
  "Top 5 countries with largest positive AIM-PHI minus FDM difference, SSP2 scenarios, 2050",
  "Ranking is based on mean signed difference across 2 scenarios x 11 seg values."
)

negative_plot <- make_plot(
  make_plot_data(negative_countries),
  "Top 5 countries with largest negative AIM-PHI minus FDM difference, SSP2 scenarios, 2050",
  "Ranking is based on mean signed difference across 2 scenarios x 11 seg values."
)

positive_file <- file.path(output_dir, "amount_comparison_top5_positive_diff_2050_SSP2.png")
negative_file <- file.path(output_dir, "amount_comparison_top5_negative_diff_2050_SSP2.png")

ggsave(positive_file, positive_plot, width = 12, height = 27.5, dpi = 300, limitsize = FALSE)
ggsave(negative_file, negative_plot, width = 12, height = 27.5, dpi = 300, limitsize = FALSE)

cat("Top 5 positive countries:", paste(positive_countries, collapse = ", "), "\n")
cat("Top 5 negative countries:", paste(negative_countries, collapse = ", "), "\n")
cat("Saved:", normalizePath(positive_file, winslash = "/", mustWork = FALSE), "\n")
cat("Saved:", normalizePath(negative_file, winslash = "/", mustWork = FALSE), "\n")
