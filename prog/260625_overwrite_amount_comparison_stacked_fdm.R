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
default_data_dir <- file.path(project_dir, "data", "260623_金額比較")
decile_data_dir <- Sys.getenv("AMOUNT_DECILE_DATA_DIR", unset = default_data_dir)
fdm_data_dir <- Sys.getenv("AMOUNT_FDM_DATA_DIR", unset = default_data_dir)
output_dir <- Sys.getenv(
  "AMOUNT_OUTPUT_DIR",
  unset = file.path(project_dir, "output", "260623_金額比較")
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

decile_country_gdx <- file.path(decile_data_dir, "DecileResults_country_10.gdx")
fdm_gdx <- file.path(
  fdm_data_dir,
  Sys.getenv("AMOUNT_FDM_FILE", unset = "FDM2PHI_10.gdx")
)

required_files <- c(decile_country_gdx, fdm_gdx)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(sprintf("Missing input file(s): %s", paste(missing_files, collapse = ", ")))
}

ref_levels <- c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No")
seg_levels <- c(as.character(1:10), "Ave")
fdm_goods <- c("Cereals", "Meats", "Dairy", "Fruits_and_Veg", "Oils_and_Fats", "Food_Other")
target_year <- "2050"
target_item <- "Food and nonalcoholic beverages"

expenditure_all <- as_tibble(rgdx.param(decile_country_gdx, "Expenditure")) %>%
  filter(
    as.character(.data$i2) %in% ref_levels,
    as.character(.data$i3) == target_year,
    as.character(.data$i4) == target_item
  ) %>%
  transmute(
    R = as.character(.data$i1),
    Ref = as.character(.data$i2),
    seg = if_else(as.character(.data$i5) == "Ave", "Ave", as.character(.data$i5)),
    value = as.numeric(.data$value)
  )

fdm_raw <- as_tibble(rgdx.param(fdm_gdx, "FoodDem_2021PPP"))
if ("quantile" %in% names(fdm_raw) && !"seg" %in% names(fdm_raw)) {
  fdm_raw <- fdm_raw %>% rename(seg = quantile)
}
if (!"seg" %in% names(fdm_raw)) {
  stop("FoodDem_2021PPP must contain either a seg or quantile domain.")
}

fdm_goods_all <- fdm_raw %>%
  filter(
    as.character(.data$Ref) %in% ref_levels,
    as.character(.data$Y) == target_year,
    as.character(.data$var) %in% fdm_goods
  ) %>%
  transmute(
    R = as.character(.data$R),
    Ref = as.character(.data$Ref),
    seg = if_else(as.character(.data$seg) == "ALL", "Ave", as.character(.data$seg)),
    component = as.character(.data$var),
    value = as.numeric(.data$FoodDem_2021PPP)
  )

fdm_total_all <- fdm_raw %>%
  filter(
    as.character(.data$Ref) %in% ref_levels,
    as.character(.data$Y) == target_year,
    as.character(.data$var) == "total"
  ) %>%
  transmute(
    R = as.character(.data$R),
    Ref = as.character(.data$Ref),
    seg = if_else(as.character(.data$seg) == "ALL", "Ave", as.character(.data$seg)),
    total = as.numeric(.data$FoodDem_2021PPP)
  )

paired_all <- inner_join(
  expenditure_all %>% rename(aim_phi = value),
  fdm_total_all %>% rename(fdm_total = total),
  by = c("R", "Ref", "seg")
) %>%
  mutate(diff = .data$aim_phi - .data$fdm_total)

complete_countries <- paired_all %>%
  count(.data$R, name = "n_points") %>%
  filter(.data$n_points == length(ref_levels) * length(seg_levels)) %>%
  pull("R")

ranking <- paired_all %>%
  filter(.data$R %in% complete_countries) %>%
  group_by(.data$R) %>%
  summarise(
    mean_diff = mean(.data$diff),
    mean_abs_diff = mean(abs(.data$diff)),
    .groups = "drop"
  )

absolute_countries <- ranking %>%
  arrange(desc(.data$mean_abs_diff), .data$R) %>%
  slice_head(n = 5) %>%
  pull("R")

positive_countries <- ranking %>%
  filter(.data$mean_diff > 0) %>%
  arrange(desc(.data$mean_diff), .data$R) %>%
  slice_head(n = 5) %>%
  pull("R")

negative_countries <- ranking %>%
  filter(.data$mean_diff < 0) %>%
  arrange(.data$mean_diff, .data$R) %>%
  slice_head(n = 5) %>%
  pull("R")

country_sets <- list(
  absolute_top5 = list(
    countries = absolute_countries,
    file = "amount_comparison_top5_countries_by_diff_2050_SSP2.png",
    title = "Top 5 countries by AIM-PHI vs FDM absolute difference, SSP2 scenarios, 2050",
    subtitle = "FDM is shown as stacked goods; ranking uses mean absolute AIM-PHI minus FDM total."
  ),
  positive_top5 = list(
    countries = positive_countries,
    file = "amount_comparison_top5_positive_diff_2050_SSP2.png",
    title = "Top 5 countries with largest positive AIM-PHI minus FDM difference, SSP2 scenarios, 2050",
    subtitle = "FDM is shown as stacked goods; ranking uses AIM-PHI minus FDM total."
  ),
  negative_top5 = list(
    countries = negative_countries,
    file = "amount_comparison_top5_negative_diff_2050_SSP2.png",
    title = "Top 5 countries with largest negative AIM-PHI minus FDM difference, SSP2 scenarios, 2050",
    subtitle = "FDM is shown as stacked goods; ranking uses AIM-PHI minus FDM total."
  ),
  developed3 = list(
    countries = c("USA", "JPN", "DEU"),
    file = "amount_comparison_developed3_representative_2050_SSP2.png",
    title = "Developed representative countries: AIM-PHI vs stacked FDM goods, SSP2 scenarios, 2050",
    subtitle = "Countries: USA, JPN, DEU."
  ),
  developing3 = list(
    countries = c("CHN", "IND", "BRA"),
    file = "amount_comparison_developing3_representative_2050_SSP2.png",
    title = "Developing/Emerging representative countries: AIM-PHI vs stacked FDM goods, SSP2 scenarios, 2050",
    subtitle = "Countries: CHN, IND, BRA."
  ),
  story = list(
    countries = c("ARE", "HKG", "KHM", "IND", "CHN", "USA"),
    story = c(
      "High-income opposite",
      "High-income opposite",
      "Developing opposite",
      "Developing opposite",
      "Large mixed sign",
      "Large mixed sign"
    ),
    file = "amount_comparison_story_countries_2050_SSP2.png",
    title = "Story countries: AIM-PHI vs stacked FDM goods, SSP2 scenarios, 2050",
    subtitle = "Selected to contrast opposite model gaps within high-income, developing, and large-economy cases."
  )
)

make_plot_data <- function(countries, story = NULL) {
  country_meta <- tibble(R = countries)
  if (!is.null(story)) {
    country_meta <- country_meta %>%
      mutate(story = story)
  }

  bind_rows(
    expenditure_all %>%
      filter(.data$R %in% countries) %>%
      mutate(component = "AIM-PHI"),
    fdm_goods_all %>%
      filter(.data$R %in% countries)
  ) %>%
    left_join(country_meta, by = "R") %>%
    mutate(
      R = factor(.data$R, levels = countries),
      Ref = factor(.data$Ref, levels = ref_levels),
      seg = factor(.data$seg, levels = seg_levels),
      component = factor(.data$component, levels = c("AIM-PHI", fdm_goods)),
      seg_index = as.integer(.data$seg),
      x_position = if_else(.data$component == "AIM-PHI", .data$seg_index - 0.19, .data$seg_index + 0.19)
    )
}

check_stack <- function(countries) {
  stack_check <- fdm_goods_all %>%
    filter(.data$R %in% countries) %>%
    group_by(.data$R, .data$Ref, .data$seg) %>%
    summarise(sum_goods = sum(.data$value), .groups = "drop") %>%
    left_join(fdm_total_all, by = c("R", "Ref", "seg")) %>%
    mutate(diff = .data$sum_goods - .data$total)

  max(abs(stack_check$diff), na.rm = TRUE)
}

make_plot <- function(data, title, subtitle, include_story = FALSE) {
  facet_rows <- if (include_story) vars(.data$story, .data$R) else vars(.data$R)

  ggplot(data, aes(x = .data$x_position, y = .data$value, fill = .data$component)) +
    geom_col(width = 0.34, color = "grey25", linewidth = 0.12) +
    facet_grid(rows = facet_rows, cols = vars(.data$Ref), scales = "free_y") +
    scale_x_continuous(breaks = seq_along(seg_levels), labels = seg_levels) +
    scale_fill_manual(
      breaks = c("AIM-PHI", fdm_goods),
      values = c(
        "AIM-PHI" = "#2F6B9A",
        "Cereals" = "#D9892B",
        "Meats" = "#B33F62",
        "Dairy" = "#7D5BA6",
        "Fruits_and_Veg" = "#4C956C",
        "Oils_and_Fats" = "#D4A017",
        "Food_Other" = "#8A8C8E"
      )
    ) +
    scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.08))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "seg",
      y = "value",
      fill = "series / FDM good",
      caption = "FDM stacked goods sum to the FDM total. DEC=Ave is matched with seg=ALL."
    ) +
    theme_bw(base_size = 13) +
    theme(
      legend.position = "bottom",
      legend.box = "vertical",
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 0, vjust = 0.5),
      strip.background = element_rect(fill = "white", color = "grey45"),
      strip.text = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0)
    )
}

saved_files <- character()

for (set_name in names(country_sets)) {
  spec <- country_sets[[set_name]]
  max_stack_diff <- check_stack(spec$countries)
  if (max_stack_diff > 1e-7) {
    stop(sprintf("FDM goods do not sum to total for %s.", set_name))
  }

  plot_data <- make_plot_data(spec$countries, story = spec$story)
  expected_rows <- length(spec$countries) * length(ref_levels) * length(seg_levels) *
    (1 + length(fdm_goods))
  if (nrow(plot_data) != expected_rows) {
    stop(sprintf("Unexpected row count for %s: %s, expected %s.", set_name, nrow(plot_data), expected_rows))
  }
  if (any(is.na(plot_data$value))) {
    stop(sprintf("Filtered data contains NA values for %s.", set_name))
  }

  plot <- make_plot(
    plot_data,
    title = spec$title,
    subtitle = spec$subtitle,
    include_story = !is.null(spec$story)
  )
  out_file <- file.path(output_dir, spec$file)
  height <- if (length(spec$countries) <= 3) 16.5 else 33
  ggsave(out_file, plot, width = 12, height = height, dpi = 300, limitsize = FALSE)
  saved_files <- c(saved_files, out_file)
  cat("Saved:", normalizePath(out_file, winslash = "/", mustWork = FALSE), "\n")
}

cat("Positive top 5:", paste(positive_countries, collapse = ", "), "\n")
cat("Negative top 5:", paste(negative_countries, collapse = ", "), "\n")
cat("Absolute top 5:", paste(absolute_countries, collapse = ", "), "\n")
