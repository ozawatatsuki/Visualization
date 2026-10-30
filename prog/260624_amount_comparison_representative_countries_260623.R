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

country_groups <- tibble(
  R = c("USA", "JPN", "DEU", "CHN", "IND", "BRA"),
  group = c("Developed", "Developed", "Developed", "Developing/Emerging", "Developing/Emerging", "Developing/Emerging")
)

ref_levels <- c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No")
seg_levels <- c(as.character(1:10), "Ave")
country_levels <- country_groups$R
target_year <- "2050"
target_item <- "Food and nonalcoholic beverages"
target_var <- "total"

expenditure <- as_tibble(rgdx.param(decile_country_gdx, "Expenditure")) %>%
  filter(
    as.character(.data$i1) %in% country_levels,
    as.character(.data$i2) %in% ref_levels,
    as.character(.data$i3) == target_year,
    as.character(.data$i4) == target_item
  ) %>%
  transmute(
    R = as.character(.data$i1),
    Ref = as.character(.data$i2),
    seg = if_else(as.character(.data$i5) == "Ave", "Ave", as.character(.data$i5)),
    series = "Expenditure",
    value = as.numeric(.data$value)
  )

food_dem <- as_tibble(rgdx.param(fdm_gdx, "FoodDem_2021PPP")) %>%
  filter(
    as.character(.data$R) %in% country_levels,
    as.character(.data$Ref) %in% ref_levels,
    as.character(.data$Y) == target_year,
    as.character(.data$var) == target_var
  ) %>%
  transmute(
    R = as.character(.data$R),
    Ref = as.character(.data$Ref),
    seg = if_else(as.character(.data$seg) == "ALL", "Ave", as.character(.data$seg)),
    series = "FoodDem_2021PPP",
    value = as.numeric(.data$FoodDem_2021PPP)
  )

all_plot_data <- bind_rows(expenditure, food_dem) %>%
  left_join(country_groups, by = "R") %>%
  mutate(
    R = factor(.data$R, levels = country_levels),
    group = factor(.data$group, levels = c("Developed", "Developing/Emerging")),
    Ref = factor(.data$Ref, levels = ref_levels),
    seg = factor(.data$seg, levels = seg_levels),
    series = factor(.data$series, levels = c("Expenditure", "FoodDem_2021PPP"))
  ) %>%
  arrange(.data$group, .data$R, .data$Ref, .data$seg, .data$series)

expected_rows <- length(country_levels) * length(ref_levels) * length(seg_levels) * 2
if (nrow(all_plot_data) != expected_rows) {
  stop(sprintf("Unexpected row count after filtering: %s, expected %s.", nrow(all_plot_data), expected_rows))
}
if (any(is.na(all_plot_data$value))) {
  stop("Filtered data contains NA values.")
}

make_plot <- function(data, title, subtitle) {
  ggplot(data, aes(x = .data$seg, y = .data$value, fill = .data$series)) +
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
      caption = "AIM-PHI is Expenditure from DecileResults_country_10.gdx. FDM is FoodDem_2021PPP. DEC=Ave is matched with seg=ALL."
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

developed_countries <- country_groups %>%
  filter(.data$group == "Developed") %>%
  pull("R")
developing_countries <- country_groups %>%
  filter(.data$group == "Developing/Emerging") %>%
  pull("R")

developed_plot <- all_plot_data %>%
  filter(.data$R %in% developed_countries) %>%
  mutate(R = factor(as.character(.data$R), levels = developed_countries)) %>%
  make_plot(
    title = "Developed representative countries: AIM-PHI vs FDM, SSP2 scenarios, 2050",
    subtitle = "Countries: USA, JPN, DEU."
  )

developing_plot <- all_plot_data %>%
  filter(.data$R %in% developing_countries) %>%
  mutate(R = factor(as.character(.data$R), levels = developing_countries)) %>%
  make_plot(
    title = "Developing/Emerging representative countries: AIM-PHI vs FDM, SSP2 scenarios, 2050",
    subtitle = "Countries: CHN, IND, BRA."
  )

developed_file <- file.path(output_dir, "amount_comparison_developed3_representative_2050_SSP2.png")
developing_file <- file.path(output_dir, "amount_comparison_developing3_representative_2050_SSP2.png")

ggsave(developed_file, developed_plot, width = 12, height = 16.5, dpi = 300, limitsize = FALSE)
ggsave(developing_file, developing_plot, width = 12, height = 16.5, dpi = 300, limitsize = FALSE)

cat("Developed countries:", paste(developed_countries, collapse = ", "), "\n")
cat("Developing/Emerging countries:", paste(developing_countries, collapse = ", "), "\n")
cat("Saved:", normalizePath(developed_file, winslash = "/", mustWork = FALSE), "\n")
cat("Saved:", normalizePath(developing_file, winslash = "/", mustWork = FALSE), "\n")
