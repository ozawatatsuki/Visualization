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

story_countries <- tibble(
  R = c("ARE", "HKG", "KHM", "IND", "CHN", "USA"),
  story = c(
    "High-income opposite",
    "High-income opposite",
    "Developing opposite",
    "Developing opposite",
    "Large mixed sign",
    "Large mixed sign"
  )
)

ref_levels <- c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No")
seg_levels <- c(as.character(1:10), "Ave")
country_levels <- story_countries$R
fdm_goods <- c("Cereals", "Meats", "Dairy", "Fruits_and_Veg", "Oils_and_Fats", "Food_Other")
target_year <- "2050"
target_item <- "Food and nonalcoholic beverages"

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
    component = "AIM-PHI",
    value = as.numeric(.data$value)
  )

food_dem_goods <- as_tibble(rgdx.param(fdm_gdx, "FoodDem_2021PPP")) %>%
  filter(
    as.character(.data$R) %in% country_levels,
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

food_dem_total <- as_tibble(rgdx.param(fdm_gdx, "FoodDem_2021PPP")) %>%
  filter(
    as.character(.data$R) %in% country_levels,
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

stack_check <- food_dem_goods %>%
  group_by(.data$R, .data$Ref, .data$seg) %>%
  summarise(sum_goods = sum(.data$value), .groups = "drop") %>%
  left_join(food_dem_total, by = c("R", "Ref", "seg")) %>%
  mutate(diff = .data$sum_goods - .data$total)
if (max(abs(stack_check$diff), na.rm = TRUE) > 1e-7) {
  stop("FDM goods do not sum to total within tolerance.")
}

plot_data <- bind_rows(expenditure, food_dem_goods) %>%
  left_join(story_countries, by = "R") %>%
  mutate(
    R = factor(.data$R, levels = country_levels),
    story = factor(.data$story, levels = unique(story_countries$story)),
    Ref = factor(.data$Ref, levels = ref_levels),
    seg = factor(.data$seg, levels = seg_levels),
    component = factor(.data$component, levels = c("AIM-PHI", fdm_goods)),
    seg_index = as.integer(.data$seg),
    x_position = if_else(.data$component == "AIM-PHI", .data$seg_index - 0.19, .data$seg_index + 0.19)
  ) %>%
  arrange(.data$story, .data$R, .data$Ref, .data$seg, .data$component)

expected_rows <- length(country_levels) * length(ref_levels) * length(seg_levels) *
  (1 + length(fdm_goods))
if (nrow(plot_data) != expected_rows) {
  stop(sprintf("Unexpected row count after filtering: %s, expected %s.", nrow(plot_data), expected_rows))
}
if (any(is.na(plot_data$value))) {
  stop("Filtered data contains NA values.")
}

plot <- ggplot(plot_data, aes(x = .data$x_position, y = .data$value, fill = .data$component)) +
  geom_col(width = 0.34, color = "grey25", linewidth = 0.12) +
  facet_grid(rows = vars(.data$story, .data$R), cols = vars(.data$Ref), scales = "free_y") +
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
    title = "Story countries: AIM-PHI vs stacked FDM goods, SSP2 scenarios, 2050",
    subtitle = "AIM-PHI remains a single bar; FDM excludes total and stacks Cereals, Meats, Dairy, Fruits/Veg, Oils/Fats, and Food Other.",
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

plot_file <- file.path(output_dir, "amount_comparison_story_countries_stacked_fdm_2050_SSP2.png")

ggsave(plot_file, plot, width = 12, height = 33, dpi = 300, limitsize = FALSE)

cat("FDM goods:", paste(fdm_goods, collapse = ", "), "\n")
cat("Max absolute stack check:", max(abs(stack_check$diff), na.rm = TRUE), "\n")
cat("Saved:", normalizePath(plot_file, winslash = "/", mustWork = FALSE), "\n")
