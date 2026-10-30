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

plot_data <- bind_rows(expenditure, food_dem) %>%
  left_join(story_countries, by = "R") %>%
  mutate(
    R = factor(.data$R, levels = country_levels),
    story = factor(.data$story, levels = unique(story_countries$story)),
    Ref = factor(.data$Ref, levels = ref_levels),
    seg = factor(.data$seg, levels = seg_levels),
    series = factor(.data$series, levels = c("Expenditure", "FoodDem_2021PPP"))
  ) %>%
  arrange(.data$story, .data$R, .data$Ref, .data$seg, .data$series)

expected_rows <- length(country_levels) * length(ref_levels) * length(seg_levels) * 2
if (nrow(plot_data) != expected_rows) {
  stop(sprintf("Unexpected row count after filtering: %s, expected %s.", nrow(plot_data), expected_rows))
}
if (any(is.na(plot_data$value))) {
  stop("Filtered data contains NA values.")
}

paired <- inner_join(
  expenditure %>% select(R, Ref, seg, AIM_PHI = value),
  food_dem %>% select(R, Ref, seg, FDM = value),
  by = c("R", "Ref", "seg")
) %>%
  mutate(diff = .data$AIM_PHI - .data$FDM)

summary_table <- paired %>%
  group_by(.data$R) %>%
  summarise(
    mean_diff = mean(.data$diff),
    diff_seg1 = mean(.data$diff[.data$seg == "1"]),
    diff_seg10 = mean(.data$diff[.data$seg == "10"]),
    scenario_change = mean(.data$diff[.data$Ref == "SSP2_400C_2025CP_NoCC_No"]) -
      mean(.data$diff[.data$Ref == "SSP2_BaU_NoCC_No"]),
    pattern = case_when(
      all(.data$diff > 0) ~ "AIM-PHI > FDM for all points",
      all(.data$diff < 0) ~ "FDM > AIM-PHI for all points",
      TRUE ~ "mixed sign by segment/scenario"
    ),
    .groups = "drop"
  ) %>%
  mutate(R = factor(.data$R, levels = country_levels)) %>%
  arrange(.data$R)

plot <- ggplot(plot_data, aes(x = .data$seg, y = .data$value, fill = .data$series)) +
  geom_col(
    position = position_dodge(width = 0.78),
    width = 0.68,
    color = "grey25",
    linewidth = 0.15
  ) +
  facet_grid(rows = vars(.data$story, .data$R), cols = vars(.data$Ref), scales = "free_y") +
  scale_fill_manual(
    breaks = c("Expenditure", "FoodDem_2021PPP"),
    labels = c("AIM-PHI", "FDM"),
    values = c("Expenditure" = "#2F6B9A", "FoodDem_2021PPP" = "#D9892B")
  ) +
  scale_y_continuous(labels = label_number(big.mark = ","), expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Story countries: AIM-PHI vs FDM, SSP2 scenarios, 2050",
    subtitle = "Selected to contrast opposite model gaps within high-income, developing, and large-economy cases.",
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

plot_file <- file.path(output_dir, "amount_comparison_story_countries_2050_SSP2.png")

ggsave(plot_file, plot, width = 12, height = 33, dpi = 300, limitsize = FALSE)

cat("Story countries:", paste(country_levels, collapse = ", "), "\n")
print(summary_table)
cat("Saved:", normalizePath(plot_file, winslash = "/", mustWork = FALSE), "\n")
