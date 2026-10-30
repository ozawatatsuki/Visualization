suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "hunger_risk_population_R17_WLD_decomposition"
)
source_csv <- file.path(
  out_dir,
  "hunger_risk_population_R17_WLD_baseline_effects_2030CP_data.csv"
)
if (!file.exists(source_csv)) {
  stop("Source data CSV not found: ", source_csv)
}

all_data <- fread(source_csv)
required_columns <- c("R17", "Year", "Component", "Value_million", "Total_million")
missing_columns <- setdiff(required_columns, names(all_data))
if (length(missing_columns) > 0L) {
  stop("Source CSV is missing columns: ", paste(missing_columns, collapse = ", "))
}

wld <- all_data[R17 == "WLD"]
if (nrow(wld) == 0L) {
  stop("WLD records are missing from the source CSV.")
}
if (anyDuplicated(wld, by = c("Year", "Component"))) {
  stop("Duplicate WLD Year x Component records.")
}

component_levels <- c("Baseline", "Income effect", "Price effect", "Cross price effect")
missing_components <- setdiff(component_levels, unique(wld$Component))
if (length(missing_components) > 0L) {
  stop("WLD is missing components: ", paste(missing_components, collapse = ", "))
}
year_levels <- sort(unique(as.integer(wld$Year)))
wld[, `:=`(
  Component = factor(Component, levels = component_levels),
  Year_factor = factor(as.integer(Year), levels = year_levels)
)]
setorder(wld, Year_factor, Component)

total_line <- unique(wld[, .(Year_factor, Year = as.integer(Year), Total_million)])
if (nrow(total_line) != length(year_levels)) {
  stop("Unexpected number of WLD total values.")
}
setorder(total_line, Year)

component_colors <- c(
  "Baseline" = "#7F7F7F",
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "Cross price effect" = "#009E73"
)

p <- ggplot(
  wld,
  aes(x = Year_factor, y = Value_million, fill = Component, group = Component)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.72,
    alpha = 0.92,
    colour = "grey28",
    linewidth = 0.15
  ) +
  geom_hline(yintercept = 0, colour = "grey30", linewidth = 0.55) +
  geom_line(
    data = total_line,
    aes(x = Year_factor, y = Total_million, group = 1),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 1.15
  ) +
  geom_point(
    data = total_line,
    aes(x = Year_factor, y = Total_million),
    inherit.aes = FALSE,
    shape = 21,
    size = 3.6,
    stroke = 0.8,
    fill = "white",
    colour = "black"
  ) +
  geom_text(
    data = total_line,
    aes(
      x = Year_factor,
      y = Total_million,
      label = label_number(accuracy = 1, big.mark = ",")(Total_million)
    ),
    inherit.aes = FALSE,
    vjust = -1.05,
    colour = "black",
    size = 4.3,
    fontface = "bold"
  ) +
  scale_fill_manual(values = component_colors, drop = FALSE) +
  scale_x_discrete(drop = FALSE) +
  scale_y_continuous(
    labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0.05, 0.13))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "WLD population at risk of hunger: Baseline and effects",
    subtitle = paste0(
      "400C / 2030CP; black line and labels show resulting PoU_In_Pr_CR. ",
      "Negative effects are stacked below zero."
    ),
    x = "Year",
    y = "Population at risk of hunger / effect (million people)",
    fill = NULL,
    caption = "Source: Fooddem_PriceEffect.gdx"
  ) +
  theme_minimal(base_size = 15) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey84", linewidth = 0.45),
    panel.border = element_rect(colour = "grey65", fill = NA, linewidth = 0.5),
    axis.text = element_text(colour = "grey18", size = 12.5),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(colour = "grey8", size = 14),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.height = grid::unit(0.6, "cm"),
    legend.key.width = grid::unit(1.0, "cm"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0),
    plot.subtitle = element_text(colour = "grey25", size = 12.5, hjust = 0),
    plot.caption = element_text(colour = "grey35", size = 10, hjust = 1),
    plot.margin = margin(12, 16, 10, 12)
  )

png_path <- file.path(out_dir, "hunger_risk_population_WLD_baseline_effects_2030CP.png")
data_path <- file.path(out_dir, "hunger_risk_population_WLD_baseline_effects_2030CP_data.csv")

ggsave(png_path, p, width = 10.5, height = 6.8, units = "in", dpi = 300, bg = "white")
fwrite(
  wld[, .(
    R17,
    Year = as.integer(Year),
    Component = as.character(Component),
    Value_million,
    Total_million
  )],
  data_path
)

if (!file.exists(png_path) || file.info(png_path)$size <= 0L) {
  stop("PNG was not created correctly: ", png_path)
}
cat("Created:", normalizePath(png_path, winslash = "/", mustWork = TRUE), "\n")
