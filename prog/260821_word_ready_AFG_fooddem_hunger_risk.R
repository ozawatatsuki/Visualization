suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

out_root <- file.path(project_dir, "output", "260807_FDM_scenario_quantile")
fooddem_dir <- file.path(
  out_root,
  "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_by_R"
)
hunger_dir <- file.path(out_root, "hunger_risk_population_R17_WLD_decomposition")

fooddem_csv <- file.path(
  fooddem_dir,
  "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_data.csv"
)
fooddem_png <- file.path(
  fooddem_dir,
  "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_AFG_2050_word_ready.png"
)
hunger_png <- file.path(
  hunger_dir,
  "hunger_risk_population_AFG_baseline_effects_2030CP_word_ready.png"
)

if (!file.exists(fooddem_csv)) {
  stop("Food-demand source CSV was not found: ", fooddem_csv)
}

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "Cross price effect" = "#009E73"
)

# AFG food-demand plot: two columns make every panel and label readable on a
# portrait Word page without changing the underlying decomposition.
fooddem <- fread(fooddem_csv)[R == "AFG" & year == 2050L]
if (nrow(fooddem) == 0L) {
  stop("No AFG 2050 records were found in the food-demand source CSV.")
}

fooddem[, `:=`(
  quantile_num = suppressWarnings(as.integer(quantile)),
  is_all = quantile == "ALL"
)]
n_quantiles <- max(fooddem[is_all == FALSE, quantile_num])
all_position <- n_quantiles + 3L
fooddem[is_all == TRUE, quantile_num := all_position]

var_order <- c(
  "Cereals", "Dairy", "Fish", "Food_Other",
  "Fruits_and_Veg", "Meats", "Oils_and_Fats", "total"
)
fooddem[, var_label := fifelse(
  var == "total",
  "Total",
  gsub("_", " ", var, fixed = TRUE)
)]
label_order <- c(
  "Cereals", "Dairy", "Fish", "Food Other",
  "Fruits and Veg", "Meats", "Oils and Fats", "Total"
)
fooddem[, var_label := factor(var_label, levels = label_order)]
fooddem[, Effect := factor(
  Effect,
  levels = c("Income effect", "Price effect", "Cross price effect")
)]

p_fooddem <- ggplot(
  fooddem,
  aes(x = quantile_num, y = Value_difference, fill = Effect)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.76,
    alpha = 0.96
  ) +
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.45) +
  geom_vline(
    xintercept = (n_quantiles + all_position) / 2,
    colour = "grey48",
    linewidth = 0.45,
    linetype = "dotted"
  ) +
  facet_wrap(vars(var_label), ncol = 2, scales = "free_y", drop = FALSE) +
  scale_fill_manual(values = effect_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = c(seq_len(n_quantiles), all_position),
    labels = c(as.character(seq_len(n_quantiles)), "ALL"),
    limits = c(0.45, all_position + 0.8),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE)) +
  labs(
    title = "AFG food-demand effects in 2050",
    subtitle = "400C / 2030CP relative to BaU; stacked by effect",
    x = "Income decile (ALL = national total)",
    y = "Difference from BaU (model value)",
    fill = NULL,
    caption = "Income, price, and cross-price effects; source: Fooddem_Quantile_10.gdx"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey85", linewidth = 0.4),
    axis.text = element_text(colour = "grey18", size = 11.5),
    axis.title = element_text(colour = "grey10", face = "bold", size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 12),
    legend.key.height = grid::unit(0.55, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(colour = "grey28", size = 13, hjust = 0.5),
    plot.caption = element_text(colour = "grey38", size = 9.5, hjust = 1),
    strip.text = element_text(face = "bold", colour = "grey10", size = 13),
    strip.background = element_rect(fill = "grey94", colour = "grey72", linewidth = 0.35),
    panel.spacing = grid::unit(0.5, "cm"),
    plot.margin = margin(10, 12, 8, 10)
  )

ggsave(
  fooddem_png,
  p_fooddem,
  width = 7.3,
  height = 9.6,
  units = "in",
  dpi = 300,
  bg = "white"
)

# AFG hunger-risk decomposition using the country-level equivalents of the
# three R17 symbols: PoU_In, PoU_In_Pr, and PoU_In_Pr_CR.
data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected one Fooddem_PriceEffect.gdx; found ", length(data_candidates), ".")
}
source_gdx <- file.path(data_candidates[[1]], "Fooddem_PriceEffect.gdx")
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2030CP_NoCC_No"

read_afg_symbol <- function(symbol, value_name) {
  out <- as.data.table(rgdx.param(source_gdx, symbol))
  required <- c("Ref", "R", "Y", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    as.character(R) == "AFG" &
      as.character(Ref) %in% c(baseline_ref, scenario_ref),
    .(
      Ref = as.character(Ref),
      Year = as.integer(as.character(Y)),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_name)
  out
}

income <- read_afg_symbol("PoU_In", "income_absolute")
price <- read_afg_symbol("PoU_In_Pr", "price_absolute")
cross <- read_afg_symbol("PoU_In_Pr_CR", "cross_absolute")
afg_all <- Reduce(
  function(x, y) merge(x, y, by = c("Ref", "Year"), all = TRUE, sort = FALSE),
  list(income, price, cross)
)

baseline <- afg_all[Ref == baseline_ref, .(Year, Baseline = income_absolute)]
afg_wide <- merge(
  afg_all[Ref == scenario_ref],
  baseline,
  by = "Year",
  all.x = TRUE,
  sort = FALSE
)
afg_wide[, `:=`(
  `Income effect` = income_absolute - Baseline,
  `Price effect` = price_absolute - income_absolute,
  `Cross price effect` = cross_absolute - price_absolute,
  Total = cross_absolute,
  Year_factor = factor(Year, levels = sort(unique(Year)))
)]

decomposition_gap <- max(abs(
  afg_wide$Baseline +
    afg_wide[["Income effect"]] +
    afg_wide[["Price effect"]] +
    afg_wide[["Cross price effect"]] -
    afg_wide$Total
))
if (!is.finite(decomposition_gap) || decomposition_gap > 1e-6) {
  stop("AFG PoU decomposition failed. Maximum gap: ", decomposition_gap)
}

afg_wide[, Total_million := Total / 1e6]
afg_long <- melt(
  afg_wide,
  id.vars = c("Year", "Year_factor", "Total_million"),
  measure.vars = c("Baseline", "Income effect", "Price effect", "Cross price effect"),
  variable.name = "Component",
  value.name = "Value"
)
afg_long[, `:=`(
  Component = factor(
    Component,
    levels = c("Baseline", "Income effect", "Price effect", "Cross price effect")
  ),
  Value_million = Value / 1e6
)]

component_colors <- c("Baseline" = "#7F7F7F", effect_colors)
p_hunger <- ggplot(
  afg_long,
  aes(x = Year_factor, y = Value_million, fill = Component)
) +
  geom_col(
    position = position_stack(reverse = TRUE),
    width = 0.72,
    alpha = 0.96
  ) +
  geom_hline(yintercept = 0, colour = "grey25", linewidth = 0.5) +
  geom_line(
    data = unique(afg_wide[, .(Year_factor, Total_million)]),
    aes(x = Year_factor, y = Total_million, group = 1),
    inherit.aes = FALSE,
    colour = "black",
    linewidth = 0.9
  ) +
  geom_point(
    data = unique(afg_wide[, .(Year_factor, Total_million)]),
    aes(x = Year_factor, y = Total_million),
    inherit.aes = FALSE,
    shape = 21,
    size = 3.2,
    stroke = 0.75,
    fill = "white",
    colour = "black"
  ) +
  scale_fill_manual(values = component_colors, drop = FALSE) +
  scale_y_continuous(
    labels = label_number(big.mark = ",", accuracy = 1),
    expand = expansion(mult = c(0.08, 0.09))
  ) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(
    title = "AFG population at risk of hunger",
    subtitle = "400C / 2030CP: baseline and effect decomposition",
    x = "Year",
    y = "Population / effect (million people)",
    fill = NULL,
    caption = "Black line: resulting PoU_In_Pr_CR; source: Fooddem_PriceEffect.gdx"
  ) +
  theme_minimal(base_size = 14, base_family = "Arial") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey84", linewidth = 0.45),
    axis.text = element_text(colour = "grey18", size = 12),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title = element_text(colour = "grey10", face = "bold", size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 11.5),
    legend.key.height = grid::unit(0.55, "cm"),
    legend.key.width = grid::unit(0.8, "cm"),
    plot.title = element_text(face = "bold", size = 20, hjust = 0.5),
    plot.subtitle = element_text(colour = "grey28", size = 13, hjust = 0.5),
    plot.caption = element_text(colour = "grey38", size = 9.5, hjust = 1),
    plot.margin = margin(10, 12, 8, 10)
  )

ggsave(
  hunger_png,
  p_hunger,
  width = 7.3,
  height = 5.6,
  units = "in",
  dpi = 300,
  bg = "white"
)

cat("Created Word-ready AFG food-demand PNG:\n")
cat(normalizePath(fooddem_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created Word-ready AFG hunger-risk PNG:\n")
cat(normalizePath(hunger_png, winslash = "/", mustWork = TRUE), "\n")
