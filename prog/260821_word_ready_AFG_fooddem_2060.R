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

plot_year <- 2060L
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2030CP_NoCC_No"

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected exactly one Fooddem_Quantile_10.gdx; found ", length(data_candidates), ".")
}
source_gdx <- file.path(data_candidates[[1]], "Fooddem_Quantile_10.gdx")

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_by_R"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_symbol <- function(symbol, value_name) {
  out <- as.data.table(rgdx.param(source_gdx, symbol))
  required <- c("Ref", "var", "R", "Y", "quantile", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    as.character(R) == "AFG" &
      as.character(Y) == as.character(plot_year) &
      as.character(Ref) %in% c(baseline_ref, scenario_ref),
    .(
      Ref = as.character(Ref),
      var = as.character(var),
      quantile = as.character(quantile),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_name)
  out
}

income_sparse <- read_symbol("FoodDemIn_Quantile", "income_value")
price_sparse <- read_symbol("FoodDem_peffect_Quantile", "price_value")
cross_sparse <- read_symbol("FoodDem_peffect_CR_Quantile", "cross_value")

domain <- as.data.table(rgdx.param(source_gdx, "FoodDemIn_Quantile"))
var_levels <- sort(unique(as.character(domain[["var"]])))
quantile_levels <- unique(as.character(domain[["quantile"]]))
grid <- CJ(
  Ref = c(baseline_ref, scenario_ref),
  var = var_levels,
  quantile = quantile_levels,
  unique = TRUE
)

complete_symbol <- function(sparse, value_name) {
  out <- merge(grid, sparse, by = c("Ref", "var", "quantile"), all.x = TRUE, sort = FALSE)
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

combined <- Reduce(
  function(x, y) merge(x, y, by = c("Ref", "var", "quantile"), all = FALSE, sort = FALSE),
  list(
    complete_symbol(income_sparse, "income_value"),
    complete_symbol(price_sparse, "price_value"),
    complete_symbol(cross_sparse, "cross_value")
  )
)

baseline <- combined[Ref == baseline_ref, .(var, quantile, bau_income = income_value)]
scenario <- merge(
  combined[Ref == scenario_ref],
  baseline,
  by = c("var", "quantile"),
  all.x = TRUE,
  sort = FALSE
)
scenario[, `:=`(
  `Income effect` = income_value - bau_income,
  `Price effect` = price_value - income_value,
  `Cross price effect` = cross_value - price_value,
  total_effect = cross_value - bau_income
)]

gap <- max(abs(
  scenario[["Income effect"]] +
    scenario[["Price effect"]] +
    scenario[["Cross price effect"]] -
    scenario$total_effect
))
if (!is.finite(gap) || gap > 1e-8) {
  stop("Food-demand decomposition failed. Maximum gap: ", gap)
}

effects <- melt(
  scenario,
  id.vars = c("var", "quantile", "total_effect"),
  measure.vars = c("Income effect", "Price effect", "Cross price effect"),
  variable.name = "Effect",
  value.name = "Value_difference"
)
effects[, `:=`(
  Effect = factor(
    Effect,
    levels = c("Income effect", "Price effect", "Cross price effect")
  ),
  is_all = quantile == "ALL",
  quantile_num = suppressWarnings(as.integer(quantile))
)]
n_quantiles <- max(effects[is_all == FALSE, quantile_num])
all_position <- n_quantiles + 3L
effects[is_all == TRUE, quantile_num := all_position]

effects[, var_label := fifelse(
  var == "total",
  "Total",
  gsub("_", " ", var, fixed = TRUE)
)]
label_order <- c(
  "Cereals", "Dairy", "Fish", "Food Other",
  "Fruits and Veg", "Meats", "Oils and Fats", "Total"
)
effects[, var_label := factor(var_label, levels = label_order)]

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "Cross price effect" = "#009E73"
)

p <- ggplot(
  effects,
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
    title = paste0("AFG food-demand effects in ", plot_year),
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

png_file <- file.path(
  out_dir,
  paste0(
    "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_AFG_",
    plot_year,
    "_word_ready.png"
  )
)
csv_file <- file.path(
  out_dir,
  paste0(
    "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_AFG_",
    plot_year,
    "_word_ready_data.csv"
  )
)

ggsave(
  png_file,
  p,
  width = 7.3,
  height = 9.6,
  units = "in",
  dpi = 300,
  bg = "white"
)
fwrite(
  effects[, .(
    R = "AFG",
    year = plot_year,
    Ref = scenario_ref,
    var,
    quantile,
    Effect = as.character(Effect),
    Value_difference,
    total_effect
  )],
  csv_file
)

cat("Created Word-ready AFG FoodDem PNG:\n")
cat(normalizePath(png_file, winslash = "/", mustWork = TRUE), "\n")
cat("Rows:", nrow(effects), " Facets:", uniqueN(effects$var_label), "\n")
