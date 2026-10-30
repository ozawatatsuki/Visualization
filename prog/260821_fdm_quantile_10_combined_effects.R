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

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
]
if (length(data_candidates) != 1L) {
  stop(
    "Expected exactly one data/260807_FDM* directory containing Fooddem_Quantile_10.gdx; found: ",
    length(data_candidates)
  )
}

source_gdx <- file.path(data_candidates[[1]], "Fooddem_Quantile_10.gdx")
plot_year <- 2050L
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2030CP_NoCC_No"
scenario_label <- "400C_2030CP"
out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_by_R"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

old_outputs <- list.files(
  out_dir,
  pattern = "\\.(png|pdf|csv)$",
  full.names = TRUE,
  ignore.case = TRUE
)
if (length(old_outputs) > 0L) {
  unlink(old_outputs, force = TRUE)
}

safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

read_quantile_symbol <- function(symbol, value_name) {
  out <- as.data.table(rgdx.param(source_gdx, symbol))
  required <- c("Ref", "var", "R", "Y", "quantile", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    as.character(Y) == as.character(plot_year) &
      as.character(Ref) %in% c(baseline_ref, scenario_ref),
    .(
      Ref = as.character(Ref),
      var = as.character(var),
      R = as.character(R),
      year = as.integer(as.character(Y)),
      quantile = as.character(quantile),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_name)
  out
}

income_sparse <- read_quantile_symbol("FoodDemIn_Quantile", "income_value")
price_sparse <- read_quantile_symbol("FoodDem_peffect_Quantile", "price_value")
cross_price_sparse <- read_quantile_symbol(
  "FoodDem_peffect_CR_Quantile",
  "cross_price_value"
)

keys <- c("Ref", "var", "R", "year", "quantile")
for (x_name in c("income_sparse", "price_sparse", "cross_price_sparse")) {
  x <- get(x_name)
  if (anyDuplicated(x, by = keys) > 0L) {
    stop("Duplicate Ref/var/R/year/quantile keys found in ", x_name)
  }
}

fooddem_country_domain <- as.data.table(rgdx.param(source_gdx, "FoodDemIn_Quantile"))
region_levels <- sort(unique(as.character(fooddem_country_domain$R)))
var_levels <- sort(unique(c(
  income_sparse$var,
  price_sparse$var,
  cross_price_sparse$var
)))
quantile_levels <- unique(c(
  income_sparse$quantile,
  price_sparse$quantile,
  cross_price_sparse$quantile
))
if (length(region_levels) == 0L || length(var_levels) == 0L || length(quantile_levels) == 0L) {
  stop("Could not identify complete country, var, and quantile domains.")
}

# GDX parameters are sparse, so omitted records in the complete domain are zeros.
complete_grid <- CJ(
  Ref = c(baseline_ref, scenario_ref),
  var = var_levels,
  R = region_levels,
  year = plot_year,
  quantile = quantile_levels,
  unique = TRUE
)

complete_symbol <- function(sparse_data, value_name) {
  out <- merge(
    complete_grid,
    sparse_data,
    by = keys,
    all.x = TRUE,
    sort = FALSE
  )
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

income <- complete_symbol(income_sparse, "income_value")
price <- complete_symbol(price_sparse, "price_value")
cross_price <- complete_symbol(cross_price_sparse, "cross_price_value")
combined <- Reduce(
  function(x, y) merge(x, y, by = keys, all = FALSE, sort = FALSE),
  list(income, price, cross_price)
)

baseline <- combined[
  Ref == baseline_ref,
  .(
    var,
    R,
    year,
    quantile,
    bau_income_value = income_value
  )
]
scenario <- merge(
  combined[Ref == scenario_ref],
  baseline,
  by = c("var", "R", "year", "quantile"),
  all.x = TRUE,
  sort = FALSE
)
if (scenario[is.na(bau_income_value), .N] > 0L) {
  stop("Missing BaU FoodDemIn_Quantile values after domain completion.")
}

scenario[, `:=`(
  income_effect = income_value - bau_income_value,
  price_effect = price_value - income_value,
  cross_price_effect = cross_price_value - price_value,
  total_effect = cross_price_value - bau_income_value
)]
decomposition_gap <- scenario[
  , max(abs(income_effect + price_effect + cross_price_effect - total_effect))
]
if (!is.finite(decomposition_gap) || decomposition_gap > 1e-8) {
  stop("Three-effect decomposition failed. Maximum gap: ", signif(decomposition_gap, 8))
}

effects <- melt(
  scenario,
  id.vars = c("Ref", "var", "R", "year", "quantile", "total_effect"),
  measure.vars = c("income_effect", "price_effect", "cross_price_effect"),
  variable.name = "Effect",
  value.name = "Value_difference"
)
effects[, Effect := factor(
  Effect,
  levels = c("income_effect", "price_effect", "cross_price_effect"),
  labels = c("Income effect", "Price effect", "Cross price effect")
)]
effects[, is_all := quantile == "ALL"]
effects[, quantile_num := suppressWarnings(as.integer(quantile))]
if (effects[!is_all & is.na(quantile_num), .N] > 0L) {
  stop("Non-numeric quantiles other than ALL were found.")
}
n_quantiles <- max(effects[is_all == FALSE, quantile_num])
if (!identical(sort(unique(effects[is_all == FALSE, quantile_num])), seq_len(n_quantiles))) {
  stop("Quantiles are not consecutive from 1 to ", n_quantiles)
}
all_position <- n_quantiles + 3L
effects[is_all == TRUE, quantile_num := all_position]
effects[, var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))]
setorder(effects, R, var, Effect, quantile_num)

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2",
  "Cross price effect" = "#009E73"
)

theme_report <- function(base_size = 9) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.major.y = element_line(color = "grey86", linewidth = 0.25),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey10"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey30"),
      strip.text = element_text(face = "bold", color = "grey10"),
      strip.background = element_rect(fill = "grey96", color = NA),
      plot.margin = margin(8, 10, 8, 8)
    )
}

plot_region <- function(region_code) {
  region_dt <- effects[R == region_code]
  ggplot(
    region_dt,
    aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect)
  ) +
    geom_col(
      position = position_stack(reverse = TRUE),
      width = 0.72,
      alpha = 0.9,
      color = "grey35",
      linewidth = 0.08
    ) +
    geom_hline(yintercept = 0, color = "grey38", linewidth = 0.28) +
    geom_vline(
      xintercept = (n_quantiles + all_position) / 2,
      color = "grey55",
      linewidth = 0.25,
      linetype = "dotted"
    ) +
    facet_wrap(vars(var), ncol = 4, scales = "free_y") +
    scale_fill_manual(values = effect_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = c(seq_len(n_quantiles), all_position),
      labels = c(as.character(seq_len(n_quantiles)), "ALL"),
      limits = c(0.5, all_position + 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = paste0(
        region_code,
        " food-demand effects relative to BaU in ",
        plot_year,
        " - ",
        scenario_label,
        " (",
        n_quantiles,
        " quantiles)"
      ),
      subtitle = paste0(
        "Income = FoodDemIn(2030CP) - FoodDemIn(BaU); ",
        "Price = FoodDem_peffect - FoodDemIn; ",
        "Cross price = FoodDem_peffect_CR - FoodDem_peffect."
      ),
      x = "Quantile",
      y = "Difference from BaU (Value)",
      fill = NULL
    ) +
    theme_report(base_size = 9)
}

regions <- sort(unique(effects$R))
manifest <- vector("list", length(regions))
cat("Writing q10 three-effect stacked-bar PNG files...\n")
for (i in seq_along(regions)) {
  region_code <- regions[[i]]
  filename <- paste0(
    "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_",
    safe_filename(region_code),
    "_",
    plot_year,
    ".png"
  )
  output_path <- file.path(out_dir, filename)
  ggsave(
    output_path,
    plot_region(region_code),
    width = 15,
    height = 9.2,
    dpi = 240,
    bg = "white"
  )
  manifest[[i]] <- data.table(
    R = region_code,
    year = plot_year,
    scenario = scenario_label,
    quantiles = n_quantiles,
    effects = "Income effect | Price effect | Cross price effect",
    plot_type = "stacked_bar",
    file = filename
  )
  if (i %% 25L == 0L || i == length(regions)) {
    cat("  wrote ", i, "/", length(regions), " R PNG files\n", sep = "")
  }
}

fwrite(
  effects[
    , .(
      R,
      year,
      Ref,
      var = as.character(var),
      quantile,
      Effect = as.character(Effect),
      Value_difference,
      total_effect
    )
  ],
  file.path(out_dir, "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_data.csv")
)
fwrite(
  rbindlist(manifest),
  file.path(out_dir, "fooddem_quantile_bau_difference_2030CP_q10_combined_effects_manifest.csv")
)

cat("Created", length(regions), "PNG files in:\n")
cat(normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n")
