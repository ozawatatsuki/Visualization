suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
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

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect_old.gdx")) &
    file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected one data/260807_FDM* directory containing the old and latest GDX files; found: ", length(data_candidates))
}
data_dir <- data_candidates[[1L]]

old_gdx <- file.path(data_dir, "Fooddem_PriceEffect_old.gdx")
latest_gdx <- file.path(data_dir, "Fooddem_Quantile_10.gdx")
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
scenario_label <- "400C / 2025CP"
target_year <- 2050L

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "past_latest_fooddem_positive_income_country_2025CP"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

read_filtered_parameter <- function(gdx_path, symbol, domains, filters = list()) {
  universe <- rgdx(gdx_path)$uels
  if (length(universe) == 0L) {
    stop("Empty GDX universe: ", gdx_path)
  }
  requested_uels <- lapply(domains, function(domain_name) {
    if (domain_name %in% names(filters)) as.character(filters[[domain_name]]) else universe
  })
  raw <- rgdx(
    gdx_path,
    list(name = symbol, form = "sparse", uels = requested_uels)
  )
  if (!identical(raw$dim, as.integer(length(domains)))) {
    stop("Unexpected dimension for ", symbol, " in ", basename(gdx_path), ".")
  }
  if (is.null(raw$val) || nrow(raw$val) == 0L) {
    stop("No filtered rows for ", symbol, " in ", basename(gdx_path), ".")
  }

  out <- as.data.table(setNames(
    lapply(seq_along(domains), function(j) raw$uels[[j]][raw$val[, j]]),
    domains
  ))
  out[[symbol]] <- as.numeric(raw$val[, length(domains) + 1L])
  out
}

complete_sparse <- function(sparse_dt, grid, keys, value_name) {
  out <- merge(grid, sparse_dt, by = keys, all.x = TRUE, sort = FALSE)
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

normalize_fooddem <- function(dt, symbol, segment_domain, value_name) {
  setnames(dt, c(segment_domain, symbol), c("quantile", value_name))
  for (col in c("Ref", "var", "R", "Y", "quantile")) {
    set(dt, j = col, value = as.character(dt[[col]]))
  }
  dt
}

prepare_effects <- function(gdx_path, version_label, income_symbol, price_symbol, segment_domain, country) {
  filters <- list(
    Ref = c(baseline_ref, scenario_ref),
    R = country,
    Y = as.character(target_year)
  )
  domains <- c("Ref", "var", "R", "Y", segment_domain)
  income <- normalize_fooddem(
    read_filtered_parameter(gdx_path, income_symbol, domains, filters),
    income_symbol,
    segment_domain,
    "income_value"
  )
  price <- normalize_fooddem(
    read_filtered_parameter(gdx_path, price_symbol, domains, filters),
    price_symbol,
    segment_domain,
    "price_value"
  )

  keys <- c("Ref", "var", "R", "Y", "quantile")
  if (anyDuplicated(income, by = keys) || anyDuplicated(price, by = keys)) {
    stop("Duplicate FoodDem keys in ", version_label, ".")
  }
  vars <- sort(unique(c(income$var, price$var)))
  quantiles <- unique(c(income$quantile, price$quantile))
  grid <- CJ(
    Ref = c(baseline_ref, scenario_ref),
    var = vars,
    R = country,
    Y = as.character(target_year),
    quantile = quantiles,
    unique = TRUE
  )
  income <- complete_sparse(income, grid, keys, "income_value")
  price <- complete_sparse(price, grid, keys, "price_value")
  combined <- merge(income, price, by = keys, all = FALSE, sort = FALSE)

  bau <- combined[Ref == baseline_ref, .(var, R, Y, quantile, bau_value = income_value)]
  scenario <- merge(
    combined[Ref == scenario_ref],
    bau,
    by = c("var", "R", "Y", "quantile"),
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(scenario$bau_value)) {
    stop("Missing BaU values in ", version_label, ".")
  }
  scenario[, `:=`(
    income_effect = income_value - bau_value,
    price_effect = price_value - income_value,
    total_effect = price_value - bau_value,
    Version = version_label
  )]
  decomposition_gap <- max(abs(scenario$income_effect + scenario$price_effect - scenario$total_effect))
  if (!is.finite(decomposition_gap) || decomposition_gap > 1e-8) {
    stop("FoodDem decomposition failed in ", version_label, ": ", decomposition_gap)
  }
  scenario
}

cat("Ranking countries by positive past-version income effects...\n")
old_income <- read_filtered_parameter(
  old_gdx,
  "FoodDemIn",
  c("Ref", "var", "R", "Y", "seg"),
  list(Ref = c(baseline_ref, scenario_ref), Y = as.character(target_year))
)
old_income <- normalize_fooddem(old_income, "FoodDemIn", "seg", "income_value")

ranking_keys <- c("Ref", "var", "R", "Y", "quantile")
if (anyDuplicated(old_income, by = ranking_keys)) {
  stop("Duplicate FoodDemIn keys in the past version.")
}
ranking_grid <- CJ(
  Ref = c(baseline_ref, scenario_ref),
  var = sort(unique(old_income$var)),
  R = sort(unique(old_income$R)),
  Y = as.character(target_year),
  quantile = unique(old_income$quantile),
  unique = TRUE
)
old_income <- complete_sparse(old_income, ranking_grid, ranking_keys, "income_value")
old_bau <- old_income[Ref == baseline_ref, .(var, R, Y, quantile, bau_value = income_value)]
old_scenario <- merge(
  old_income[Ref == scenario_ref],
  old_bau,
  by = c("var", "R", "Y", "quantile"),
  all.x = TRUE,
  sort = FALSE
)
if (anyNA(old_scenario$bau_value)) {
  stop("Missing past-version BaU values while ranking countries.")
}
old_scenario[, income_effect := income_value - bau_value]

tolerance <- 1e-10
detail_ranking <- old_scenario[
  var != "total" & quantile != "ALL" & grepl("^[A-Z]{3}$", R) & R != "WLD",
  .(
  detailed_cells = .N,
  positive_cells = sum(income_effect > tolerance),
  negative_cells = sum(income_effect < -tolerance),
  positive_share = mean(income_effect > tolerance),
  negative_share = mean(income_effect < -tolerance),
  mean_income_effect = mean(income_effect),
  median_income_effect = median(income_effect),
  net_income_effect = sum(income_effect),
  positive_magnitude = sum(pmax(income_effect, 0)),
  negative_magnitude = sum(pmin(income_effect, 0))
  ),
  by = R
]

total_all <- old_scenario[
  var == "total" & quantile == "ALL" & grepl("^[A-Z]{3}$", R) & R != "WLD",
  .(
  total_all_income_effect = sum(income_effect)
  ),
  by = R
]
ranking <- merge(detail_ranking, total_all, by = "R", all.x = TRUE, sort = FALSE)
ranking[is.na(total_all_income_effect), total_all_income_effect := 0]
ranking[, eligible :=
  positive_share >= 0.70 &
    mean_income_effect > tolerance &
    total_all_income_effect > tolerance
]
ranking[, prominence_score := fifelse(
  eligible,
  mean_income_effect * positive_share,
  -Inf
)]
setorder(
  ranking,
  -eligible,
  -prominence_score,
  -positive_share,
  -total_all_income_effect,
  R
)
ranking[, rank := seq_len(.N)]
setcolorder(ranking, c("rank", setdiff(names(ranking), "rank")))

if (!any(ranking$eligible)) {
  stop("No country met the positive-income-effect selection criteria.")
}
target_country <- "IRN"
if (ranking[R == target_country, .N] != 1L) {
  stop("Could not identify exactly one ranking row for ", target_country, ".")
}
selected_metrics <- ranking[R == target_country][1L]

ranking_csv <- file.path(out_dir, "FoodDem_past_positive_income_country_ranking_2050.csv")
fwrite(ranking, ranking_csv)
cat("Target country:", target_country, "\n")
print(ranking[1:min(12L, .N), .(
  rank, R, positive_share, mean_income_effect, total_all_income_effect, prominence_score
)])

cat("Reading past and latest FoodDem data for ", target_country, "...\n", sep = "")
fooddem_past <- prepare_effects(
  old_gdx,
  "Past version",
  "FoodDemIn",
  "FoodDem_peffect",
  "seg",
  target_country
)
fooddem_latest <- prepare_effects(
  latest_gdx,
  "Latest version",
  "FoodDemIn_Quantile",
  "FoodDem_peffect_Quantile",
  "quantile",
  target_country
)

if (!identical(sort(unique(fooddem_past$var)), sort(unique(fooddem_latest$var)))) {
  stop("Past/latest FoodDem category sets differ for ", target_country, ".")
}
if (!setequal(unique(fooddem_past$quantile), unique(fooddem_latest$quantile))) {
  stop("Past/latest FoodDem quantile sets differ for ", target_country, ".")
}

fooddem <- rbindlist(list(fooddem_past, fooddem_latest), use.names = TRUE)
fooddem[, quantile_num := suppressWarnings(as.integer(quantile))]
if (fooddem[quantile != "ALL" & is.na(quantile_num), .N] > 0L) {
  stop("Unexpected non-numeric quantile label for ", target_country, ".")
}
n_quantiles <- max(fooddem[quantile != "ALL", quantile_num])
if (!identical(sort(unique(fooddem[quantile != "ALL", quantile_num])), seq_len(n_quantiles))) {
  stop("Quantiles are not consecutive from 1 to ", n_quantiles, ".")
}
all_position <- n_quantiles + 2L
fooddem[quantile == "ALL", quantile_num := all_position]
fooddem[, Version := factor(Version, levels = c("Past version", "Latest version"))]
var_levels <- c(sort(setdiff(unique(fooddem$var), "total")), intersect("total", unique(fooddem$var)))
fooddem[, var := factor(var, levels = var_levels)]

effects <- melt(
  fooddem,
  id.vars = c("Version", "Ref", "var", "R", "Y", "quantile", "quantile_num", "total_effect"),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "Effect",
  value.name = "Value"
)
effects[, Effect := factor(
  Effect,
  levels = c("income_effect", "price_effect"),
  labels = c("Income effect", "Price effect")
)]
setorder(effects, var, Version, quantile_num, Effect)

effect_colors <- c("Income effect" = "#D55E00", "Price effect" = "#0072B2")
fooddem_var_labels <- c(
  "Cereals" = "Cereals",
  "Dairy" = "Dairy",
  "Fish" = "Fish",
  "Food_Other" = "Other foods",
  "Fruits_and_Veg" = "Fruits & vegetables",
  "Meats" = "Meat",
  "Oils_and_Fats" = "Oils & fats",
  "total" = "Total"
)

panel_keys <- unlist(lapply(
  c("Past version", "Latest version"),
  function(version_name) paste(version_name, var_levels, sep = " | ")
))
panel_labels <- setNames(
  unlist(lapply(
    c("Past version", "Latest version"),
    function(version_name) paste(version_name, unname(fooddem_var_labels[var_levels]), sep = " | ")
  )),
  panel_keys
)
effects[, Panel := factor(
  paste(as.character(Version), as.character(var), sep = " | "),
  levels = panel_keys
)]

positive_share_label <- percent(selected_metrics$positive_share, accuracy = 1)
total_all_label <- number(selected_metrics$total_all_income_effect, accuracy = 0.1, big.mark = ",")
selection_note <- paste0(
  "For IRN, in the past version, ", positive_share_label,
  " of category-decile income effects are positive; total ALL income effect = +",
  total_all_label, " kcal/day/capita."
)
subtitle_text <- paste(
  strwrap(
    paste0(
      selection_note,
      " Values above zero increase food demand; the shaded area marks this direction. ",
      "Versions are separated into panels so the past-version increase remains visible."
    ),
    width = 150
  ),
  collapse = "\n"
)
caption_text <- paste(
  strwrap(
    paste0(
      scenario_label, ". Past: Fooddem_PriceEffect_old.gdx; Latest: Fooddem_Quantile_10.gdx. ",
      "Income = FoodDemIn(scenario) - FoodDemIn(BaU); ",
      "Price = FoodDem_peffect(scenario) - FoodDemIn(scenario). Cross-price effects are excluded."
    ),
    width = 185
  ),
  collapse = "\n"
)

positive_panels <- unique(effects[, .(Panel)])
positive_panels[, `:=`(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf)]
past_income_all <- effects[
  Version == "Past version" & Effect == "Income effect" & quantile == "ALL"
]

p <- ggplot(
  effects,
  aes(
    x = quantile_num,
    y = Value,
    color = Effect,
    group = Effect
  )
) +
  geom_rect(
    data = positive_panels,
    aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    inherit.aes = FALSE,
    fill = "#FFF3E8",
    alpha = 0.58
  ) +
  geom_hline(yintercept = 0, color = "grey40", linewidth = 0.55) +
  geom_line(
    data = effects[quantile != "ALL"],
    linewidth = 1.15
  ) +
  geom_line(
    data = effects[quantile != "ALL" & Version == "Past version" & Effect == "Income effect"],
    linewidth = 1.55
  ) +
  geom_point(size = 2.35, stroke = 0.65) +
  geom_point(
    data = past_income_all,
    size = 3.15,
    stroke = 0.95
  ) +
  geom_vline(
    xintercept = n_quantiles + 1L,
    color = "grey52",
    linewidth = 0.45,
    linetype = "dotted"
  ) +
  facet_wrap(
    vars(Panel),
    ncol = 4,
    scales = "free_y",
    labeller = as_labeller(panel_labels)
  ) +
  scale_color_manual(values = effect_colors, drop = FALSE) +
  scale_x_continuous(
    breaks = c(seq_len(n_quantiles), all_position),
    labels = c(as.character(seq_len(n_quantiles)), "ALL"),
    limits = c(0.5, all_position + 0.65),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1, override.aes = list(linewidth = 1.4))
  ) +
  labs(
    title = paste0(target_country, " FoodDem effects in ", target_year, ": past vs latest"),
    subtitle = subtitle_text,
    x = "Income decile",
    y = "Effect on FoodDem (kcal/day/capita)",
    color = "Effect",
    caption = caption_text
  ) +
  theme_minimal(base_size = 13.5) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey84", linewidth = 0.35),
    panel.border = element_rect(color = "grey72", fill = NA, linewidth = 0.4),
    axis.text = element_text(color = "grey22", size = 10.5),
    axis.title = element_text(color = "grey10", size = 12.5),
    legend.position = "bottom",
    legend.box = "vertical",
    legend.text = element_text(size = 11.5),
    legend.title = element_text(size = 11.5, face = "bold"),
    plot.title = element_text(face = "bold", color = "grey10", size = 18),
    plot.subtitle = element_text(color = "grey25", size = 11.5, margin = margin(b = 8)),
    plot.caption = element_text(color = "grey34", hjust = 0, size = 9.5),
    strip.text = element_text(face = "bold", color = "grey10", size = 11.5),
    strip.background = element_rect(fill = "grey95", color = NA),
    plot.margin = margin(12, 15, 12, 12)
  )

plot_base <- paste0(target_country, "_FoodDem_income_price_effects_past_vs_latest_2050")
png_path <- file.path(out_dir, paste0(plot_base, ".png"))
data_csv <- file.path(out_dir, paste0(plot_base, "_data.csv"))
selection_csv <- file.path(out_dir, paste0(target_country, "_selection_metrics_2050.csv"))

ggsave(png_path, p, width = 16, height = 12.5, units = "in", dpi = 300, bg = "white")
fwrite(
  effects[, .(
    Version = as.character(Version), Ref, var = as.character(var), R, Y,
    quantile, Panel = as.character(Panel), Effect = as.character(Effect), Value, total_effect
  )],
  data_csv
)
fwrite(selected_metrics, selection_csv)

if (!file.exists(png_path) || file.info(png_path)$size <= 0L) {
  stop("PNG was not created correctly: ", png_path)
}

cat("Created:", normalizePath(png_path, winslash = "/", mustWork = TRUE), "\n")
cat("Ranking:", normalizePath(ranking_csv, winslash = "/", mustWork = TRUE), "\n")
cat("Plot data:", normalizePath(data_csv, winslash = "/", mustWork = TRUE), "\n")
