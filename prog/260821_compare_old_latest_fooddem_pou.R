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
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect_old.gdx")) &
    file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx")) &
    file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected one data/260807_FDM* directory containing all three GDX files; found: ", length(data_candidates))
}
data_dir <- data_candidates[[1L]]

old_price_gdx <- file.path(data_dir, "Fooddem_PriceEffect_old.gdx")
latest_price_gdx <- file.path(data_dir, "Fooddem_PriceEffect.gdx")
latest_q10_gdx <- file.path(data_dir, "Fooddem_Quantile_10.gdx")

baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
scenario_label <- "400C / 2025CP"
target_country <- "AFG"
target_year <- 2050L

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "past_latest_fooddem_pou_comparison_2025CP"
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

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

theme_report <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.major.y = element_line(color = "grey86", linewidth = 0.25),
      panel.border = element_rect(color = "grey78", fill = NA, linewidth = 0.3),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey10"),
      legend.position = "bottom",
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey30"),
      plot.caption = element_text(color = "grey35", hjust = 0),
      strip.text = element_text(face = "bold", color = "grey10"),
      strip.background = element_rect(fill = "grey96", color = NA),
      plot.margin = margin(10, 12, 10, 10)
    )
}

prepare_fooddem <- function(gdx_path, version_label, income_symbol, price_symbol, segment_domain) {
  filters <- list(
    Ref = c(baseline_ref, scenario_ref),
    R = target_country,
    Y = as.character(target_year)
  )
  domains <- c("Ref", "var", "R", "Y", segment_domain)
  income <- read_filtered_parameter(gdx_path, income_symbol, domains, filters)
  price <- read_filtered_parameter(gdx_path, price_symbol, domains, filters)
  setnames(income, c(segment_domain, income_symbol), c("quantile", "income_value"))
  setnames(price, c(segment_domain, price_symbol), c("quantile", "price_value"))

  normalize_cols <- c("Ref", "var", "R", "Y", "quantile")
  for (col in normalize_cols) {
    set(income, j = col, value = as.character(income[[col]]))
    set(price, j = col, value = as.character(price[[col]]))
  }
  keys <- c("Ref", "var", "R", "Y", "quantile")
  if (anyDuplicated(income, by = keys) || anyDuplicated(price, by = keys)) {
    stop("Duplicate FoodDem keys in ", version_label, ".")
  }

  vars <- sort(unique(c(income$var, price$var)))
  quantiles <- unique(c(income$quantile, price$quantile))
  grid <- CJ(
    Ref = c(baseline_ref, scenario_ref),
    var = vars,
    R = target_country,
    Y = as.character(target_year),
    quantile = quantiles,
    unique = TRUE
  )
  income_full <- complete_sparse(income, grid, keys, "income_value")
  price_full <- complete_sparse(price, grid, keys, "price_value")
  combined <- merge(income_full, price_full, by = keys, all = FALSE, sort = FALSE)

  bau <- combined[
    Ref == baseline_ref,
    .(var, R, Y, quantile, bau_value = income_value)
  ]
  scenario <- merge(
    combined[Ref == scenario_ref],
    bau,
    by = c("var", "R", "Y", "quantile"),
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(scenario$bau_value)) {
    stop("Missing FoodDem BaU rows in ", version_label, ".")
  }
  scenario[, `:=`(
    income_effect = income_value - bau_value,
    price_effect = price_value - income_value,
    total_effect = price_value - bau_value,
    Version = version_label
  )]
  gap <- max(abs(scenario$income_effect + scenario$price_effect - scenario$total_effect))
  if (!is.finite(gap) || gap > 1e-8) {
    stop("FoodDem decomposition failed in ", version_label, ": ", gap)
  }
  scenario
}

cat("Reading AFG FoodDem data...\n")
fooddem_past <- prepare_fooddem(
  old_price_gdx,
  "Past version",
  "FoodDemIn",
  "FoodDem_peffect",
  "seg"
)
fooddem_latest <- prepare_fooddem(
  latest_q10_gdx,
  "Latest version",
  "FoodDemIn_Quantile",
  "FoodDem_peffect_Quantile",
  "quantile"
)

past_vars <- sort(unique(fooddem_past$var))
latest_vars <- sort(unique(fooddem_latest$var))
past_quantiles <- sort(unique(fooddem_past$quantile))
latest_quantiles <- sort(unique(fooddem_latest$quantile))
if (!identical(past_vars, latest_vars)) {
  stop("Past/latest FoodDem var sets differ.")
}
if (!setequal(past_quantiles, latest_quantiles)) {
  stop("Past/latest FoodDem quantile sets differ.")
}

fooddem <- rbindlist(list(fooddem_past, fooddem_latest), use.names = TRUE)
fooddem[, quantile_num := suppressWarnings(as.integer(quantile))]
if (fooddem[quantile != "ALL" & is.na(quantile_num), .N] > 0L) {
  stop("Unexpected non-numeric FoodDem quantile label.")
}
n_quantiles <- max(fooddem[quantile != "ALL", quantile_num])
if (!identical(sort(unique(fooddem[quantile != "ALL", quantile_num])), seq_len(n_quantiles))) {
  stop("FoodDem quantiles are not consecutive from 1 to ", n_quantiles, ".")
}
all_position <- n_quantiles + 2L
fooddem[quantile == "ALL", quantile_num := all_position]
fooddem[, Version := factor(Version, levels = c("Past version", "Latest version"))]
var_levels <- c(sort(setdiff(past_vars, "total")), intersect("total", past_vars))
fooddem[, var := factor(var, levels = var_levels)]

fooddem_effects <- melt(
  fooddem,
  id.vars = c("Version", "Ref", "var", "R", "Y", "quantile", "quantile_num", "total_effect"),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "Effect",
  value.name = "Value"
)
fooddem_effects[, Effect := factor(
  Effect,
  levels = c("income_effect", "price_effect"),
  labels = c("Income effect", "Price effect")
)]
setorder(fooddem_effects, var, Version, quantile_num, Effect)

version_linetype <- c("Past version" = "22", "Latest version" = "solid")
version_shape <- c("Past version" = 1, "Latest version" = 16)
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

p_fooddem <- ggplot(
  fooddem_effects,
  aes(
    x = quantile_num,
    y = Value,
    color = Effect,
    linetype = Version,
    shape = Version,
    group = interaction(Effect, Version)
  )
) +
  geom_hline(yintercept = 0, color = "grey52", linewidth = 0.35) +
  geom_line(
    data = fooddem_effects[quantile != "ALL"],
    linewidth = 0.9
  ) +
  geom_point(
    size = 1.9,
    stroke = 0.45
  ) +
  geom_vline(
    xintercept = n_quantiles + 1L,
    color = "grey58",
    linewidth = 0.3,
    linetype = "dotted"
  ) +
  facet_wrap(
    vars(var),
    ncol = 4,
    scales = "free_y",
    labeller = labeller(var = as_labeller(fooddem_var_labels))
  ) +
  scale_color_manual(values = effect_colors, drop = FALSE) +
  scale_linetype_manual(values = version_linetype, drop = FALSE) +
  scale_shape_manual(values = version_shape, drop = FALSE) +
  scale_x_continuous(
    breaks = c(seq_len(n_quantiles), all_position),
    labels = c(as.character(seq_len(n_quantiles)), "ALL"),
    limits = c(0.5, all_position + 0.6),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  labs(
    title = paste0("AFG FoodDem effects in ", target_year, ": past vs latest"),
    subtitle = paste0(
      scenario_label,
      "; color shows the effect and line style shows the data version. ",
      "ALL is plotted separately at the right."
    ),
    x = "Income decile",
    y = "Effect on FoodDem (kcal/day/capita)",
    color = "Effect",
    linetype = "Data version",
    shape = "Data version",
    caption = paste0(
      "Past: Fooddem_PriceEffect_old.gdx; Latest: Fooddem_Quantile_10.gdx. ",
      "Income = FoodDemIn(scenario) - FoodDemIn(BaU); ",
      "Price = FoodDem_peffect(scenario) - FoodDemIn(scenario). Cross-price effects are excluded."
    )
  ) +
  theme_report(base_size = 11) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    strip.text = element_text(size = 10.5)
  )

fooddem_png <- file.path(out_dir, "AFG_FoodDem_income_price_effects_past_vs_latest_2050.png")
fooddem_csv <- file.path(out_dir, "AFG_FoodDem_income_price_effects_past_vs_latest_2050_data.csv")
ggsave(fooddem_png, p_fooddem, width = 15, height = 8.6, units = "in", dpi = 260, bg = "white")
fwrite(
  fooddem_effects[, .(
    Version = as.character(Version), Ref, var = as.character(var), R, Y,
    quantile, Effect = as.character(Effect), Value, total_effect
  )],
  fooddem_csv
)

prepare_pou <- function(gdx_path, version_label, income_symbol, price_symbol) {
  read_pou_symbol <- function(symbol, value_name) {
    out <- as.data.table(rgdx.param(gdx_path, symbol))
    required <- c("Ref", "R17", "Y", symbol)
    if (!all(required %in% names(out))) {
      stop("Unexpected ", symbol, " columns in ", basename(gdx_path), ".")
    }
    out <- out[
      as.character(Ref) %in% c(baseline_ref, scenario_ref),
      .(
        Ref = as.character(Ref),
        R17 = as.character(R17),
        Year = as.integer(as.character(Y)),
        value = as.numeric(get(symbol))
      )
    ]
    setnames(out, "value", value_name)
    out
  }

  income_sparse <- read_pou_symbol(income_symbol, "income_value")
  price_sparse <- read_pou_symbol(price_symbol, "price_value")
  regions <- sort(unique(c(income_sparse$R17, price_sparse$R17)))
  years <- sort(unique(c(income_sparse$Year, price_sparse$Year)))
  grid <- CJ(
    Ref = c(baseline_ref, scenario_ref),
    R17 = regions,
    Year = years,
    unique = TRUE
  )
  keys <- c("Ref", "R17", "Year")
  income <- complete_sparse(income_sparse, grid, keys, "income_value")
  price <- complete_sparse(price_sparse, grid, keys, "price_value")
  combined <- merge(income, price, by = keys, all = FALSE, sort = FALSE)

  bau_price_gap <- max(abs(combined[Ref == baseline_ref, income_value - price_value]))
  if (!is.finite(bau_price_gap) || bau_price_gap > 1e-6) {
    stop("PoU income/price values differ under BaU in ", version_label, ": ", bau_price_gap)
  }
  bau <- combined[
    Ref == baseline_ref,
    .(R17, Year, baseline_value = income_value)
  ]
  scenario <- merge(
    combined[Ref == scenario_ref],
    bau,
    by = c("R17", "Year"),
    all.x = TRUE,
    sort = FALSE
  )
  if (anyNA(scenario$baseline_value)) {
    stop("Missing PoU BaU rows in ", version_label, ".")
  }
  scenario[, `:=`(
    Baseline = baseline_value / 1e6,
    `Income effect` = (income_value - baseline_value) / 1e6,
    `Price effect` = (price_value - income_value) / 1e6,
    `After income effect` = income_value / 1e6,
    `After income + price effects` = price_value / 1e6,
    Total = price_value / 1e6,
    Version = version_label,
    Income_symbol = income_symbol,
    Price_symbol = price_symbol
  )]
  gap <- max(abs(
    scenario$Baseline + scenario$`Income effect` + scenario$`Price effect` - scenario$Total
  ))
  if (!is.finite(gap) || gap > 1e-8) {
    stop("PoU decomposition failed in ", version_label, ": ", gap)
  }
  scenario
}

cat("Reading R17+WLD PoU data...\n")
pou_past <- prepare_pou(
  old_price_gdx,
  "Past version",
  "PoU_In_Ragg_calc",
  "PoU_In_Pr_Ragg_calc"
)
pou_latest <- prepare_pou(
  latest_price_gdx,
  "Latest version",
  "PoU_In_Ragg_calc",
  "PoU_In_Pr_Ragg_calc"
)

if (!setequal(unique(pou_past$R17), unique(pou_latest$R17))) {
  stop("Past/latest PoU R17 sets differ.")
}
if (!setequal(unique(pou_past$Year), unique(pou_latest$Year))) {
  stop("Past/latest PoU year sets differ.")
}
pou <- rbindlist(list(pou_past, pou_latest), use.names = TRUE)
pou[, Version := factor(Version, levels = c("Past version", "Latest version"))]

r17_order <- c(
  "WLD", "R17XSA", "R17XER", "R17XNF", "R17XAF", "R17XLM",
  "R17CIS", "R17XOC", "R17XE25", "R17XME", "R17BRA", "R17XSE",
  "R17CAN", "R17CHN", "R17IND", "R17JPN", "R17TUR", "R17USA"
)
missing_r17 <- setdiff(unique(pou$R17), r17_order)
if (length(missing_r17) > 0L) {
  stop("Unexpected R17 codes: ", paste(missing_r17, collapse = ", "))
}
pou[, R17 := factor(R17, levels = r17_order)]

pou_effects <- melt(
  pou,
  id.vars = c("Version", "R17", "Year", "Total", "Income_symbol", "Price_symbol"),
  measure.vars = c("Income effect", "Price effect"),
  variable.name = "Effect",
  value.name = "Value"
)
pou_effects[, Effect := factor(
  Effect,
  levels = c("Income effect", "Price effect")
)]
pou_levels <- melt(
  pou,
  id.vars = c("Version", "R17", "Year", "Income_symbol", "Price_symbol"),
  measure.vars = c("Baseline", "After income effect", "After income + price effects"),
  variable.name = "Stage",
  value.name = "PoU"
)
pou_levels[, Stage := factor(
  Stage,
  levels = c("Baseline", "After income effect", "After income + price effects")
)]
setorder(pou_effects, R17, Year, Version, Effect)
setorder(pou_levels, R17, Year, Version, Stage)

# Direct differences make the version change explicit. Positive values mean
# that the latest version has a larger PoU level/effect than the past version.
effect_delta_wide <- dcast(
  copy(pou_effects)[, Version := as.character(Version)],
  R17 + Year + Effect ~ Version,
  value.var = "Value"
)
effect_delta_wide[, Difference := `Latest version` - `Past version`]
effect_delta <- effect_delta_wide[, .(
  R17,
  Year,
  Component = as.character(Effect),
  Difference
)]

total_delta_wide <- dcast(
  copy(pou)[, .(
    Version = as.character(Version),
    R17,
    Year,
    Value = Total
  )],
  R17 + Year ~ Version,
  value.var = "Value"
)
total_delta_wide[, Difference := `Latest version` - `Past version`]
total_delta <- total_delta_wide[, .(
  R17,
  Year,
  Component = "Resulting PoU",
  Difference
)]
pou_version_delta <- rbindlist(list(total_delta, effect_delta), use.names = TRUE)
pou_version_delta[, `:=`(
  R17 = factor(as.character(R17), levels = r17_order),
  Component = factor(
    Component,
    levels = c("Resulting PoU", "Income effect", "Price effect")
  )
)]
setorder(pou_version_delta, R17, Year, Component)

stage_colors <- c(
  "Baseline" = "grey42",
  "After income effect" = "#D55E00",
  "After income + price effects" = "#0072B2"
)
pou_effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

p_pou_levels <- ggplot(
  pou_levels,
  aes(
    x = Year,
    y = PoU,
    color = Stage,
    linetype = Version,
    shape = Version,
    group = interaction(Stage, Version)
  )
) +
  geom_line(linewidth = 0.82) +
  geom_point(size = 1.55, stroke = 0.4) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = stage_colors, drop = FALSE) +
  scale_linetype_manual(values = version_linetype, drop = FALSE) +
  scale_shape_manual(values = version_shape, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 20),
    labels = function(x) sprintf("%d", as.integer(x))
  ) +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  labs(
    title = paste0("PoU levels in R17 + WLD: past vs latest (", scenario_label, ")"),
    subtitle = "Color shows the cumulative stage; dashed/hollow = past, solid/filled = latest.",
    x = "Year",
    y = "Population at risk of hunger (million people)",
    color = "PoU stage",
    linetype = "Data version",
    shape = "Data version",
    caption = paste0(
      "Past Fooddem_PriceEffect_old.gdx: income-only = PoU_In_Ragg_calc; ",
      "income + price = PoU_In_Pr_Ragg_calc. Latest uses the same symbols from Fooddem_PriceEffect.gdx."
    )
  ) +
  theme_report(base_size = 9.5) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8.5)
  )

p_pou_effects <- ggplot(
  pou_effects,
  aes(
    x = Year,
    y = Value,
    color = Effect,
    linetype = Version,
    shape = Version,
    group = interaction(Effect, Version)
  )
) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.35) +
  geom_line(linewidth = 0.82) +
  geom_point(size = 1.55, stroke = 0.4) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = pou_effect_colors, drop = FALSE) +
  scale_linetype_manual(values = version_linetype, drop = FALSE) +
  scale_shape_manual(values = version_shape, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 20),
    labels = function(x) sprintf("%d", as.integer(x))
  ) +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  labs(
    title = paste0("PoU income and price effects in R17 + WLD (", scenario_label, ")"),
    subtitle = "The zero line means no change from the preceding stage; dashed/hollow = past, solid/filled = latest.",
    x = "Year",
    y = "Effect on population at risk of hunger (million people)",
    color = "Effect",
    linetype = "Data version",
    shape = "Data version",
    caption = paste0(
      "Income effect = PoU_In_Ragg_calc(scenario) - PoU_In_Ragg_calc(BaU); ",
      "Price effect = PoU_In_Pr_Ragg_calc(scenario) - PoU_In_Ragg_calc(scenario). ",
      "Cross-price effects are excluded."
    )
  ) +
  theme_report(base_size = 9.5) +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8.5)
  )

version_delta_colors <- c(
  "Resulting PoU" = "grey35",
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)
version_delta_shapes <- c(
  "Resulting PoU" = 16,
  "Income effect" = 17,
  "Price effect" = 15
)
p_pou_version_delta <- ggplot(
  pou_version_delta,
  aes(
    x = Year,
    y = Difference,
    color = Component,
    shape = Component,
    group = Component
  )
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.4) +
  geom_line(linewidth = 0.88) +
  geom_point(size = 1.7, stroke = 0.45) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = version_delta_colors, drop = FALSE) +
  scale_shape_manual(values = version_delta_shapes, drop = FALSE) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 20),
    labels = function(x) sprintf("%d", as.integer(x))
  ) +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(nrow = 1),
    shape = guide_legend(nrow = 1)
  ) +
  labs(
    title = paste0("PoU change from past to latest version in R17 + WLD (", scenario_label, ")"),
    subtitle = "Latest minus past: positive values indicate a larger PoU level/effect in the latest version.",
    x = "Year",
    y = "Latest version minus past version (million people)",
    color = "PoU measure",
    shape = "PoU measure",
    caption = paste0(
      "Both versions use PoU_In_Ragg_calc for the income-only stage and ",
      "PoU_In_Pr_Ragg_calc for the income + price stage. Cross-price effects are excluded."
    )
  ) +
  theme_report(base_size = 9.5) +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(size = 8.5)
  )

pou_png <- file.path(out_dir, "PoU_R17_WLD_income_price_effects_past_vs_latest.png")
pou_effects_png <- file.path(out_dir, "PoU_R17_WLD_effect_magnitudes_past_vs_latest.png")
pou_delta_png <- file.path(out_dir, "PoU_R17_WLD_latest_minus_past_corrected.png")
pou_csv <- file.path(out_dir, "PoU_R17_WLD_income_price_effects_past_vs_latest_data.csv")
levels_csv <- file.path(out_dir, "PoU_R17_WLD_levels_past_vs_latest_data.csv")
pou_delta_csv <- file.path(out_dir, "PoU_R17_WLD_latest_minus_past_corrected_data.csv")
ggsave(pou_png, p_pou_levels, width = 16, height = 11.8, units = "in", dpi = 260, bg = "white")
ggsave(pou_effects_png, p_pou_effects, width = 16, height = 11.8, units = "in", dpi = 260, bg = "white")
ggsave(pou_delta_png, p_pou_version_delta, width = 16, height = 11.8, units = "in", dpi = 260, bg = "white")
fwrite(
  pou_effects[, .(
    Version = as.character(Version), R17 = as.character(R17), Year,
    Effect = as.character(Effect), Value, Total, Income_symbol, Price_symbol
  )],
  pou_csv
)
fwrite(
  pou_levels[, .(
    Version = as.character(Version), R17 = as.character(R17), Year,
    Stage = as.character(Stage), PoU, Income_symbol, Price_symbol
  )],
  levels_csv
)
fwrite(
  pou_version_delta[, .(
    R17 = as.character(R17),
    Year,
    Component = as.character(Component),
    Difference_latest_minus_past_million = Difference
  )],
  pou_delta_csv
)

manifest <- data.table(
  figure = c(
    "AFG FoodDem income and price effects",
    "R17 + WLD PoU cumulative levels",
    "R17 + WLD PoU effect magnitudes",
    "R17 + WLD PoU latest minus past"
  ),
  scenario = scenario_label,
  year_scope = c(
    as.character(target_year),
    paste0(min(pou$Year), "-", max(pou$Year)),
    paste0(min(pou$Year), "-", max(pou$Year)),
    paste0(min(pou$Year), "-", max(pou$Year))
  ),
  past_source = basename(old_price_gdx),
  latest_source = c(
    basename(latest_q10_gdx),
    basename(latest_price_gdx),
    basename(latest_price_gdx),
    basename(latest_price_gdx)
  ),
  past_income_symbol = c(NA_character_, "PoU_In_Ragg_calc", "PoU_In_Ragg_calc", "PoU_In_Ragg_calc"),
  past_price_symbol = c(NA_character_, "PoU_In_Pr_Ragg_calc", "PoU_In_Pr_Ragg_calc", "PoU_In_Pr_Ragg_calc"),
  cross_price_included = FALSE,
  png = c(basename(fooddem_png), basename(pou_png), basename(pou_effects_png), basename(pou_delta_png)),
  data_csv = c(basename(fooddem_csv), basename(levels_csv), basename(pou_csv), basename(pou_delta_csv))
)
fwrite(manifest, file.path(out_dir, "past_latest_comparison_manifest.csv"))

png_count <- length(list.files(out_dir, pattern = "\\.png$", ignore.case = TRUE))
pdf_count <- length(list.files(out_dir, pattern = "\\.pdf$", ignore.case = TRUE))
if (png_count != 4L || pdf_count != 0L) {
  stop("Unexpected output count: PNG=", png_count, ", PDF=", pdf_count)
}

cat("Created FoodDem comparison:", normalizePath(fooddem_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created PoU levels comparison:", normalizePath(pou_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created PoU effects comparison:", normalizePath(pou_effects_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created PoU latest-minus-past comparison:", normalizePath(pou_delta_png, winslash = "/", mustWork = TRUE), "\n")
cat("FoodDem facets:", uniqueN(fooddem$var), " Rows:", nrow(fooddem_effects), "\n")
cat("PoU regions:", uniqueN(pou$R17), " Years:", uniqueN(pou$Year), " Rows:", nrow(pou_effects), "\n")
