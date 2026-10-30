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
    file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected one data/260807_FDM* directory containing both PoU GDX files; found: ", length(data_candidates))
}
data_dir <- data_candidates[[1L]]

past_gdx <- file.path(data_dir, "Fooddem_PriceEffect_old.gdx")
current_gdx <- file.path(data_dir, "Fooddem_PriceEffect.gdx")
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
scenario_label <- "400C / 2025CP"

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "past_current_pou_R17_WLD_2025CP"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

complete_sparse <- function(sparse_dt, grid, keys, value_name) {
  out <- merge(grid, sparse_dt, by = keys, all.x = TRUE, sort = FALSE)
  set(out, which(is.na(out[[value_name]])), value_name, 0)
  out
}

prepare_pou <- function(gdx_path, version_label) {
  read_symbol <- function(symbol, value_name) {
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

  income_sparse <- read_symbol("PoU_In_Ragg_calc", "income_value")
  price_sparse <- read_symbol("PoU_In_Pr_Ragg_calc", "price_value")
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

  bau_stage_gap <- max(abs(combined[Ref == baseline_ref, income_value - price_value]))
  if (!is.finite(bau_stage_gap) || bau_stage_gap > 1e-6) {
    stop("PoU income and price stages differ under BaU in ", version_label, ": ", bau_stage_gap)
  }
  bau <- combined[Ref == baseline_ref, .(R17, Year, baseline_value = income_value)]
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
    `Resulting PoU` = price_value / 1e6,
    Version = version_label
  )]
  gap <- max(abs(
    scenario$Baseline + scenario$`Income effect` + scenario$`Price effect` - scenario$`Resulting PoU`
  ))
  if (!is.finite(gap) || gap > 1e-8) {
    stop("PoU decomposition failed in ", version_label, ": ", gap)
  }
  scenario
}

cat("Reading past and current R17+WLD PoU...\n")
past <- prepare_pou(past_gdx, "Past version")
current <- prepare_pou(current_gdx, "Current version")

if (!setequal(unique(past$R17), unique(current$R17))) {
  stop("Past/current PoU region sets differ. Past-only: ",
       paste(setdiff(unique(past$R17), unique(current$R17)), collapse = ", "),
       "; current-only: ",
       paste(setdiff(unique(current$R17), unique(past$R17)), collapse = ", "))
}
if (!setequal(unique(past$Year), unique(current$Year))) {
  stop("Past/current PoU year sets differ.")
}

r17_order <- c(
  "WLD", "R17XSA", "R17XER", "R17XNF", "R17XAF", "R17XLM",
  "R17CIS", "R17XOC", "R17XE25", "R17XME", "R17BRA", "R17XSE",
  "R17CAN", "R17CHN", "R17IND", "R17JPN", "R17TUR", "R17USA"
)
observed_regions <- sort(unique(c(past$R17, current$R17)))
if (!setequal(observed_regions, r17_order)) {
  stop(
    "Expected exactly 17 regions + WLD. Missing: ",
    paste(setdiff(r17_order, observed_regions), collapse = ", "),
    "; unexpected: ",
    paste(setdiff(observed_regions, r17_order), collapse = ", ")
  )
}

pou <- rbindlist(list(past, current), use.names = TRUE)
pou[, `:=`(
  Version = factor(Version, levels = c("Past version", "Current version")),
  R17 = factor(R17, levels = r17_order)
)]

effects <- melt(
  pou,
  id.vars = c("Version", "R17", "Year", "Resulting PoU"),
  measure.vars = c("Income effect", "Price effect"),
  variable.name = "Effect",
  value.name = "Value"
)
effects[, Effect := factor(Effect, levels = c("Income effect", "Price effect"))]

stages <- melt(
  pou,
  id.vars = c("Version", "R17", "Year"),
  measure.vars = c("Baseline", "After income effect", "After income + price effects"),
  variable.name = "Stage",
  value.name = "Value"
)
stages[, Stage := factor(
  Stage,
  levels = c("Baseline", "After income effect", "After income + price effects")
)]

delta_effects <- dcast(
  copy(effects)[, Version := as.character(Version)],
  R17 + Year + Effect ~ Version,
  value.var = "Value"
)
delta_effects[, Difference := `Current version` - `Past version`]
delta_effects <- delta_effects[, .(
  R17,
  Year,
  Measure = as.character(Effect),
  Difference
)]
delta_total <- dcast(
  copy(pou)[, .(
    Version = as.character(Version),
    R17,
    Year,
    Value = `Resulting PoU`
  )],
  R17 + Year ~ Version,
  value.var = "Value"
)
delta_total[, Difference := `Current version` - `Past version`]
delta_total <- delta_total[, .(
  R17,
  Year,
  Measure = "Resulting PoU",
  Difference
)]
delta <- rbindlist(list(delta_total, delta_effects), use.names = TRUE)
delta[, `:=`(
  R17 = factor(as.character(R17), levels = r17_order),
  Measure = factor(Measure, levels = c("Resulting PoU", "Income effect", "Price effect"))
)]

version_linetype <- c("Past version" = "22", "Current version" = "solid")
version_shape <- c("Past version" = 1, "Current version" = 16)
effect_colors <- c("Income effect" = "#D55E00", "Price effect" = "#0072B2")
stage_colors <- c(
  "Baseline" = "grey42",
  "After income effect" = "#D55E00",
  "After income + price effects" = "#0072B2"
)
delta_colors <- c(
  "Resulting PoU" = "grey35",
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)
delta_shapes <- c("Resulting PoU" = 16, "Income effect" = 17, "Price effect" = 15)

theme_report <- function() {
  theme_minimal(base_size = 10.5) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
      panel.grid.major.y = element_line(color = "grey84", linewidth = 0.35),
      panel.border = element_rect(color = "grey70", fill = NA, linewidth = 0.45),
      axis.text = element_text(color = "grey22", size = 9.3),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title = element_text(color = "grey10", size = 11.5),
      legend.position = "bottom",
      legend.box = "vertical",
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10, face = "bold"),
      plot.title = element_text(face = "bold", color = "grey10", size = 17),
      plot.subtitle = element_text(color = "grey28", size = 10.5),
      plot.caption = element_text(color = "grey35", hjust = 0, size = 8.7),
      strip.text = element_text(face = "bold", color = "grey10", size = 9.5),
      strip.background = element_rect(fill = "grey95", color = NA),
      plot.margin = margin(10, 12, 10, 10)
    )
}

common_x <- scale_x_continuous(
  breaks = seq(2020, 2100, by = 20),
  labels = function(x) sprintf("%d", as.integer(x))
)

p_effects <- ggplot(
  effects,
  aes(
    x = Year,
    y = Value,
    color = Effect,
    linetype = Version,
    shape = Version,
    group = interaction(Effect, Version)
  )
) +
  geom_hline(yintercept = 0, color = "grey48", linewidth = 0.42) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.75, stroke = 0.45) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = effect_colors, drop = FALSE) +
  scale_linetype_manual(values = version_linetype, drop = FALSE) +
  scale_shape_manual(values = version_shape, drop = FALSE) +
  common_x +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  labs(
    title = paste0("PoU income and price effects: past vs current (", scenario_label, ")"),
    subtitle = "Each R17 region and WLD is faceted; dashed/hollow = past, solid/filled = current.",
    x = "Year",
    y = "Effect on population at risk of hunger (million people)",
    color = "Effect",
    linetype = "Data version",
    shape = "Data version",
    caption = paste0(
      "Both versions use PoU_In_Ragg_calc for the income-only stage and ",
      "PoU_In_Pr_Ragg_calc for the income + price stage. Cross-price effects are excluded."
    )
  ) +
  theme_report()

p_stages <- ggplot(
  stages,
  aes(
    x = Year,
    y = Value,
    color = Stage,
    linetype = Version,
    shape = Version,
    group = interaction(Stage, Version)
  )
) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.75, stroke = 0.45) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = stage_colors, drop = FALSE) +
  scale_linetype_manual(values = version_linetype, drop = FALSE) +
  scale_shape_manual(values = version_shape, drop = FALSE) +
  common_x +
  scale_y_continuous(labels = label_number()) +
  guides(
    color = guide_legend(order = 1, nrow = 1),
    linetype = guide_legend(order = 2, nrow = 1),
    shape = guide_legend(order = 2, nrow = 1)
  ) +
  labs(
    title = paste0("PoU levels by cumulative stage: past vs current (", scenario_label, ")"),
    subtitle = "Baseline, after income effect, and after income + price effects are compared through time.",
    x = "Year",
    y = "Population at risk of hunger (million people)",
    color = "PoU stage",
    linetype = "Data version",
    shape = "Data version",
    caption = paste0(
      "Past: Fooddem_PriceEffect_old.gdx; Current: Fooddem_PriceEffect.gdx. ",
      "Both use the R17 aggregation variables PoU_In_Ragg_calc and PoU_In_Pr_Ragg_calc."
    )
  ) +
  theme_report()

p_delta <- ggplot(
  delta,
  aes(x = Year, y = Difference, color = Measure, shape = Measure, group = Measure)
) +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.42) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8, stroke = 0.45) +
  facet_wrap(vars(R17), ncol = 5, scales = "free_y") +
  scale_color_manual(values = delta_colors, drop = FALSE) +
  scale_shape_manual(values = delta_shapes, drop = FALSE) +
  common_x +
  scale_y_continuous(labels = label_number()) +
  guides(color = guide_legend(nrow = 1), shape = guide_legend(nrow = 1)) +
  labs(
    title = paste0("PoU difference between current and past versions (", scenario_label, ")"),
    subtitle = "Current minus past; positive values indicate a larger level or effect in the current version.",
    x = "Year",
    y = "Current minus past (million people)",
    color = "PoU measure",
    shape = "PoU measure",
    caption = "Resulting PoU is the population after both income and price effects; cross-price effects are excluded."
  ) +
  theme_report()

effects_png <- file.path(out_dir, "PoU_R17_WLD_income_price_effects_past_vs_current.png")
stages_png <- file.path(out_dir, "PoU_R17_WLD_stages_past_vs_current.png")
delta_png <- file.path(out_dir, "PoU_R17_WLD_current_minus_past.png")
effects_csv <- file.path(out_dir, "PoU_R17_WLD_income_price_effects_past_vs_current_data.csv")
stages_csv <- file.path(out_dir, "PoU_R17_WLD_stages_past_vs_current_data.csv")
delta_csv <- file.path(out_dir, "PoU_R17_WLD_current_minus_past_data.csv")

ggsave(effects_png, p_effects, width = 16, height = 11.8, units = "in", dpi = 300, bg = "white")
ggsave(stages_png, p_stages, width = 16, height = 11.8, units = "in", dpi = 300, bg = "white")
ggsave(delta_png, p_delta, width = 16, height = 11.8, units = "in", dpi = 300, bg = "white")
fwrite(effects[, .(
  Version = as.character(Version), R17 = as.character(R17), Year,
  Effect = as.character(Effect), Value, `Resulting PoU`
)], effects_csv)
fwrite(stages[, .(
  Version = as.character(Version), R17 = as.character(R17), Year,
  Stage = as.character(Stage), Value
)], stages_csv)
fwrite(delta[, .(
  R17 = as.character(R17), Year, Measure = as.character(Measure), Difference
)], delta_csv)

for (path in c(effects_png, stages_png, delta_png)) {
  if (!file.exists(path) || file.info(path)$size <= 0L) {
    stop("PNG was not created correctly: ", path)
  }
}
cat("Regions verified:", length(observed_regions), "(17 regions + WLD)\n")
cat("Created:", normalizePath(effects_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created:", normalizePath(stages_png, winslash = "/", mustWork = TRUE), "\n")
cat("Created:", normalizePath(delta_png, winslash = "/", mustWork = TRUE), "\n")
