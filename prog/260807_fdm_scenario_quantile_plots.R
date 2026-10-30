suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
trailing_args <- commandArgs(trailingOnly = TRUE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx")) &
    file.exists(file.path(data_candidates, "Fooddem_Quantile_100.gdx"))
]
if (length(data_candidates) != 1L) {
  stop(
    "Expected exactly one data/260807_FDM* directory containing both target GDX files; found: ",
    length(data_candidates)
  )
}

data_dir <- data_candidates[[1]]
price_effect_gdx <- file.path(data_dir, "Fooddem_PriceEffect.gdx")
quantile_gdx <- file.path(data_dir, "Fooddem_Quantile_100.gdx")

if (!file.exists(price_effect_gdx)) {
  stop("Missing GDX file: ", normalizePath(price_effect_gdx, winslash = "/", mustWork = FALSE))
}
if (!file.exists(quantile_gdx)) {
  stop("Missing GDX file: ", normalizePath(quantile_gdx, winslash = "/", mustWork = FALSE))
}

out_dir <- file.path(project_dir, "output", "260807_FDM_scenario_quantile")
hunger_dir <- file.path(out_dir, "hunger_risk_population")
quantile_dir <- file.path(out_dir, "fooddem_quantile_bau_difference_by_R")
quantile_2030_dir <- file.path(out_dir, "fooddem_quantile_bau_difference_2030CP_by_R")
dir.create(hunger_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(quantile_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(quantile_2030_dir, recursive = TRUE, showWarnings = FALSE)

only_2030_quantile <- "--only-2030-quantile" %in% trailing_args

quantile_plot_years <- 2050L
if ("--all-quantile-years" %in% trailing_args) {
  quantile_plot_years <- NULL
}

r17_regions <- c(
  "WLD",
  "R17XSA",
  "R17XER",
  "R17XNF",
  "R17XAF",
  "R17XLM",
  "R17CIS",
  "R17XOC",
  "R17XE25",
  "R17XME",
  "R17BRA",
  "R17XSE",
  "R17CAN",
  "R17CHN",
  "R17IND",
  "R17JPN",
  "R17TUR",
  "R17USA"
)

safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

clear_old_plot_files <- function(path) {
  old_files <- list.files(
    path,
    pattern = "\\.(pdf|png)$",
    recursive = FALSE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(old_files) > 0L) {
    unlink(old_files, force = TRUE)
  }
  invisible(NULL)
}

if (!only_2030_quantile) {
  clear_old_plot_files(hunger_dir)
  clear_old_plot_files(quantile_dir)
}
clear_old_plot_files(quantile_2030_dir)

theme_report <- function(base_size = 10) {
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

ref_palette <- function(refs) {
  cols <- c(
    "#009E73",
    "#CC79A7",
    "#0072B2",
    "#D55E00",
    "#56B4E9",
    "#F0E442",
    "#E69F00"
  )
  setNames(rep(cols, length.out = length(refs)), refs)
}

pick_baseline_ref <- function(refs) {
  preferred <- "SSP2_BaU_NoCC_No"
  if (preferred %in% refs) {
    return(preferred)
  }
  bau_refs <- grep("BaU", refs, value = TRUE)
  if (length(bau_refs) == 1L) {
    return(bau_refs)
  }
  stop("Could not identify a unique BaU baseline Ref. Refs are: ", paste(refs, collapse = ", "))
}

read_pou_ragg <- function(path, symbol, value_col) {
  out <- as.data.table(rgdx.param(path, symbol))
  if ("R17" %in% names(out)) {
    setnames(out, "R17", "R")
  }
  if (!all(c("Ref", "R", "Y", symbol) %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    ,
    .(
      Ref = as.character(Ref),
      R = as.character(R),
      year = as.integer(as.character(Y)),
      value = as.numeric(get(symbol))
    )
  ]
  setnames(out, "value", value_col)
  out
}

if (!only_2030_quantile) {
cat("Reading PoU R17/WLD data...\n")
pou_income <- read_pou_ragg(price_effect_gdx, "PoU_In_Ragg_calc", "income_only")
pou_income_price <- read_pou_ragg(price_effect_gdx, "PoU_In_Pr_Ragg_calc", "income_price")

refs <- sort(unique(c(pou_income$Ref, pou_income_price$Ref)))
baseline_ref <- pick_baseline_ref(refs)
scenario_refs <- setdiff(refs, baseline_ref)
if (length(scenario_refs) == 0L) {
  stop("No non-BaU scenario Ref was found.")
}

present_regions <- intersect(r17_regions, sort(unique(pou_income$R)))
missing_regions <- setdiff(r17_regions, present_regions)
if (length(missing_regions) > 0L) {
  warning("Missing R17/WLD regions in PoU data: ", paste(missing_regions, collapse = ", "))
}

pou_base <- pou_income[
  Ref == baseline_ref & R %in% r17_regions,
  .(R, year, baseline = income_only)
]

build_hunger_scenario <- function(scenario_ref) {
  scenario_income <- pou_income[
    Ref == scenario_ref & R %in% r17_regions,
    .(R, year, income_only)
  ]
  scenario_income_price <- pou_income_price[
    Ref == scenario_ref & R %in% r17_regions,
    .(R, year, income_price)
  ]
  out <- Reduce(
    function(x, y) merge(x, y, by = c("R", "year"), all = FALSE),
    list(pou_base, scenario_income, scenario_income_price)
  )
  out[, scenario := scenario_ref]
  out[
    ,
    `:=`(
      baseline_million = baseline / 1e6,
      income_only_million = income_only / 1e6,
      income_price_million = income_price / 1e6,
      income_effect_million = (income_only - baseline) / 1e6,
      price_effect_million = (income_price - income_only) / 1e6,
      total_effect_million = (income_price - baseline) / 1e6
    )
  ]
  out[, R := factor(R, levels = r17_regions)]
  setorder(out, R, year)
  out
}

hunger_effects <- rbindlist(lapply(scenario_refs, build_hunger_scenario), use.names = TRUE)
fwrite(
  hunger_effects[
    order(scenario, R, year),
    .(
      scenario,
      R,
      year,
      baseline,
      income_only,
      income_price,
      baseline_million,
      income_only_million,
      income_price_million,
      income_effect_million,
      price_effect_million,
      total_effect_million
    )
  ],
  file.path(hunger_dir, "hunger_risk_population_r17_wld_effects.csv")
)

effect_fills <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

series_colors <- c(
  "BaU (no income/price effect)" = "grey30",
  "Income effect only" = "#D55E00",
  "Income + price effect" = "#0072B2"
)

plot_hunger_scenario <- function(plot_dt, scenario_ref) {
  ggplot(plot_dt, aes(x = year)) +
    geom_ribbon(
      aes(
        ymin = pmin(baseline_million, income_only_million),
        ymax = pmax(baseline_million, income_only_million),
        fill = "Income effect"
      ),
      alpha = 0.46
    ) +
    geom_ribbon(
      aes(
        ymin = pmin(income_only_million, income_price_million),
        ymax = pmax(income_only_million, income_price_million),
        fill = "Price effect"
      ),
      alpha = 0.46
    ) +
    geom_line(aes(y = baseline_million, color = "BaU (no income/price effect)"), linewidth = 0.45) +
    geom_line(aes(y = income_only_million, color = "Income effect only"), linewidth = 0.55, linetype = "dashed") +
    geom_line(aes(y = income_price_million, color = "Income + price effect"), linewidth = 0.65) +
    facet_wrap(~ R, ncol = 6, scales = "free_y") +
    scale_fill_manual(values = effect_fills, drop = FALSE) +
    scale_color_manual(values = series_colors, drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(plot_dt$year))) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    labs(
      title = "Population at risk of hunger by R17/WLD",
      subtitle = paste0("Scenario = ", scenario_ref, "; BaU baseline = ", baseline_ref),
      x = "Year",
      y = "Population at risk of hunger (million people)",
      fill = "Effect decomposition",
      color = "Series"
    ) +
    theme_report(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.box = "vertical"
    )
}

hunger_manifest <- data.table()
for (scenario_ref in scenario_refs) {
  scenario_dt <- hunger_effects[scenario == scenario_ref]
  p <- plot_hunger_scenario(scenario_dt, scenario_ref)
  png_path <- file.path(
    hunger_dir,
    paste0("hunger_risk_population_r17_wld_", safe_filename(scenario_ref), ".png")
  )
  ggsave(png_path, p, width = 18, height = 9.6, dpi = 300, bg = "white")
  hunger_manifest <- rbind(
    hunger_manifest,
    data.table(type = "effect_decomposition_png", scenario = scenario_ref, path = png_path),
    use.names = TRUE
  )
}

pou_three_scenario <- pou_income_price[
  Ref %in% refs & R %in% r17_regions,
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    year = year,
    hunger_risk_population = income_price,
    hunger_risk_million = income_price / 1e6
  )
]
three_ref_levels <- c(baseline_ref, sort(setdiff(refs, baseline_ref)))
pou_three_scenario[, Ref := factor(Ref, levels = three_ref_levels)]
pou_three_scenario[, R := factor(R, levels = r17_regions)]
setorder(pou_three_scenario, R, Ref, year)
fwrite(
  pou_three_scenario,
  file.path(hunger_dir, "hunger_risk_population_r17_wld_three_scenarios_line.csv")
)

three_ref_colors <- ref_palette(three_ref_levels)
p_three_scenario <- ggplot(
  pou_three_scenario,
  aes(x = year, y = hunger_risk_million, color = Ref, group = Ref)
) +
  geom_line(linewidth = 0.72, alpha = 0.92) +
  geom_point(size = 1.15, alpha = 0.9) +
  facet_wrap(~ R, ncol = 6, scales = "free_y") +
  scale_color_manual(values = three_ref_colors, drop = FALSE) +
  scale_x_continuous(breaks = sort(unique(pou_three_scenario$year))) +
  scale_y_continuous(labels = label_number(big.mark = ",")) +
  guides(color = guide_legend(nrow = 2)) +
  labs(
    title = "Population at risk of hunger: three scenarios by R17/WLD",
    subtitle = "PoU_In_Pr_Ragg_calc; BaU is the no-income/no-price-effect baseline.",
    x = "Year",
    y = "Population at risk of hunger (million people)",
    color = NULL
  ) +
  theme_report(base_size = 9) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.box = "vertical"
  )

three_scenario_png_path <- file.path(
  hunger_dir,
  "hunger_risk_population_r17_wld_three_scenarios_line.png"
)
ggsave(
  three_scenario_png_path,
  p_three_scenario,
  width = 18,
  height = 9.6,
  dpi = 300,
  bg = "white"
)
hunger_manifest <- rbind(
  hunger_manifest,
  data.table(type = "three_scenario_line_png", scenario = paste(three_ref_levels, collapse = "|"), path = three_scenario_png_path),
  use.names = TRUE
)
fwrite(hunger_manifest, file.path(hunger_dir, "hunger_risk_population_manifest.csv"))
}

read_fooddem_quantile <- function(path, symbol, value_name) {
  out <- as.data.table(rgdx.param(path, symbol))
  required_cols <- c("Ref", "var", "R", "Y", "quantile", symbol)
  if (!all(required_cols %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[
    !is.na(get(symbol)),
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

cat("Reading FoodDem_peffect_Quantile and FoodDemIn_Quantile data...\n")
quantile_price <- read_fooddem_quantile(
  quantile_gdx,
  "FoodDem_peffect_Quantile",
  "income_price_value"
)
quantile_income <- read_fooddem_quantile(
  quantile_gdx,
  "FoodDemIn_Quantile",
  "income_value"
)

available_quantile_years <- sort(intersect(unique(quantile_price$year), unique(quantile_income$year)))
if (is.null(quantile_plot_years)) {
  quantile_plot_years <- available_quantile_years
} else {
  missing_quantile_years <- setdiff(quantile_plot_years, available_quantile_years)
  if (length(missing_quantile_years) > 0L) {
    stop("Requested quantile plot years are missing: ", paste(missing_quantile_years, collapse = ", "))
  }
}

quantile_price <- quantile_price[year %in% quantile_plot_years]
quantile_income <- quantile_income[year %in% quantile_plot_years]
quantile_keys <- c("Ref", "var", "R", "year", "quantile")
if (anyDuplicated(quantile_price, by = quantile_keys) > 0L ||
    anyDuplicated(quantile_income, by = quantile_keys) > 0L) {
  stop("Duplicate Ref/var/R/year/quantile keys found in the quantile symbols.")
}

quantile_combined <- merge(
  quantile_income,
  quantile_price,
  by = quantile_keys,
  all = TRUE
)
if (quantile_combined[is.na(income_value) | is.na(income_price_value), .N] > 0L) {
  stop("FoodDemIn_Quantile and FoodDem_peffect_Quantile do not have identical keys.")
}

quantile_refs <- sort(unique(quantile_combined$Ref))
quantile_baseline_ref <- pick_baseline_ref(quantile_refs)
quantile_scenario_refs <- sort(setdiff(quantile_refs, quantile_baseline_ref))
if (length(quantile_scenario_refs) == 0L) {
  stop("No non-BaU Ref found in the quantile symbols.")
}

quantile_baseline <- quantile_combined[
  Ref == quantile_baseline_ref,
  .(
    var,
    R,
    year,
    quantile,
    bau_value = income_value,
    bau_price_value = income_price_value
  )
]
bau_symbol_gap <- max(
  abs(quantile_baseline$bau_value - quantile_baseline$bau_price_value),
  na.rm = TRUE
)
if (!is.finite(bau_symbol_gap) || bau_symbol_gap > 1e-8) {
  warning(
    "The two quantile symbols differ under BaU; FoodDemIn_Quantile is used as the BaU baseline. Max absolute gap = ",
    signif(bau_symbol_gap, 6)
  )
}

quantile_effect_wide <- merge(
  quantile_combined[Ref %in% quantile_scenario_refs],
  quantile_baseline[, .(var, R, year, quantile, bau_value)],
  by = c("var", "R", "year", "quantile"),
  all.x = TRUE
)
if (quantile_effect_wide[is.na(bau_value), .N] > 0L) {
  stop("Missing BaU values for one or more scenario quantile observations.")
}
quantile_effect_wide[
  ,
  `:=`(
    income_effect = income_value - bau_value,
    price_effect = income_price_value - income_value,
    total_effect = income_price_value - bau_value
  )
]
if (quantile_effect_wide[
  abs((income_effect + price_effect) - total_effect) > 1e-8,
  .N
] > 0L) {
  stop("Income and price effects do not sum to the total difference from BaU.")
}

quantile_effects <- melt(
  quantile_effect_wide,
  id.vars = c(
    "Ref", "var", "R", "year", "quantile",
    "income_value", "income_price_value", "bau_value", "total_effect"
  ),
  measure.vars = c("income_effect", "price_effect"),
  variable.name = "Effect",
  value.name = "Value_difference"
)
quantile_effects[
  ,
  Effect := factor(
    Effect,
    levels = c("income_effect", "price_effect"),
    labels = c("Income effect", "Price effect")
  )
]
quantile_effects[, is_all := quantile == "ALL"]
quantile_effects[, quantile_num := suppressWarnings(as.integer(quantile))]
if (quantile_effects[!is_all & is.na(quantile_num), .N] > 0L) {
  bad_quantiles <- unique(quantile_effects[!is_all & is.na(quantile_num), quantile])
  stop("Non-numeric quantiles other than ALL found: ", paste(head(bad_quantiles, 20), collapse = ", "))
}
quantile_effects[is_all == TRUE, quantile_num := 118L]

quantile_ref_levels <- quantile_scenario_refs
quantile_effects[, Ref := factor(Ref, levels = quantile_ref_levels)]
quantile_effects[
  ,
  var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))
]

short_ref_label <- function(ref) {
  sub("^SSP2_", "", sub("_NoCC_No$", "", ref))
}
scenario_levels <- short_ref_label(quantile_ref_levels)
quantile_effects[
  ,
  Scenario := factor(short_ref_label(as.character(Ref)), levels = scenario_levels)
]
setorder(quantile_effects, R, year, Scenario, var, Effect, quantile_num)

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

plot_quantile_region_year <- function(
    region_dt,
    region_code,
    plot_year,
    single_scenario = NULL) {
  if (!is.null(single_scenario)) {
    region_dt <- region_dt[Scenario == single_scenario]
    if (nrow(region_dt) == 0L) {
      stop("No quantile data found for scenario: ", single_scenario)
    }
  }
  area_dt <- region_dt[is_all == FALSE]
  all_dt <- region_dt[is_all == TRUE]
  facet_layer <- if (is.null(single_scenario)) {
    facet_wrap(vars(Scenario, var), ncol = 4, scales = "free_y")
  } else {
    facet_wrap(vars(var), ncol = 4, scales = "free_y")
  }
  title_suffix <- if (is.null(single_scenario)) "" else paste0(" - ", single_scenario)
  layout_subtitle <- if (is.null(single_scenario)) {
    "Scenarios are separate panels; effects are stacked for quantiles 1-100 and ALL."
  } else {
    "Income and price effects are stacked for quantiles 1-100 and ALL."
  }
  ggplot() +
    geom_area(
      data = area_dt,
      aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect),
      position = position_stack(reverse = TRUE),
      alpha = 0.78,
      color = NA
    ) +
    geom_hline(yintercept = 0, color = "grey38", linewidth = 0.28) +
    geom_vline(xintercept = 105, color = "grey55", linewidth = 0.25, linetype = "dotted") +
    geom_col(
      data = all_dt,
      aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect),
      position = position_stack(reverse = TRUE),
      width = 5.5,
      alpha = 0.9,
      color = "grey35",
      linewidth = 0.12
    ) +
    facet_layer +
    scale_fill_manual(
      values = effect_colors,
      breaks = c("Income effect", "Price effect"),
      drop = FALSE
    ) +
    scale_x_continuous(
      breaks = c(1, 25, 50, 75, 100, 118),
      labels = c("1", "25", "50", "75", "100", "ALL"),
      limits = c(1, 128),
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = paste0(
        region_code,
        " food-demand effects relative to BaU in ",
        plot_year,
        title_suffix
      ),
      subtitle = paste0(
        "Income effect = FoodDemIn_Quantile - BaU; price effect = ",
        "FoodDem_peffect_Quantile - FoodDemIn_Quantile.\n",
        layout_subtitle
      ),
      x = "Quantile",
      y = "Difference from BaU (Value)",
      fill = NULL
    ) +
    theme_report(base_size = 9) +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5),
      legend.box = "vertical"
    )
}

regions <- sort(unique(quantile_effects$R))
years <- sort(unique(quantile_effects$year))
if (!only_2030_quantile) {
  cat("Writing R-split quantile effect PNG files...\n")
  quantile_manifest <- data.table()

  for (region_idx in seq_along(regions)) {
    region_code <- regions[[region_idx]]
    region_dt <- quantile_effects[R == region_code]
    for (plot_year in years) {
      plot_dt <- region_dt[year == plot_year]
      png_path <- file.path(
        quantile_dir,
        paste0("fooddem_quantile_bau_difference_", safe_filename(region_code), "_", plot_year, ".png")
      )
      p <- plot_quantile_region_year(plot_dt, region_code, plot_year)
      ggsave(png_path, p, width = 15, height = 14, dpi = 240, bg = "white")
      quantile_manifest <- rbind(
        quantile_manifest,
        data.table(R = region_code, year = plot_year, path = png_path),
        use.names = TRUE
      )
    }
    if (region_idx %% 25L == 0L || region_idx == length(regions)) {
      cat("  wrote ", region_idx, "/", length(regions), " R PNG groups\n", sep = "")
    }
  }

  fwrite(quantile_manifest, file.path(quantile_dir, "fooddem_quantile_bau_difference_manifest.csv"))
}

scenario_2030 <- grep("2030CP", scenario_levels, value = TRUE)
if (length(scenario_2030) != 1L) {
  stop("Expected one 2030CP scenario, found: ", paste(scenario_2030, collapse = ", "))
}

cat("Writing 2030CP-only R-split quantile effect PNG files...\n")
quantile_2030_manifest <- data.table()
for (region_idx in seq_along(regions)) {
  region_code <- regions[[region_idx]]
  region_dt <- quantile_effects[R == region_code]
  for (plot_year in years) {
    plot_dt <- region_dt[year == plot_year]
    png_path <- file.path(
      quantile_2030_dir,
      paste0(
        "fooddem_quantile_bau_difference_2030CP_",
        safe_filename(region_code),
        "_",
        plot_year,
        ".png"
      )
    )
    p <- plot_quantile_region_year(
      plot_dt,
      region_code,
      plot_year,
      single_scenario = scenario_2030
    )
    ggsave(png_path, p, width = 15, height = 9.2, dpi = 240, bg = "white")
    quantile_2030_manifest <- rbind(
      quantile_2030_manifest,
      data.table(R = region_code, year = plot_year, scenario = scenario_2030, path = png_path),
      use.names = TRUE
    )
  }
  if (region_idx %% 25L == 0L || region_idx == length(regions)) {
    cat("  wrote ", region_idx, "/", length(regions), " 2030CP R PNG groups\n", sep = "")
  }
}
fwrite(
  quantile_2030_manifest,
  file.path(quantile_2030_dir, "fooddem_quantile_bau_difference_2030CP_manifest.csv")
)

cat("Done.\n")
if (!only_2030_quantile) {
  cat("Hunger outputs: ", normalizePath(hunger_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
  cat("Quantile outputs: ", normalizePath(quantile_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
}
cat("2030CP-only outputs: ", normalizePath(quantile_2030_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
