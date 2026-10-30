suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
trailing_args <- commandArgs(trailingOnly = TRUE)
q10_only <- "--q10-only" %in% trailing_args
region_args <- grep("^--region=", trailing_args, value = TRUE)
requested_regions <- if (length(region_args) > 0L) {
  unique(strsplit(sub("^--region=", "", region_args[[1]]), ",", fixed = TRUE)[[1]])
} else {
  character()
}
partial_region_run <- length(requested_regions) > 0L
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

data_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
if (q10_only) {
  data_candidates <- data_candidates[
    file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx"))
  ]
} else {
  data_candidates <- data_candidates[
    file.exists(file.path(data_candidates, "Fooddem_Quantile_10.gdx")) &
      file.exists(file.path(data_candidates, "Fooddem_Quantile_100.gdx"))
  ]
}
if (length(data_candidates) != 1L) {
  stop(
    if (q10_only) {
      "Expected one data/260807_FDM* directory containing Fooddem_Quantile_10.gdx; found: "
    } else {
      "Expected one data/260807_FDM* directory containing both quantile GDX files; found: "
    },
    length(data_candidates)
  )
}

data_dir <- data_candidates[[1]]
gdx_q10 <- file.path(data_dir, "Fooddem_Quantile_10.gdx")
gdx_q100 <- file.path(data_dir, "Fooddem_Quantile_100.gdx")
plot_year <- 2050L

out_root <- file.path(project_dir, "output", "260807_FDM_scenario_quantile")
q10_dir <- file.path(out_root, "fooddem_quantile_bau_difference_2030CP_q10_by_R")
q10_cr_dir <- file.path(out_root, "fooddem_quantile_bau_difference_2030CP_q10_CR_by_R")
q100_dir <- file.path(out_root, "fooddem_quantile_bau_difference_2030CP_by_R")
afg_bar_dir <- file.path(out_root, "fooddem_quantile_bau_difference_2030CP_AFG_bars")
dir.create(q10_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(q10_cr_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(q100_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(afg_bar_dir, recursive = TRUE, showWarnings = FALSE)

clear_old_plot_files <- function(path) {
  old_files <- list.files(
    path,
    pattern = "\\.(pdf|png)$|manifest\\.csv$",
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(old_files) > 0L) {
    unlink(old_files, force = TRUE)
  }
  invisible(NULL)
}

if (!partial_region_run) {
  clear_old_plot_files(q10_dir)
  clear_old_plot_files(q10_cr_dir)
}
if (!q10_only) {
  clear_old_plot_files(q100_dir)
  clear_old_plot_files(afg_bar_dir)
}

safe_filename <- function(x) {
  gsub("[^A-Za-z0-9_-]+", "_", x)
}

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

effect_colors <- c(
  "Income effect" = "#D55E00",
  "Price effect" = "#0072B2"
)

pick_baseline_ref <- function(refs) {
  preferred <- "SSP2_BaU_NoCC_No"
  if (preferred %in% refs) {
    return(preferred)
  }
  candidates <- grep("BaU", refs, value = TRUE)
  if (length(candidates) == 1L) {
    return(candidates)
  }
  stop("Could not identify one BaU Ref: ", paste(refs, collapse = ", "))
}

read_quantile_symbol <- function(path, symbol, value_name, year, region_filter = NULL) {
  out <- as.data.table(rgdx.param(path, symbol))
  required <- c("Ref", "var", "R", "Y", "quantile", symbol)
  if (!all(required %in% names(out))) {
    stop("Unexpected columns for ", symbol, ": ", paste(names(out), collapse = ", "))
  }
  out <- out[as.character(Y) == as.character(year)]
  if (!is.null(region_filter)) {
    out <- out[as.character(R) %in% region_filter]
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

prepare_effect_data <- function(
    path,
    year,
    region_filter = NULL,
    price_symbol = "FoodDem_peffect_Quantile",
    series_label = "Standard") {
  income <- read_quantile_symbol(
    path,
    "FoodDemIn_Quantile",
    "income_value",
    year,
    region_filter
  )
  income_price <- read_quantile_symbol(
    path,
    price_symbol,
    "income_price_value",
    year,
    region_filter
  )

  keys <- c("Ref", "var", "R", "year", "quantile")
  if (anyDuplicated(income, by = keys) > 0L || anyDuplicated(income_price, by = keys) > 0L) {
    stop("Duplicate Ref/var/R/year/quantile keys found in: ", basename(path))
  }

  refs <- sort(unique(c(income$Ref, income_price$Ref)))
  baseline_ref <- pick_baseline_ref(refs)
  scenario_ref <- grep("2030CP", refs, value = TRUE)
  if (length(scenario_ref) != 1L) {
    stop("Expected one 2030CP Ref in ", basename(path), ": ", paste(scenario_ref, collapse = ", "))
  }

  fooddem_country_domain <- as.data.table(rgdx.param(path, "FoodDemIn_Quantile"))
  region_levels <- sort(unique(as.character(fooddem_country_domain$R)))
  if (!is.null(region_filter)) {
    region_levels <- intersect(region_levels, region_filter)
  }
  var_levels <- sort(unique(c(income$var, income_price$var)))
  quantile_levels <- unique(c(income$quantile, income_price$quantile))
  if (length(region_levels) == 0L || length(var_levels) == 0L || length(quantile_levels) == 0L) {
    stop("Could not identify complete q10 domains in: ", basename(path))
  }

  # GDX parameters are sparse. Missing records in this complete domain are zeros.
  complete_grid <- CJ(
    Ref = c(baseline_ref, scenario_ref),
    var = var_levels,
    R = region_levels,
    year = as.integer(year),
    quantile = quantile_levels,
    unique = TRUE
  )
  income <- merge(
    complete_grid,
    income[Ref %in% c(baseline_ref, scenario_ref)],
    by = keys,
    all.x = TRUE,
    sort = FALSE
  )
  income[is.na(income_value), income_value := 0]
  income_price <- merge(
    complete_grid,
    income_price[Ref %in% c(baseline_ref, scenario_ref)],
    by = keys,
    all.x = TRUE,
    sort = FALSE
  )
  income_price[is.na(income_price_value), income_price_value := 0]
  combined <- merge(income, income_price, by = keys, all = FALSE)
  rm(income, income_price, fooddem_country_domain, complete_grid)
  gc(verbose = FALSE)

  baseline <- combined[
    Ref == baseline_ref,
    .(
      var,
      R,
      year,
      quantile,
      bau_value = income_value,
      bau_price_value = income_price_value
    )
  ]
  bau_gap <- max(abs(baseline$bau_value - baseline$bau_price_value), na.rm = TRUE)
  if (!is.finite(bau_gap) || bau_gap > 1e-8) {
    warning(
      "BaU differs between symbols in ", basename(path),
      "; FoodDemIn_Quantile is used. Max gap = ", signif(bau_gap, 6)
    )
  }

  wide <- merge(
    combined[Ref == scenario_ref],
    baseline[, .(var, R, year, quantile, bau_value)],
    by = c("var", "R", "year", "quantile"),
    all.x = TRUE
  )
  if (wide[is.na(bau_value), .N] > 0L) {
    stop("Missing BaU values for scenario observations in: ", basename(path))
  }
  wide[
    ,
    `:=`(
      income_effect = income_value - bau_value,
      price_effect = income_price_value - income_value,
      total_effect = income_price_value - bau_value
    )
  ]
  if (wide[abs(income_effect + price_effect - total_effect) > 1e-8, .N] > 0L) {
    stop("Income and price effects do not sum to the BaU difference in: ", basename(path))
  }

  effects <- melt(
    wide,
    id.vars = c("Ref", "var", "R", "year", "quantile", "total_effect"),
    measure.vars = c("income_effect", "price_effect"),
    variable.name = "Effect",
    value.name = "Value_difference"
  )
  effects[
    ,
    Effect := factor(
      Effect,
      levels = c("income_effect", "price_effect"),
      labels = c("Income effect", "Price effect")
    )
  ]
  effects[, is_all := quantile == "ALL"]
  effects[, quantile_num := suppressWarnings(as.integer(quantile))]
  if (effects[!is_all & is.na(quantile_num), .N] > 0L) {
    stop("Non-numeric quantiles other than ALL found in: ", basename(path))
  }
  n_quantiles <- max(effects[is_all == FALSE, quantile_num])
  expected_quantiles <- seq_len(n_quantiles)
  found_quantiles <- sort(unique(effects[is_all == FALSE, quantile_num]))
  if (!identical(found_quantiles, expected_quantiles)) {
    stop("Quantiles are not consecutive in: ", basename(path))
  }
  all_position <- if (n_quantiles <= 10L) {
    n_quantiles + 3L
  } else {
    n_quantiles + 10L
  }
  effects[is_all == TRUE, quantile_num := all_position]
  effects[
    ,
    var := factor(var, levels = c(sort(setdiff(unique(var), "total")), "total"))
  ]
  setorder(effects, R, var, Effect, quantile_num)

  list(
    data = effects,
    baseline_ref = baseline_ref,
    scenario_ref = scenario_ref,
    scenario_label = "400C_2030CP",
    price_symbol = price_symbol,
    series_label = series_label,
    n_quantiles = n_quantiles,
    all_position = all_position
  )
}

axis_spec <- function(n_quantiles, all_position) {
  if (n_quantiles <= 10L) {
    breaks <- c(seq_len(n_quantiles), all_position)
    labels <- c(as.character(seq_len(n_quantiles)), "ALL")
  } else {
    core_breaks <- unique(c(1L, 25L, 50L, 75L, n_quantiles))
    core_breaks <- core_breaks[core_breaks <= n_quantiles]
    breaks <- c(core_breaks, all_position)
    labels <- c(as.character(core_breaks), "ALL")
  }
  list(breaks = breaks, labels = labels)
}

plot_area_region <- function(prepared, region_code, year) {
  region_dt <- prepared$data[R == region_code]
  area_dt <- region_dt[is_all == FALSE]
  all_dt <- region_dt[is_all == TRUE]
  axis_values <- axis_spec(prepared$n_quantiles, prepared$all_position)

  ggplot() +
    geom_area(
      data = area_dt,
      aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect),
      position = position_stack(reverse = TRUE),
      alpha = 0.78,
      color = NA
    ) +
    geom_hline(yintercept = 0, color = "grey38", linewidth = 0.28) +
    geom_vline(
      xintercept = (prepared$n_quantiles + prepared$all_position) / 2,
      color = "grey55",
      linewidth = 0.25,
      linetype = "dotted"
    ) +
    geom_col(
      data = all_dt,
      aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect),
      position = position_stack(reverse = TRUE),
      width = 0.85,
      alpha = 0.9,
      color = "grey35",
      linewidth = 0.12
    ) +
    facet_wrap(vars(var), ncol = 4, scales = "free_y") +
    scale_fill_manual(values = effect_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = axis_values$breaks,
      labels = axis_values$labels,
      limits = c(0.5, prepared$all_position + 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = paste0(
        region_code,
        " food-demand effects relative to BaU in ",
        year,
        " - ",
        prepared$scenario_label,
        " (",
        prepared$n_quantiles,
        " quantiles)",
        if (prepared$series_label == "CR") " - CR" else ""
      ),
      subtitle = paste0(
        "Income effect = FoodDemIn_Quantile - BaU; price effect = ",
        prepared$price_symbol,
        " - FoodDemIn_Quantile.\n",
        "Effects are stacked for quantiles and ALL."
      ),
      x = "Quantile",
      y = "Difference from BaU (Value)",
      fill = NULL
    ) +
    theme_report(base_size = 9)
}

plot_bar_region <- function(prepared, region_code, year) {
  region_dt <- prepared$data[R == region_code]
  axis_values <- axis_spec(prepared$n_quantiles, prepared$all_position)

  ggplot(
    region_dt,
    aes(x = quantile_num, y = Value_difference, fill = Effect, group = Effect)
  ) +
    geom_col(
      position = position_stack(reverse = TRUE),
      width = if (prepared$n_quantiles <= 10L) 0.72 else 0.88,
      alpha = 0.88,
      color = "grey35",
      linewidth = 0.08
    ) +
    geom_hline(yintercept = 0, color = "grey38", linewidth = 0.28) +
    geom_vline(
      xintercept = (prepared$n_quantiles + prepared$all_position) / 2,
      color = "grey55",
      linewidth = 0.25,
      linetype = "dotted"
    ) +
    facet_wrap(vars(var), ncol = 4, scales = "free_y") +
    scale_fill_manual(values = effect_colors, drop = FALSE) +
    scale_x_continuous(
      breaks = axis_values$breaks,
      labels = axis_values$labels,
      limits = c(0.5, prepared$all_position + 1),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(labels = label_number(big.mark = ",")) +
    guides(fill = guide_legend(nrow = 1)) +
    labs(
      title = paste0(
        region_code,
        " stacked-bar food-demand effects relative to BaU in ",
        year,
        " - ",
        prepared$scenario_label,
        " (",
        prepared$n_quantiles,
        " quantiles)",
        if (prepared$series_label == "CR") " - CR" else ""
      ),
      subtitle = paste0(
        "Income effect = FoodDemIn_Quantile - BaU; price effect = ",
        prepared$price_symbol,
        " - FoodDemIn_Quantile."
      ),
      x = "Quantile",
      y = "Difference from BaU (Value)",
      fill = NULL
    ) +
    theme_report(base_size = 9)
}

cat("Reading and preparing Fooddem_Quantile_10.gdx...\n")
prepared_q10 <- prepare_effect_data(gdx_q10, plot_year)
regions_q10 <- sort(unique(prepared_q10$data$R))
cat("Reading and preparing FoodDem_peffect_CR_Quantile...\n")
prepared_q10_cr <- prepare_effect_data(
  gdx_q10,
  plot_year,
  price_symbol = "FoodDem_peffect_CR_Quantile",
  series_label = "CR"
)
regions_q10_cr <- sort(unique(prepared_q10_cr$data$R))
if (!identical(regions_q10, regions_q10_cr)) {
  stop(
    "Standard and CR q10 region sets differ. Standard-only: ",
    paste(setdiff(regions_q10, regions_q10_cr), collapse = ", "),
    "; CR-only: ",
    paste(setdiff(regions_q10_cr, regions_q10), collapse = ", ")
  )
}
if (partial_region_run) {
  missing_requested <- setdiff(requested_regions, regions_q10)
  if (length(missing_requested) > 0L) {
    stop("Requested regions are not in the q10 domain: ", paste(missing_requested, collapse = ", "))
  }
  regions_q10 <- intersect(regions_q10, requested_regions)
  regions_q10_cr <- intersect(regions_q10_cr, requested_regions)
}
if (!q10_only) {
  cat("Reading and preparing Fooddem_Quantile_100.gdx...\n")
  prepared_q100 <- prepare_effect_data(gdx_q100, plot_year)
  regions_q100 <- sort(unique(prepared_q100$data$R))

  if (!identical(regions_q10, regions_q100)) {
    stop(
      "Q10 and Q100 region sets differ. Q10-only: ",
      paste(setdiff(regions_q10, regions_q100), collapse = ", "),
      "; Q100-only: ",
      paste(setdiff(regions_q100, regions_q10), collapse = ", ")
    )
  }
}
if (!"AFG" %in% unique(prepared_q10$data$R)) {
  stop("AFG is missing from Fooddem_Quantile_10.gdx.")
}

q10_bar_path <- file.path(
  afg_bar_dir,
  paste0("fooddem_quantile_bau_difference_2030CP_AFG_q10_bar_", plot_year, ".png")
)
ggsave(
  q10_bar_path,
  plot_bar_region(prepared_q10, "AFG", plot_year),
  width = 15,
  height = 9.2,
  dpi = 240,
  bg = "white"
)
q10_bar_manifest_row <- data.table(
  R = "AFG",
  year = plot_year,
  scenario = "400C_2030CP",
  quantiles = prepared_q10$n_quantiles,
  plot_type = "stacked_bar",
  path = q10_bar_path
)
bar_manifest_path <- file.path(
  afg_bar_dir,
  "fooddem_quantile_bau_difference_2030CP_AFG_bar_manifest.csv"
)
if (!q10_only) {
  q100_bar_path <- file.path(
    afg_bar_dir,
    paste0("fooddem_quantile_bau_difference_2030CP_AFG_q100_bar_", plot_year, ".png")
  )
  ggsave(
    q100_bar_path,
    plot_bar_region(prepared_q100, "AFG", plot_year),
    width = 15,
    height = 9.2,
    dpi = 240,
    bg = "white"
  )
  bar_manifest <- rbind(
    q10_bar_manifest_row,
    data.table(
      R = "AFG",
      year = plot_year,
      scenario = "400C_2030CP",
      quantiles = prepared_q100$n_quantiles,
      plot_type = "stacked_bar",
      path = q100_bar_path
    ),
    use.names = TRUE
  )
} else {
  existing_bar_manifest <- if (file.exists(bar_manifest_path)) {
    fread(bar_manifest_path)
  } else {
    data.table()
  }
  if (nrow(existing_bar_manifest) > 0L && "quantiles" %in% names(existing_bar_manifest)) {
    existing_bar_manifest <- existing_bar_manifest[as.integer(quantiles) != 10L]
  }
  bar_manifest <- rbind(
    existing_bar_manifest,
    q10_bar_manifest_row,
    use.names = TRUE,
    fill = TRUE
  )
  setorder(bar_manifest, quantiles)
}
fwrite(
  bar_manifest,
  bar_manifest_path
)

q10_manifest_path <- file.path(
  q10_dir,
  "fooddem_quantile_bau_difference_2030CP_q10_bar_manifest.csv"
)
q10_manifest <- if (partial_region_run && file.exists(q10_manifest_path)) {
  fread(q10_manifest_path)[!R %in% regions_q10]
} else {
  data.table()
}
cat("Writing 2030CP q10 R-split stacked-bar PNG files...\n")
for (region_idx in seq_along(regions_q10)) {
  region_code <- regions_q10[[region_idx]]
  png_path <- file.path(
    q10_dir,
    paste0(
      "fooddem_quantile_bau_difference_2030CP_q10_bar_",
      safe_filename(region_code),
      "_",
      plot_year,
      ".png"
    )
  )
  p <- plot_bar_region(prepared_q10, region_code, plot_year)
  ggsave(png_path, p, width = 15, height = 9.2, dpi = 240, bg = "white")
  q10_manifest <- rbind(
    q10_manifest,
    data.table(
      R = region_code,
      year = plot_year,
      scenario = prepared_q10$scenario_label,
      quantiles = prepared_q10$n_quantiles,
      plot_type = "stacked_bar",
      path = png_path
    ),
    use.names = TRUE
  )
  if (region_idx %% 25L == 0L || region_idx == length(regions_q10)) {
    cat("  wrote ", region_idx, "/", length(regions_q10), " q10 R PNG groups\n", sep = "")
  }
}
setorder(q10_manifest, R)
fwrite(
  q10_manifest,
  q10_manifest_path
)

q10_cr_manifest_path <- file.path(
  q10_cr_dir,
  "fooddem_quantile_bau_difference_2030CP_q10_CR_bar_manifest.csv"
)
q10_cr_manifest <- if (partial_region_run && file.exists(q10_cr_manifest_path)) {
  fread(q10_cr_manifest_path)[!R %in% regions_q10_cr]
} else {
  data.table()
}
cat("Writing 2030CP q10 CR R-split stacked-bar PNG files...\n")
for (region_idx in seq_along(regions_q10_cr)) {
  region_code <- regions_q10_cr[[region_idx]]
  png_path <- file.path(
    q10_cr_dir,
    paste0(
      "fooddem_quantile_bau_difference_2030CP_q10_CR_bar_",
      safe_filename(region_code),
      "_",
      plot_year,
      ".png"
    )
  )
  p <- plot_bar_region(prepared_q10_cr, region_code, plot_year)
  ggsave(png_path, p, width = 15, height = 9.2, dpi = 240, bg = "white")
  q10_cr_manifest <- rbind(
    q10_cr_manifest,
    data.table(
      R = region_code,
      year = plot_year,
      scenario = prepared_q10_cr$scenario_label,
      quantiles = prepared_q10_cr$n_quantiles,
      series = "CR",
      price_symbol = prepared_q10_cr$price_symbol,
      plot_type = "stacked_bar",
      path = png_path
    ),
    use.names = TRUE
  )
  if (region_idx %% 25L == 0L || region_idx == length(regions_q10_cr)) {
    cat("  wrote ", region_idx, "/", length(regions_q10_cr), " q10 CR R PNG groups\n", sep = "")
  }
}
setorder(q10_cr_manifest, R)
fwrite(
  q10_cr_manifest,
  q10_cr_manifest_path
)

if (!q10_only) {
  q100_manifest <- data.table()
  cat("Writing 2030CP q100 R-split stacked-bar PNG files...\n")
  for (region_idx in seq_along(regions_q100)) {
    region_code <- regions_q100[[region_idx]]
    png_path <- file.path(
      q100_dir,
      paste0(
        "fooddem_quantile_bau_difference_2030CP_q100_bar_",
        safe_filename(region_code),
        "_",
        plot_year,
        ".png"
      )
    )
    p <- plot_bar_region(prepared_q100, region_code, plot_year)
    ggsave(png_path, p, width = 15, height = 9.2, dpi = 240, bg = "white")
    q100_manifest <- rbind(
      q100_manifest,
      data.table(
        R = region_code,
        year = plot_year,
        scenario = prepared_q100$scenario_label,
        quantiles = prepared_q100$n_quantiles,
        plot_type = "stacked_bar",
        path = png_path
      ),
      use.names = TRUE
    )
    if (region_idx %% 25L == 0L || region_idx == length(regions_q100)) {
      cat("  wrote ", region_idx, "/", length(regions_q100), " q100 R PNG groups\n", sep = "")
    }
  }
  fwrite(
    q100_manifest,
    file.path(q100_dir, "fooddem_quantile_bau_difference_2030CP_q100_bar_manifest.csv")
  )
}

cat("Done.\n")
cat("Q10 R outputs: ", normalizePath(q10_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("Q10 CR R outputs: ", normalizePath(q10_cr_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
if (!q10_only) {
  cat("Q100 R outputs: ", normalizePath(q100_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
}
cat("AFG bar outputs: ", normalizePath(afg_bar_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
