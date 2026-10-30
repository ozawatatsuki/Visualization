suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
})

args <- commandArgs(trailingOnly = TRUE)
data_variant <- if (length(args) >= 1 && nzchar(args[[1]])) args[[1]] else "1or0.1"

base_dir <- file.path("C:/A_economy/Visualization/data/260608_Labseminar", data_variant, "selected")
out_dir <- file.path("C:/A_economy/Visualization/output/260608_Labseminar", paste0(data_variant, "_selected_country_figs"))
income_dir <- file.path(out_dir, "income_elasticity")
income_value_dir <- file.path(out_dir, "income_elasticity_value")
share_dir <- file.path(out_dir, "consumption_share")
share_stacked_dir <- file.path(out_dir, "consumption_share_stacked")
share_line_dir <- file.path(out_dir, "consumption_share_line")
share_diff_dir <- file.path(out_dir, "consumption_share_diff")
dir.create(income_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(income_value_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_stacked_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_line_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(share_diff_dir, recursive = TRUE, showWarnings = FALSE)

countries_aidads <- c("FRA", "JPN", "AUS", "BDI", "GHA", "NGA")
countries_pov <- c("FRA", "JPN", "AUS", "BDI", "GHA", "NGA")
ssps <- c("SSP1", "SSP2", "SSP3")
ssp_cols <- c("SSP1" = "#1b9e77", "SSP2" = "#377eb8", "SSP3" = "#d95f02")
ssp_shapes <- c("SSP1" = 16, "SSP2" = 17, "SSP3" = 15)
pov_scenarios <- tibble::tribble(
  ~label, ~suffix, ~file_stub,
  "BaU_NoCC_No", "_BaU_NoCC_No", "BaU_NoCC_No",
  "400C_2025CP_NoCC_No", "_400C_2025CP_NoCC_No", "400C_2025CP_NoCC_No"
)

aidads_path <- file.path(base_dir, "AIDADSCalibNationResults_SSPs.gdx")
pov_base_path <- file.path(base_dir, "AnalysisExpenditure.gdx")
pov_ssp_path <- file.path(base_dir, "AnalysisExpenditure_SSPs.gdx")
required_paths <- c(aidads_path, pov_base_path, pov_ssp_path)
missing_paths <- required_paths[!file.exists(required_paths)]
if (length(missing_paths) > 0) {
  stop(sprintf("Required GDX file(s) not found:\n%s", paste(missing_paths, collapse = "\n")))
}

good_labels <- c(
  "Food and nonalcoholic beverages" = "Food",
  "Alcoholic beverages, tobacco, and narcotics" = "Alcohol/Tobacco",
  "Clothing and footwear" = "Clothing",
  "Housing" = "Housing",
  "Water" = "Water",
  "Energy" = "Energy",
  "Furnishings, household equipment and maintenance" = "Furnishings",
  "Health" = "Health",
  "Transport" = "Transport",
  "Communication" = "Comms",
  "Recreation and culture" = "Recreation",
  "Education" = "Education",
  "Restaurants and hotels" = "Restaurants",
  "Miscellaneous goods and services" = "Misc."
)

short_good_label <- function(x) {
  out <- unname(good_labels[as.character(x)])
  ifelse(is.na(out), as.character(x), out)
}

wrap_label <- function(x, width = 18) {
  vapply(as.character(x), function(s) paste(strwrap(s, width = width), collapse = "\n"), character(1))
}

h_sort_key <- function(h) {
  h <- as.character(h)
  area <- ifelse(grepl("_rural$", h), 0L, ifelse(grepl("_urban$", h), 1L, 2L))
  base <- sub("_(rural|urban)$", "", h)
  seg_rank <- match(base, c("Lowest", "Low", "Middle", "Higher"))
  dec_rank <- suppressWarnings(as.integer(sub("^DECILE([0-9]+)$", "\\1", base)))
  qui_rank <- suppressWarnings(as.integer(sub("^QUINTILE([0-9]+)$", "\\1", base)))
  type <- ifelse(
    !is.na(seg_rank),
    1L,
    ifelse(grepl("^DECILE[0-9]+$", base), 2L, ifelse(grepl("^QUINTILE[0-9]+$", base), 3L, 9L))
  )
  rank <- ifelse(
    !is.na(seg_rank),
    seg_rank,
    ifelse(!is.na(dec_rank), dec_rank, ifelse(!is.na(qui_rank), qui_rank, 999L))
  )
  sprintf("%02d_%02d_%03d_%s", type, area, rank, h)
}

latest_by_country <- function(df) {
  df %>%
    group_by(R) %>%
    summarise(Y = max(Y, na.rm = TRUE), .groups = "drop")
}

device_arg <- if (requireNamespace("ragg", quietly = TRUE)) ragg::agg_png else "png"

theme_common <- theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey94", colour = "grey75"),
    strip.text = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 55, hjust = 1, vjust = 1, size = 6)
  )

inc_raw <- as_tibble(rgdx.param(aidads_path, "PoutputIncomeElaNational")) %>%
  transmute(
    R = as.character(i1),
    SSP = as.character(i2),
    H = as.character(i3),
    Y = as.numeric(as.character(i4)),
    Good = as.character(i5),
    elasticity = as.numeric(value)
  ) %>%
  filter(R %in% countries_aidads, SSP %in% ssps)

inc_latest <- latest_by_country(inc_raw)
inc_df <- inc_raw %>%
  inner_join(inc_latest, by = c("R", "Y")) %>%
  mutate(
    R = factor(R, levels = countries_aidads),
    SSP = factor(SSP, levels = ssps),
    CountryYear = factor(
      paste0(R, " (", Y, ")"),
      levels = paste0(countries_aidads, " (", inc_latest$Y[match(countries_aidads, inc_latest$R)], ")")
    )
  )

h_levels_inc <- inc_df %>%
  distinct(H) %>%
  mutate(key = h_sort_key(H)) %>%
  arrange(key) %>%
  pull(H)
good_levels <- inc_df %>%
  distinct(Good) %>%
  pull(Good)
good_short_levels <- short_good_label(good_levels)
stack_good_levels <- c("Food", "Energy", setdiff(good_short_levels, c("Food", "Energy")))
good_stack_cols <- setNames(grDevices::hcl.colors(length(stack_good_levels), palette = "Spectral"), stack_good_levels)
inc_df <- inc_df %>%
  mutate(
    H = factor(H, levels = h_levels_inc),
    Good = factor(Good, levels = good_levels),
    GoodShort = factor(short_good_label(Good), levels = good_short_levels)
  )

for (country in countries_aidads) {
  plot_df <- inc_df %>%
    filter(as.character(R) == country) %>%
    mutate(H = droplevels(H), GoodShort = droplevels(GoodShort))
  plot_year <- unique(plot_df$Y)

  p_income_value <- ggplot(plot_df, aes(x = H, y = elasticity, colour = SSP, shape = SSP)) +
    geom_hline(yintercept = 0, colour = "grey45", linewidth = 0.35) +
    geom_point(position = position_dodge(width = 0.65), size = 2.6, alpha = 0.95) +
    facet_wrap(~GoodShort, ncol = 4, scales = "free_y") +
    scale_colour_manual(values = ssp_cols, drop = FALSE) +
    scale_shape_manual(values = ssp_shapes, drop = FALSE) +
    labs(
      title = paste0("Income elasticity by attribute and SSP: ", country),
      subtitle = paste0("PoutputIncomeElaNational; year = ", paste(plot_year, collapse = ", "), "; panels = goods"),
      x = "Attribute",
      y = "Income elasticity",
      colour = "SSP",
      shape = "SSP"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.6, "lines")
    )

  ggsave(
    file.path(income_value_dir, sprintf("income_elasticity_value_%s.png", country)),
    p_income_value,
    width = 16,
    height = 11,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

income_diff_cols <- c("SSP1 - SSP2" = "#1b9e77", "SSP3 - SSP2" = "#d95f02")
inc_diff_df <- inc_df %>%
  select(R, Y, CountryYear, H, Good, GoodShort, SSP, elasticity) %>%
  pivot_wider(names_from = SSP, values_from = elasticity) %>%
  transmute(
    R,
    Y,
    CountryYear,
    H,
    Good,
    GoodShort,
    `SSP1 - SSP2` = SSP1 - SSP2,
    `SSP3 - SSP2` = SSP3 - SSP2
  ) %>%
  pivot_longer(
    cols = c(`SSP1 - SSP2`, `SSP3 - SSP2`),
    names_to = "Comparison",
    values_to = "elasticity_diff"
  ) %>%
  mutate(Comparison = factor(Comparison, levels = names(income_diff_cols)))

for (country in countries_aidads) {
  plot_df <- inc_diff_df %>%
    filter(as.character(R) == country) %>%
    mutate(H = droplevels(H), GoodShort = droplevels(GoodShort))
  plot_year <- unique(plot_df$Y)

  p_income <- ggplot(plot_df, aes(x = H, y = elasticity_diff, fill = Comparison)) +
    geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed", linewidth = 0.4) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68, colour = "grey25", linewidth = 0.12) +
    facet_wrap(~GoodShort, ncol = 4, scales = "free_y") +
    scale_fill_manual(values = income_diff_cols, drop = FALSE) +
    labs(
      title = paste0("Income elasticity difference from SSP2: ", country),
      subtitle = paste0("PoutputIncomeElaNational; year = ", paste(plot_year, collapse = ", "), "; panels = goods"),
      x = "Attribute",
      y = "Difference in income elasticity",
      fill = "Comparison"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.6, "lines")
    )

  ggsave(
    file.path(income_dir, sprintf("income_elasticity_%s.png", country)),
    p_income,
    width = 16,
    height = 11,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

share_raw <- as_tibble(rgdx.param(aidads_path, "PoutputNational")) %>%
  transmute(
    R = as.character(i1),
    SSP = as.character(i2),
    Type = as.character(i3),
    Estimate = as.character(i4),
    H = as.character(i6),
    Y = as.numeric(as.character(i7)),
    Good = as.character(i8),
    share = as.numeric(value)
  ) %>%
  filter(R %in% countries_aidads, SSP %in% ssps, Type == "share", Estimate == "EST")

share_latest <- latest_by_country(share_raw)
share_df <- share_raw %>%
  inner_join(share_latest, by = c("R", "Y")) %>%
  mutate(
    R = factor(R, levels = countries_aidads),
    SSP = factor(SSP, levels = ssps),
    CountryYear = factor(
      paste0(R, " (", Y, ")"),
      levels = paste0(countries_aidads, " (", share_latest$Y[match(countries_aidads, share_latest$R)], ")")
    ),
    Good = factor(Good, levels = good_levels),
    GoodShort = factor(short_good_label(Good), levels = good_short_levels),
    GoodStack = factor(short_good_label(Good), levels = stack_good_levels)
  )

h_levels_share <- share_df %>%
  distinct(H) %>%
  mutate(key = h_sort_key(H)) %>%
  arrange(key) %>%
  pull(H)
share_df <- share_df %>%
  mutate(
    H = factor(H, levels = h_levels_share),
    H_SSP = factor(paste(H, SSP, sep = " | "), levels = as.vector(t(outer(h_levels_share, ssps, paste, sep = " | "))))
  )
share_y_upper <- max(share_df$share, na.rm = TRUE)

x_labs <- setNames(gsub(" \\| ", "\n", levels(share_df$H_SSP)), levels(share_df$H_SSP))

for (country in countries_aidads) {
  plot_df <- share_df %>%
    filter(as.character(R) == country) %>%
    mutate(H = droplevels(H), H_SSP = droplevels(H_SSP), GoodStack = droplevels(GoodStack))
  plot_year <- unique(plot_df$Y)
  x_labs_country <- x_labs[levels(plot_df$H_SSP)]
  stacked_width <- max(16, length(levels(plot_df$H)) * 2.2)

  p_share <- ggplot(plot_df, aes(x = H_SSP, y = share, fill = GoodStack)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.85, colour = "grey20", linewidth = 0.08) +
    scale_x_discrete(labels = x_labs_country, drop = TRUE) +
    scale_y_continuous(labels = function(x) sprintf("%.2f%%", x * 100), expand = expansion(mult = c(0, 0.02))) +
    scale_fill_manual(values = good_stack_cols, drop = FALSE) +
    labs(
      title = paste0("Consumption share composition by attribute and SSP: ", country),
      subtitle = paste0("PoutputNational; type = share, estimate = EST; year = ", paste(plot_year, collapse = ", ")),
      x = "Attribute and SSP",
      y = "Consumption share",
      fill = "Good"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 75, hjust = 1, size = 7),
      legend.key.size = unit(0.45, "lines")
    )

  ggsave(
    file.path(share_dir, sprintf("consumption_share_%s.png", country)),
    p_share,
    width = 18,
    height = 11,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )

  p_share_stacked <- ggplot(plot_df, aes(x = SSP, y = share, fill = GoodStack)) +
    geom_col(position = position_stack(reverse = TRUE), width = 0.72, colour = "grey20", linewidth = 0.08) +
    geom_text(
      aes(label = ifelse(share >= 0.025, sprintf("%.1f%%", share * 100), "")),
      position = position_stack(vjust = 0.5, reverse = TRUE),
      size = 2.4,
      colour = "black"
    ) +
    facet_wrap(~H, nrow = 1) +
    scale_y_continuous(labels = function(x) sprintf("%.0f%%", x * 100), expand = expansion(mult = c(0, 0.01))) +
    scale_fill_manual(values = good_stack_cols, drop = FALSE) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(
      title = paste0("Consumption share stacked by income attribute: ", country),
      subtitle = paste0("PoutputNational; type = share, estimate = EST; year = ", paste(plot_year, collapse = ", "), "; bottom = Food, above = Energy"),
      x = "SSP",
      y = "Consumption share",
      fill = "Good"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 0, hjust = 0.5, size = 8),
      strip.text = element_text(face = "bold", size = 8),
      legend.key.size = unit(0.45, "lines"),
      panel.spacing = unit(0.45, "lines")
    )

  ggsave(
    file.path(share_stacked_dir, sprintf("consumption_share_stacked_%s.png", country)),
    p_share_stacked,
    width = stacked_width,
    height = 8.5,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

for (country in countries_aidads) {
  plot_df <- share_df %>%
    filter(as.character(R) == country) %>%
    mutate(H = droplevels(H), GoodShort = droplevels(GoodShort))
  plot_year <- unique(plot_df$Y)

  p_share_line <- ggplot(plot_df, aes(x = H, y = share, fill = SSP)) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68, colour = "grey25", linewidth = 0.12) +
    facet_wrap(~GoodShort, ncol = 4) +
    scale_fill_manual(values = ssp_cols, drop = FALSE) +
    scale_y_continuous(
      labels = function(x) sprintf("%.2f%%", x * 100),
      limits = c(0, share_y_upper),
      expand = expansion(mult = c(0, 0.02))
    ) +
    labs(
      title = paste0("Consumption share by attribute and SSP: ", country),
      subtitle = paste0("PoutputNational; type = share, estimate = EST; year = ", paste(plot_year, collapse = ", "), "; panels = goods"),
      x = "Attribute",
      y = "Consumption share",
      fill = "SSP"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.6, "lines")
    )

  ggsave(
    file.path(share_line_dir, sprintf("consumption_share_line_%s.png", country)),
    p_share_line,
    width = 16,
    height = 11,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

share_diff_cols <- c("SSP1 - SSP2" = "#1b9e77", "SSP3 - SSP2" = "#d95f02")
share_diff_df <- share_df %>%
  select(R, Y, CountryYear, H, Good, GoodShort, SSP, share) %>%
  pivot_wider(names_from = SSP, values_from = share) %>%
  transmute(
    R,
    Y,
    CountryYear,
    H,
    Good,
    GoodShort,
    `SSP1 - SSP2` = SSP1 - SSP2,
    `SSP3 - SSP2` = SSP3 - SSP2
  ) %>%
  pivot_longer(
    cols = c(`SSP1 - SSP2`, `SSP3 - SSP2`),
    names_to = "Comparison",
    values_to = "share_diff"
  ) %>%
  mutate(Comparison = factor(Comparison, levels = names(share_diff_cols)))

for (country in countries_aidads) {
  plot_df <- share_diff_df %>%
    filter(as.character(R) == country) %>%
    mutate(H = droplevels(H), GoodShort = droplevels(GoodShort))
  plot_year <- unique(plot_df$Y)

  p_share_diff <- ggplot(plot_df, aes(x = H, y = share_diff, fill = Comparison)) +
    geom_hline(yintercept = 0, colour = "grey45", linetype = "dashed", linewidth = 0.4) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68, colour = "grey25", linewidth = 0.12) +
    facet_wrap(~GoodShort, ncol = 4, scales = "free_y") +
    scale_fill_manual(values = share_diff_cols, drop = FALSE) +
    scale_y_continuous(labels = function(x) sprintf("%.2f pp", x * 100)) +
    labs(
      title = paste0("Consumption share difference from SSP2: ", country),
      subtitle = paste0("PoutputNational; type = share, estimate = EST; year = ", paste(plot_year, collapse = ", "), "; panels = goods"),
      x = "Attribute",
      y = "Difference in consumption share",
      fill = "Comparison"
    ) +
    theme_common +
    theme(
      axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
      strip.text = element_text(face = "bold", size = 9),
      panel.spacing = unit(0.6, "lines")
    )

  ggsave(
    file.path(share_diff_dir, sprintf("consumption_share_diff_%s.png", country)),
    p_share_diff,
    width = 16,
    height = 11,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

read_pov <- function(path, source_label, scenario_suffix) {
  as_tibble(rgdx.param(path, "PoVExp")) %>%
    transmute(
      Source = source_label,
      Ref = as.character(Ref),
      R = as.character(R),
      Y = as.numeric(as.character(Y)),
      TH = as.character(TH),
      pov_million = as.numeric(PoVExp) / 1e6
    ) %>%
    filter(R %in% countries_pov, TH == "pop_2.15", grepl(scenario_suffix, Ref, fixed = TRUE)) %>%
    mutate(
      SSP = sub("_.*$", "", Ref),
      R = factor(R, levels = countries_pov),
      SSP = factor(SSP, levels = ssps)
    )
}

plot_poverty_scenario <- function(scenario_label, scenario_suffix, file_stub) {
  pov_base <- read_pov(pov_base_path, "AnalysisExpenditure.gdx", scenario_suffix)
  pov_ssp <- read_pov(pov_ssp_path, "AnalysisExpenditure_SSPs.gdx", scenario_suffix)
  pov_df <- bind_rows(pov_base, pov_ssp)

  p_pov <- ggplot(pov_df, aes(x = Y, y = pov_million, colour = SSP, linetype = Source, group = interaction(Source, SSP))) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 1.3) +
    facet_wrap(~R, ncol = 3, scales = "free_y") +
    scale_colour_manual(values = ssp_cols, drop = FALSE) +
    scale_linetype_manual(values = c("AnalysisExpenditure.gdx" = "dotted", "AnalysisExpenditure_SSPs.gdx" = "solid")) +
    scale_x_continuous(breaks = sort(unique(pov_df$Y))) +
    labs(
      title = paste0("Poverty population comparison: ", scenario_label),
      subtitle = paste0("PoVExp; TH = pop_2.15; refs = ", scenario_label, "; dotted = base, solid = SSPs"),
      x = "Y",
      y = "Poverty population (million)",
      colour = "SSP",
      linetype = "File"
    ) +
    theme_common +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  ggsave(
    file.path(out_dir, sprintf("03_poverty_population_comparison_%s.png", file_stub)),
    p_pov,
    width = 18,
    height = 10,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )

  target_diff_ssps <- c("SSP1", "SSP3")
  base_one <- pov_base %>%
    filter(SSP %in% target_diff_ssps) %>%
    select(Ref, R, Y, SSP, base_million = pov_million)
  ssp_one <- pov_ssp %>%
    filter(SSP %in% target_diff_ssps) %>%
    select(Ref, R, Y, SSP, ssp_million = pov_million)
  diff_df <- inner_join(base_one, ssp_one, by = c("Ref", "R", "Y", "SSP")) %>%
    mutate(
      diff_people = (ssp_million - base_million) * 1e6,
      Y = factor(Y, levels = sort(unique(Y))),
      SSP = factor(SSP, levels = target_diff_ssps)
    )

  p <- ggplot(diff_df, aes(x = Y, y = diff_people, fill = SSP)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey35", linewidth = 0.45) +
    geom_col(position = position_dodge(width = 0.78), width = 0.68, colour = "grey25", linewidth = 0.15) +
    facet_wrap(~R, ncol = 3, scales = "free_y") +
    scale_fill_manual(values = ssp_cols[target_diff_ssps], drop = FALSE) +
    scale_y_continuous(labels = function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)) +
    labs(
      title = paste0("Poverty population difference: SSP1 and SSP3 | ", scenario_label),
      subtitle = paste0("AnalysisExpenditure_SSPs.gdx minus AnalysisExpenditure.gdx; PoVExp TH = pop_2.15; refs = ", scenario_label),
      x = "Y",
      y = "Difference (people)",
      fill = "SSP"
    ) +
    theme_common +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

  ggsave(
    file.path(out_dir, sprintf("04_poverty_difference_SSP1_SSP3_%s.png", file_stub)),
    p,
    width = 18,
    height = 10,
    dpi = 300,
    limitsize = FALSE,
    device = device_arg
  )
}

for (scenario_i in seq_len(nrow(pov_scenarios))) {
  plot_poverty_scenario(
    pov_scenarios$label[[scenario_i]],
    pov_scenarios$suffix[[scenario_i]],
    pov_scenarios$file_stub[[scenario_i]]
  )
}

unlink(
  file.path(
    out_dir,
    c(
      "03_poverty_population_comparison.png",
      "04_poverty_difference_SSP1_SSP3.png",
      "04_poverty_difference_SSP1.png",
      "05_poverty_difference_SSP3.png"
    )
  ),
  force = TRUE
)

pngs <- list.files(out_dir, pattern = "\\.png$", full.names = TRUE, recursive = TRUE)
expected_png_count <- length(countries_aidads) * 6 + 4
if (length(pngs) != expected_png_count) {
  stop(sprintf("Expected exactly %d PNG files, found %d", expected_png_count, length(pngs)))
}

cat(paste(normalizePath(pngs, winslash = "/"), collapse = "\n"), "\n")
