suppressPackageStartupMessages({
  library(dplyr)
  library(forcats)
  library(gdxrrw)
  library(ggplot2)
  library(patchwork)
  library(readr)
  library(scales)
  library(stringr)
  library(tibble)
  library(tidyr)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

outdir_kyoeiken <- file.path(project_dir, "output", "kyoeiken")
fdm_file <- file.path(outdir_kyoeiken, "FDM_kyoeiken_260402.gdx")
aimhub_file <- file.path(project_dir, "data", "HungerTool", "Region", "AIMHub.gdx")

missing_files <- c(fdm_file, aimhub_file)[!file.exists(c(fdm_file, aimhub_file))]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}
dir.create(outdir_kyoeiken, recursive = TRUE, showWarnings = FALSE)

theme_kyoeiken <- function(base_size = 6) {
  theme_bw(base_size = base_size) +
    theme(
      panel.border = element_rect(fill = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.line = element_line(colour = "black", linewidth = 0.2),
      panel.background = element_rect(fill = "white"),
      strip.background = element_rect(fill = "white", colour = "black"),
      strip.text = element_text(size = base_size, face = "bold"),
      legend.position = "bottom",
      legend.text = element_text(size = base_size),
      legend.title = element_text(size = base_size),
      legend.key.size = grid::unit(0.35, "cm"),
      axis.text.x = element_text(size = base_size),
      axis.text.y = element_text(size = base_size),
      axis.title.x = element_text(size = base_size, margin = margin(t = 2)),
      axis.title.y = element_text(size = base_size, margin = margin(r = 2)),
      panel.spacing = grid::unit(0.25, "lines"),
      plot.margin = margin(5, 5, 5, 5, "pt")
    )
}

component_cols <- c(
  "Baseline" = "#C7C7C7",
  "Income" = "#FF7F00",
  "Price" = "#4DAF4A"
)
comparison_cols <- c(
  "fao_based" = "#009E73",
  "FDM" = "#0072B2"
)

save_png <- function(plot, filename, width, height) {
  path <- file.path(outdir_kyoeiken, filename)
  ggsave(
    filename = path,
    plot = plot,
    width = width,
    height = height,
    units = "cm",
    dpi = 300,
    bg = "white"
  )
  normalizePath(path, winslash = "\\", mustWork = FALSE)
}

FD <- as_tibble(rgdx.param(fdm_file, "FoodDem_base_add"))
if ("FoodDem_base_add" %in% names(FD)) {
  FD <- FD |> rename(Value = FoodDem_base_add)
} else if (!("Value" %in% names(FD))) {
  stop("FoodDem_base_add must contain `FoodDem_base_add` or `Value`.")
}

FD_fig1 <- FD |>
  filter(as.character(eff) %in% c("Income", "Price")) |>
  mutate(
    eff = factor(as.character(eff), levels = c("Income", "Price")),
    seg_chr = as.character(seg),
    is_all = str_to_upper(seg_chr) == "ALL",
    seg_num = suppressWarnings(readr::parse_number(seg_chr) / 10)
  )

if (nrow(FD_fig1) == 0) {
  stop("No FoodDem_base_add rows for eff = Income or Price.")
}

seg_max_fig1 <- max(FD_fig1$seg_num[!FD_fig1$is_all], na.rm = TRUE)
seg_all_x_fig1 <- seg_max_fig1 + max(1, 0.05 * seg_max_fig1)
all_bar_width_fig1 <- max(1, 0.03 * seg_max_fig1)

FD_fig1 <- FD_fig1 |>
  mutate(seg_x = ifelse(is_all | is.na(seg_num), seg_all_x_fig1, seg_num))

p_fig1 <- ggplot() +
  geom_area(
    data = FD_fig1 |> filter(!is_all, !is.na(seg_num)) |> arrange(seg_x),
    mapping = aes(x = seg_x, y = Value, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.15
  ) +
  geom_col(
    data = FD_fig1 |> filter(is_all | is.na(seg_num)),
    mapping = aes(x = seg_x, y = Value, fill = eff),
    position = position_stack(reverse = TRUE),
    width = all_bar_width_fig1,
    colour = "black",
    linewidth = 0.15
  ) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ var, ncol = 2) +
  scale_fill_manual(values = component_cols[c("Income", "Price")]) +
  scale_x_continuous(
    name = "seg",
    breaks = c(pretty(c(0, seg_max_fig1)), seg_all_x_fig1),
    labels = function(x) ifelse(abs(x - seg_all_x_fig1) < 1e-9, "\nAve", x)
  ) +
  labs(
    y = "Changes in food consumption(kcal/day/capita)",
    fill = NULL
  ) +
  theme_kyoeiken()

FD_fig1_base <- FD |>
  filter(as.character(eff) == "Base") |>
  mutate(seg_chr = as.character(seg)) |>
  select(var, seg_chr, BaseValue = Value)

FD_fig1_pct <- FD_fig1 |>
  left_join(FD_fig1_base, by = c("var", "seg_chr")) |>
  mutate(
    Value_pct = ifelse(is.na(BaseValue) | BaseValue == 0, NA_real_, 100 * Value / BaseValue)
  )

p_fig1_pct <- ggplot() +
  geom_area(
    data = FD_fig1_pct |> filter(!is_all, !is.na(seg_num)) |> arrange(seg_x),
    mapping = aes(x = seg_x, y = Value_pct, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.15
  ) +
  geom_col(
    data = FD_fig1_pct |> filter(is_all | is.na(seg_num)),
    mapping = aes(x = seg_x, y = Value_pct, fill = eff),
    position = position_stack(reverse = TRUE),
    width = all_bar_width_fig1,
    colour = "black",
    linewidth = 0.15
  ) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ var, scales = "free_y", nrow = 2, ncol = 4) +
  scale_fill_manual(values = component_cols[c("Income", "Price")]) +
  scale_x_continuous(
    name = "seg",
    breaks = c(pretty(c(0, seg_max_fig1)), seg_all_x_fig1),
    labels = function(x) ifelse(abs(x - seg_all_x_fig1) < 1e-9, "\nAve", x)
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    y = "Changes in food consumption (% of Baseline)",
    fill = NULL
  ) +
  theme_kyoeiken()

PoU_FDM <- as_tibble(rgdx.param(fdm_file, "PoU_base_add"))
if ("PoU_base_add" %in% names(PoU_FDM)) {
  PoU_FDM <- PoU_FDM |> rename(Value = PoU_base_add)
} else if (!("Value" %in% names(PoU_FDM))) {
  stop("PoU_base_add must contain `PoU_base_add` or `Value`.")
}

PoU_years_fig2 <- 2020:2100
PoU_fig2 <- PoU_FDM |>
  mutate(
    Value = Value / 1000000,
    Y = suppressWarnings(as.integer(as.character(Y))),
    eff = as.character(eff)
  ) |>
  filter(!is.na(Y), Y %in% PoU_years_fig2) |>
  filter(eff %in% c("Base", "Income", "Price")) |>
  mutate(Y = factor(Y, levels = PoU_years_fig2))

PoU_fig2_base <- PoU_fig2 |>
  filter(eff == "Base") |>
  mutate(panel = factor("Baseline", levels = c("Baseline", "400C")))

PoU_fig2_400c_abs <- PoU_fig2 |>
  filter(eff %in% c("Base", "Income", "Price")) |>
  mutate(
    panel = factor("400C", levels = c("Baseline", "400C")),
    component = case_when(eff == "Base" ~ "Baseline", TRUE ~ eff),
    component = factor(component, levels = c("Baseline", "Income", "Price"))
  )

PoU_x_breaks_fig2 <- as.character(seq(2020, 2100, by = 10))

p_fig2 <- ggplot() +
  geom_col(
    data = PoU_fig2_base,
    mapping = aes(x = factor(Y), y = Value),
    fill = component_cols[["Baseline"]],
    width = 1,
    colour = "black",
    linewidth = 0.15,
    show.legend = FALSE
  ) +
  geom_col(
    data = PoU_fig2_400c_abs,
    mapping = aes(x = factor(Y), y = Value, fill = component),
    position = position_stack(reverse = TRUE),
    width = 1,
    colour = "black",
    linewidth = 0.15
  ) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_discrete(drop = FALSE, breaks = PoU_x_breaks_fig2, expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = component_cols, breaks = names(component_cols), drop = FALSE) +
  labs(
    x = "Y",
    y = "Population at risk of hunger(million)",
    fill = NULL
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_kyoeiken() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

df_FDM <- as_tibble(rgdx.param(fdm_file, "PoU_value"))
if ("PoU_value" %in% names(df_FDM)) {
  df_FDM <- df_FDM |> rename(value = PoU_value)
} else if (!("value" %in% names(df_FDM))) {
  stop("PoU_value must contain `PoU_value` or `value`.")
}

df_FDM <- df_FDM |>
  filter(as.character(eff) != "Income") |>
  mutate(
    Y = suppressWarnings(as.integer(as.character(Y))),
    U = "FDM",
    value = value / 1000000,
    Ref = if_else(as.character(eff) == "Base", "Baseline", "400C")
  ) |>
  select(Y, U, Ref, value)

df_FAO <- as_tibble(rgdx.param(aimhub_file, "Risk_agg")) |>
  filter(
    as.character(U) == "fao_based",
    as.character(S) %in% c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No"),
    as.character(R_agg) == "WLD"
  ) |>
  transmute(
    Y = suppressWarnings(as.integer(as.character(T))),
    U = "fao_based",
    Ref = if_else(as.character(S) == "SSP2_BaU_NoCC_No", "Baseline", "400C"),
    value = Risk_agg
  ) |>
  filter(Y %in% df_FDM$Y)

df_comp <- bind_rows(df_FDM, df_FAO) |>
  mutate(
    Ref = factor(Ref, levels = c("Baseline", "400C")),
    U = factor(U, levels = c("fao_based", "FDM"))
  ) |>
  filter(!is.na(Y), !is.na(value))

if (nrow(df_FAO) == 0) {
  stop("No matching Risk_agg rows were found in AIMHub.gdx.")
}

linetype_map <- c("fao_based" = "solid", "FDM" = "dashed")

p_fig3a <- ggplot(df_comp, aes(x = Y, y = value, colour = U, linetype = U)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  geom_line(linewidth = 0.45) +
  facet_wrap(~ Ref, nrow = 1) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_colour_manual(values = comparison_cols, drop = FALSE) +
  scale_linetype_manual(values = linetype_map, drop = FALSE) +
  labs(
    x = "Year",
    y = "Population at risk of hunger (million people)",
    colour = "U",
    linetype = "U"
  ) +
  theme_kyoeiken(base_size = 8) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    panel.spacing.x = grid::unit(0.7, "lines")
  )

PoUchange_FDM <- as_tibble(rgdx.param(fdm_file, "PoU_base_add"))
if ("PoU_base_add" %in% names(PoUchange_FDM)) {
  PoUchange_FDM <- PoUchange_FDM |> rename(value = PoU_base_add)
}
PoUchange_FDM <- PoUchange_FDM |>
  mutate(Y = suppressWarnings(as.integer(as.character(Y)))) |>
  filter(as.character(eff) %in% c("Income", "Price")) |>
  group_by(Y) |>
  summarise(change = sum(value, na.rm = TRUE) / 1000000, .groups = "drop") |>
  mutate(U = "FDM")

PoUchange_FAO <- df_FAO |>
  select(Y, U, Ref, value) |>
  pivot_wider(names_from = Ref, values_from = value) |>
  mutate(change = `400C` - Baseline) |>
  select(Y, U, change)

df_comp2 <- bind_rows(PoUchange_FDM, PoUchange_FAO) |>
  mutate(
    U = factor(U, levels = c("fao_based", "FDM")),
    Y = factor(Y, levels = sort(unique(Y)))
  ) |>
  filter(!is.na(change))

p_fig3b <- ggplot(df_comp2, aes(x = Y, y = change, fill = U)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  geom_col(
    width = 0.85,
    position = position_dodge(width = 0.9),
    colour = "black",
    linewidth = 0.15
  ) +
  scale_fill_manual(values = comparison_cols, drop = FALSE) +
  labs(
    x = "Year",
    y = "Change from BaU (million people)",
    fill = "U"
  ) +
  theme_kyoeiken(base_size = 8) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

p_fig3 <- (
  p_fig3a +
    labs(tag = "a") +
    theme(plot.tag = element_text(face = "bold", size = 10))
) / (
  p_fig3b +
    labs(tag = "b") +
    theme(plot.tag = element_text(face = "bold", size = 10))
) +
  plot_layout(heights = c(1.1, 1))

p_fig3a_separate <- p_fig3a +
  facet_wrap(~ Ref, ncol = 1) +
  labs(tag = "a") +
  theme(
    plot.tag = element_text(face = "bold", size = 10),
    panel.spacing.y = grid::unit(0.45, "lines")
  )

p_fig3b_separate <- p_fig3b +
  labs(tag = "b") +
  theme(plot.tag = element_text(face = "bold", size = 10))

out_files <- c(
  save_png(p_fig1, "fig1_food_consumption_change_2050_color.png", width = 9, height = 12),
  save_png(p_fig1_pct, "fig1_food_consumption_change_2050_vsBase_pct_color.png", width = 16, height = 8),
  save_png(p_fig2, "fig2_hunger_risk_population_change_2050_color.png", width = 12, height = 7),
  save_png(p_fig3, "fig3_hungertool_comparison_color.png", width = 15, height = 11.5),
  save_png(p_fig3a_separate, "fig3a_hungertool_comparison_color.png", width = 9, height = 10),
  save_png(p_fig3b_separate, "fig3b_hungertool_comparison_color.png", width = 11, height = 10)
)

cat("Saved color figures:\n")
cat(paste(out_files, collapse = "\n"), "\n")
