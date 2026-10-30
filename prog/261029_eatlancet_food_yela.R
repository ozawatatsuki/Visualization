# Create regional Eat-Lancet comparisons from the IAMC GDX parameter.
# Run from any directory: Rscript prog/261029_eatlancet_food_yela.R

# 1. Paths and packages -----------------------------------------------------
user_library <- file.path(Sys.getenv("USERPROFILE"), "AppData", "Local", "R", "win-library",
                          paste(R.version$major, sub("\\..*$", "", R.version$minor), sep = "."))
if (dir.exists(user_library)) .libPaths(c(user_library, .libPaths()))
required_packages <- c("gdxrrw", "data.table", "ggplot2")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                             logical(1), quietly = TRUE)]
if (length(missing_packages)) stop("Install packages: ", paste(missing_packages, collapse = ", "))
suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
})
script_argument <- grep("^--file=", commandArgs(), value = TRUE)
project_dir <- if (length(script_argument)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_argument[1]), winslash = "/")))
} else normalizePath(getwd(), winslash = "/")
gdx_file <- file.path(project_dir, "data/EatLancet/new_ela/global_17_IAMC.gdx")
output_dir <- file.path(project_dir, "output/EatLancet/new_ela")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(gdx_file)) stop("GDX file not found: ", gdx_file)

# 2. Select GDX labels explicitly ------------------------------------------
read_set <- function(name) {
  result <- gdxrrw::rgdx(gdx_file, list(name = name))
  result$uels[[1]][result$val[, 1]]
}
regions <- read_set("REMF")
regions <- c(regions[grepl("^R5", regions)], regions[tolower(regions) == "world"])
if (length(regions) != 6L) stop("Expected five R5 regions and World.")
scenario_labels <- c(SSP2_BaU_NoCC_No = "BaU", SSP2_BaU_DEMFWR_No = "Lancet")
if (!setequal(grep("^SSP2_BaU", read_set("SCENARIO"), value = TRUE),
              names(scenario_labels))) stop("Unexpected SSP2_BaU scenarios.")
variables <- grep("^AGMIP_.+_(food|YELA)$", read_set("VEMF"), value = TRUE)
years <- read_set("YEMF")

# Read only the required slices; indices in val refer to the returned uels.
raw <- gdxrrw::rgdx(gdx_file, list(
  name = "IAMC_Template", form = "sparse",
  uels = list(names(scenario_labels), regions, variables, years)
))
data <- as.data.table(lapply(seq_len(4L), function(i) raw$uels[[i]][raw$val[, i]]))
setnames(data, c("SCENARIO", "REMF", "VEMF", "YEMF"))
data[, value := raw$val[, 5]]
data[, `:=`(scenario = factor(unname(scenario_labels[SCENARIO]), c("BaU", "Lancet")),
             region = factor(REMF, regions), year = as.integer(YEMF),
             good = sub("_(food|YELA)$", "", sub("^AGMIP_", "", VEMF)))]
if (anyDuplicated(data[, .(SCENARIO, REMF, VEMF, YEMF)])) stop("Duplicate observations.")

# Units come from the file rather than an assumed interpretation of food/YELA.
unit_map <- gdxrrw::rgdx(gdx_file, list(name = "VUMAP"))
units <- data.table(VEMF = unit_map$uels[[1]][unit_map$val[, 1]],
                    unit = unit_map$uels[[2]][unit_map$val[, 2]])
axis_label <- function(selected_variables, label) {
  selected_units <- unique(units[VEMF %in% selected_variables, unit])
  if (length(selected_units) != 1L) stop("Expected one shared unit for ", label)
  paste0(label, " (", selected_units, ")")
}

# 3. Prepare the datasets and verify coverage -------------------------------
food_timeseries <- copy(data[grepl("_food$", VEMF)])
food <- copy(data[grepl("_food$", VEMF) & year == 2070])
yela <- copy(data[grepl("_YELA$", VEMF)])
validate_coverage <- function(selected, selected_variables, selected_years) {
  expected <- CJ(SCENARIO = names(scenario_labels), REMF = regions,
                 VEMF = selected_variables, YEMF = selected_years)
  missing <- fsetdiff(expected, selected[, .(SCENARIO, REMF, VEMF, YEMF)])
  if (nrow(missing)) {
    fwrite(missing, file.path(output_dir, "missing_observations.csv"))
    stop("Missing observations; see missing_observations.csv. Sparse omissions are not assumed to be zero.")
  }
  if (any(!is.finite(selected$value))) stop("Non-finite values found.")
}
validate_coverage(food, grep("_food$", variables, value = TRUE), "2070")
validate_coverage(food_timeseries, grep("_food$", variables, value = TRUE), years)
validate_coverage(yela, grep("_YELA$", variables, value = TRUE), years)
food[, good := factor(good, unique(sub("^AGMIP_(.+)_food$", "\\1", variables[grepl("_food$", variables)])))]
food_timeseries[, good := factor(good, levels(food$good))]
if (any(food_timeseries$value < 0)) stop("Stacked areas require non-negative food values.")
yela[, good := factor(good, unique(sub("^AGMIP_(.+)_YELA$", "\\1", variables[grepl("_YELA$", variables)])))]

# 4. Shared style and plots -------------------------------------------------
plot_theme <- theme_bw(base_size = 12) + theme(
  panel.grid.minor = element_blank(), strip.background = element_rect(fill = "#EFF3F6"),
  strip.text = element_text(face = "bold"), legend.position = "bottom",
  plot.title = element_text(face = "bold"), plot.caption = element_text(hjust = 0)
)
food_plot <- ggplot(food, aes(good, value, fill = scenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  facet_wrap(vars(region), ncol = 3) +
  scale_fill_manual(values = c(BaU = "#4677A5", Lancet = "#DE8053")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.06)), labels = scales::label_number()) +
  labs(title = "Food consumption in 2070", x = "Good (AGMIP code)",
       y = axis_label(unique(food$VEMF), "Food"), fill = "Scenario",
       caption = "Source: global_17_IAMC.gdx | IAMC_Template | Aggregates included as stored in the GDX.") +
  plot_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Version 1: each good has its own y scale, shared across the six regions.
food_by_good_plot <- ggplot(food, aes(scenario, value, fill = scenario)) +
  geom_col(width = 0.7) +
  facet_grid(rows = vars(good), cols = vars(region), scales = "free_y") +
  scale_fill_manual(values = c(BaU = "#4677A5", Lancet = "#DE8053")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08)),
                     labels = scales::label_number()) +
  labs(title = "Food consumption in 2070: scales by good", x = NULL,
       y = axis_label(unique(food$VEMF), "Food"), fill = "Scenario",
       caption = "Each row uses a separate y scale; regions within a row share the same scale.") +
  plot_theme

# Version 2: pair observations by region and good before subtraction.
food_difference <- dcast(food, REMF + region + VEMF + good + year ~ scenario,
                         value.var = "value")
if (anyNA(food_difference[, .(BaU, Lancet)])) stop("Unpaired food scenarios.")
food_difference[, difference := Lancet - BaU]
food_difference[, direction := factor(ifelse(difference >= 0, "Increase", "Decrease"),
                                     levels = c("Increase", "Decrease"))]
food_difference_plot <- ggplot(food_difference, aes(good, difference, fill = direction)) +
  geom_hline(yintercept = 0, colour = "#555555", linewidth = 0.4) +
  geom_col(width = 0.7) + facet_wrap(vars(region), ncol = 3) +
  scale_fill_manual(values = c(Increase = "#DE8053", Decrease = "#4677A5")) +
  scale_y_continuous(labels = scales::label_number(), expand = expansion(mult = 0.08)) +
  labs(title = "Food consumption difference in 2070: Lancet - BaU",
       x = "Good (AGMIP code)", y = "Lancet - BaU (1000 t)", fill = NULL,
       caption = "Positive values: Lancet exceeds BaU. Negative values: Lancet is below BaU.") +
  plot_theme + theme(axis.text.x = element_text(angle = 45, hjust = 1))

good_colors <- setNames(c("#0072B2", "#D55E00", "#009E73", "#CC79A7",
                          "#E69F00", "#56B4E9", "#7B4F24", "#333333"), levels(yela$good))
yela_plot <- ggplot(yela, aes(year, value, color = good, linetype = scenario,
                             group = interaction(good, scenario))) +
  geom_line(linewidth = 0.8) + facet_wrap(vars(region), ncol = 3) +
  scale_color_manual(values = good_colors) +
  scale_linetype_manual(values = c(BaU = "solid", Lancet = "dashed")) +
  scale_x_continuous(breaks = pretty(range(yela$year), n = 5)) +
  labs(title = "YELA over time", x = "Year (YEMF)",
       y = axis_label(unique(yela$VEMF), "YELA"), color = "Good", linetype = "Scenario",
       caption = "Source: global_17_IAMC.gdx | IAMC_Template") +
  plot_theme + guides(color = guide_legend(nrow = 2, order = 1),
                      linetype = guide_legend(order = 2))

# Match the requested wildcard, including CR5/CRP/AGR aggregates.
# The stack is a sum of the selected series, not a non-overlapping food total.
food_colors <- c(
  PAS = "#88B04B", WHT = "#E6B84D", CGR = "#D97936", RIC = "#56B4E9",
  OSD = "#CC79A7", SUG = "#F0D77A", RUM = "#A64B3C", NRM = "#7B4F24",
  DRY = "#0072B2", CR5 = "#78968B", CRP = "#8C85B8", AGR = "#495866"
)
food_area_plot <- ggplot(food_timeseries, aes(year, value, fill = good)) +
  geom_area(position = position_stack(reverse = TRUE), linewidth = 0) +
  facet_grid(rows = vars(scenario), cols = vars(region)) +
  scale_fill_manual(values = food_colors) +
  scale_x_continuous(breaks = c(2020, 2060, 2100), expand = expansion(mult = 0)) +
  scale_y_continuous(labels = scales::label_number(),
                     expand = expansion(mult = c(0, 0.04))) +
  labs(title = "Food consumption over time", x = "Year (YEMF)", y = "1000 t",
       fill = "Good (AGMIP code)",
       caption = paste("Source: global_17_IAMC.gdx | IAMC_Template",
                       "All AGMIP_*_food series included; CR5/CRP/AGR aggregates overlap component goods.",
                       sep = "\n")) +
  plot_theme + guides(fill = guide_legend(nrow = 2, byrow = TRUE))

# 5. Export figures and the exact observations used -------------------------
save_plot <- function(plot, name, width = 15, height = 9) {
  for (extension in c("png", "pdf")) {
    ggsave(file.path(output_dir, paste0(name, ".", extension)), plot,
           width = width, height = height, units = "in", dpi = 300, bg = "white")
  }
}
save_plot(food_plot, "food_2070_R5_world")
save_plot(food_by_good_plot, "food_2070_scales_by_good_R5_world", width = 18, height = 26)
save_plot(food_difference_plot, "food_2070_Lancet_minus_BaU_R5_world")
fwrite(food_difference[, .(REMF, VEMF, year, good, BaU, Lancet, difference)],
       file.path(output_dir, "food_2070_Lancet_minus_BaU_plot_data.csv"))
save_plot(yela_plot, "yela_timeseries_R5_world")
save_plot(food_area_plot, "food_stacked_area_R5_world", width = 20, height = 9)
fwrite(food_timeseries[, .(SCENARIO, REMF, VEMF, year, scenario, good, value)],
       file.path(output_dir, "food_stacked_area_plot_data.csv"))
fwrite(food[, .(SCENARIO, REMF, VEMF, year, scenario, good, value)],
       file.path(output_dir, "food_2070_plot_data.csv"))
fwrite(yela[, .(SCENARIO, REMF, VEMF, year, scenario, good, value)],
       file.path(output_dir, "yela_timeseries_plot_data.csv"))
fwrite(units[VEMF %in% variables], file.path(output_dir, "variable_units.csv"))
message("Saved figures to: ", output_dir)
print(data[, .(observations = .N, min_year = min(year), max_year = max(year)), by = scenario])
