suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(rnaturalearth)
  library(scales)
  library(sf)
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
data_candidates <- data_candidates[file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx"))]
if (length(data_candidates) != 1L) {
  stop(
    "Expected exactly one data/260807_FDM* directory containing Fooddem_PriceEffect.gdx; found: ",
    length(data_candidates)
  )
}

source_gdx <- file.path(data_candidates[[1]], "Fooddem_PriceEffect.gdx")
out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "hunger_risk_population_world_maps"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_years <- c(2020L, 2050L, 2100L)
scenario_labels <- c(
  SSP2_BaU_NoCC_No = "BaU",
  SSP2_400C_2025CP_NoCC_No = "400C / 2025CP",
  SSP2_400C_2030CP_NoCC_No = "400C / 2030CP"
)

hunger <- as.data.table(rgdx.param(source_gdx, "PoU_In_Pr"))
required_columns <- c("Ref", "R", "Y", "PoU_In_Pr")
missing_columns <- setdiff(required_columns, names(hunger))
if (length(missing_columns) > 0L) {
  stop("PoU_In_Pr is missing required columns: ", paste(missing_columns, collapse = ", "))
}

hunger[, `:=`(
  Ref = as.character(Ref),
  R = as.character(R),
  Year = as.integer(as.character(Y)),
  population_million = PoU_In_Pr / 1e6
)]
model_countries <- sort(unique(hunger$R))
hunger_sparse <- hunger[
  Year %in% target_years & Ref %in% names(scenario_labels),
  .(Ref, R, Year, population_million)
]
if (hunger_sparse[, anyDuplicated(paste(Ref, R, Year))] > 0L) {
  stop("PoU_In_Pr has duplicate Ref x R x Year records in the requested maps.")
}

# GDX parameters are sparse: omitted Ref x R x Year records represent zero.
hunger_grid <- CJ(
  Ref = names(scenario_labels),
  R = model_countries,
  Year = target_years,
  unique = TRUE
)
hunger <- merge(
  hunger_grid,
  hunger_sparse,
  by = c("Ref", "R", "Year"),
  all.x = TRUE,
  sort = FALSE
)
hunger[is.na(population_million), population_million := 0]
hunger[, Scenario := factor(
  unname(scenario_labels[Ref]),
  levels = unname(scenario_labels)
)]

missing_years <- setdiff(target_years, unique(hunger$Year))
missing_scenarios <- setdiff(names(scenario_labels), unique(hunger$Ref))
if (length(missing_years) > 0L) {
  stop("PoU_In_Pr is missing requested years: ", paste(missing_years, collapse = ", "))
}
if (length(missing_scenarios) > 0L) {
  stop("PoU_In_Pr is missing requested scenarios: ", paste(missing_scenarios, collapse = ", "))
}
if (any(!is.finite(hunger$population_million)) || any(hunger$population_million < 0)) {
  stop("PoU_In_Pr contains non-finite or negative values in the requested years/scenarios.")
}

expected_rows <- length(model_countries) * length(target_years) * length(scenario_labels)
if (nrow(hunger) != expected_rows || hunger[, anyDuplicated(paste(Ref, R, Year))] > 0L) {
  stop("PoU_In_Pr does not form a complete Ref x R x Year grid for the requested maps.")
}

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$admin != "Antarctica", ]
model_codes <- unique(hunger$R)

# Prefer the Natural Earth code variant that exactly matches the model's ISO3 code.
candidate_fields <- c("iso_a3_eh", "iso_a3", "adm0_a3")
candidate_fields <- candidate_fields[candidate_fields %in% names(world)]
world$map_code <- NA_character_
for (field in candidate_fields) {
  candidate <- as.character(world[[field]])
  use <- is.na(world$map_code) & candidate %in% model_codes
  world$map_code[use] <- candidate[use]
}

mapped_codes <- unique(stats::na.omit(world$map_code))
unmatched_codes <- setdiff(model_codes, mapped_codes)
fwrite(
  data.table(R = unmatched_codes),
  file.path(out_dir, "unmatched_model_country_codes.csv")
)
if (length(unmatched_codes) > 0L) {
  stop("Natural Earth geometry is missing model country codes: ", paste(unmatched_codes, collapse = ", "))
}

shared_limit <- max(hunger$population_million)
legend_breaks <- c(0, 1, 5, 10, 25, 50, 100, 200, 300)
legend_breaks <- legend_breaks[legend_breaks <= shared_limit]
if (tail(legend_breaks, 1L) < shared_limit * 0.75) {
  legend_breaks <- c(legend_breaks, shared_limit)
}

make_map_data <- function(year_value) {
  pieces <- lapply(names(scenario_labels), function(ref_value) {
    values <- hunger[Year == year_value & Ref == ref_value]
    map_piece <- world
    value_index <- match(map_piece$map_code, values$R)
    map_piece$population_million <- values$population_million[value_index]
    map_piece$Scenario <- factor(
      scenario_labels[[ref_value]],
      levels = unname(scenario_labels)
    )
    map_piece
  })
  do.call(rbind, pieces)
}

output_rows <- vector("list", length(target_years))
for (i in seq_along(target_years)) {
  year_value <- target_years[[i]]
  map_data <- make_map_data(year_value)

  p <- ggplot(map_data) +
    geom_sf(
      aes(fill = population_million),
      colour = "white",
      linewidth = 0.08
    ) +
    facet_wrap(vars(Scenario), nrow = 1) +
    scale_fill_gradientn(
      colours = c("#FFF7EC", "#FDD49E", "#FC8D59", "#D7301F", "#7F0000"),
      values = rescale(c(0, 0.05, 0.2, 0.55, 1)),
      limits = c(0, shared_limit),
      breaks = legend_breaks,
      trans = "sqrt",
      labels = label_number(accuracy = 1),
      na.value = "#D9D9D9",
      name = "Population at risk of hunger (million)"
    ) +
    coord_sf(crs = "ESRI:54030", datum = NA, expand = FALSE) +
    guides(fill = guide_colourbar(
      title.position = "top",
      title.hjust = 0.5,
      barwidth = grid::unit(12, "cm"),
      barheight = grid::unit(0.45, "cm"),
      ticks.colour = "#444444",
      frame.colour = "#777777"
    )) +
    labs(
      title = paste0("Population at risk of hunger in ", year_value),
      subtitle = "Income and food-price effects (PoU_In_Pr)",
      caption = "Source: Fooddem_PriceEffect.gdx"
    ) +
    theme_void(base_size = 12) +
    theme(
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "#EAF2F5", colour = NA),
      strip.background = element_rect(fill = "#F2F2F2", colour = "#D0D0D0", linewidth = 0.4),
      strip.text = element_text(face = "bold", colour = "#222222", size = 12, margin = margin(7, 6, 7, 6)),
      plot.title = element_text(face = "bold", size = 18, hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(size = 11, colour = "#555555", hjust = 0.5, margin = margin(b = 10)),
      plot.caption = element_text(size = 8.5, colour = "#666666", hjust = 1, margin = margin(t = 7)),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.margin = margin(t = 2),
      panel.spacing = grid::unit(0.25, "cm"),
      plot.margin = margin(14, 16, 10, 16)
    )

  filename <- sprintf("hunger_risk_population_world_map_%d.png", year_value)
  output_path <- file.path(out_dir, filename)
  ggsave(
    output_path,
    p,
    width = 18,
    height = 7.2,
    units = "in",
    dpi = 300,
    bg = "white"
  )

  output_rows[[i]] <- data.table(
    year = year_value,
    file = filename,
    metric = "PoU_In_Pr",
    unit = "million people",
    scenarios = paste(unname(scenario_labels), collapse = " | "),
    model_country_count = uniqueN(hunger$R),
    mapped_country_count = length(mapped_codes),
    shared_scale_max_million = shared_limit
  )
}

fwrite(
  hunger[, .(Ref, Scenario = as.character(Scenario), R, Year, population_million)],
  file.path(out_dir, "hunger_risk_population_world_map_data.csv")
)
fwrite(
  rbindlist(output_rows),
  file.path(out_dir, "hunger_risk_population_world_map_manifest.csv")
)

cat("Created", length(target_years), "PNG maps in:\n")
cat(normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n")
