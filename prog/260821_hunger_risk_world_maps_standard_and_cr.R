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
out_root <- file.path(project_dir, "output", "260807_FDM_scenario_quantile")
target_years <- c(2020L, 2050L, 2100L)
scenario_labels_all <- c(
  SSP2_BaU_NoCC_No = "BaU",
  SSP2_400C_2025CP_NoCC_No = "400C / 2025CP",
  SSP2_400C_2030CP_NoCC_No = "400C / 2030CP"
)
series_specs <- list(
  Standard = list(
    symbol = "PoU_In_Pr",
    ragg_symbol = "PoU_In_Pr_Ragg_calc",
    output_dir = file.path(out_root, "hunger_risk_population_world_maps"),
    file_tag = ""
  ),
  CR = list(
    symbol = "PoU_In_Pr_CR",
    ragg_symbol = "PoU_In_Pr_CR_Ragg_calc",
    output_dir = file.path(out_root, "hunger_risk_population_world_maps_CR"),
    file_tag = "_CR"
  )
)

read_map_series <- function(series_name, spec) {
  raw <- as.data.table(rgdx.param(source_gdx, spec$symbol))
  required_columns <- c("Ref", "R", "Y", spec$symbol)
  missing_columns <- setdiff(required_columns, names(raw))
  if (length(missing_columns) > 0L) {
    stop(spec$symbol, " is missing required columns: ", paste(missing_columns, collapse = ", "))
  }

  raw[, `:=`(
    Ref = as.character(Ref),
    R = as.character(R),
    Year = as.integer(as.character(Y)),
    population_million = as.numeric(get(spec$symbol)) / 1e6
  )]
  if (any(!is.finite(raw$population_million)) || any(raw$population_million < 0)) {
    stop(spec$symbol, " contains non-finite or negative values.")
  }

  model_countries <- sort(unique(raw[Ref == "SSP2_BaU_NoCC_No", R]))
  if (length(model_countries) == 0L) {
    stop("No BaU countries found in ", spec$symbol)
  }
  coverage <- raw[
    Ref %in% names(scenario_labels_all),
    .(country_count = uniqueN(R)),
    by = Ref
  ]
  coverage[, `:=`(
    expected_country_count = length(model_countries),
    coverage_share = country_count / length(model_countries),
    included = country_count / length(model_countries) >= 0.9
  )]
  selected_refs <- names(scenario_labels_all)[
    names(scenario_labels_all) %in% coverage[included == TRUE, Ref]
  ]
  if (!all(c("SSP2_BaU_NoCC_No", "SSP2_400C_2030CP_NoCC_No") %in% selected_refs)) {
    stop(
      spec$symbol,
      " must contain near-complete BaU and 2030CP country coverage; selected refs: ",
      paste(selected_refs, collapse = ", ")
    )
  }

  sparse <- raw[
    Year %in% target_years & Ref %in% selected_refs,
    .(Ref, R, Year, population_million)
  ]
  if (sparse[, anyDuplicated(paste(Ref, R, Year))] > 0L) {
    stop(spec$symbol, " has duplicate Ref x R x Year records.")
  }
  grid <- CJ(
    Ref = selected_refs,
    R = model_countries,
    Year = target_years,
    unique = TRUE
  )
  complete <- merge(
    grid,
    sparse,
    by = c("Ref", "R", "Year"),
    all.x = TRUE,
    sort = FALSE
  )
  complete[is.na(population_million), population_million := 0]
  selected_labels <- scenario_labels_all[selected_refs]
  complete[, `:=`(
    Scenario = factor(unname(selected_labels[Ref]), levels = unname(selected_labels)),
    Series = series_name,
    Symbol = spec$symbol
  )]

  ragg <- as.data.table(rgdx.param(source_gdx, spec$ragg_symbol))
  ragg[, `:=`(
    Ref = as.character(Ref),
    R17 = as.character(R17),
    Year = as.integer(as.character(Y)),
    WLD = as.numeric(get(spec$ragg_symbol))
  )]
  country_sums <- complete[, .(country_sum = sum(population_million) * 1e6), by = .(Ref, Year)]
  world_totals <- ragg[
    R17 == "WLD" & Ref %in% selected_refs & Year %in% target_years,
    .(Ref, Year, WLD)
  ]
  check <- merge(country_sums, world_totals, by = c("Ref", "Year"), all.x = TRUE)
  if (check[is.na(WLD) | abs(country_sum - WLD) > pmax(1, abs(WLD)) * 1e-8, .N] > 0L) {
    stop("Country sums do not match WLD for ", spec$symbol)
  }

  list(
    data = complete,
    coverage = coverage,
    model_countries = model_countries,
    selected_refs = selected_refs,
    selected_labels = selected_labels,
    spec = spec,
    series_name = series_name
  )
}

prepared <- Map(read_map_series, names(series_specs), series_specs)
names(prepared) <- names(series_specs)
if (!identical(prepared$Standard$model_countries, prepared$CR$model_countries)) {
  stop("Standard and CR country sets differ.")
}
if (!identical(prepared$Standard$selected_refs, prepared$CR$selected_refs)) {
  stop("Standard and CR complete-scenario sets differ.")
}

model_codes <- prepared$Standard$model_countries
world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world[world$admin != "Antarctica", ]
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
if (length(unmatched_codes) > 0L) {
  stop("Natural Earth geometry is missing model country codes: ", paste(unmatched_codes, collapse = ", "))
}

shared_limit <- max(vapply(prepared, function(x) max(x$data$population_million), numeric(1)))
legend_breaks <- c(0, 1, 5, 10, 25, 50, 100, 200, 300)
legend_breaks <- legend_breaks[legend_breaks <= shared_limit]
if (tail(legend_breaks, 1L) < shared_limit * 0.75) {
  legend_breaks <- c(legend_breaks, shared_limit)
}

make_map_data <- function(prepared_series, year_value) {
  pieces <- lapply(prepared_series$selected_refs, function(ref_value) {
    values <- prepared_series$data[Year == year_value & Ref == ref_value]
    map_piece <- world
    value_index <- match(map_piece$map_code, values$R)
    map_piece$population_million <- values$population_million[value_index]
    map_piece$Scenario <- factor(
      prepared_series$selected_labels[[ref_value]],
      levels = unname(prepared_series$selected_labels)
    )
    map_piece
  })
  do.call(rbind, pieces)
}

for (prepared_series in prepared) {
  spec <- prepared_series$spec
  dir.create(spec$output_dir, recursive = TRUE, showWarnings = FALSE)
  fwrite(
    data.table(R = unmatched_codes),
    file.path(spec$output_dir, "unmatched_model_country_codes.csv")
  )
  fwrite(
    prepared_series$coverage,
    file.path(spec$output_dir, "scenario_country_coverage.csv")
  )

  output_rows <- vector("list", length(target_years))
  for (i in seq_along(target_years)) {
    year_value <- target_years[[i]]
    map_data <- make_map_data(prepared_series, year_value)
    panel_count <- length(prepared_series$selected_refs)

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
        barwidth = grid::unit(10, "cm"),
        barheight = grid::unit(0.45, "cm"),
        ticks.colour = "#444444",
        frame.colour = "#777777"
      )) +
      labs(
        title = paste0(
          "Population at risk of hunger in ",
          year_value,
          if (prepared_series$series_name == "CR") " - CR" else ""
        ),
        subtitle = paste0("Income and food-price effects (", spec$symbol, ")"),
        caption = paste0(
          "Source: Fooddem_PriceEffect.gdx. Shared colour scale across Standard and CR."
        )
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

    filename <- sprintf(
      "hunger_risk_population_world_map%s_%d.png",
      spec$file_tag,
      year_value
    )
    output_path <- file.path(spec$output_dir, filename)
    ggsave(
      output_path,
      p,
      width = if (panel_count == 2L) 14 else 18,
      height = 7.2,
      units = "in",
      dpi = 300,
      bg = "white"
    )

    output_rows[[i]] <- data.table(
      year = year_value,
      file = filename,
      series = prepared_series$series_name,
      metric = spec$symbol,
      unit = "million people",
      scenarios = paste(unname(prepared_series$selected_labels), collapse = " | "),
      model_country_count = length(model_codes),
      mapped_country_count = length(mapped_codes),
      shared_scale_max_million = shared_limit
    )
  }

  fwrite(
    prepared_series$data[
      , .(Series, Symbol, Ref, Scenario = as.character(Scenario), R, Year, population_million)
    ],
    file.path(
      spec$output_dir,
      paste0("hunger_risk_population_world_map", spec$file_tag, "_data.csv")
    )
  )
  fwrite(
    rbindlist(output_rows),
    file.path(
      spec$output_dir,
      paste0("hunger_risk_population_world_map", spec$file_tag, "_manifest.csv")
    )
  )
}

cat("Created Standard and CR maps for", length(target_years), "years.\n")
for (prepared_series in prepared) {
  cat(
    prepared_series$series_name,
    ": ",
    normalizePath(prepared_series$spec$output_dir, winslash = "/", mustWork = TRUE),
    "\n",
    sep = ""
  )
}
