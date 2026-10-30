suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

input_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "input_RAS.gdx"))
if (length(input_candidates) != 1) {
  stop("Expected exactly one input_RAS.gdx under data/260727_FDM*, found: ", length(input_candidates))
}
input_ras_path <- input_candidates[[1]]

map_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "region_iso_R17PHI.map"))
if (length(map_candidates) != 1) {
  stop("Expected exactly one region_iso_R17PHI.map under data/260727_FDM*, found: ", length(map_candidates))
}
region_map_path <- map_candidates[[1]]

out_dir <- file.path(
  project_dir,
  "output",
  "260727_FDM_hierarchy_update",
  "lognormal_food_consumption_distribution_2050"
)
country_out_dir <- file.path(out_dir, "country_by_r17")
dir.create(country_out_dir, recursive = TRUE, showWarnings = FALSE)
legacy_out_dir <- file.path(
  project_dir,
  "output",
  "260727_FDM_hierarchy_update",
  "lognormal_income_distribution_2050"
)

plot_year <- 2050L
baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
refs <- c(baseline_ref, scenario_ref)
density_points <- 1001L
upper_probability <- 0.999

is_country_code <- function(x) {
  !x %in% "WLD" & !grepl("^R17", x)
}

read_region_map <- function(path) {
  lines <- trimws(readLines(path, warn = FALSE))
  lines <- lines[nzchar(lines) & !grepl("^\\*", lines)]
  parts <- tstrsplit(lines, ".", fixed = TRUE)
  if (length(parts) != 2) {
    stop("region_iso_R17PHI.map must contain rows like ISO3.R17.")
  }

  mapping <- data.table(
    R = trimws(parts[[1]]),
    R17 = trimws(parts[[2]]),
    R17_mapping_source = "region_iso_R17PHI.map"
  )
  mapping <- mapping[nzchar(R) & nzchar(R17)]
  if (anyDuplicated(mapping$R) > 0) {
    stop("Duplicate ISO3 rows found in region_iso_R17PHI.map.")
  }
  mapping
}

supplemented_region_map <- data.table(
  R = c("ATG", "FSM", "GRD", "KIR", "SDN", "SYC"),
  R17 = c("R17XLM", "R17XSA", "R17XLM", "R17XSA", "R17XAF", "R17XAF"),
  R17_mapping_source = "supplemented"
)

theme_report <- function(base_size = 10) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "grey88", linewidth = 0.25),
      panel.grid.major.y = element_line(color = "grey86", linewidth = 0.25),
      axis.text = element_text(color = "grey25"),
      axis.title = element_text(color = "grey10"),
      plot.title = element_text(face = "bold", color = "grey10"),
      plot.subtitle = element_text(color = "grey30"),
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.margin = margin(8, 10, 8, 8)
    )
}

clear_pngs <- function(path) {
  png_files <- list.files(path, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
  if (length(png_files) > 0) {
    unlink(png_files, force = TRUE)
  }
}

cat("Reading mu_ln and sigma...\n")
mu <- as.data.table(rgdx.param(input_ras_path, "mu_ln"))
sigma <- as.data.table(rgdx.param(input_ras_path, "sigma"))

mu <- mu[
  Ref %in% refs &
    Y == as.character(plot_year) &
    is_country_code(as.character(R)),
  .(
    R = as.character(R),
    Ref = as.character(Ref),
    year = as.integer(as.character(Y)),
    mu_ln = as.numeric(mu_ln)
  )
]
sigma <- sigma[
  Ref %in% refs &
    Y == as.character(plot_year) &
    is_country_code(as.character(R)),
  .(
    R = as.character(R),
    Ref = as.character(Ref),
    year = as.integer(as.character(Y)),
    sigma = as.numeric(sigma)
  )
]

parameters <- merge(mu, sigma, by = c("R", "Ref", "year"), all = FALSE)
if (any(!is.finite(parameters$mu_ln)) || any(!is.finite(parameters$sigma))) {
  stop("Non-finite mu_ln or sigma values found.")
}
if (any(parameters$sigma <= 0)) {
  stop("sigma must be positive for every country and Ref.")
}

parameter_counts <- parameters[, .N, by = .(R)]
if (any(parameter_counts$N != length(refs))) {
  stop(
    "Countries without both Ref parameter sets: ",
    paste(parameter_counts[N != length(refs), R], collapse = ", ")
  )
}

region_map <- unique(
  rbindlist(
    list(read_region_map(region_map_path), supplemented_region_map),
    use.names = TRUE
  ),
  by = "R"
)
parameters <- merge(parameters, region_map, by = "R", all.x = TRUE)
if (anyNA(parameters$R17)) {
  stop(
    "Missing R17 mapping: ",
    paste(sort(unique(parameters[is.na(R17), R])), collapse = ", ")
  )
}
parameters[, Ref := factor(Ref, levels = refs)]

country_upper <- parameters[
  ,
  .(
    x_upper = max(
      qlnorm(upper_probability, meanlog = mu_ln, sdlog = sigma),
      na.rm = TRUE
    )
  ),
  by = R
]
parameters <- merge(parameters, country_upper, by = "R", all.x = TRUE)
setorder(parameters, R17, R, Ref)

fwrite(
  parameters,
  file.path(out_dir, "lognormal_food_consumption_parameters_2050_country.csv")
)

fill_values <- c(
  "SSP2_BaU_NoCC_No" = "#009E73",
  "SSP2_400C_2025CP_NoCC_No" = "#CC79A7"
)

clear_pngs(country_out_dir)
density_results <- vector("list", length = uniqueN(parameters$R))
countries <- sort(unique(parameters$R))

cat("Plotting country lognormal distributions...\n")
for (country_idx in seq_along(countries)) {
  country <- countries[[country_idx]]
  country_parameters <- parameters[R == country]
  x_values <- seq(0, country_parameters$x_upper[[1]], length.out = density_points)

  country_density <- country_parameters[
    ,
    .(
      x = x_values,
      density = dlnorm(x_values, meanlog = mu_ln, sdlog = sigma)
    ),
    by = .(R17, R, Ref, year, mu_ln, sigma, x_upper, R17_mapping_source)
  ]
  density_results[[country_idx]] <- country_density

  r17 <- country_parameters$R17[[1]]
  target_dir <- file.path(country_out_dir, r17)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  p <- ggplot(
    country_density,
    aes(x = x, y = density, fill = Ref)
  ) +
    geom_area(position = "identity", alpha = 0.42, color = NA) +
    scale_fill_manual(values = fill_values, drop = FALSE) +
    scale_x_continuous(
      labels = label_number(big.mark = ","),
      limits = c(0, country_parameters$x_upper[[1]]),
      expand = expansion(mult = c(0, 0))
    ) +
    scale_y_continuous(
      labels = label_number(accuracy = 0.00001),
      expand = expansion(mult = c(0, 0.04))
    ) +
    labs(
      title = paste0(country, " lognormal food consumption distribution in ", plot_year),
      subtitle = "meanlog = mu_ln; sdlog = sigma; x-range covers 99.9% of both Ref distributions",
      x = "Food consumption (kcal/day)",
      y = "Probability density",
      fill = NULL
    ) +
    theme_report(base_size = 10)

  ggsave(
    file.path(
      target_dir,
      paste0("lognormal_food_consumption_distribution_2050_", country, ".png")
    ),
    p,
    width = 11,
    height = 7.2,
    dpi = 300,
    bg = "white"
  )
}

all_density <- rbindlist(density_results, use.names = TRUE)
fwrite(
  all_density,
  file.path(out_dir, "lognormal_food_consumption_density_2050_country.csv")
)

if (dir.exists(legacy_out_dir)) {
  unlink(legacy_out_dir, recursive = TRUE, force = TRUE)
}

cat("Output directory:", normalizePath(out_dir, winslash = "\\", mustWork = FALSE), "\n")
cat(
  "Country plots:",
  length(list.files(country_out_dir, pattern = "\\.png$", recursive = TRUE)),
  "\n"
)
