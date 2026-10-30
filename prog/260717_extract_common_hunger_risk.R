suppressPackageStartupMessages({
  library(countrycode)
  library(dplyr)
  library(gdxrrw)
  library(readr)
  library(tibble)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

aimhub_path <- file.path(
  project_dir,
  "data",
  "260716_環境システム用比較",
  "HungerTool",
  "Country",
  "AIMHub.gdx"
)
fdm_path <- file.path(
  project_dir,
  "data",
  "260716_環境システム用比較",
  "FDM",
  "Fooddem_PriceEffect_1000.gdx"
)
out_dir <- file.path(project_dir, "output", "260716_env_system_comparison")
out_csv <- file.path(out_dir, "common_country_year_scenario_hunger_risk_population.csv")
out_diff_csv <- file.path(out_dir, "wld_scenario_difference_hunger_risk_population.csv")

missing_files <- c(aimhub_path, fdm_path)[!file.exists(c(aimhub_path, fdm_path))]
if (length(missing_files) > 0) {
  stop("Missing input file(s):\n", paste(missing_files, collapse = "\n"))
}
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

aimhub_risk <- as_tibble(rgdx.param(aimhub_path, "Risk")) |>
  mutate(
    U = as.character(U),
    scenario = as.character(S),
    fao_area_code = suppressWarnings(as.integer(as.character(R))),
    country = countrycode(
      fao_area_code,
      origin = "fao",
      destination = "iso3c",
      warn = FALSE
    ),
    year = suppressWarnings(as.integer(as.character(T))),
    hunger_risk_population = Risk * 1000000,
    series = "AIMHub_Risk_fao_based"
  ) |>
  filter(
    U == "fao_based",
    !is.na(country),
    !is.na(year),
    is.finite(hunger_risk_population)
  ) |>
  select(country, year, scenario, series, hunger_risk_population)

fdm_pou <- as_tibble(rgdx.param(fdm_path, "PoU_In_Pr")) |>
  transmute(
    country = as.character(R),
    year = suppressWarnings(as.integer(as.character(Y))),
    scenario = as.character(Ref),
    series = "FDM_PoU_In_Pr",
    hunger_risk_population = PoU_In_Pr
  ) |>
  filter(
    !is.na(country),
    !is.na(year),
    is.finite(hunger_risk_population)
  )

common_keys <- inner_join(
  aimhub_risk |> distinct(country, year, scenario),
  fdm_pou |> distinct(country, year, scenario),
  by = c("country", "year", "scenario")
)

aimhub_common <- semi_join(
  aimhub_risk,
  common_keys,
  by = c("country", "year", "scenario")
)
fdm_common <- semi_join(
  fdm_pou,
  common_keys,
  by = c("country", "year", "scenario")
)

common_long <- bind_rows(aimhub_common, fdm_common)
wld_rows <- common_long |>
  group_by(year, scenario, series) |>
  summarise(
    hunger_risk_population = sum(hunger_risk_population, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(country = "WLD") |>
  select(country, year, scenario, series, hunger_risk_population)

diff_scenario <- "SSP2_400C_2025CP_NoCC_No_minus_SSP2_BaU_NoCC_No"
wld_diff_rows <- inner_join(
  wld_rows |>
    filter(scenario == "SSP2_400C_2025CP_NoCC_No") |>
    transmute(year, series, value_400c = hunger_risk_population),
  wld_rows |>
    filter(scenario == "SSP2_BaU_NoCC_No") |>
    transmute(year, series, value_bau = hunger_risk_population),
  by = c("year", "series")
) |>
  transmute(
    country = "WLD",
    year,
    scenario = diff_scenario,
    series,
    hunger_risk_population = value_400c - value_bau
  )

out_df <- bind_rows(common_long, wld_rows, wld_diff_rows) |>
  mutate(hunger_risk_population = round(hunger_risk_population, 6)) |>
  arrange(scenario, year, country == "WLD", country, series)

write_csv(out_df, out_csv)
write_csv(
  wld_diff_rows |> mutate(hunger_risk_population = round(hunger_risk_population, 6)),
  out_diff_csv
)

unmapped_codes <- as_tibble(rgdx.param(aimhub_path, "Risk")) |>
  mutate(
    U = as.character(U),
    fao_area_code = suppressWarnings(as.integer(as.character(R))),
    country = countrycode(
      fao_area_code,
      origin = "fao",
      destination = "iso3c",
      warn = FALSE
    )
  ) |>
  filter(U == "fao_based", is.na(country)) |>
  distinct(fao_area_code) |>
  arrange(fao_area_code) |>
  pull(fao_area_code)

cat("Output CSV:", normalizePath(out_csv, winslash = "\\", mustWork = FALSE), "\n")
cat("WLD difference CSV:", normalizePath(out_diff_csv, winslash = "\\", mustWork = FALSE), "\n")
cat("Common country-year-scenario combinations:", nrow(common_keys), "\n")
cat("Common countries:", n_distinct(common_keys$country), "\n")
cat("Common years:", paste(sort(unique(common_keys$year)), collapse = ", "), "\n")
cat("Common scenarios:", paste(sort(unique(common_keys$scenario)), collapse = ", "), "\n")
cat("Rows excluding WLD:", nrow(common_long), "\n")
cat("WLD scenario-difference rows:", nrow(wld_diff_rows), "\n")
cat("Rows including WLD and WLD differences:", nrow(out_df), "\n")
cat("AIMHub Risk unit conversion: million people -> people\n")
if (length(unmapped_codes) > 0) {
  cat("Unmapped AIMHub FAOSTAT codes:", paste(unmapped_codes, collapse = ", "), "\n")
}
