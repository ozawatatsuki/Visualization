# Compare country-level hunger-risk populations in 2020.

Sys.setlocale("LC_CTYPE", ".UTF-8")

required_packages <- c("gamstransfer", "dplyr", "tibble")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(missing_packages) > 0) {
  stop(
    "Missing required packages: ", paste(missing_packages, collapse = ", "),
    call. = FALSE
  )
}

library(dplyr)

scenario_code <- "SSP2_BaU_NoCC_No"
target_year <- 2020L

conventional_path <- file.path("data", "HungerTool", "country", "AIMHub.gdx")
mapping_path <- file.path("data", "HungerTool", "country", "FAOISO.map")
study_path <- file.path(
  "data", "FDM", "詳細修正後", "Fooddem_PriceEffect.gdx"
)

for (path in c(conventional_path, mapping_path, study_path)) {
  if (!file.exists(path)) {
    stop("Input file not found: ", path, call. = FALSE)
  }
}

fao_iso <- read.delim(
  mapping_path,
  header = FALSE,
  sep = "\t",
  quote = "\"",
  col.names = c("iso3", "separator", "fao_id"),
  stringsAsFactors = FALSE,
  strip.white = TRUE
) |>
  transmute(
    iso3 = if_else(as.character(iso3) == "F351", "CHN", as.character(iso3)),
    fao_id = as.character(fao_id)
  )

conventional_gdx <- gamstransfer::Container$new()
conventional_gdx$read(conventional_path, symbols = "Risk")
conventional <- conventional_gdx["Risk"]$records |>
  tibble::as_tibble() |>
  transmute(
    method = as.character(.data[["U"]]),
    scenario_code = as.character(.data[["S"]]),
    fao_id = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["T"]])),
    conventional_method = .data[["value"]]
  ) |>
  filter(
    method == "fao_based",
    scenario_code == .env$scenario_code,
    year == .env$target_year
  ) |>
  left_join(fao_iso, by = "fao_id")

if (anyNA(conventional$iso3)) {
  stop("One or more conventional records could not be mapped to ISO3.", call. = FALSE)
}

study_gdx <- gamstransfer::Container$new()
study_gdx$read(study_path, symbols = "PoU_In_Pr_CR")
this_study <- study_gdx["PoU_In_Pr_CR"]$records |>
  tibble::as_tibble() |>
  transmute(
    scenario_code = as.character(.data[["Ref"]]),
    iso3 = as.character(.data[["R"]]),
    year = as.integer(as.character(.data[["Y"]])),
    this_study = .data[["value"]] / 1e6
  ) |>
  filter(
    scenario_code == .env$scenario_code,
    year == .env$target_year
  )

comparison <- full_join(
  select(conventional, iso3, conventional_method),
  select(this_study, iso3, this_study),
  by = "iso3"
) |>
  mutate(
    difference = conventional_method - this_study,
    absolute_difference = abs(difference)
  )

shared <- comparison |>
  filter(!is.na(conventional_method), !is.na(this_study))
conventional_only <- comparison |>
  filter(!is.na(conventional_method), is.na(this_study)) |>
  arrange(iso3)
study_only <- comparison |>
  filter(is.na(conventional_method), !is.na(this_study)) |>
  arrange(iso3)

cat("\nCoverage and totals (million people)\n")
print(
  as.data.frame(tibble::tibble(
    conventional_countries = nrow(conventional),
    this_study_countries = nrow(this_study),
    shared_countries = nrow(shared),
    conventional_total = sum(conventional$conventional_method),
    this_study_total = sum(this_study$this_study),
    shared_conventional_total = sum(shared$conventional_method),
    shared_this_study_total = sum(shared$this_study)
  )),
  row.names = FALSE
)

cat("\nLargest absolute differences (million people)\n")
print(
  shared |>
    arrange(desc(absolute_difference)) |>
    select(
      iso3,
      conventional_method,
      this_study,
      difference,
      absolute_difference
    ) |>
    slice_head(n = 25),
  n = 25
)

cat("\nCountries available only in the conventional method\n")
print(select(conventional_only, iso3, conventional_method), n = Inf)

cat("\nCountries available only in this study\n")
print(select(study_only, iso3, this_study), n = Inf)
