cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0L) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
  script_dir <- file.path(project_dir, "prog")
}

current_candidates <- Sys.glob(file.path(project_dir, "data", "260807_FDM*"))
current_candidates <- current_candidates[file.exists(file.path(current_candidates, "FDM2PHI.gdx"))]
if (length(current_candidates) != 1L) {
  stop("Expected one data/260807_FDM* directory containing FDM2PHI.gdx; found: ", length(current_candidates))
}
current_data_dir <- normalizePath(current_candidates[[1L]], winslash = "/", mustWork = TRUE)
benchmark_data_dir <- normalizePath(
  file.path(current_data_dir, "AIMPHI"),
  winslash = "/",
  mustWork = TRUE
)
required_benchmarks <- file.path(
  benchmark_data_dir,
  c("DecileResults_country_10.gdx", "DecileResults_region_10.gdx")
)
if (any(!file.exists(required_benchmarks))) {
  stop("The current AIMPHI expenditure GDX files are required for this comparison.")
}

out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "amount_comparison_using_current_260807_fdm"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

Sys.setenv(
  AMOUNT_DECILE_DATA_DIR = benchmark_data_dir,
  AMOUNT_FDM_DATA_DIR = current_data_dir,
  AMOUNT_FDM_FILE = "FDM2PHI.gdx",
  AMOUNT_OUTPUT_DIR = normalizePath(out_dir, winslash = "/", mustWork = TRUE)
)

scripts <- c(
  "260624_amount_comparison_260623.R",
  "260624_amount_comparison_AUS_260623.R",
  "260625_overwrite_amount_comparison_stacked_fdm.R"
)
for (script in scripts) {
  script_path <- file.path(script_dir, script)
  if (!file.exists(script_path)) {
    stop("Missing amount-comparison script: ", script_path)
  }
  cat("Running ", script, "...\n", sep = "")
  sys.source(script_path, envir = new.env(parent = globalenv()))
}

# The original 260623 output directory contained this alternate filename for
# the same stacked story-country composition. Keep it for one-to-one handoff.
story_source <- file.path(out_dir, "amount_comparison_story_countries_2050_SSP2.png")
story_alias <- file.path(out_dir, "amount_comparison_story_countries_stacked_fdm_2050_SSP2.png")
if (!file.copy(story_source, story_alias, overwrite = TRUE)) {
  stop("Could not create the stacked story-country alias output.")
}

png_files <- sort(list.files(out_dir, pattern = "\\.png$", full.names = TRUE))
expected_names <- c(
  "amount_comparison_AUS_2050_SSP2.png",
  "amount_comparison_developed3_representative_2050_SSP2.png",
  "amount_comparison_developing3_representative_2050_SSP2.png",
  "amount_comparison_story_countries_2050_SSP2.png",
  "amount_comparison_story_countries_stacked_fdm_2050_SSP2.png",
  "amount_comparison_top5_countries_by_diff_2050_SSP2.png",
  "amount_comparison_top5_negative_diff_2050_SSP2.png",
  "amount_comparison_top5_positive_diff_2050_SSP2.png",
  "amount_comparison_WLD_2050_SSP2.png"
)
missing_outputs <- expected_names[!file.exists(file.path(out_dir, expected_names))]
if (length(missing_outputs) > 0L) {
  stop("Missing expected amount-comparison outputs: ", paste(missing_outputs, collapse = ", "))
}
if (any(file.info(file.path(out_dir, expected_names))$size <= 0L)) {
  stop("At least one amount-comparison PNG is empty.")
}

source_manifest <- data.frame(
  role = c(
    "Current FDM monetary food demand",
    "Current AIM-PHI country expenditure",
    "Current AIM-PHI region expenditure"
  ),
  file_within_260807_data = c(
    "FDM2PHI.gdx",
    "AIMPHI/DecileResults_country_10.gdx",
    "AIMPHI/DecileResults_region_10.gdx"
  ),
  stringsAsFactors = FALSE
)
write.csv(
  source_manifest,
  file.path(out_dir, "amount_comparison_source_manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
output_manifest <- data.frame(
  file = expected_names,
  full_path = normalizePath(file.path(out_dir, expected_names), winslash = "/", mustWork = TRUE),
  stringsAsFactors = FALSE
)
write.csv(
  output_manifest,
  file.path(out_dir, "amount_comparison_output_manifest.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

cat("Created amount-comparison PNGs:", length(expected_names), "\n")
cat("Output:", normalizePath(out_dir, winslash = "/", mustWork = TRUE), "\n")
