suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
})

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
if (length(file_arg) > 0) {
  script_dir <- dirname(normalizePath(sub("^--file=", "", file_arg[1]), winslash = "/", mustWork = TRUE))
  project_dir <- normalizePath(file.path(script_dir, ".."), winslash = "/", mustWork = TRUE)
} else {
  project_dir <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

gdx_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "Inputdata.gdx"))
if (length(gdx_candidates) != 1) {
  stop("Expected exactly one Inputdata.gdx under data/260727_FDM*, found: ", length(gdx_candidates))
}
gdx_path <- gdx_candidates[[1]]

map_candidates <- Sys.glob(file.path(project_dir, "data", "260727_FDM*", "region_iso_R17PHI.map"))
if (length(map_candidates) != 1) {
  stop("Expected exactly one region_iso_R17PHI.map under data/260727_FDM*, found: ", length(map_candidates))
}
region_map_path <- map_candidates[[1]]

out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
classified_file <- file.path(out_dir, "Incomecap_2020_5year_comparison_classified.csv")
greater_file <- file.path(out_dir, "Incomecap_2020_5year_BaU_gt_400C.csv")
less_file <- file.path(out_dir, "Incomecap_2020_5year_BaU_lt_400C.csv")
equal_file <- file.path(out_dir, "Incomecap_2020_5year_BaU_eq_400C.csv")
legacy_files <- file.path(
  out_dir,
  c(
    "Incomecap_2020plus_comparison_classified.csv",
    "Incomecap_2020plus_BaU_gt_400C.csv",
    "Incomecap_2020plus_BaU_lt_400C.csv",
    "Incomecap_2020plus_BaU_eq_400C.csv"
  )
)

baseline_ref <- "SSP2_BaU_NoCC_No"
scenario_ref <- "SSP2_400C_2025CP_NoCC_No"
first_year <- 2020L
year_interval <- 5L

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
  R = c(
    "ATG", "BMU", "CUW", "CYM", "DMA", "FSM", "GRD", "GRL",
    "KIR", "KNA", "SDN", "SXM", "SYC", "TCA", "VIR"
  ),
  R17 = c(
    "R17XLM", "R17XLM", "R17XLM", "R17XLM", "R17XLM", "R17XSA",
    "R17XLM", "R17XER", "R17XSA", "R17XLM", "R17XAF", "R17XLM",
    "R17XAF", "R17XLM", "R17XLM"
  ),
  R17_mapping_source = "supplemented"
)

cat("Reading Incomecap...\n")
income <- as.data.table(rgdx.param(gdx_path, "Incomecap"))
required_cols <- c("Ref", "R", "Y", "IncomeCap")
missing_cols <- setdiff(required_cols, names(income))
if (length(missing_cols) > 0) {
  stop("Missing Incomecap columns: ", paste(missing_cols, collapse = ", "))
}

income <- income[
  Ref %in% c(baseline_ref, scenario_ref) &
    as.integer(as.character(Y)) >= first_year &
    (as.integer(as.character(Y)) - first_year) %% year_interval == 0,
  .(
    Ref = as.character(Ref),
    R = as.character(R),
    Y = as.character(Y),
    year = as.integer(as.character(Y)),
    value = as.numeric(IncomeCap)
  )
]

duplicate_keys <- income[
  ,
  .N,
  by = .(Ref, R, Y)
][N > 1]
if (nrow(duplicate_keys) > 0) {
  stop("Duplicate rows found for Ref/R/Y keys.")
}

comparison <- dcast(
  income,
  R + Y + year ~ Ref,
  value.var = "value"
)
setnames(
  comparison,
  c(baseline_ref, scenario_ref),
  c("value_BaU", "value_400C")
)

missing_pairs <- comparison[is.na(value_BaU) | is.na(value_400C)]
if (nrow(missing_pairs) > 0) {
  warning("Unmatched scenario pairs excluded: ", nrow(missing_pairs))
}

comparison <- comparison[
  !is.na(value_BaU) & !is.na(value_400C),
  `:=`(
    difference_BaU_minus_400C = value_BaU - value_400C,
    difference_percent_of_400C = fifelse(
      value_400C == 0,
      NA_real_,
      100 * (value_BaU - value_400C) / value_400C
    )
  )
]
comparison[, classification := fcase(
  difference_BaU_minus_400C > 0, "BaU > 400C",
  difference_BaU_minus_400C < 0, "BaU < 400C",
  default = "BaU = 400C"
)]
setcolorder(
  comparison,
  c(
    "classification",
    "R",
    "Y",
    "year",
    "value_BaU",
    "value_400C",
    "difference_BaU_minus_400C",
    "difference_percent_of_400C"
  )
)
setorder(comparison, classification, R, year)

region_map <- rbindlist(
  list(read_region_map(region_map_path), supplemented_region_map),
  use.names = TRUE
)
region_map <- unique(region_map, by = "R")
less_result <- merge(
  comparison[classification == "BaU < 400C"],
  region_map,
  by = "R",
  all.x = TRUE,
  sort = FALSE
)
if (anyNA(less_result$R17)) {
  stop(
    "Missing R17 mapping for BaU < 400C countries: ",
    paste(sort(unique(less_result[is.na(R17), R])), collapse = ", ")
  )
}
setcolorder(
  less_result,
  c(
    "classification",
    "R",
    "R17",
    "R17_mapping_source",
    "Y",
    "year",
    "value_BaU",
    "value_400C",
    "difference_BaU_minus_400C",
    "difference_percent_of_400C"
  )
)
setorder(less_result, R17, R, year)

existing_legacy_files <- legacy_files[file.exists(legacy_files)]
if (length(existing_legacy_files) > 0) {
  invisible(file.remove(existing_legacy_files))
}

fwrite(comparison, classified_file)
fwrite(comparison[classification == "BaU > 400C"], greater_file)
fwrite(less_result, less_file)
fwrite(comparison[classification == "BaU = 400C"], equal_file)

cat("Matched comparison rows:", nrow(comparison), "\n")
print(comparison[, .N, by = classification][order(classification)])
cat("Classified output:", normalizePath(classified_file, winslash = "\\", mustWork = FALSE), "\n")
cat("BaU > 400C output:", normalizePath(greater_file, winslash = "\\", mustWork = FALSE), "\n")
cat("BaU < 400C output:", normalizePath(less_file, winslash = "\\", mustWork = FALSE), "\n")
cat("BaU = 400C output:", normalizePath(equal_file, winslash = "\\", mustWork = FALSE), "\n")
