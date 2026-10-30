suppressPackageStartupMessages({
  library(data.table)
  library(gdxrrw)
  library(ggplot2)
  library(scales)
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
data_candidates <- data_candidates[
  file.exists(file.path(data_candidates, "Fooddem_PriceEffect.gdx"))
]
if (length(data_candidates) != 1L) {
  stop("Expected exactly one Fooddem_PriceEffect.gdx; found ", length(data_candidates), ".")
}

source_gdx <- file.path(data_candidates[[1]], "Fooddem_PriceEffect.gdx")
out_dir <- file.path(
  project_dir,
  "output",
  "260807_FDM_scenario_quantile",
  "AFG_Pchange_nonfood_by_I"
)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

excluded_item <- "Food and nonalcoholic beverages"
ref_order <- c(
  "SSP2_BaU_NoCC_No",
  "SSP2_400C_2025CP_NoCC_No",
  "SSP2_400C_2030CP_NoCC_No"
)
ref_labels <- c(
  "SSP2_BaU_NoCC_No" = "BaU",
  "SSP2_400C_2025CP_NoCC_No" = "400C / 2025CP",
  "SSP2_400C_2030CP_NoCC_No" = "400C / 2030CP"
)

pchange_raw <- as.data.table(rgdx.param(source_gdx, "Pchange"))
required <- c("Ref", "I", "R", "Y", "Pchange")
if (!all(required %in% names(pchange_raw))) {
  stop("Unexpected Pchange columns: ", paste(names(pchange_raw), collapse = ", "))
}

item_order <- unique(as.character(pchange_raw[["I"]]))
item_order <- item_order[item_order != excluded_item]

pchange <- pchange_raw[
  as.character(R) == "AFG" &
    as.character(I) != excluded_item &
    as.character(Ref) %in% ref_order,
  .(
    Ref = as.character(Ref),
    I = as.character(I),
    Year = as.integer(as.character(Y)),
    Pchange = as.numeric(Pchange)
  )
]

if (nrow(pchange) == 0L) {
  stop("No AFG Pchange records were found after filtering.")
}
if (anyDuplicated(pchange, by = c("Ref", "I", "Year")) > 0L) {
  stop("Duplicate Ref/I/Year records were found.")
}
if (!setequal(unique(pchange$Ref), ref_order)) {
  stop("The expected BaU, 2025CP, and 2030CP references were not all found.")
}

pchange[, `:=`(
  Ref_label = factor(ref_labels[Ref], levels = unname(ref_labels[ref_order])),
  I = factor(I, levels = item_order)
)]
setorder(pchange, I, Ref_label, Year)

scenario_colors <- c(
  "BaU" = "#666666",
  "400C / 2025CP" = "#0072B2",
  "400C / 2030CP" = "#D55E00"
)
scenario_linetypes <- c(
  "BaU" = "dotted",
  "400C / 2025CP" = "solid",
  "400C / 2030CP" = "longdash"
)
scenario_shapes <- c(
  "BaU" = 16,
  "400C / 2025CP" = 17,
  "400C / 2030CP" = 15
)

p <- ggplot(
  pchange,
  aes(
    x = Year,
    y = Pchange,
    colour = Ref_label,
    linetype = Ref_label,
    shape = Ref_label,
    group = Ref_label
  )
) +
  geom_hline(yintercept = 1, colour = "grey65", linewidth = 0.4) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2, stroke = 0.4) +
  facet_wrap(
    vars(I),
    ncol = 4,
    scales = "fixed",
    labeller = labeller(I = label_wrap_gen(width = 28))
  ) +
  scale_colour_manual(values = scenario_colors, drop = FALSE) +
  scale_linetype_manual(values = scenario_linetypes, drop = FALSE) +
  scale_shape_manual(values = scenario_shapes, drop = FALSE) +
  scale_x_continuous(
    breaks = c(2020, 2040, 2060, 2080, 2100),
    minor_breaks = seq(2020, 2100, by = 10),
    expand = expansion(mult = c(0.025, 0.025))
  ) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 5),
    labels = label_number(accuracy = 0.1),
    expand = expansion(mult = c(0.04, 0.06))
  ) +
  guides(
    colour = guide_legend(nrow = 1, byrow = TRUE),
    linetype = guide_legend(nrow = 1, byrow = TRUE),
    shape = guide_legend(nrow = 1, byrow = TRUE)
  ) +
  labs(
    title = "AFG price-change index by expenditure item",
    subtitle = paste0("Pchange in Fooddem_PriceEffect.gdx; excluding ", excluded_item),
    x = "Year",
    y = "Pchange (index; BaU = 1)",
    colour = NULL,
    linetype = NULL,
    shape = NULL,
    caption = "Source: Fooddem_PriceEffect.gdx"
  ) +
  theme_minimal(base_size = 13, base_family = "Arial") +
  theme(
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(colour = "grey86", linewidth = 0.35),
    axis.text = element_text(colour = "grey22", size = 10.5),
    axis.title = element_text(colour = "grey10", face = "bold", size = 13),
    legend.position = "bottom",
    legend.text = element_text(size = 11.5),
    legend.key.width = grid::unit(1.15, "cm"),
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    plot.subtitle = element_text(colour = "grey30", size = 11, hjust = 0.5),
    plot.caption = element_text(colour = "grey40", size = 9, hjust = 1),
    strip.text = element_text(face = "bold", colour = "grey10", size = 11.5),
    strip.background = element_rect(fill = "grey95", colour = "grey78", linewidth = 0.3),
    panel.spacing = grid::unit(0.42, "cm"),
    plot.margin = margin(10, 12, 8, 10)
  )

png_file <- file.path(out_dir, "AFG_Pchange_nonfood_by_I_4col_word_ready.png")
csv_file <- file.path(out_dir, "AFG_Pchange_nonfood_by_I_data.csv")
manifest_file <- file.path(out_dir, "AFG_Pchange_nonfood_by_I_manifest.csv")

ggsave(
  png_file,
  p,
  width = 10.5,
  height = 8.2,
  units = "in",
  dpi = 300,
  bg = "white"
)
fwrite(
  pchange[, .(
    Ref,
    Scenario = as.character(Ref_label),
    I = as.character(I),
    R = "AFG",
    Year,
    Pchange
  )],
  csv_file
)
fwrite(
  data.table(
    source = normalizePath(source_gdx, winslash = "/", mustWork = TRUE),
    region = "AFG",
    excluded_I = excluded_item,
    facet_count = uniqueN(pchange$I),
    scenarios = paste(unname(ref_labels[ref_order]), collapse = " | "),
    years = paste(range(pchange$Year), collapse = "-"),
    png = basename(png_file),
    data_csv = basename(csv_file)
  ),
  manifest_file
)

cat("Created AFG Pchange PNG:\n")
cat(normalizePath(png_file, winslash = "/", mustWork = TRUE), "\n")
cat("Facets:", uniqueN(pchange$I), " Rows:", nrow(pchange), "\n")
