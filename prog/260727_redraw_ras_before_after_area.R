suppressPackageStartupMessages({
  library(data.table)
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
out_dir <- file.path(project_dir, "output", "260727_FDM_hierarchy_update", "ras_before_after_comparison")
csv_path <- file.path(out_dir, "ras_before_after_2050_r17_wld.csv")

r17_regions <- c(
  "WLD",
  "R17XSA",
  "R17XER",
  "R17XNF",
  "R17XAF",
  "R17XLM",
  "R17CIS",
  "R17XOC",
  "R17XE25",
  "R17XME",
  "R17BRA",
  "R17XSE",
  "R17CAN",
  "R17CHN",
  "R17IND",
  "R17JPN",
  "R17TUR",
  "R17USA"
)
var_order <- c(
  "Cereals",
  "Dairy",
  "Fish",
  "Food_Other",
  "Fruits_and_Veg",
  "Meats",
  "Oils_and_Fats",
  "total"
)
compare_colors <- c("Before RAS" = "#009E73", "After RAS" = "#CC79A7")
refs <- c("SSP2_BaU_NoCC_No", "SSP2_400C_2025CP_NoCC_No")
plot_year <- 2050L

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      plot.title = element_text(face = "bold", size = base_size + 4),
      plot.subtitle = element_text(color = "grey35", margin = margin(b = 8)),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    )
}

ras_compare <- fread(csv_path)
ras_compare[, R := factor(R, levels = r17_regions)]
ras_compare[, var := factor(var, levels = var_order)]
ras_compare[, method := factor(method, levels = names(compare_colors))]
ras_compare[, BudgetInitial := as.numeric(BudgetInitial)]
ras_compare[, consumption := as.numeric(consumption)]

png_files <- list.files(out_dir, pattern = "\\.png$", recursive = TRUE, full.names = TRUE)
if (length(png_files) > 0) {
  unlink(png_files, force = TRUE)
}

for (ref_id in refs) {
  ref_dir <- file.path(out_dir, ref_id)
  dir.create(ref_dir, recursive = TRUE, showWarnings = FALSE)
  for (region in r17_regions) {
    compare_region <- ras_compare[Ref == ref_id & R == region]
    compare_plot <- ggplot(compare_region, aes(x = BudgetInitial, y = consumption, fill = method)) +
      geom_area(position = "identity", alpha = 0.42, color = NA) +
      facet_wrap(~ var, ncol = 2, scales = "free_y") +
      scale_fill_manual(values = compare_colors, drop = FALSE) +
      scale_x_log10(labels = label_number(big.mark = ",")) +
      scale_y_continuous(labels = label_number(big.mark = ",")) +
      labs(
        title = paste0(region, " RAS before/after comparison in ", plot_year),
        subtitle = paste0("Ref = ", ref_id, "; country consumption weighted by Population_seg"),
        x = "BudgetInitial (log scale)",
        y = "Food consumption (kcal/day/capita)",
        fill = NULL
      ) +
      theme_report(base_size = 11)

    ggsave(
      file.path(ref_dir, paste0("ras_before_after_2050_", ref_id, "_", region, ".png")),
      compare_plot,
      width = 12,
      height = 14,
      dpi = 300,
      bg = "white"
    )
  }
}

cat("RAS before/after area plots:", length(list.files(out_dir, pattern = "\\.png$", recursive = TRUE)), "\n")
