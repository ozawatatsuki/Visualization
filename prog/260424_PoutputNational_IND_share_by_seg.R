suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(scales)
})

gdx_base <- "data/260420_AIDADS/demand/base/IND.gdx"
gdx_ssp <- "data/260420_AIDADS/demand/ssp/IND.gdx"
output_dir <- "output/260420"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_base)) stop(sprintf("GDX file was not found: %s", gdx_base))
if (!file.exists(gdx_ssp)) stop(sprintf("GDX file was not found: %s", gdx_ssp))

read_poutput_national <- function(path, dataset_label) {
  raw <- as_tibble(rgdx.param(path, "PoutputNational"))
  if (!"PoutputNational" %in% names(raw)) {
    stop(sprintf("PoutputNational in %s is expected to have a value column", path))
  }
  raw %>%
    transmute(
      dataset = dataset_label,
      Ref = as.character(Ref),
      Seg = as.character(Seg),
      Y = as.character(Y),
      I = as.character(I),
      value = as.numeric(PoutputNational)
    )
}

read_budget_initial <- function(path) {
  raw <- as_tibble(rgdx.param(path, "BudgetInitial"))
  if (!"BudgetInitial" %in% names(raw)) {
    stop(sprintf("BudgetInitial in %s is expected to have a value column", path))
  }
  raw %>%
    transmute(
      Seg = as.character(Seg),
      BudgetInitial = as.numeric(BudgetInitial)
    )
}

p_base <- read_poutput_national(gdx_base, "base")
p_ssp <- read_poutput_national(gdx_ssp, "ssp")
budget_base <- read_budget_initial(gdx_base)
budget_ssp <- read_budget_initial(gdx_ssp)

budget_cmp <- inner_join(budget_base, budget_ssp, by = "Seg", suffix = c("_base", "_ssp"))
if (nrow(budget_cmp) == 0) stop("BudgetInitial has no overlapping segments between base and ssp files.")
max_budget_diff <- max(abs(budget_cmp$BudgetInitial_base - budget_cmp$BudgetInitial_ssp), na.rm = TRUE)
if (max_budget_diff > 1e-8) {
  warning(sprintf(
    "BudgetInitial differs between files (max abs diff = %.6g). Using base ordering for alignment.",
    max_budget_diff
  ))
}

ref_pattern <- "BaU_NoCC_No"
target_year <- "2050"

plot_raw <- bind_rows(p_base, p_ssp) %>%
  filter(grepl(ref_pattern, Ref, fixed = TRUE), Y == target_year)

if (nrow(plot_raw) == 0) {
  stop(sprintf("No rows after filtering Ref contains '%s' and Y == %s.", ref_pattern, target_year))
}

ref_levels <- plot_raw %>%
  filter(dataset == "base") %>%
  distinct(Ref) %>%
  pull(Ref) %>%
  as.character()
if (length(ref_levels) == 0) {
  ref_levels <- plot_raw %>% distinct(Ref) %>% pull(Ref) %>% as.character()
}
ref_levels <- sort(unique(ref_levels))
preferred_ref_levels <- c("SSP1_BaU_NoCC_No", "SSP2_BaU_NoCC_No", "SSP3_BaU_NoCC_No")
if (all(preferred_ref_levels %in% ref_levels)) {
  ref_levels <- preferred_ref_levels
}

item_levels <- plot_raw %>%
  filter(dataset == "base") %>%
  distinct(I) %>%
  pull(I) %>%
  as.character()
if (length(item_levels) == 0) {
  item_levels <- plot_raw %>% distinct(I) %>% pull(I) %>% as.character()
}
item_levels <- unique(item_levels)

seg_in_data <- plot_raw %>%
  distinct(Seg) %>%
  pull(Seg) %>%
  as.character()

seg_levels <- budget_base %>%
  filter(Seg %in% seg_in_data) %>%
  arrange(BudgetInitial, Seg) %>%
  pull(Seg) %>%
  as.character()

missing_budget <- setdiff(seg_in_data, seg_levels)
if (length(missing_budget) > 0) {
  stop(sprintf(
    "Some Seg in PoutputNational are missing in BudgetInitial (first few): %s",
    paste(head(missing_budget, 10), collapse = ", ")
  ))
}

plot_df <- plot_raw %>%
  mutate(
    dataset = factor(dataset, levels = c("base", "ssp"), labels = c("base/IND.gdx", "ssp/IND.gdx")),
    Ref = factor(Ref, levels = ref_levels),
    I = factor(I, levels = item_levels),
    Seg = factor(Seg, levels = seg_levels)
  ) %>%
  group_by(dataset, Ref, Seg) %>%
  mutate(
    total = sum(value, na.rm = TRUE),
    share = if_else(total > 0, value / total * 100, NA_real_)
  ) %>%
  ungroup()

n_seg <- length(seg_levels)
break_idx <- unique(pmin(n_seg, round(seq(1, n_seg, length.out = 10))))
seg_breaks <- seg_levels[break_idx]

ref_caption <- paste(ref_levels, collapse = ", ")

p <- ggplot(plot_df, aes(x = Seg, y = share, fill = I)) +
  geom_col(width = 0.9, color = "grey20", linewidth = 0.12) +
  facet_grid(Ref ~ dataset) +
  scale_fill_viridis_d(option = "turbo", begin = 0.05, end = 0.95, drop = FALSE) +
  scale_x_discrete(breaks = seg_breaks, drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(
    title = "PoutputNational composition by segment (share %): IND",
    subtitle = paste0(
      "Filter: Ref contains '", ref_pattern, "', Year=", target_year,
      " | Seg ordered by BudgetInitial | Ref=", ref_caption
    ),
    x = "Seg (ordered by BudgetInitial)",
    y = "Share (%)",
    fill = "Item (I)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "bottom",
    legend.box = "vertical"
  )

out_png <- file.path(output_dir, "07_PoutputNational_share_by_seg_IND_2050_facetgrid.png")
ggsave(out_png, p, width = 22, height = 14, dpi = 300)

cat("Saved:", out_png, "\n")
