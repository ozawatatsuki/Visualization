suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(scales)
  library(patchwork)
  library(ggrepel)
})

gdx_base <- "data/260420_AIDADS/demand/base/DEU.gdx"
gdx_ssp <- "data/260420_AIDADS/demand/ssp/DEU.gdx"
output_dir <- "output/260420"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_base)) stop(sprintf("GDX file was not found: %s", gdx_base))
if (!file.exists(gdx_ssp)) stop(sprintf("GDX file was not found: %s", gdx_ssp))

ref_target <- "SSP1_BaU_NoCC_No"
year_target <- "2050"

highlight_items <- c("Food and nonalcoholic beverages", "Energy")

read_poutput_national <- function(path, dataset_label) {
  raw <- as_tibble(rgdx.param(path, "PoutputNational"))
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
  raw %>%
    transmute(
      Seg = as.character(Seg),
      BudgetInitial = as.numeric(BudgetInitial)
    )
}

budget <- read_budget_initial(gdx_base)
p_base <- read_poutput_national(gdx_base, "base")
p_ssp <- read_poutput_national(gdx_ssp, "ssp")

base_f <- p_base %>%
  filter(Ref == ref_target, Y == year_target) %>%
  select(dataset, Seg, I, value)
ssp_f <- p_ssp %>%
  filter(Ref == ref_target, Y == year_target) %>%
  select(dataset, Seg, I, value)

if (nrow(base_f) == 0) stop("No base rows after filtering Ref/year.")
if (nrow(ssp_f) == 0) stop("No ssp rows after filtering Ref/year.")

common_segs <- intersect(unique(base_f$Seg), unique(ssp_f$Seg))
segs_only_in_base <- setdiff(unique(base_f$Seg), common_segs)

base_f <- base_f %>% filter(Seg %in% common_segs)
ssp_f <- ssp_f %>% filter(Seg %in% common_segs)

share_df <- bind_rows(base_f, ssp_f) %>%
  group_by(dataset, Seg) %>%
  mutate(total = sum(value, na.rm = TRUE), share = value / total * 100) %>%
  ungroup() %>%
  select(dataset, Seg, I, share) %>%
  left_join(budget, by = "Seg")

item_levels <- share_df %>%
  filter(dataset == "base") %>%
  distinct(I) %>%
  pull(I) %>%
  as.character()
if (length(item_levels) == 0) item_levels <- unique(share_df$I)
item_levels <- c(highlight_items[highlight_items %in% item_levels], setdiff(item_levels, highlight_items))

share_df <- share_df %>%
  mutate(
    dataset = factor(dataset, levels = c("base", "ssp"), labels = c("base", "ssp")),
    I = factor(I, levels = item_levels)
  )

base_share <- share_df %>%
  filter(dataset == "base") %>%
  transmute(Seg, I, BudgetInitial, share_base = share)
ssp_share <- share_df %>%
  filter(dataset == "ssp") %>%
  transmute(Seg, I, BudgetInitial, share_ssp = share)

joined <- inner_join(base_share, ssp_share, by = c("Seg", "I", "BudgetInitial")) %>%
  mutate(diff_pp = share_ssp - share_base, absdiff_pp = abs(diff_pp))

tv_df <- joined %>%
  group_by(Seg, BudgetInitial) %>%
  summarise(tv_pp = 0.5 * sum(abs(diff_pp)), .groups = "drop") %>%
  arrange(BudgetInitial, Seg)

top_n <- 12
top_segs <- tv_df %>%
  arrange(desc(tv_pp), BudgetInitial) %>%
  slice_head(n = top_n)

seg_levels_budget <- tv_df %>% arrange(BudgetInitial, Seg) %>% pull(Seg) %>% as.character()

share_df <- share_df %>%
  mutate(Seg = factor(Seg, levels = seg_levels_budget))
joined <- joined %>%
  mutate(Seg = factor(Seg, levels = seg_levels_budget))
tv_df <- tv_df %>%
  mutate(Seg = factor(Seg, levels = seg_levels_budget))

top_labels <- top_segs %>%
  mutate(
    SegLabel = paste0(
      Seg,
      " (B=", BudgetInitial,
      ", TV=", sprintf("%.1f", tv_pp), "pp)"
    )
  ) %>%
  arrange(desc(tv_pp), BudgetInitial)

top_label_levels <- top_labels$SegLabel
top_seg_map <- top_labels %>% select(Seg, SegLabel)

share_top <- share_df %>%
  filter(as.character(Seg) %in% as.character(top_segs$Seg)) %>%
  left_join(top_seg_map, by = "Seg") %>%
  mutate(SegLabel = factor(SegLabel, levels = top_label_levels))

other_items <- setdiff(item_levels, highlight_items)
other_palette <- viridis::viridis(length(other_items), option = "viridis", begin = 0.15, end = 0.9)
fill_values <- c(
  "Food and nonalcoholic beverages" = "#E41A1C",
  "Energy" = "#FF7F00",
  setNames(other_palette, other_items)
)

label_df <- share_top %>%
  filter(as.character(I) %in% highlight_items) %>%
  mutate(label = paste0(round(share, 1), "%"))

p_overview <- ggplot(tv_df, aes(x = BudgetInitial, y = tv_pp)) +
  geom_line(color = "grey40", linewidth = 0.6) +
  geom_point(color = "grey40", size = 1.2) +
  geom_point(
    data = top_segs,
    aes(x = BudgetInitial, y = tv_pp),
    color = "#d62728",
    size = 2.0
  ) +
  ggrepel::geom_text_repel(
    data = top_segs,
    aes(label = Seg),
    size = 3,
    min.segment.length = 0,
    box.padding = 0.2,
    point.padding = 0.1,
    max.overlaps = Inf
  ) +
  scale_y_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    title = "Where do base vs ssp shares differ most? (SSP1, 2050)",
    subtitle = "Segment-level total variation distance (TV = 0.5 * sum |Δshare|). Higher = larger composition change.",
    x = "BudgetInitial (segment order)",
    y = "TV distance (pp)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2)
  )

p_top <- ggplot(share_top, aes(x = dataset, y = share, fill = I)) +
  geom_col(
    width = 0.75,
    color = "grey20",
    linewidth = 0.12,
    position = position_stack(reverse = TRUE)
  ) +
  geom_text(
    data = label_df,
    aes(label = label),
    position = position_stack(vjust = 0.5, reverse = TRUE),
    size = 2.6,
    color = "white",
    show.legend = FALSE
  ) +
  facet_wrap(~SegLabel, ncol = 4) +
  scale_fill_manual(values = fill_values, drop = FALSE) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    labels = function(x) paste0(x, "%")
  ) +
  labs(
    title = paste0("Top ", top_n, " segments by TV distance (composition, %)"),
    subtitle = "Two bars per segment: base vs ssp. Food (red) and Energy (orange) are labeled.",
    x = NULL,
    y = "Share (%)",
    fill = "Item (I)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 9),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom"
  )

caption_text <- paste0(
  "Ref=", ref_target, ", Year=", year_target,
  " | Common segments=", length(common_segs),
  if (length(segs_only_in_base) > 0) paste0(" (", length(segs_only_in_base), " segments exist only in base and are excluded)") else ""
)

final <- p_overview / p_top +
  plot_layout(heights = c(1, 2)) +
  plot_annotation(caption = caption_text)

out_png <- file.path(output_dir, "08_PoutputNational_SSP1_base_vs_ssp_diff.png")
ggsave(out_png, final, width = 22, height = 14, dpi = 300)

cat("Saved:", out_png, "\n")
