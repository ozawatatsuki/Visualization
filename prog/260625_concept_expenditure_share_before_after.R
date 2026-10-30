suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(scales)
})

output_dir <- "output/260625_concept"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

share_df <- tibble::tribble(
  ~state, ~item, ~share,
  "価格反映前", "穀物・主食", 0.24,
  "価格反映前", "肉・乳製品", 0.18,
  "価格反映前", "野菜・果物", 0.16,
  "価格反映前", "エネルギー", 0.12,
  "価格反映前", "その他消費", 0.30,
  "価格反映後", "穀物・主食", 0.30,
  "価格反映後", "肉・乳製品", 0.14,
  "価格反映後", "野菜・果物", 0.15,
  "価格反映後", "エネルギー", 0.16,
  "価格反映後", "その他消費", 0.25
)

state_levels <- c("価格反映前", "価格反映後")
item_levels <- c("その他消費", "エネルギー", "野菜・果物", "肉・乳製品", "穀物・主食")

plot_df <- share_df %>%
  mutate(
    state = factor(.data$state, levels = state_levels),
    item = factor(.data$item, levels = item_levels)
  ) %>%
  mutate(label = percent(.data$share, accuracy = 1))

label_df <- plot_df %>%
  filter(.data$share >= 0.08)

fill_values <- c(
  "穀物・主食" = "#D55E00",
  "肉・乳製品" = "#CC79A7",
  "野菜・果物" = "#009E73",
  "エネルギー" = "#E69F00",
  "その他消費" = "#56B4E9"
)

font_family <- if (.Platform$OS.type == "windows") "Yu Gothic" else ""

p <- ggplot(plot_df, aes(x = .data$state, y = .data$share, fill = .data$item)) +
  geom_col(width = 0.58, color = "white", linewidth = 0.8) +
  geom_text(
    data = label_df,
    aes(label = .data$label),
    position = position_stack(vjust = 0.5),
    color = "white",
    fontface = "bold",
    size = 4.4,
    show.legend = FALSE,
    family = font_family
  ) +
  annotate(
    "curve",
    x = 1.34,
    xend = 1.66,
    y = 1.06,
    yend = 1.06,
    curvature = -0.18,
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    linewidth = 0.7,
    color = "grey35"
  ) +
  annotate(
    "text",
    x = 1.5,
    y = 1.12,
    label = "価格ベクトルを反映",
    size = 4.2,
    color = "grey25",
    family = font_family
  ) +
  scale_fill_manual(values = fill_values, breaks = rev(item_levels), drop = FALSE) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    breaks = seq(0, 1, by = 0.25),
    expand = expansion(mult = c(0, 0.16))
  ) +
  coord_cartesian(ylim = c(0, 1.16), clip = "off") +
  labs(
    title = "一つの家計における消費シェアの変化（概念図）",
    subtitle = "ExpenditureModel：価格反映前後で支出配分がどう見えるかを示すダミー例",
    x = NULL,
    y = "消費シェア",
    fill = "消費項目",
    caption = "仮定値：各棒は合計100%。価格反映後は主食・エネルギーのシェアが上がり、その他項目が相対的に圧縮される例。"
  ) +
  theme_minimal(base_size = 14, base_family = font_family) +
  theme(
    plot.title = element_text(face = "bold", size = 18, margin = margin(b = 6)),
    plot.subtitle = element_text(size = 12.5, color = "grey30", margin = margin(b = 18)),
    plot.caption = element_text(size = 10.5, color = "grey35", hjust = 0),
    axis.text.x = element_text(face = "bold", size = 14, color = "grey15"),
    axis.text.y = element_text(color = "grey25"),
    axis.title.y = element_text(face = "bold", margin = margin(r = 10)),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "right",
    legend.title = element_text(face = "bold"),
    legend.key.height = unit(0.7, "cm"),
    plot.margin = margin(16, 22, 16, 16)
  )

out_png <- file.path(output_dir, "expenditure_share_before_after_concept.png")
ggsave(out_png, p, width = 9.6, height = 6.2, dpi = 300, bg = "white")

cat("Saved:", out_png, "\n")
