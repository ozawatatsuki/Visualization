library(tidyverse)
library(dplyr)
library(gdxrrw)
library(readxl)
library(cowplot)
library(RColorBrewer)
library(segmented)
library(ioanalysis)
library(httr)
library(readr)
library(WDI)
library(tidyr)
library(ggplot2)
library(countrycode)
library(stringr)
library(ggpmisc)
library(scales)
library(conflicted)
library(patchwork)
library(ggpattern)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
theme1 <- theme(
    panel.border = element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA),
    axis.line = element_line(colour="black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill="white", colour="white"))

fileFDM <- paste0("data/FDM_kyoeiken.gdx")
fileFAO <- paste0("data/HungerTool.gdx")

#fig1-----------------------------------------------------------

##data
FD <- rgdx.param(fileFDM,"FoodDem_base_add") |> 
  filter(eff!="Base")

##fig

## Fig1: Changes in food consumption (2050)
## Source: FD
outdir_kyoeiken <- file.path("output", "kyoeiken")
dir.create(outdir_kyoeiken, recursive = TRUE, showWarnings = FALSE)

FD_fig1 <- FD
if ("FoodDem_base_add" %in% names(FD_fig1)) {
  FD_fig1 <- FD_fig1 |> rename(Value = FoodDem_base_add)
} else if (!("Value" %in% names(FD_fig1))) {
  stop("FD must contain `FoodDem_base_add` (or `Value`) column.")
}

FD_fig1 <- FD_fig1 |>
  filter(eff %in% c("Income", "Price")) |>
  mutate(
    eff = factor(eff, levels = c("Income", "Price")),
    seg_chr = as.character(seg),
    is_all = str_to_upper(seg_chr) == "ALL",
    seg_num = suppressWarnings(readr::parse_number(seg_chr) / 10)
  )

seg_max_fig1 <- max(FD_fig1$seg_num[!FD_fig1$is_all], na.rm = TRUE)
seg_all_x_fig1 <- seg_max_fig1 + max(1, 0.05 * seg_max_fig1)
all_bar_width_fig1 <- max(1, 0.03 * seg_max_fig1)

FD_fig1 <- FD_fig1 |>
  mutate(seg_x = ifelse(is_all | is.na(seg_num), seg_all_x_fig1, seg_num))

fill_eff_fig1 <- c("Income" = "grey80", "Price" = "grey40")

p_fig1 <- ggplot() +
  geom_area(
    data = FD_fig1 |> filter(!is_all, !is.na(seg_num)) |> arrange(seg_x),
    mapping = aes(x = seg_x, y = Value, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.15
  ) +
  geom_col(
    data = FD_fig1 |> filter(is_all | is.na(seg_num)),
    mapping = aes(x = seg_x, y = Value, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    width = all_bar_width_fig1
  ) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ var, ncol = 2) +
  scale_fill_manual(values = fill_eff_fig1) +
  scale_x_continuous(
    name = "seg",
    breaks = c(pretty(c(0, seg_max_fig1)), seg_all_x_fig1),
    labels = function(x) ifelse(abs(x - seg_all_x_fig1) < 1e-9, "\nAve", x)
  ) +
  labs(
    y = "Changes in food consumption(kcal/day/capita)",
    fill = NULL
  ) +
  theme_bw(base_size = 6) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 6, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(0.35, "cm"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, margin = margin(t = 2)),
    axis.title.y = element_text(size = 6, margin = margin(r = 2)),
    panel.spacing = grid::unit(0.25, "lines"),
    plot.margin = margin(5, 5, 5, 5, "pt")
  )

ggsave(
  filename = file.path(outdir_kyoeiken, "fig1_food_consumption_change_2050_bw.png"),
  plot = p_fig1,
  width = 9,
  height = 12,
  units = "cm",
  dpi = 300
)

## Fig1 (percent): relative change vs Base
FD_fig1_base <- rgdx.param(fileFDM, "FoodDem_base_add")
if ("FoodDem_base_add" %in% names(FD_fig1_base)) {
  FD_fig1_base <- FD_fig1_base |> rename(BaseValue = FoodDem_base_add)
} else if ("Value" %in% names(FD_fig1_base)) {
  FD_fig1_base <- FD_fig1_base |> rename(BaseValue = Value)
} else {
  stop("FD base must contain `FoodDem_base_add` (or `Value`) column.")
}

FD_fig1_base <- FD_fig1_base |>
  filter(eff == "Base") |>
  mutate(seg_chr = as.character(seg)) |>
  select(var, seg_chr, BaseValue)

FD_fig1_pct <- FD_fig1 |>
  left_join(FD_fig1_base, by = c("var", "seg_chr")) |>
  mutate(
    Value_pct = ifelse(is.na(BaseValue) | BaseValue == 0, NA_real_, 100 * Value / BaseValue)
  )

p_fig1_pct <- ggplot() +
  geom_area(
    data = FD_fig1_pct |> filter(!is_all, !is.na(seg_num)) |> arrange(seg_x),
    mapping = aes(x = seg_x, y = Value_pct, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    linewidth = 0.15
  ) +
  geom_col(
    data = FD_fig1_pct |> filter(is_all | is.na(seg_num)),
    mapping = aes(x = seg_x, y = Value_pct, fill = eff),
    position = position_stack(reverse = TRUE),
    colour = "black",
    width = all_bar_width_fig1
  ) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  facet_wrap(~ var, scales = "free_y", ncol = 2) +
  scale_fill_manual(values = fill_eff_fig1) +
  scale_x_continuous(
    name = "seg",
    breaks = c(pretty(c(0, seg_max_fig1)), seg_all_x_fig1),
    labels = function(x) ifelse(abs(x - seg_all_x_fig1) < 1e-9, "\nAve", x)
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%")) +
  labs(
    y = "Changes in food consumption (% of Baseline)",
    fill = NULL
  ) +
  theme_bw(base_size = 6) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 6, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
    legend.key.size = grid::unit(0.35, "cm"),
    axis.text.x = element_text(size = 6),
    axis.text.y = element_text(size = 6),
    axis.title.x = element_text(size = 6, margin = margin(t = 2)),
    axis.title.y = element_text(size = 6, margin = margin(r = 2)),
    panel.spacing = grid::unit(0.25, "lines"),
    plot.margin = margin(5, 5, 5, 5, "pt")
  ) +
  theme1 

ggsave(
  filename = file.path(outdir_kyoeiken, "fig1_food_consumption_change_2050_vsBase_pct_bw.png"),
  plot = p_fig1_pct,
  width = 9,
  height = 12,
  units = "cm",
  dpi = 300
)


#fig2-----------------------------------------------------------

##data
PoU_FDM <-  rgdx.param(fileFDM,"PoU_base_add") |> 
  mutate(PoU_base_add=PoU_base_add/1000000) 



##fig



## Fig2: Population at risk of hunger (2020-2100)
## Source: PoU_FDM
PoU_fig2 <- PoU_FDM
if ("PoU_base_add" %in% names(PoU_fig2)) {
  PoU_fig2 <- PoU_fig2 |> rename(Value = PoU_base_add)
} else if (!("Value" %in% names(PoU_fig2))) {
  stop("PoU_FDM must contain `PoU_base_add` (or `Value`) column.")
}

PoU_years_fig2 <- 2020:2100

PoU_fig2 <- PoU_fig2 |>
  mutate(
    Y = suppressWarnings(as.integer(as.character(Y))),
    eff = as.character(eff)
  ) |>
  filter(!is.na(Y), Y %in% PoU_years_fig2) |>
  filter(eff %in% c("Base", "Income", "Price")) |>
  mutate(Y = factor(Y, levels = PoU_years_fig2))

PoU_fig2_base <- PoU_fig2 |>
  filter(eff == "Base") |>
  mutate(panel = factor("Baseline", levels = c("Baseline", "400C")))

PoU_fig2_400c_abs <- PoU_fig2 |>
  filter(eff %in% c("Base", "Income", "Price")) |>
  mutate(
    panel = factor("400C", levels = c("Baseline", "400C")),
    component = case_when(
      eff == "Base" ~ "Baseline",
      TRUE ~ eff
    ),
    component = factor(component, levels = c("Baseline", "Income", "Price"))
  )

fill_eff_fig2 <- c("Baseline" = "grey70", "Income" = "white", "Price" = "grey20")
PoU_x_breaks_fig2 <- as.character(seq(2020, 2100, by = 10))

p_fig2 <- ggplot() +
  geom_col(
    data = PoU_fig2_base,
    mapping = aes(x = factor(Y), y = Value),
    fill = "grey70",
    colour = "black",
    width = 1,
    show.legend = FALSE
  ) +
  geom_col(
    data = PoU_fig2_400c_abs,
    mapping = aes(x = factor(Y), y = Value, fill = component),
    position = position_stack(reverse = TRUE),
    colour = "black",
    width = 1
  ) +
  facet_wrap(~ panel, nrow = 1) +
  scale_x_discrete(drop = FALSE, breaks = PoU_x_breaks_fig2, expand = expansion(mult = c(0, 0))) +
  scale_fill_manual(values = fill_eff_fig2, breaks = names(fill_eff_fig2), drop = FALSE) +
  labs(
    x = "Y",
    y = "Population at risk of hunger(million)",
    fill = NULL
  ) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw(base_size = 6) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "white", colour = "black"),
    strip.text = element_text(size = 6, face = "bold"),
    legend.position = "bottom",
    legend.text = element_text(size = 6),
     legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
  ) +
  theme1 

ggsave(
  filename = file.path(outdir_kyoeiken, "fig2_hunger_risk_population_change_2050_bw.png"),
  plot = p_fig2,
  width = 9,
  height = 7,
  units = "cm",
  dpi = 300
)

#fig3-----------------------------------------------------------

##data
fileFDM <- paste0("data/FDM_kyoeiken.gdx")
fileFAO <- paste0("data/HungerTool.gdx")

df_FDM <- rgdx.param(fileFDM,"PoU_value") |> 
  filter(eff!="Income") |> 
  mutate(U="FDM",
         PoU_value=PoU_value/1000000,
         Ref = if_else(eff == "Base", "Baseline", "400C")) |> 
  select(-eff) |> 
  rename(value=PoU_value)
df_FAO <- rgdx.param("data/HungerTool.gdx","Risk_agg") |> 
  filter(U == "fao_based",str_detect(S, "SSP2"),R_agg=="WLD") |> 
  rename(Ref=S,R=R_agg,Y=T) |> 
  mutate(Ref = if_else(Ref == "SSP2_BaU_NoCC_No", "Baseline", "400C"))|> 
  rename(value=Risk_agg) |> 
  select(-R) |> 
  filter(Y %in% df_FDM$Y)
df_comp <- rbind(df_FDM,df_FAO)

PoUchange_FDM_raw <-  rgdx.param(fileFDM,"PoU_base_add") |> 
  mutate(PoU_base_add=PoU_base_add/1000000,
        U="FDM") |> 
  filter(eff %in% c("Income", "Price"))
PoUchange_FDM <- PoUchange_FDM_raw |>
  group_by(Y, U) |>
  summarise(change = sum(PoU_base_add, na.rm = TRUE), eff = "all", .groups = "drop")
PoUchange_FAO <- df_FAO |> 
  pivot_wider(
    names_from = Ref,
    values_from = value
  ) |> 
  mutate(
    change = `400C` - Baseline,
    eff="all"
  ) |> 
  select(-`400C`,-Baseline)
df_comp2 <- rbind(PoUchange_FDM,PoUchange_FAO)


##fig

## Fig3: Comparison with HungerTool
## Source: df_comp, df_comp2

df_comp_fig3 <- df_comp |>
  mutate(
    Y = suppressWarnings(as.integer(as.character(Y))),
    Ref = factor(Ref, levels = c("Baseline", "400C")),
    U = forcats::fct_relevel(as.factor(U), c("fao_based", "FDM"))
  ) |>
  filter(!is.na(Y))

u_levels_fig3 <- levels(df_comp_fig3$U)
linetype_seq_fig3 <- c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
linetype_map_fig3 <- setNames(
  rep(linetype_seq_fig3, length.out = length(u_levels_fig3)),
  u_levels_fig3
)

p_fig3a <- ggplot(df_comp_fig3, aes(x = Y, y = value, linetype = U)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  geom_line(colour = "black", linewidth = 0.4) +
  facet_wrap(~ Ref, nrow = 1) +
  scale_x_continuous(
    breaks = seq(2020, 2100, by = 10),
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  scale_linetype_manual(values = linetype_map_fig3, drop = FALSE) +
  labs(
    x = "Year",
    y = "Population at risk of hunger (million people)",
    linetype = "U"
  ) +
  theme_bw(base_size = 6) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.text = element_text(size = 6, face = "bold"),
    legend.position = "bottom",
    legend.title = element_text(size = 7),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  theme1

df_comp2_fig3 <- df_comp2 |>
  mutate(
    Y = suppressWarnings(as.integer(as.character(Y))),
    U = forcats::fct_relevel(as.factor(U), c("fao_based", "FDM"))
  ) |>
  filter(!is.na(Y)) |>
  mutate(Y = factor(Y, levels = sort(unique(Y))))

u_levels_fig3_b <- levels(df_comp2_fig3$U)
fill_map_fig3 <- setNames(
  scales::grey_pal(start = 0.8, end = 0.2)(length(u_levels_fig3_b)),
  u_levels_fig3_b
)

p_fig3b <- ggplot(df_comp2_fig3, aes(x = Y, y = change, fill = U)) +
  geom_hline(yintercept = 0, colour = "black", linetype = "dashed", linewidth = 0.3) +
  geom_col(
    colour = "black",
    width = 0.85,
    position = position_dodge(width = 0.9)
  ) +
  scale_fill_manual(values = fill_map_fig3, drop = FALSE) +
  labs(
    x = "Year",
    y = "Change from BaU (million people)",
    fill = "U"
  ) +
  theme_bw(base_size = 6) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    legend.title = element_text(size = 6),
    legend.text = element_text(size = 6),
    legend.key.size = unit(0.4, "cm"),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
  ) +
  theme1

p_fig3 <- p_fig3a / p_fig3b +
  plot_layout(heights = c(1.1, 1)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = "bold", size = 8))

ggsave(
  filename = file.path(outdir_kyoeiken, "fig3_hungertool_comparison_bw.png"),
  plot = p_fig3,
  width = 9,
  height = 11,
  units = "cm",
  dpi = 300
)
