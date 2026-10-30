suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(scales)
})

gdx_ssp <- "data/260420_AIDADS/AIDADSCalibCrossCountry_SSP.gdx"
gdx_base <- "data/260420_AIDADS/AIDADSCalibCrossCountry.gdx"
output_dir <- "output/260420"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_ssp)) {
  stop(sprintf("GDX file was not found: %s", gdx_ssp))
}
if (!file.exists(gdx_base)) {
  stop(sprintf("GDX file was not found: %s", gdx_base))
}

rep_countries <- tibble::tibble(
  dev_class = factor(
    c("先進国", "中くらい", "途上国"),
    levels = c("先進国", "中くらい", "途上国")
  ),
  R = c("DEU", "CHN", "IND")
) %>%
  mutate(country_label = paste0(as.character(dev_class), " (", R, ")"))

read_poutput_share_est <- function(path, dataset_label, add_ssp = FALSE) {
  raw <- as_tibble(rgdx.param(path, "Poutput"))
  if (!"Poutput" %in% names(raw)) {
    stop(sprintf("Poutput in %s is expected to have a column named 'Poutput'", path))
  }
  if (add_ssp && !"SSP" %in% names(raw)) {
    stop(sprintf("Poutput in %s is expected to include SSP", path))
  }

  i_levels <- if ("I" %in% names(raw) && is.factor(raw$I)) levels(raw$I) else NULL

  df <- raw %>%
    transmute(
      VC = as.character(VC),
      Indicator = as.character(Indicator),
      SSP = if (add_ssp) as.character(SSP) else "SSP2",
      Stat = as.character(Stat),
      Seg = as.character(Seg),
      Y = as.character(Y),
      R = as.character(R),
      I = as.character(I),
      EXPshare = as.numeric(Poutput)
    ) %>%
    filter(VC == "share", Indicator == "EST", Stat == "-", Seg == "TOTAL") %>%
    select(-VC, -Indicator, -Stat, -Seg) %>%
    mutate(dataset = dataset_label)

  if (!is.null(i_levels)) {
    df <- df %>% mutate(I = factor(I, levels = i_levels))
  }

  df
}

df_ssp <- read_poutput_share_est(
  gdx_ssp,
  dataset_label = "AIDADSCalibCrossCountry_SSP.gdx",
  add_ssp = TRUE
)
df_base <- read_poutput_share_est(
  gdx_base,
  dataset_label = "AIDADSCalibCrossCountry.gdx",
  add_ssp = FALSE
)

common_years <- function(df, regions) {
  years_by_region <- df %>%
    filter(R %in% regions) %>%
    distinct(R, Y) %>%
    mutate(Y = suppressWarnings(as.integer(Y))) %>%
    filter(!is.na(Y)) %>%
    group_by(R) %>%
    summarise(years = list(sort(unique(Y))), .groups = "drop")

  missing_regions <- setdiff(regions, years_by_region$R)
  if (length(missing_regions) > 0) {
    stop(sprintf(
      "Missing EXPshare data for region(s): %s",
      paste(missing_regions, collapse = ", ")
    ))
  }

  Reduce(intersect, years_by_region$years)
}

regions <- rep_countries$R
years_ssp <- common_years(df_ssp, regions)
years_base <- common_years(df_base, regions)
years_all <- intersect(years_ssp, years_base)

if (length(years_all) > 0) {
  selected_year <- max(years_all)
  year_pick <- tibble::tibble(
    dataset = c("AIDADSCalibCrossCountry_SSP.gdx", "AIDADSCalibCrossCountry.gdx"),
    selected_year = selected_year
  )
} else {
  year_pick <- tibble::tibble(
    dataset = c("AIDADSCalibCrossCountry_SSP.gdx", "AIDADSCalibCrossCountry.gdx"),
    selected_year = c(
      if (length(years_ssp) > 0) max(years_ssp) else NA_integer_,
      if (length(years_base) > 0) max(years_base) else NA_integer_
    )
  )
}

plot_df <- bind_rows(df_ssp, df_base) %>%
  inner_join(year_pick, by = "dataset") %>%
  filter(Y == as.character(selected_year)) %>%
  inner_join(rep_countries, by = "R") %>%
  mutate(
    SSP = factor(SSP, levels = c("SSP1", "SSP2", "SSP3")),
    dataset = factor(dataset, levels = c("AIDADSCalibCrossCountry_SSP.gdx", "AIDADSCalibCrossCountry.gdx"))
  )

ssp_levels <- c("SSP1", "SSP2", "SSP3")
country_label_levels <- rep_countries$country_label
x_levels <- as.vector(outer(ssp_levels, country_label_levels, paste, sep = "\n"))

plot_df <- plot_df %>%
  mutate(
    x = factor(paste0(as.character(SSP), "\n", country_label), levels = x_levels),
    I = if (is.factor(I)) I else factor(I)
  )

panel_year_text <- year_pick %>%
  mutate(panel = paste0(dataset, " (year=", selected_year, ")")) %>%
  select(dataset, panel)

plot_df <- plot_df %>% left_join(panel_year_text, by = "dataset")

p <- ggplot(plot_df, aes(x = x, y = EXPshare, fill = I)) +
  geom_col(width = 0.85, color = "grey20", linewidth = 0.15) +
  facet_wrap(~panel, ncol = 1) +
  scale_fill_viridis_d(option = "turbo", end = 0.95, begin = 0.05, drop = FALSE) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = percent_format(accuracy = 1)
  ) +
  labs(
    title = "EXPshare by SSP (stacked by Item)",
    subtitle = paste0("Representative countries: ", paste(rep_countries$country_label, collapse = ", ")),
    caption = "Note: AIDADSCalibCrossCountry.gdx has no SSP dimension; values match SSP2 in the _SSP file.",
    x = "SSP",
    y = "EXPshare",
    fill = "Item"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 9, lineheight = 0.95),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    strip.background = element_rect(fill = "grey95", color = "grey80"),
    legend.position = "bottom",
    legend.key.height = unit(3.2, "mm"),
    legend.key.width = unit(6, "mm")
  )

out_png <- file.path(output_dir, "04_EXPshare_by_SSP.png")
ggsave(out_png, p, width = 16, height = 12, dpi = 300)

cat("Saved:", out_png, "\n")
