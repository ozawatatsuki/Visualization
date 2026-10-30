suppressPackageStartupMessages({
  library(gdxrrw)
  library(dplyr)
  library(ggplot2)
  library(viridis)
  library(scales)
})

gdx_file <- "data/260420_AIDADS/AIDADSCalibCrossCountry_SSP.gdx"
output_dir <- "output/260420"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(gdx_file)) {
  stop(sprintf("GDX file was not found: %s", gdx_file))
}

rep_countries <- tibble::tibble(
  dev_class = factor(
    c("先進国", "中進国", "途上国"),
    levels = c("先進国", "中進国", "途上国")
  ),
  R = c("DEU", "CHN", "IND")
) %>%
  mutate(country_label = paste0(as.character(dev_class), " (", R, ")"))

ex_raw <- as_tibble(rgdx.param(gdx_file, "Expshare"))

i_levels <- ex_raw %>%
  filter(as.character(.i1) == "EST") %>%
  distinct(I) %>%
  pull(I) %>%
  as.character()

ex <- ex_raw %>%
  transmute(
    kind = as.character(.i1),
    SSP = as.character(SSP),
    Y = as.character(Y),
    R = as.character(R),
    I = as.character(I),
    EXPshare = as.numeric(Expshare)
  ) %>%
  filter(kind == "EST") %>%
  select(-kind) %>%
  mutate(I = factor(I, levels = i_levels))

regions <- rep_countries$R
years_by_region <- ex %>%
  filter(R %in% regions) %>%
  distinct(R, Y) %>%
  mutate(Y_int = suppressWarnings(as.integer(Y))) %>%
  filter(!is.na(Y_int)) %>%
  group_by(R) %>%
  summarise(years = list(sort(unique(Y_int))), .groups = "drop")

missing_regions <- setdiff(regions, years_by_region$R)
if (length(missing_regions) > 0) {
  stop(sprintf(
    "Missing EXPshare data for region(s): %s",
    paste(missing_regions, collapse = ", ")
  ))
}

common_years <- Reduce(intersect, years_by_region$years)

if (length(common_years) > 0) {
  selected_year <- max(common_years)
  year_tbl <- tibble::tibble(R = regions, selected_year = selected_year)
  subtitle_text <- paste0(
    "Representative countries: ",
    paste(rep_countries$country_label, collapse = ", "),
    " | Selected common year: ",
    selected_year
  )
} else {
  year_tbl <- years_by_region %>%
    mutate(selected_year = vapply(years, max, integer(1))) %>%
    select(R, selected_year)

  subtitle_text <- paste0(
    "Representative countries: ",
    paste(rep_countries$country_label, collapse = ", "),
    " | No common year (picked per-country): ",
    paste0(year_tbl$R, "=", year_tbl$selected_year, collapse = ", ")
  )
}

plot_df <- ex %>%
  inner_join(rep_countries, by = "R") %>%
  inner_join(year_tbl, by = "R") %>%
  filter(Y == as.character(selected_year)) %>%
  filter(SSP %in% c("SSP1", "SSP2", "SSP3")) %>%
  mutate(SSP = factor(SSP, levels = c("SSP1", "SSP2", "SSP3")))

ssp_levels <- levels(plot_df$SSP)
country_levels <- rep_countries %>%
  arrange(dev_class) %>%
  pull(country_label)

x_levels <- character(0)
for (cl in country_levels) {
  for (ssp in ssp_levels) {
    x_levels <- c(x_levels, paste(cl, ssp, sep = "\n"))
  }
}

plot_df <- plot_df %>%
  mutate(
    country_label = factor(country_label, levels = country_levels),
    x = factor(paste(as.character(country_label), as.character(SSP), sep = "\n"), levels = x_levels)
  )

highlight_items <- c("Food and nonalcoholic beverages", "Energy")
item_levels <- levels(plot_df$I)
item_levels <- c(highlight_items[highlight_items %in% item_levels], setdiff(item_levels, highlight_items))
plot_df <- plot_df %>% mutate(I = factor(I, levels = item_levels))

other_items <- setdiff(item_levels, highlight_items)
other_palette <- viridis::viridis(length(other_items), option = "viridis", begin = 0.15, end = 0.9)
fill_values <- c(
  "Food and nonalcoholic beverages" = "#E41A1C",
  "Energy" = "#FF7F00",
  setNames(other_palette, other_items)
)

label_df <- plot_df %>%
  filter(as.character(I) %in% highlight_items) %>%
  mutate(label = percent(EXPshare, accuracy = 0.1))

p <- ggplot(plot_df, aes(x = x, y = EXPshare, fill = I)) +
  geom_vline(xintercept = c(3.5, 6.5), color = "grey85", linewidth = 0.35) +
  geom_col(
    width = 0.85,
    color = "grey20",
    linewidth = 0.15,
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
  scale_fill_manual(values = fill_values, drop = FALSE) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = percent_format(accuracy = 1)
  ) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(
    title = "EXPshare by SSP (AIDADSCalibCrossCountry_SSP.gdx, EST)",
    subtitle = paste0(subtitle_text, " | Grouped by country (SSP1→SSP3)"),
    x = "SSP",
    y = "EXPshare",
    fill = "Item"
  ) +
  theme_bw(base_size = 11) +
  theme(
    axis.text.x = element_text(size = 9, lineheight = 0.95),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    legend.position = "bottom",
    legend.key.height = unit(3.2, "mm"),
    legend.key.width = unit(6, "mm")
  )

out_png <- file.path(output_dir, "05_EXPshare_SSP_3countries.png")
ggsave(out_png, p, width = 16, height = 10, dpi = 300)

cat("Saved:", out_png, "\n")
