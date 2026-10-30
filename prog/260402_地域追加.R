# package-------------------------------------------------------------------------
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

THEME2 <- theme_bw() +
  theme(
    axis.text.x = element_text(size = 25, angle = 45, vjust = 0.5, hjust = 0.5, 
                               margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y = element_text(size = 25, margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    panel.border = element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA),
    axis.line = element_line(colour="black"),
    panel.background = element_rect(fill = "white"),
    panel.grid.major = element_blank(),
    strip.background = element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=25, colour = "black", angle = 0, face="bold"),
    legend.text = element_text(size = 30),
    axis.ticks.length=unit(-0.15,"cm"),
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    plot.title = element_text(size=10),
    legend.title = element_text(size = 30),  
    strip.text.y = element_text(size = 20),
    axis.title.y = element_text(size = 30),
    axis.title.x = element_text(size = 30),
    
  )

#parameter

n_split <- 1000
plotY <- 2050
Ragg <- c(
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
Base <- c(
  "SSP1_BaU_NoCC_No",
  "SSP2_BaU_NoCC_No",
  "SSP3_BaU_NoCC_No"
)

CP <- c(
  "SSP1_400C_2025CP_NoCC_No",
  "SSP2_400C_2025CP_NoCC_No",
  "SSP3_400C_2025CP_NoCC_No"
)
palette <- c(
  "Cereals" = "#ffff00",
  "Meats" = "#8DD3C7",
  "Fish" = "#FCCDE5",
  "Dairy" = "#BEBADA",
  "Oils_and_Fats" = "#FB8072",
  "Fruits_and_Veg" = "#80B1D3",
  "Food_Other" = "#FDB462"
)
SSP_list <- c("SSP1", "SSP2", "SSP3")
# PoU_SSPs_シナリオ比較-------------------------------------------------------------------------
file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")

PoU_FDM <- rgdx.param(file,"PoU_In_Ragg_calc") %>% 
  rename(value = "PoU_In_Ragg_calc",
         R = R17) %>% 
  filter(Ref %in% Base) %>% 
  mutate(value = ifelse(value <= 0.01, 0, value),
         eff = "base") %>% 
  mutate(value = value/1000000) 

PoU_CP_In <- rgdx.param(file,"PoU_In_Ragg_calc") %>% 
  rename(value = "PoU_In_Ragg_calc",
         R = R17) %>% 
  filter(Ref %in% CP) %>% 
  mutate(value = ifelse(value <= 0.01, 0, value),
         eff = "Income") %>% 
  mutate(value = value/1000000) 

PoU_CP_In_Pr <- rgdx.param(file,"PoU_In_Pr_Ragg_calc") %>% 
  rename(value = "PoU_In_Pr_Ragg_calc",
         R = R17) %>% 
  filter(Ref %in% CP) %>% 
  mutate(value = ifelse(value <= 0.01, 0, value),
         eff = "Income+Price") %>% 
  mutate(value = value/1000000) 

PoU <- bind_rows(list(PoU_FDM, PoU_CP_In, PoU_CP_In_Pr)) %>%
  mutate(
    SSP = "SSP",
    SSP = case_when(
      grepl("SSP1", Ref) ~ "SSP1",
      grepl("SSP2", Ref) ~ "SSP2",
      grepl("SSP3", Ref) ~ "SSP3",
      TRUE ~ SSP
    )
  ) %>% 
  select(-Ref) %>% 
  group_by(R, Y, SSP) %>%
  mutate(
    change = value - value[eff == "base"]
  ) %>%
  ungroup()

Price_rows <- PoU %>%
  group_by(R, Y, SSP) %>%
  summarise(
    change = value[eff == "Income+Price"][1] - value[eff == "Income"][1],
    value  = value[eff == "base"][1] + change,
    eff    = "Price",
    .groups = "drop"
  )

PoU <- bind_rows(PoU, Price_rows)


plot <- ggplot() +
  geom_line(data = PoU %>% filter(eff == "base"),aes(x = Y ,y = value,color = SSP,group = SSP), linewidth = 1.5)+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3,scales="free_y")+
  labs(
    y = "Population at risk of Hunger (million people)",
    x = "Year"
  ) +
  THEME2+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/260402_地域追加/PoU_SSPs_baseline.png")
ggsave(fileout, plot = plot, width = 30, height = 18)


plot <- ggplot() +
  geom_line(data = PoU %>% filter(eff == "Income+Price"),aes(x = Y ,y = value,color = SSP,group = SSP), linewidth = 1.5)+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3,scales="free_y")+
  labs(
    y = "Population at risk of Hunger (million people)",
    x = "Year"
  ) +
  THEME2+
  theme(legend.position = "bottom")+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/260402_地域追加/PoU_SSPs_2025CP.png")
ggsave(fileout, plot = plot, width = 30, height = 18)


plot_df <- PoU %>%
  filter(eff %in% c("base", "Income", "Price","Income+Price")) %>%
  mutate(
    eff = factor(eff, levels = c("Income+Price","Income", "Price", "base")),
    plot_value = case_when(
      eff == "base"   ~ value,
      TRUE            ~ change
    )
  )

paletteeff <- c(
  "Income" = "#FF4B00",
  "Price" = "#03AF7A",
  "base" = "#005AFF"
)


for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_bar(
      data = plot_df %>% filter(SSP == ssp,eff != "Income+Price"),
      aes(x = Y, y = plot_value, fill = eff),
      stat = "identity",
      position = "stack"
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
    facet_wrap(~ R, nrow = 3) +
    scale_fill_manual(values = paletteeff) +
    labs(
      y = "Population at risk of Hunger (million people)",
      x = "Year"
    ) +
    THEME2 +
    theme(legend.position = "bottom") +
    theme(
      axis.text.x = element_text(
        size = 20, angle = 45, vjust = 0.5, hjust = 0.5,
        margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")
      )
    )
  fileout <- paste0("output/260402_地域追加/PoU_", ssp, ".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}

for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_bar(
      data = plot_df %>% filter(SSP == ssp,eff %in% c("Income","Price")),
      aes(x = Y, y = plot_value, fill = eff),
      stat = "identity",
      position = "stack"
    ) +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 1) +
    facet_wrap(~ R, nrow = 3) +
    scale_fill_manual(values = paletteeff) +
    labs(
      y = "Additional population at risk of Hunger (million people)",
      x = "Year"
    ) +
    THEME2 +
    theme(legend.position = "bottom") +
    theme(
      axis.text.x = element_text(
        size = 20, angle = 45, vjust = 0.5, hjust = 0.5,
        margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")
      )
    )
  fileout <- paste0("output/260402_地域追加/PoU_change_", ssp, ".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}

plot_df <- plot_df %>% 
  mutate(U = "FDM",
         Y = as.numeric(as.character(Y))) 
# HungerTool比較 ------------------------------------------------------------
file <- paste0("data/HungerTool.gdx")

PoU_HTool <- rgdx.param(file,"Risk_agg") %>% 
  rename(value = "Risk_agg",
         Ref = S,
         R = R_agg,
         Y = T) %>% 
  mutate(Y = as.numeric(as.character(Y))) %>% 
  filter(R == "WLD",
         Y <= 2100,
         Y >= 2020) %>%
  mutate(
    U = recode(U,"model_based" = "AIM-Hub_based"),
  ) %>%
  mutate(
    eff = case_when(
      grepl("BaU", Ref)  ~ "base",
      grepl("400C", Ref) ~ "Income+Price",
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    SSP = "SSP",
    SSP = case_when(
      grepl("SSP1", Ref) ~ "SSP1",
      grepl("SSP2", Ref) ~ "SSP2",
      grepl("SSP3", Ref) ~ "SSP3",
      TRUE ~ SSP
    )
  ) %>% 
  select(-Ref) %>% 
  group_by(U,R, Y, SSP) %>%
  mutate(
    change = value - value[eff == "base"]
  )  %>%
  mutate(
    plot_value = case_when(
      eff == "base"   ~ value,
      TRUE            ~ change
    )
  )
plot_df_WLD <- plot_df %>% 
  filter(
    R == "WLD"
  )
comp <- bind_rows(plot_df_WLD,PoU_HTool)
comp_A <- comp %>% 
  filter(!((eff == "Income"| eff == "Price")& U == "FDM")) %>% 
  mutate(Ref = "Ref",
         Ref = case_when(
           grepl("Income", eff) ~ paste0(SSP, "_2025CP_NoCC_No"),
           grepl("base", eff) ~ paste0(SSP, "_BaU_NoCC_No"),
           TRUE ~ Ref
         )
  ) 

years <- seq(2030, 2100, by = 10)
for (ssp in SSP_list) {
  
  pA <- ggplot() +
    geom_line(data = comp_A %>% 
                filter(SSP == ssp) %>% 
                mutate(
                  Ref = factor(Ref, levels = c(paste0(ssp,"_BaU_NoCC_No"),paste0(ssp,"_2025CP_NoCC_No")))
                ),
              aes(x = Y ,y = value,color = U),linewidth=1.5)+
    geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
    facet_wrap(~ Ref)+
    labs(
      y = "Population at risk of Hunger (million people)",
      x = "Year"
    ) +
    THEME2+
    scale_x_continuous(breaks = seq(min(comp$Y),
                                    max(comp$Y),
                                    by = 10))+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
  
  
  pB <- ggplot() +
    geom_bar(data = comp_A %>% 
               filter(SSP == ssp) %>% 
               filter(Y %in% years) %>% 
               mutate(
                 U = factor(U, levels = c("FDM","fao_based","AIM-Hub_based"))
               ),
             aes(x = Y, y = change, fill= U),
             position = "dodge",
             stat="identity") +
    geom_hline(yintercept = 0,
               linetype = "dashed",
               linewidth = 1) +
    labs(
      y = "Change from BaU (million people)",
      x = "Year"
    ) +
    THEME2 +
    scale_x_continuous(breaks = seq(min(comp$Y),
                                    max(comp$Y),
                                    by = 10))+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
  plot <- (pA / pB) +
    plot_annotation(tag_levels = "a")&
    theme(
      plot.tag = element_text(size = 30, face = "bold")
    )
  
  fileout <- paste0("output/260402_地域追加/",ssp,"_PoU_FDM_HTool.png")
  ggsave(fileout, plot = plot, width = 26, height = 20)
  
}


# FoodDemand,MDER,CV比較 -------------------------------------------------------
file <- paste0("data/stats_HungerTool.gdx")
CV_HT <- rgdx.param(file,"CV_out") %>% 
  rename(Ref = S,
         R = RISO,
         value = CV_out,
         Y = T) %>% 
  mutate(model = "HungerTool") %>%
  filter(grepl("SSP2_BaU", Ref),Y == 2020)  %>% 
  mutate(R = as.character(R),
         R=ifelse(R == "F351", "CHN", R))
FD_HT <- rgdx.param(file,"PCAL_out") %>% 
  rename(Ref = S,
         R = RISO,
         value = PCAL_out,
         Y = T) %>% 
  mutate(model = "HungerTool",
  ) %>%
  filter(grepl("SSP2_BaU", Ref),Y == 2020) %>% 
  mutate(R = as.character(R),
         R=ifelse(R == "F351", "CHN", R))
MDER_HT <- rgdx.param(file,"MDER_out") %>% 
  rename(Ref = S,
         R = RISO,
         value = MDER_out,
         Y = T) %>% 
  mutate(model = "HungerTool") %>%
  filter(grepl("SSP2_BaU", Ref),Y == 2020)  %>% 
  mutate(R = as.character(R),
         R=ifelse(R == "F351", "CHN", R))

file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")
FD_FDM <- rgdx.param(file,"FoodDemIn") %>% 
  filter(grepl("SSP2_BaU", Ref),Y == 2020,var=="total",seg=="ALL") %>% 
  rename(value = FoodDemIn) %>% 
  select(Ref,R,Y,value) %>% 
  mutate(model = "FDM") 
MDER_FDM <- rgdx.param(file,"MDER") %>% 
  filter(grepl("SSP2_BaU", Ref),Y == 2020) %>% 
  rename(value = MDER) %>% 
  select(Ref,R,Y,value) %>% 
  mutate(model = "FDM") 
file <- paste0("data/260402_地域追加/CV.gdx")
CV_FDM <- rgdx.param(file,"CV_regression") %>% 
  rename(Ref = i1,
         R = i2,
         Y = i3) %>% 
  filter(grepl("SSP2_BaU", Ref),Y == 2020) %>% 
  select(Ref,R,Y,value) %>% 
  mutate(model = "FDM") 
FD_FDM <- FD_FDM %>% 
  mutate(U = "FDM")

FD_all <- rbind(FD_HT,FD_FDM) %>% 
  filter(!grepl("R17", R),R!="WLD") %>% 
  rename(Demand = value)
MDER_HT1 <- MDER_HT %>% 
  mutate(U = "fao_based")
MDER_HT2 <- MDER_HT %>% 
  mutate(U = "model_based")
MDER_HT3 <- rbind(MDER_HT1,MDER_HT2)
MDER_FDM <- MDER_FDM %>% 
  mutate(U = "FDM")
MDER_all <- rbind(MDER_HT3,MDER_FDM) %>% 
  filter(!grepl("R17", R),R!="WLD") %>% 
  rename(MDER = value)
FD_MDER <- FD_all %>%
  left_join(MDER_all,by = c("Ref", "R", "Y","model","U")) %>% 
  drop_na()

CV_all <- rbind(CV_HT,CV_FDM) %>% 
  filter(!grepl("R17", R),R!="WLD") %>% 
  pivot_wider(
    names_from = model,
    values_from = value
  ) %>% 
  mutate(HungerTool = ifelse(is.na(HungerTool), 0, HungerTool),
         FDM = ifelse(is.na(FDM), 0, FDM))



file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")
PoU_FDM <- rgdx.param(file,"PoU_In") %>% 
  filter(grepl("SSP2_BaU", Ref),Y == 2020) %>% 
  rename(PoU_FDM = PoU_In) %>% 
  mutate(PoU_FDM = PoU_FDM/1000000) %>% 
  select(R,PoU_FDM) %>% 
  filter(!grepl("R17", R),R!="WLD")

file <- paste0("data/stats_HungerTool.gdx")
make_FDcomp <- function(ht_u, file_suffix = "") {
  PoU_HT <- rgdx.param(file, "PoU_out") %>%
    rename(
      Ref = S,
      PoU_HT = PoU_out,
      Y = T,
      R = RISO
    ) %>%
    filter(grepl("SSP2_BaU", Ref), Y == 2020, U == ht_u) %>%
    select(R, PoU_HT)

  FD_plot <- FD_all %>%
    filter(U %in% c("FDM", ht_u)) %>%
    select(-U, -Y) %>%
    pivot_wider(
      names_from = model,
      values_from = Demand
    ) %>%
    left_join(PoU_FDM, by = c("R")) %>%
    left_join(PoU_HT, by = c("R")) %>%
    mutate(
      HungerTool = ifelse(is.na(HungerTool), 0, HungerTool),
      FDM = ifelse(is.na(FDM), 0, FDM),
      PoU_FDM = ifelse(is.na(PoU_FDM), 0, PoU_FDM),
      PoU_HT = ifelse(is.na(PoU_HT), 0, PoU_HT),
      PoU_change = (PoU_HT - PoU_FDM),
      PoU_change = ifelse(
        PoU_FDM < 1e-3 & PoU_HT < 1e-3,
        0,
        PoU_change
      )
    )

  top20 <- FD_plot %>%
    arrange(desc(abs(PoU_change))) %>%
    slice_head(n = 20)

  plot <- ggplot(FD_plot) +
    geom_point(
      data = FD_plot,
      aes(
        x = FDM, y = HungerTool, size = abs(PoU_change), color = case_when(
          PoU_change > 0 ~ "Increase",
          PoU_change < 0 ~ "Decrease",
          TRUE ~ "No change"
        )
      ),
      alpha = 0.3
    ) +
    geom_abline(
      slope = 1,
      intercept = 0,
      linetype = "dashed",
      color = "black"
    ) +
    geom_text(
      data = top20,
      aes(
        x = FDM,
        y = HungerTool,
        label = R
      ),
      size = 10,
      vjust = -0.5
    ) +
    labs(
      y = "HungerTool",
      x = "FDM"
    ) +
    scale_size_continuous(range = c(5, 60)) +
    scale_color_manual(
      values = c(
        "Increase" = "red",
        "Decrease" = "blue",
        "No change" = "black"
      ),
      name = "PoU change"
    ) +
    THEME2 +
    theme(legend.position = "bottom") +
    theme(
      axis.text.x = element_text(
        size = 20, angle = 45, vjust = 0.5, hjust = 0.5,
        margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit = "cm")
      )
    )

  fileout <- paste0("output/260402_地域追加/FDcomp", file_suffix, ".png")
  ggsave(fileout, plot = plot, width = 24, height = 22, limitsize = FALSE)

  invisible(FD_plot)
}

# 既存: HungerToolの摂取量は model_based を利用
FD_plot <- make_FDcomp("model_based")
# 追加: HungerToolの摂取量を fao_based にした場合も同じ出力先へ
make_FDcomp("fao_based", "_fao_based")


plot <- ggplot() +
  geom_point(
    data=CV_all,
    aes(x = FDM,y = HungerTool
    ),
    size=3
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "black"
  )+
  labs(
    y = "HungerTool",
    x = "FDM"
  ) +
  THEME2 +
  theme(
    axis.text.x = element_text(
      size = 20, angle = 45, vjust = 0.5, hjust = 0.5,
      margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit="cm")
    )
  )
fileout <- paste0("output/260402_地域追加/CVcomp.png")
ggsave(fileout, plot = plot, width = 20, height = 20,limitsize = FALSE)


NA_FDM <- FD_plot %>% 
  filter(PoU_FDM == 0) %>% 
  mutate(
    country <- countrycode(R, 
                           origin = "iso3c", 
                           destination = "country.name")
  )
NA_FDM <- NA_FDM[order(-NA_FDM$PoU_HT), ]

write.csv(NA_FDM,
          "output/260402_地域追加/NA_FDMchange.csv",
          row.names = FALSE,
          fileEncoding = "UTF-8") 
# priceEla-------------------------------------------------------------------------
file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")
priceEla_in <- rgdx.param(file,"priceEla") %>% 
  filter(R %in% Ragg & Ref != "SSP2_BaU_NoCC" & seg != "ALL") %>% 
  mutate(
    seg=as.numeric(as.character(seg)),
    seg= seg*100/n_split
  )
priceEla_in <- priceEla_in %>% 
  filter(!grepl("BaU",Ref)) %>% 
  mutate(
    SSP = "SSP",
    SSP = case_when(
      grepl("SSP1", Ref) ~ "SSP1",
      grepl("SSP2", Ref) ~ "SSP2",
      grepl("SSP3", Ref) ~ "SSP3",
      TRUE ~ SSP
    )
  )
for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_line(data = priceEla_in %>% filter(Y == 2050,SSP == ssp),aes(x = seg ,y = priceEla,color = var),linewidth=2)+
    geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
    scale_color_manual(values = palette)+
    facet_wrap(~ R,nrow = 3)+
    labs(
      y = "Price Elasticity",
      x = "Income segment(%)"
    ) +
    THEME2+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
  fileout <- paste0("output/260402_地域追加/PriceEla_est_2050_",ssp,".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}

# PriceEla_alpha比較-------------------------------------------------------------------------
c_USDA <- c(
  "Cereals",
  "Fruits_and_Veg",
  "Food_Other",
  "Oils_and_Fats",
  "Bev_and_Tobacco",
  "Meats",
  "Dairy",
  "Cereals"
)

file <- paste0("data/alpha_弾力性/regression.gdx")

PEla_alpha_alpha<- rgdx.param(file,"alpha_pe") %>% 
  mutate(para = "alpha",
         model = "alpha") %>% 
  rename(value = alpha_pe)
PEla_alpha_beta<- rgdx.param(file,"beta_pe") %>% 
  mutate(para = "beta",
         model = "alpha")%>% 
  rename(value = beta_pe)

PEla_alpha <- rbind(PEla_alpha_alpha,PEla_alpha_beta) %>% 
  mutate(
    c = case_when(
      c == "rce" ~ "Cereals",
      c == "str" ~ "Fruits_and_Veg",
      c == "sgr" ~ "Food_Other",
      c == "vol" ~ "Oils_and_Fats",
      c == "stm" ~ "Bev_and_Tobacco",
      c == "pmt" ~ "Meats",
      c == "dai" ~ "Dairy",
      TRUE ~ c
    ) 
  ) %>% 
  filter(c %in% c_USDA)


file <- paste0("data/reg_priceEla.gdx")

PEla_FDM <- rgdx.param(file,"para") %>% 
  mutate(
    para = "a",
    para = case_when(
      grepl("Coe", i2) ~ "beta",
      grepl("Int", i2) ~ "alpha",
      TRUE ~ para
    ),
    model="FDM",
    i1 = case_when(
      grepl("Bev.", i1) ~ "Bev_and_Tobacco",
      TRUE ~ i1
    ),
  ) %>% 
  filter(i3 == "estimate") %>% 
  select(i1,value,para,model) %>% 
  rename(
    c=i1
  ) 

PEla_all <- rbind(PEla_alpha,PEla_FDM) %>%
  pivot_wider(
    names_from = model,
    values_from = value
  ) %>% 
  filter(c != "Fish")

df_long <- PEla_all %>%
  pivot_longer(
    cols = c(alpha, FDM),
    names_to = "model",
    values_to = "value"
  )

coef_df <- df_long %>%
  pivot_wider(
    id_cols = c(c, model),
    names_from = para,
    values_from = value
  )

gdp_grid <- data.frame(
  GDPcap = seq(100, 100000, length.out = 200)
)

plot_df <- coef_df %>%
  crossing(gdp_grid) %>%
  mutate(
    Ela=1,
    Ela = case_when(
      grepl("alpha", model) ~ alpha + beta * log(GDPcap),
      grepl("FDM", model) ~ alpha + beta * log10(GDPcap),
      TRUE ~ Ela)
  )


plot <- ggplot() +
  geom_line(data=plot_df, aes(x = GDPcap, y = Ela, color = model),linewidth=2) +
  #  scale_x_log10() +
  facet_wrap(~ c, scales = "free_y") +
  labs(
    y = "Price elasticity",
    x = "GDP per capita"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/260402_地域追加/comp_PEla_alpha_FDM2.png")
ggsave(fileout, plot = plot, width = 30, height = 18)


# price推移 -----------------------------------------------------------------
file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")
price_in <- rgdx.param(file,"pchange") %>% 
  filter(R %in% Ragg) %>% 
  filter(!grepl("BaU",Ref)) %>% 
  mutate(
    SSP = "SSP",
    SSP = case_when(
      grepl("SSP1", Ref) ~ "SSP1",
      grepl("SSP2", Ref) ~ "SSP2",
      grepl("SSP3", Ref) ~ "SSP3",
      TRUE ~ SSP
    )
  )%>%
  mutate(Y = as.numeric(as.character(Y)))

for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_line(data = price_in %>% filter(SSP == ssp),aes(x = Y ,y = pchange,color = var2,group=var2),linewidth=1.5)+
    geom_hline(yintercept = 1,color = "black", linetype = "dashed", linewidth = 1)+ 
    scale_color_manual(values = palette)+
    facet_wrap(~ R,nrow = 3)+
    labs(
      y = "Price change (baseline=1)",
      x = "Year"
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2100, by = 10)
    )+
    THEME2+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
  fileout <- paste0("output/260402_地域追加/Pricechange_",ssp,".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}

# GDPloss推移-------------------------------------------------------------------------
file <- paste0("data/260402_地域追加/Fooddem_PriceEffect_1000.gdx")
GDP <- rgdx.param(file,"GDPcapDist")%>% 
  filter(R %in% Ragg,seg=="ALL") 
GDP_in <- GDP %>% 
  rename(Y = Yall,
         GDPcap = GDPcapDist) %>% 
  select(-seg) %>% 
  mutate(
    SSP = "SSP",
    SSP = case_when(
      grepl("SSP1", Ref) ~ "SSP1",
      grepl("SSP2", Ref) ~ "SSP2",
      grepl("SSP3", Ref) ~ "SSP3",
      TRUE ~ SSP
    )) %>% 
  group_by(SSP,R,Y) %>% 
  mutate(
    baseGDP = GDPcap[str_detect(Ref, "BaU")],
    GDPchange = 100 * (GDPcap / baseGDP - 1),
    GDPchange = ifelse(str_detect(Ref, "BaU"), 0, GDPchange)
  ) %>% 
  ungroup() %>% 
  mutate(Y = as.numeric(as.character(Y)))

for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_line(data = GDP_in %>% filter(SSP == ssp),aes(x = Y ,y = GDPcap,color=Ref),linewidth=1.5)+
    geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
    facet_wrap(~ R,nrow = 3)+
    labs(
      y = "GDP per cap",
      x = "Year"
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2100, by = 10)
    )+
    THEME2+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
          legend.position = "bottom")
  fileout <- paste0("output/260402_地域追加/GDP_",ssp,".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}

for (ssp in SSP_list) {
  plot <- ggplot() +
    geom_line(data = GDP_in %>% filter(SSP == ssp,!grepl("BaU",Ref)),aes(x = Y ,y = GDPchange),linewidth=1.5)+
    geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
    scale_color_manual(values = palette)+
    facet_wrap(~ R,nrow = 3)+
    labs(
      y = "GDP loss rate (%)",
      x = "Year"
    ) +
    scale_x_continuous(
      breaks = seq(2020, 2100, by = 10)
    )+
    THEME2+
    theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                     margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
  fileout <- paste0("output/260402_地域追加/GDPloss_",ssp,".png")
  ggsave(fileout, plot = plot, width = 30, height = 18)
}


file <- paste0("data/regression.gdx")
GDP_alpha <- rgdx.param(file,"GDPpc") %>% 
  rename(R = cty,
         SSP = GDPpc)
file <- paste0("data/Inputdata")
GDP_FDM <- rgdx.param(file,"GDPCap")%>% 
  filter(Ref == "WDI",Y == 2005,!(R %in% Ragg),!grepl("R5",R),!grepl("R2",R)) %>% 
  select(R,GDPCap) %>% 
  rename(WDI = GDPCap)

GDP_all <- GDP_FDM %>% 
  left_join(GDP_alpha,by = c("R")) %>% 
  drop_na()


plot <- ggplot() +
  geom_point(
    data=GDP_all,
    aes(x = SSP,y = WDI
    ),
    size=3
  ) +
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = "dashed",
    color = "black"
  )+
  labs(
    y = "GDPcap_WDI",
    x = "GDPcap_SSP"
  ) +
  THEME2 +
  theme(
    axis.text.x = element_text(
      size = 20, angle = 45, vjust = 0.5, hjust = 0.5,
      margin = margin(t = 0.3, r = 0, b = 0, l = 0, unit="cm")
    )
  )
fileout <- paste0("output/260402_地域追加/GDPcap_WDI_SSP.png")
ggsave(fileout, plot = plot, width = 20, height = 16,limitsize = FALSE)


# FoodDemand --------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_1000.gdx")
FD_in <- rgdx.param(file,"FoodDemIn") %>% 
  rename(Income = FoodDemIn) %>% 
  filter(R %in% Ragg)
FD_in_pr <- rgdx.param(file,"FoodDem_peffect") %>% 
  rename("Income+Price" = FoodDem_peffect) %>% 
  filter(R %in% Ragg)

FD <- FD_in %>% 
  left_join(FD_in_pr,by = c("Ref","var","R","Y","seg")) %>% 
  pivot_longer(
    cols = c(Income, "Income+Price"),
    names_to = "effect",
    values_to = "consumption"
  ) %>% 
  filter(!(Ref == "SSP2_BaU_NoCC" & effect == "Income")) %>% 
  mutate(effect = ifelse(Ref == "SSP2_BaU_NoCC" & effect == "Income+Price","Baseline",effect)) 
ave <- FD %>% 
  filter(seg == "ALL")

FD_seg2 <- FD %>% 
  filter(seg != "ALL") %>% 
  mutate(
    seg=as.numeric(as.character(seg)),
    seg= seg*100/n_split
  )

sc <- "Income"

plot <- ggplot() +
  geom_area(data =FD_seg2 %>% filter(Y == 2050 & var != "total" & effect == sc) ,aes(x = seg ,y = consumption, group = var,fill = var),stat="identity",position="stack")+
  geom_line(data =FD_seg2 %>% filter(Y == 2050 & var == "total" & effect == "Income"),aes(x = seg, y = consumption),color = "blue",linetype = "dashed")+
  geom_line(data =FD_seg2 %>% filter(Y == 2050 & var == "total" & effect == "Baseline"),aes(x = seg, y = consumption),color = "red",linetype = "dashed")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  scale_fill_manual(values = palette)+
  facet_wrap(~ R)+
  labs(
    y = "Calorie intake (kcal/day/capita)",
    x = "Income segment (%)",
    fill ="Item"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/260402_地域追加/FoodDemand_R17_",sc,"_seg_2050.png")
ggsave(fileout, plot = plot, width = 30, height = 25)

# -------------------------------------------------------------------------
