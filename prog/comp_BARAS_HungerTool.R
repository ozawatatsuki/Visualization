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
palette <- c(
  "Cereals" = "#ffff00",
  "Meats" = "#8DD3C7",
  "Fish" = "#FCCDE5",
  "Dairy" = "#BEBADA",
  "Oils_and_Fats" = "#FB8072",
  "Fruits_and_Veg" = "#80B1D3",
  "Food_Other" = "#FDB462"
)

# BeforeRAS-------------------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_Agg_seg_",n_split,".gdx")
Food_BRAS <- rgdx.param(file,"fooddemUSDA_calib")
Food_BRAS_total <- rgdx.param(file,"fooddemUSDA_ALL_calib")
Food_BRAS_total <- Food_BRAS_total %>% 
  mutate(var = "total") %>% 
  rename(value = fooddemUSDA_ALL_calib)
Food_BRAS <- Food_BRAS %>% 
  rename(var = CUSDA,
         value = fooddemUSDA_calib) 
Food_BRAS <- bind_rows(Food_BRAS, Food_BRAS_total)
Food_BRAS <- Food_BRAS %>%
  mutate(var = factor(var, 
                        levels = c(setdiff(unique(var), "total"), "total"))) %>%
  arrange(Ref, var ,R, Y, seg)
# AfterRAS-----------------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
Food_ARAS <- rgdx.param(file,"FoodDem_peffect") %>% 
  rename(value = "FoodDem_peffect")

# Heatmap-------------------------------------------------------------------------
Food_BRAS <- Food_BRAS %>% 
  mutate(data = "Before_RAS")
Food_ARAS <- Food_ARAS %>% 
  mutate(data = "After_RAS")

plotY <- 2050
map <- bind_rows(Food_ARAS, Food_BRAS) %>% 
  filter(R == "WLD", seg != "ALL",Y == plotY,Ref == "SSP2_BaU_NoCC") %>%
  mutate(seg = as.numeric(as.character(seg)),
         seg = 100*seg/n_split,
         var = as.character(var)) 

map_normal <- map %>% 
  group_by(var) %>%
  mutate(value_scaled = (value - min(value)) /
           (max(value) - min(value))) %>%
  ungroup() 
  
df_diff <- map_normal %>% 
  group_by(Ref, var, R, Y, seg) %>% 
  summarise(
    value = value[data == "After_RAS"] - value[data == "Before_RAS"],
    value_scaled = value_scaled[data == "After_RAS"] - value_scaled[data == "Before_RAS"],
    .groups = "drop"
  ) %>% 
  mutate(data = "diff")

map_normal <- bind_rows(map_normal, df_diff)

map_normal <- map_normal %>% 
  mutate(
    data = factor(data,
                  levels = c("Before_RAS", "After_RAS", "diff"))
  )
Hmap <- ggplot(map_normal, aes(x = var, y = seg, fill = value_scaled)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma") +  # 色はお好みで
  facet_wrap(~ data) +
  labs(
    x = "Item",
    y = "Income segment(%)",
    fill = "value"
  ) +
  coord_equal() +
  THEME2+
  theme(
    aspect.ratio = 1
  )

fileout <- paste0("output/",n_split,"/RAS_Heatmap_BaU_WLD_",plotY,".png")
ggsave(fileout, plot = Hmap, width = 30, height = 18)

# PoU_FDM-------------------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
PoU_FDM <- rgdx.param(file,"PoU_In_Pr_Ragg_calc") %>% 
  rename(value = "PoU_In_Pr_Ragg_calc",
         R = R17) %>% 
  mutate(value = value/1000000,
         U = "FDM_based",
         Y = as.numeric(as.character(Y)))%>% 
  filter(R == "WLD")


# HungerTool-------------------------------------------------------------------------
file <- paste0("data/HungerTool/AIMHub.gdx")

PoU_HTool <- rgdx.param(file,"Risk_agg") %>% 
  rename(value = "Risk_agg",
         Ref = S,
         R = R_agg,
         Y = T) %>% 
  mutate(Y = as.numeric(as.character(Y))) %>% 
  filter(Ref != "SSP2_500C_CACN_FullComb_NoCC",
         R == "WLD",
         Y <= 2100,
         Y >= 2020)

comp <- bind_rows(PoU_FDM, PoU_HTool) %>%
  mutate(
    U = recode(U,"model_based" = "AIM-Hub_based"),
    U = factor(U,
                  levels = c("fao_based", "AIM-Hub_based", "FDM_based"))
  )

comp2 <- comp %>%
  select(Ref, Y, U, value) %>%
  pivot_wider(names_from = Ref, values_from = value)%>%
  mutate(change = SSP2_500C_CACN_DAC_NoCC - SSP2_BaU_NoCC)

pA <- ggplot() +
  geom_line(data = comp,aes(x = Y ,y = value,color = U),linewidth=1.5)+
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
  geom_bar(data = comp2,
            aes(x = Y, y = change, fill= U),
            position = "dodge",
            stat="identity",
            linewidth = 1.5) +
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

fileout <- paste0("output/",n_split,"/Comp_PoU_FDM_HTool.png")
ggsave(fileout, plot = plot, width = 30, height = 18)

