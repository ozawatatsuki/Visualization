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
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
# Fooddemand-------------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
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

GDP_in <- rgdx.param(file,"GDPcapDist") %>% 
  rename(GDPcap = GDPcapDist,
         Y = Yall) %>% 
  filter(R %in% Ragg & seg != "ALL") %>% 
  mutate(
    seg=as.numeric(as.character(seg)),
    seg= seg*100/n_split
  ) 
GDP_ave <- rgdx.param(file,"GDPcapDist") %>% 
  rename(GDPcap = GDPcapDist,
         Y = Yall) %>% 
  filter(R %in% Ragg & seg == "ALL")%>% 
  group_by(R,Y,seg) %>% 
  mutate(
    baseGDP = GDPcap[Ref == "SSP2_BaU_NoCC"],
    GDPchange = 100 * (GDPcap / baseGDP - 1),
    GDPchange = ifelse(Ref == "SSP2_BaU_NoCC", 0, GDPchange)
  )

FD_GDP_seg2 <- FD_seg2 %>% 
  left_join(GDP_in,by = c("Ref","R","Y","seg")) %>% 
  group_by(var, R, Y, seg) %>%
  mutate(
    diff_Base = consumption - consumption[effect == "Baseline"][1],
    diff_Income = consumption - consumption[effect == "Income"][1],
    diff_Income_Price = consumption - consumption[effect == "Income+Price"][1],
    diff_Price = -consumption[effect == "Income+Price"][1] + consumption[effect == "Income"][1]
  ) %>%
  ungroup()

diff <- FD_GDP_seg2 %>% 
  filter(effect == "Baseline") %>% 
  select(var,R,Y,seg,consumption,diff_Income,diff_Price,diff_Income_Price) %>% 
  pivot_longer(
    cols=starts_with("diff"),
    names_to = "effect",
    values_to = "value"
    ) %>% 
  mutate(value = -value)

# GDP_seg-------------------------------------------------------------------------
GDP_change <- GDP_in %>% 
  group_by(R,Y,seg) %>% 
  mutate(
    baseGDP = GDPcap[Ref == "SSP2_BaU_NoCC"],
    GDPchange = 100 * (GDPcap / baseGDP - 1),
    GDPchange = ifelse(Ref == "SSP2_BaU_NoCC", 0, GDPchange)
  )

plot <- ggplot() +
  geom_line(data =GDP_in %>% filter(Y == plotY),aes(x = seg, y = GDPcap, color = Ref),linewidth=1.5)+
  geom_hline(data =GDP_ave %>% filter(Y == plotY),aes(yintercept = GDPcap, color = Ref),linetype = "dashed",linewidth = 1.5)+
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Income per capita [log scale]",
    x = "Income segment (%)"
  ) +
  scale_y_log10() +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
        legend.position = "bottom")
fileout <- paste0("output/",n_split,"/GDPcap_",plotY,"seg.png")
ggsave(fileout, plot = plot, width = 30, height = 18)

plot <- ggplot() +
  geom_bar(data =GDP_change %>% filter(Y == plotY,Ref != "SSP2_BaU_NoCC"),aes(x = seg, y = GDPchange),fill="blue",stat="identity",position="dodge")+
  geom_bar(data =GDP_ave %>% filter(Y == plotY,Ref != "SSP2_BaU_NoCC"),aes(x = 105, y = GDPchange),fill= "red",stat="identity",position="dodge")+
  geom_hline(yintercept = 0,linetype = "dashed",linewidth = 1.5)+
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Income change rate (%)",
    x = "Income segment",
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
        legend.position = "bottom")
fileout <- paste0("output/",n_split,"/GDPcapChange_",plotY,"_seg.png")
ggsave(fileout, plot = plot, width = 30, height = 18)


# FoodDemand-------------------------------------------------------------------------
#select scenario(Income,Income+Price)
sc <- "Income"
  
plot <- ggplot() +
  geom_area(data =FD_GDP_seg2 %>% filter(Y == plotY & var != "total" & effect == sc) ,aes(x = seg ,y = consumption, group = var,fill = var),stat="identity",position="stack")+
  geom_line(data =FD_GDP_seg2 %>% filter(Y == plotY & var == "total" & effect == "Income"),aes(x = seg, y = consumption),color = "blue",linetype = "dashed")+
  geom_line(data =FD_GDP_seg2 %>% filter(Y == plotY & var == "total" & effect == "Baseline"),aes(x = seg, y = consumption),color = "red",linetype = "dashed")+
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
fileout <- paste0("output/",n_split,"/seg/FoodDemand_R17_",sc,"_seg_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 25)

# Difference from Baseline-------------------------------------------------------------------------
#Income,Price,Income_Price

sc <- "Price"
eff <- paste0("diff_",sc)

plot <- ggplot() +
  geom_area(data =diff %>% filter(Y == plotY & var != "total" & effect ==  eff) ,aes(x = seg ,y = value,fill = var),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  scale_fill_manual(values = palette)+
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Difference of calorie intake (kcal/day/capita)",
    x = "Income segment (%)",
    fill ="Item"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/seg/FDdiff_R17_",eff,"_seg_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 18)

# -------------------------------------------------------------------------
#total
plot <- ggplot() +
  geom_bar(data =diff %>% filter(Y == plotY & var == "total" & effect !=  "diff_Income_Price") ,aes(x = seg ,y = value,fill = effect),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Difference of calorie intake (kcal/day/capita)",
    x = "Income segment (%)",
    fill ="Item"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/seg/FDdiff_R17_total_seg_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 18)
# -------------------------------------------------------------------------
plot <- ggplot() +
  geom_area(data =FD_GDP_seg2 %>% filter(Y == plotY & var != "total" & effect == sc) ,aes(x = GDPcap ,y = consumption, group = var,fill = var),stat="identity",position="dodge")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  scale_fill_manual(values = palette)+
  facet_wrap(~ R, scales = "free_x")+
  labs(
    y = "Calorie intake (kcal/day/capita)",
    x = "GDP per capita",
    fill ="Item"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = , angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/GDP/FoodDemand_R17_",sc,"_GDP_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 25)


# Foodprice-------------------------------------------------------------------------

FP <- rgdx.param(file,"Foodprice") %>% 
  mutate(
    Y = as.numeric(as.character(Y))
  ) %>% 
  filter(R %in% Ragg) %>% 
  pivot_wider(
    names_from = Ref,
    values_from = Foodprice
  ) %>% 
  mutate(change = SSP2_500C_CACN_DAC_NoCC/SSP2_BaU_NoCC - 1)

plot <- ggplot() +
  geom_line(data = FP ,aes(x = Y ,y = change,color = var),linewidth = 1.5)+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  scale_color_manual(values = palette)+
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Foodprice change (500C/BaU -1)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/FoodpriceChange.png")
ggsave(fileout, plot = plot, width = 30, height = 18)


plot <- ggplot() +
  geom_bar(data = FP %>% filter(Y == plotY),aes(x = var ,y = change,fill = var),,stat="identity",position="dodge")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  scale_fill_manual(values = palette)+
  facet_wrap(~ R,nrow=3)+
  labs(
    y = "Foodprice change (500C/BaU -1)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/FoodpriceChange_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 18)

#Price elasticity(regression) -------------------------------------------------------------------------
PE <- rgdx.param("data/reg_priceEla.gdx","para") %>% 
  filter(i3 == "estimate") %>%
  select(-i3,-i4) %>% 
  rename(var = i1) %>% 
  pivot_wider(
    names_from = i2,
    values_from = value
    )%>% 
  mutate(
    var = case_when(
      var == "Oils & Fats"        ~ "Oils_and_Fats",
      var == "Fruits & Veg."      ~ "Fruits_and_Veg",
      var == "Food Other"         ~ "Food_Other",
      var == "Bev. & Tobacco"     ~ "Bev_and_Tobacco",
      TRUE                        ~ var
    )) 

Incomecap <- rgdx.param("data/AnalysisExpenditure.gdx","IncomeCap") %>% 
  filter(Y == "2005" & Ref == "SSP2") %>% 
  select(R,IncomeCap)
PriceEla <- read_excel("data/USDA_priceEla.xls",skip = 1, col_names = TRUE)
PriceEla <- PriceEla[complete.cases(PriceEla), ] %>% 
  select(-`Per capita food`) %>%
  mutate(
    across(
      -Country,
      ~ readr::parse_number(as.character(.))
    ) ) %>% 
  pivot_longer(
    cols = -c(Country),
    names_to = "var",
    values_to = "PriceEla"
  ) %>% 
  mutate(R = countrycode(Country, "country.name", "iso3c", warn = F), .after = Country) %>% 
  select(-Country) %>% 
  left_join(Incomecap,by = "R") %>% 
  select(-R) %>%
  filter(!is.na(IncomeCap)) %>% 
  mutate(
  var = case_when(
    var == "Oils & Fats"        ~ "Oils_and_Fats",
    var == "Fruits & Veg."      ~ "Fruits_and_Veg",
    var == "Food Other"         ~ "Food_Other",
    var == "Bev. & Tobacco"     ~ "Bev_and_Tobacco",
    TRUE                        ~ var
  )) 

label_df <- PE %>%
  mutate(
    label = paste0(
      "y = ",
      round(-Coefficient, 3), " * log10(x) ",
      ifelse(-Intercept >= 0, "+ ", "- "),
      abs(round(Intercept, 3))
    )
  ) %>%
  left_join(
    PriceEla %>%
      group_by(var) %>%
      mutate(Incomecap = as.numeric(as.character(IncomeCap))) %>% 
      summarise(
        x_pos = log10(Incomecap)/2, 
        y_pos = 1.7,   
        .groups = "drop"
      ),
    by = "var"
  )
  
plot <- ggplot() +
  geom_point(data = PriceEla %>% filter(var != "Bev_and_Tobacco"),aes(x = IncomeCap ,y = -PriceEla),color = "blue")+
  geom_abline(
    data = PE %>% filter(var != "Bev_and_Tobacco"),
    aes(slope = -Coefficient, intercept = -Intercept),color = "red"      
  ) +
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ var,nrow = 2)+
  labs(
    y = "Price Elasticity",
    x = "abs(log10(IncomeCap))"
  ) +
  scale_x_log10() +
  THEME2+
  geom_text_npc(
    data = label_df %>% filter(var != "Bev_and_Tobacco"),
    aes(label = label),
    npcx = 0.5,   # 横中央（0〜1）
    npcy = 0.95,  # 上から少し下
    size = 8,
    inherit.aes = FALSE
  )+
  coord_cartesian(ylim = c(0, 1.8))+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/PriceEla_reg.png")
ggsave(fileout, plot = plot, width = 25, height = 18)

#Price elasticity(estimate) -------------------------------------------------------------------------

priceEla_in <- rgdx.param(file,"priceEla") %>% 
  filter(R %in% Ragg & Ref != "SSP2_BaU_NoCC" & seg != "ALL") %>% 
  mutate(
    seg=as.numeric(as.character(seg)),
    seg= seg*100/n_split
  )

plot <- ggplot() +
  geom_line(data = priceEla_in %>% filter(Y == plotY),aes(x = seg ,y = priceEla,color = var),linewidth=1.5)+
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
fileout <- paste0("output/",n_split,"/PriceEla_est_",plotY,".png")
ggsave(fileout, plot = plot, width = 30, height = 18)
  



# Hunger-------------------------------------------------------------------------

file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
PoU_In_Pr <- rgdx.param(file,"PoU_In_Pr_Ragg_calc") %>% 
  filter(Ref == "SSP2_500C_CACN_DAC_NoCC") %>% 
  select(-Ref) %>%
  rename(eff_Income_Price = 3)%>% 
  rename(R = R17)
PoU_In <- rgdx.param(file,"PoU_In_Ragg_calc") %>% 
  rename(R = R17)

PoU_base <- PoU_In %>% 
  filter(Ref == "SSP2_BaU_NoCC") %>% 
  select(-Ref) %>%
  rename(Baseline = 3)
PoU_In <-  PoU_In %>% 
  filter(Ref == "SSP2_500C_CACN_DAC_NoCC") %>% 
  select(-Ref) %>%
  rename(eff_Income = 3)

PoU_all <- PoU_base %>%
  left_join(PoU_In,    by = c("R","Y")) %>%
  left_join(PoU_In_Pr, by = c("R","Y")) %>% 
  mutate(
    add_Income = eff_Income - Baseline,
    add_Income_Price = eff_Income_Price - Baseline,
    add_Price = add_Income_Price - add_Income
  )

plotPoU <- PoU_all %>% 
  select(R,Y,Baseline,add_Income,add_Price) %>%
  pivot_longer(
    cols = c(Baseline,add_Income,add_Price),
    names_to = "effect",
    values_to = "PoU"
  ) %>% 
  mutate(PoU = PoU/1000000)

paletteeff <- c(
  "add_Income" = "#FF4B00",
  "add_Price" = "#03AF7A",
  "Baseline" = "#005AFF"
)

plot <- ggplot() +
  geom_bar(data = plotPoU,aes(x = Y ,y = PoU,fill = effect),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3)+
  scale_fill_manual(values = paletteeff)+
  labs(
    y = "Population at risk of Hunger (million people)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/PoU.png")
ggsave(fileout, plot = plot, width = 30, height = 18)

plot <- ggplot() +
  geom_bar(data = plotPoU %>% filter(effect != "Baseline"),aes(x = Y ,y = PoU,fill = effect),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3)+
  scale_fill_manual(values = paletteeff)+
  labs(
    y = "Additional population at risk of Hunger (million people)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/Add_PoU.png")
ggsave(fileout, plot = plot, width = 30, height = 18)

a <- plotPoU %>% 
  filter(effect=="Baseline",Y == 2020,R!="WLD") 
b <- a %>% 
  summarise(total = sum(PoU))

# Hunger rate-------------------------------------------------------------------------
file <- paste0("data/",n_split,"/Fooddem_PriceEffect_",n_split,".gdx")
PoUrate_In_Pr <- rgdx.param(file,"PoUrate_In_Pr_Ragg_calc") %>% 
  filter(Ref == "SSP2_500C_CACN_DAC_NoCC") %>% 
  select(-Ref) %>%
  rename(eff_Income_Price = 3)%>% 
  rename(R = R17)
PoUrate_In <- rgdx.param(file,"PoUrate_In_Ragg_calc") %>% 
  rename(R = R17)

PoUrate_base <- PoUrate_In %>% 
  filter(Ref == "SSP2_BaU_NoCC") %>% 
  select(-Ref) %>%
  rename(Baseline = 3)
PoUrate_In <-  PoUrate_In %>% 
  filter(Ref == "SSP2_500C_CACN_DAC_NoCC") %>% 
  select(-Ref) %>%
  rename(eff_Income = 3)

PoUrate_all <- PoUrate_base %>%
  left_join(PoUrate_In,    by = c("R","Y")) %>%
  left_join(PoUrate_In_Pr, by = c("R","Y")) %>% 
  mutate(
    add_Income = eff_Income - Baseline,
    add_Income_Price = eff_Income_Price - Baseline,
    add_Price = add_Income_Price - add_Income
  )

plotPoUrate <- PoUrate_all %>% 
  select(R,Y,Baseline,add_Income,add_Price) %>%
  pivot_longer(
    cols = c(Baseline,add_Income,add_Price),
    names_to = "effect",
    values_to = "PoUrate"
  ) %>% 
  mutate(PoUrate = 100*PoUrate)

paletteeff <- c(
  "add_Income" = "#FF4B00",
  "add_Price" = "#03AF7A",
  "Baseline" = "#005AFF"
)

plot <- ggplot() +
  geom_bar(data = plotPoUrate,aes(x = Y ,y = PoUrate,fill = effect),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3)+
  scale_fill_manual(values = paletteeff)+
  labs(
    y = "Prevalence of undernourishment (%)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/PoUrate.png")
ggsave(fileout, plot = plot, width = 30, height = 18)

plot <- ggplot() +
  geom_bar(data = plotPoUrate %>% filter(effect != "Baseline"),aes(x = Y ,y = PoUrate,fill = effect),stat="identity",position="stack")+
  geom_hline(yintercept = 0,color = "black", linetype = "dashed", linewidth = 1)+ 
  facet_wrap(~ R,nrow = 3)+
  scale_fill_manual(values = paletteeff)+
  labs(
    y = "Increase in Prevalence of Undernourishment (%)",
    x = "Year"
  ) +
  THEME2+
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 0.5, hjust = 0.5, 
                                   margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")))
fileout <- paste0("output/",n_split,"/Add_PoUrate.png")
ggsave(fileout, plot = plot, width = 30, height = 18)
