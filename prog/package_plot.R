#package load-----------------------------------------------------------------------
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

THEME2 <- theme_bw() +
  theme(
    axis.text.x = element_text(size = 28, angle = 45, vjust = 0.5, hjust = 0.5, 
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
    axis.title.y = element_text(size = 20),
    
  )

# -------------------------------------------------------------------------