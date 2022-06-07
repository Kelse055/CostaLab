#Packages
library(tidyverse)
library(dplyr)
library(readxl)
library(scales)

#Transformation of Raw Data
Methane01 <- read_excel("Syntrophy_Methane_vs_Substrate.xlsx", 
                        col_types = c("text", "text", "numeric","numeric", "numeric"))
Methane02 <- Methane01 %>% group_by(substrate) %>%
  summarize(Mean = mean(umol_CH4_normalizedOD), Std = sd(umol_CH4_normalizedOD))

#Legend Creation
PD_legend = bquote(paste("1,3 Propanediol"))
PD_color = bquote("#b0799a")

EtOH_legend = bquote(paste("Ethanol"))
EtOH_color = bquote("#2c6b67")

EtOHN2_legend = bquote(paste("Ethanolamine"))
EtOHN2_color = bquote("#88a0dc")

#Levels to Order Substrate by Hydrogen Content
Methane02$substrate <- factor(Methane02$substrate, levels = c("EtOHN2", "EtOH", "13PD"))

#GGPlot of Data

ggplot(Methane02, aes(x=substrate, y=Mean, fill=substrate)) +
  geom_bar(stat="identity", color="black", show.legend = FALSE) +
  geom_errorbar(aes(ymin =(Mean-Std), ymax = (Mean + Std)), color="black", width=0.2) +
  theme_classic(base_size = 11) +
  labs(
       x="Substrate", y = "\U03BCmol Methane per OD") +
  scale_fill_manual(name = "Substrate",
                    labels = c(EtOHN2_legend, EtOH_legend,PD_legend),
                    values = c(EtOHN2_color, EtOH_color, PD_color)) +
  scale_x_discrete(labels=c("EtOHN2" = "Ethanolamine", "EtOH" = "Ethanol", "13PD" = "1,3 Propanediol")) +
  theme(text = element_text(size = 15))


#Rename To Save

#GGSave
ggsave(file="Methane_vs_SubstrateRecolord-06072022.pdf", width=4, height=3)


