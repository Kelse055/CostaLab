library(tidyverse)
library(dplyr)
library(readxl)
library(scales)


#-------------------------------------Syntrophy Species vs Substrate--------------

Species <- read_excel("Syntrophy_SpeciesProportion_vs_Substrate.xlsx", 
                      col_types = c("text", "text", "numeric"))


Species01 <- Species %>% group_by(substrate,species) %>% summarize(Mean=mean(proportion), Std = sd(proportion))

Species01$substrate <- factor(Species01$substrate, levels = c("EtOHN2", "EtOH", "13PD"))
#-----Create Legends Before Running GGPlot Code to Ensure Color and Text is Correct--------

Mmari_legend = bquote(paste(italic("M.maripaludis")))
Mmari_color = bquote("#574571")

PCarb_legend = bquote(paste(italic("S.carbinolica")))
Pcarb_color = bquote("#7fa074")

#-------Species vs Substrate Plot--------
ggplot(Species01, aes(x=substrate, y=Mean, fill=species)) +
  geom_bar(stat="identity", color="black", position=position_fill(reverse=TRUE)) +
  geom_errorbar(data=filter(Species01, species == 'Mmari'), aes(ymin =(Mean-Std), 
                                                                ymax = (Mean + Std)), color="black", width=0.2) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic(base_size = 11) +
  labs(title="Species Proportion by Substrate",
       x="Substrate", y = "Proportion") +
  scale_fill_manual(name = "Species",
                    labels = c(Mmari_legend, PCarb_legend),
                    values = c(Mmari_color, Pcarb_color),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels=c("EtOHN2" = "Ethanolamine", "EtOH" = "Ethanol", "13PD" = "1,3 Propanediol")) 

#-------Name Graph to Save--------
Species_Graph <-ggplot(Species01, aes(x=substrate, y=Mean, fill=species)) +
  geom_bar(stat="identity", color="black", position=position_fill(reverse=TRUE)) +
  geom_errorbar(data=filter(Species01, species == 'Mmari'), aes(ymin =(Mean-Std), 
                                                                ymax = (Mean + Std)), color="black", width=0.2) +
  scale_y_continuous(labels = function(x) paste0(x*100, "%")) +
  theme_classic(base_size = 11) +
  labs(title="Species Proportion by Substrate",
       x="Substrate", y = "Proportion") +
  scale_fill_manual(name = "Species",
                    labels = c(Mmari_legend, PCarb_legend),
                    values = c(Mmari_color, Pcarb_color),
                    guide = guide_legend(reverse = TRUE)) +
  scale_x_discrete(labels=c("EtOHN2" = "Ethanolamine", "EtOH" = "Ethanol", "13PD" = "1,3 Propanediol")) 

#-------------GGSave-----------
ggsave(file="Species_vs_Substrate.png", plot=Species_Graph, width=8, height=5.3)
