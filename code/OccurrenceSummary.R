# Study Area Map Stoffer & Chang
# Author: M. Stoffer, A. Chang
# Date: 30 July 2026

# Loading Required Packages 
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}

pacman::p_load(tidyverse,cowplot)

# Loading Data
transects<-read.csv("data/nns/transects.csv")
taxa<-read.csv("data/nns/field_surveys.csv")

taxa_transect<-taxa %>% 
  unite(transect_tidal, site_code, tidal_zone, season, sep="_")

taxa_transect<-taxa_transect %>% 
  count(transect_tidal, taxa)

taxa_frequency<-left_join(taxa_transect, transects, by= "transect_tidal")

taxa_frequency <-taxa_frequency %>% 
  mutate(frequency_occur=n/total_quadrats)

# Frequency of Occurrence Plots
watersipora_freq_plot<-ggplot(subset(taxa_frequency, taxa %in% "watersipora"), aes(x = site_code, y = frequency_occur*100, color=protection_level, shape=tidal_zone))+
  scale_shape_manual(labels= c("low" = "Low Intertidal", "mid" = "Mid Intertidal", "high" ="High Intertidal"), values=c("low"= 19, "mid"=17, "high"=15), breaks=c("low", "mid", "high"))+
  scale_color_manual(labels=c("limited"= "Minimally Protected" , "SMCA" = "Highly Protected" , "SMR"= "Fully Protected"), values=c("#8c8e8f","#479dea","#ea6347"), name="Level of Protection", guide="none")+
  geom_jitter(size=4,width = 0.2)+
  stat_summary(fun=mean, geom="point", shape=4, size=8, colour="black", position= position_dodge(0.75))+
  labs(x="Site", y=expression(paste("% of Quadrats with ", italic("Watersipora subatra"))))+
  scale_x_discrete(limits=c('DUX','SR','MB','DV','NB','RV','BC','McB','LP','PP'))+
  labs(color= "Level of Protection", shape="Tidal Zone") +
  theme_cowplot()+
  theme(legend.position = "bottom")+
  theme(axis.title.y = element_text(size=14),axis.title.x=element_text(size=14))
watersipora_freq_plot

ggsave(
  filename = "figures/watersipora_freq.png",
  width = 3.5,       
  height = 3,     
  units = "in",
  scale=2,
  dpi = 300 )

bugula_freq_plot<-ggplot(subset(taxa_frequency, taxa %in% "bugula_neritina"), aes(x = site_code, y = frequency_occur*100, color=protection_level, shape=tidal_zone))+
  scale_shape_manual(labels= c("low" = "Low Intertidal", "mid" = "Mid Intertidal", "high" ="High Intertidal"), values=c("low"= 19, "mid"=17, "high"=15), breaks=c("low", "mid", "high"), guide="none")+
  scale_color_manual(labels=c("limited"= "Minimally Protected" , "SMCA" = "Highly Protected" , "SMR"= "Fully Protected"), values=c("#8c8e8f","#479dea","#ea6347"), name="Level of Protection")+
  geom_point(size=3)+
  stat_summary(fun=mean, geom="point", shape=4, size=8, colour="black", position= position_dodge(0.75))+
  labs(x="Site", y=expression(paste("% of Quadrats with ", italic("Bugula neritina"))))+
  scale_x_discrete(limits=c('DUX','SR','MB','DV','NB','RV','BC','McB','LP','PP'))+
  labs(color= "Level of Protection", shape="Tidal Zone")+
  theme_cowplot()+
  theme(legend.position = "bottom", legend.justification="left")+
  theme(axis.title.y = element_text(size=14),axis.title.x=element_text(size=14))
bugula_freq_plot

ggsave(
  filename = "figures/bugula_freq.png",
  width = 3.5,       
  height = 3,     
  units = "in",
  scale=2,
  dpi = 300 )
