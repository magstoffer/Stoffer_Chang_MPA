# Study Area Map Stoffer & Chang
# Author: M. Stoffer, A. Chang
# Date: 30 July 2026

# Loading Required Packages 
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}

pacman::p_load(tidyverse,vegan)

pak::pkg_install("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

# Loading the Data
counts.raw<-read.csv("data/nns/counts.csv")
counts.metadata<-read.csv("data/nns/counts_metadata.csv")

# Merging Count Data with Transect Data
counts<- left_join(counts.raw, counts.metadata, by= "transect")
counts

# Creating Bray-Curtis Matrix
counts.raw.bc <- vegdist(counts.raw[, 3:9], method='bray')
counts.raw.bc

# PERMANOVA
set.seed(123)
adonis2(formula=counts.raw.bc ~ counts$protection_level + 
          counts$distance_to_source + 
          counts$region + 
          counts$tidal_zone, 
          by="margin")

# Post-Hoc Tests PERMANOVA (parwise.adonis)
pairwise.adonis2(counts.raw.bc ~ region + distance_to_source +
                   protection_level + tidal_zone, data = counts, by = "margin")

pairwise.adonis2(counts.raw.bc ~ protection_level + region + distance_to_source + tidal_zone, data = counts, by = "margin")


pairwise.adonis2(counts.raw.bc ~ tidal_zone + region+ distance_to_source +
                   protection_level , data = counts, by = "margin")

pairwise.adonis2(counts.raw.bc ~ protection_level + region + distance_to_source + tidal_zone, data = counts, by = "margin")


# Dispersion Tests
disper_protection<-betadisper(counts.raw.bc, counts$protection_level)
permutest(disper_protection)

disper_region<-betadisper(counts.raw.bc, counts$region)
permutest(disper_region)

disper_tidal<-betadisper(counts.raw.bc, counts$tidal_zone)
permutest(disper_tidal)

# nMDS
counts.mds <- metaMDS(counts.raw.bc, distance = "bray")

counts.mds.points <- data.frame(protection_level = counts$protection_level, counts.mds$points, region=counts$region, tidal_zone=counts$tidal_zone)

# Plot Not Broken Down by Region
p <- ggplot(data = counts.mds.points, aes(y = MDS2, x = MDS1, shape=tidal_zone, color = protection_level))
p + geom_point(alpha=0.8, size=4) +
  scale_shape_manual(labels= c("low" = "Low Intertidal", "mid" = "Mid Intertidal", "high" ="High Intertidal"), values=c("low"= 19, "mid"=17, "high"=15), breaks=c("low", "mid", "high"))+
  scale_color_manual(labels=c("limited"= "Minimally Protected" , "highly" = "Highly Protected" , "fully"= "Fully Protected"), values=c("limited"= "#8c8e8f","highly"="#479dea","fully"="#ea6347"), name="Level of Protection")+
  labs(shape="Tidal Zone")+
  facet_wrap(~region, ncol=2) +
  theme_minimal()+
  theme(strip.text.x = element_text(face = "bold", size=10))+
  theme(legend.text=element_text(size=10), legend.box="horizontal", legend.position= c(0.75, 0.25), legend.title=element_text(size=10),strip.background=element_blank(), panel.border = element_rect(color = "black", fill = NA,linewidth =0.5))

ggsave(
  filename = "figures/nMDS1.png",
  width = 4,       
  height = 3,     
  units = "in",
  scale=2,
  dpi = 300 )

# Plot of Region
p <- ggplot(data = counts.mds.points, aes(y = MDS2, x = MDS1, color = region))
p + geom_point(alpha = 0.75, size=5) +
  scale_color_manual(values=c("black","#07b815","magenta"), name="Region")+
  theme_minimal()+
  theme(legend.text=element_text(size=10))

ggsave(
  filename = "figures/nMDS2.png",
  width = 4,       
  height = 3,     
  units = "in",
  scale=2,
  dpi = 300 )




