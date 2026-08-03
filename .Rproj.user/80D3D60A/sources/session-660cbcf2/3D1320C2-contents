# Study Area Map Stoffer & Chang
# Author: M. Stoffer, A. Chang
# Date: 30 July 2026

# Loading Required Packages 
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")}

pacman::p_load(tidyverse,DHARMa, glmmTMB, emmeans, car, cowplot)

pak::pkg_install("pmartinezarbizu/pairwiseAdonis/pairwiseAdonis")
library(pairwiseAdonis)

# Loading the Data
NNS_frequency<-read.csv("data/nns/NNS_frequency_all.csv")

# Running the Model
set.seed(123)
all_glmm<- glmmTMB(frequency_occur ~ distance_to_source_m + protection_level+ region + tidal_zone+ (1|season), data = NNS_frequency, ziformula= ~1, family = beta_family())
summary(all_glmm)
Anova(all_glmm)

# Examining Residuals and Joint Tests
plot(simulateResiduals(all_glmm))
joint_tests(all_glmm)

# Post-Hoc Tests
emmeans(all_glmm, pairwise~protection_level)
emmeans(all_glmm, pairwise~region)
emmeans(all_glmm, pairwise~tidal_zone)

# Visualizing Results of GLMMs
ggplot(NNS_frequency, aes(x=(distance_to_source_m/1000), y=frequency_occur*100, color= protection_level, shape=tidal_zone)) + 
  scale_shape_manual(labels= c("low" = "Low Intertidal", "mid" = "Mid Intertidal", "high" ="High Intertidal"), values=c("low"= 19, "mid"=17, "high"=15), breaks=c("low", "mid", "high"))+
  scale_color_manual(labels=c("limited"= "Minimally Protected" , "highly" = "Highly Protected" , "fully"= "Fully Protected"), values=c("limited"= "#8c8e8f","highly"="#479dea","fully"="#ea6347"), name="Level of Protection")+
  geom_jitter(size=3, width=0.05, height=0.05)+
  geom_smooth(aes(group = 1), method = "glm", se = FALSE, color="black")+
  xlab("Distance from Source (km)")+
  ylab("% of Quadrats in Transect with NNS") +
  labs(shape="Tidal Zone")+
  facet_wrap(~region, scales="free", ncol=2)+
  theme_cowplot()+
  theme(strip.text.x = element_text(face = "bold", size=10))+
  theme(legend.text=element_text(size=10), legend.box="horizontal", legend.position= c(0.65, 0.25), legend.title=element_text(size=10),strip.background=element_blank(), panel.border = element_rect(color = "black", fill = NA,linewidth =0.5))


ggsave(
  filename = "figures/distancetosource.png",
  width = 7,       
  height = 5,     
  units = "in",
  scale=1.5,
  dpi = 300 )



