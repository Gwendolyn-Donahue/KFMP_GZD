#Sept 7, 2026
#Gwendolyn Donahue
#KFMP Figure S3: Comparison of stipe counts from two undergraduate researchers.

##-----------------------------------------------------Load libraries----------
library(tidyverse)
library(ggpubr)

##-----------------------------------------------------Set working directory---
setwd("/Users/gwendolyndonahue/Desktop/Stanford/KFE_2024/KF_DIMES_manuscript/KFMP_Clean_Code_Donahue")

##-----------------------------------------------------Load data set-----------
####Notes: only using GZD kelp video count data and JD swath-adjusted Patiria video count data
df_figS3_kelpvideo <- read.csv("KFMP_df_KelpVideoSummaryCounts_fullmetadata.csv")

##-----------------------------------------------------Wrangle data------------
df_observereffect <- df_figS3_kelpvideo %>%
  rename(insitu_macro_individuals_count = macro_individuals,
         insitu_macro_stipes_count= macro_stipes,
         video_macro_individuals_count = video_individuals_count,
         video_macro_stipes_count = video_stipe_count) %>% 
  select(transect_id, video_recorder, video_macro_stipes_count) %>%
  pivot_wider(names_from = video_recorder,
              values_from = video_macro_stipes_count) %>% #looking at stipe counts, not individuals
  drop_na(`Maya Green`, `Gwendolyn Donahue`) %>% #only keep videos both observed
  #rename columns to preserve anonymity 
  rename(observer1 = `Maya Green`,
         observer2 = `Gwendolyn Donahue`)

##-----------------------------------------------------Simple stats------------
observerlinearmodel <- lm(observer2 ~ observer1, data = df_observereffect)
summary(observerlinearmodel)

##-----------------------------------------------------Plot Supp figure S3-----
KFMP_figureS3 <- ggplot(df_observereffect,
       aes(x = observer1, y = observer2)) +
  #jitter so the urchin barren sites can be clearly seen
  geom_jitter(size = 3, color = "#636B2F", alpha = 0.5, width = 4, height = 4) +
  geom_smooth(method = "lm", se = TRUE, color = "#636B2F") +
  #1-1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  labs(x = expression("Observer 1 " * italic("Macrocystis pyrifera") * " Stipe Counts"),
       y = expression("Observer 2 " * italic("Macrocystis pyrifera") * " Stipe Counts")) +
  theme_pubr(base_family = "Times New Roman")

KFMP_figureS3

##-----------------------------------------------------Save plot---------------
ggsave("KFMP_figureS3_20260908.png", plot=KFMP_figureS3)
