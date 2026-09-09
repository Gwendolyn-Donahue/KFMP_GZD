#Sept 7, 2026
#Gwendolyn Donahue
#KFMP Figure 3: Comparison of video observer counts and in situ counts of Macrocystis pyrifera stipes and Patiria miniata individuals

##-----------------------------------------------------Load libraries----------
library(tidyverse)
library(ggpubr)

##-----------------------------------------------------Set working directory---
setwd("/Users/gwendolyndonahue/Desktop/Stanford/KFE_2024/KF_DIMES_manuscript/KFMP_Clean_Code_Donahue")

##-----------------------------------------------------Load data set-----------
####Notes: only using GZD kelp video count data and JD swath-adjusted Patiria video count data
df_fig3_kelpstars_combined <- read.csv("KFMP_df_KelpStars_insitu_videoGZDJD_counts.csv")

##-----------------------------------------------------Data set adjustments----
df_select_kelp_data <- df_fig3_kelpstars_combined %>%
  select(insitu_macro_stipes_count, video_macro_stipes_count) %>%
  mutate(group = "Macrocystis Stipe Counts")
df_select_star_data <- df_fig3_kelpstars_combined %>%
  select(patiria_adjusted, patiria_min_vid) %>%
  rename(insitu_count = patiria_adjusted, video_count = patiria_min_vid) %>%
  mutate(group = "Patiria Counts (Adjusted)")

df_fig3_select_combined_data <- bind_rows(
  df_select_kelp_data %>% 
    rename(insitu_count = insitu_macro_stipes_count, 
           video_count = video_macro_stipes_count),
  df_select_star_data) %>% 
  drop_na()

##-----------------------------------------------------Stats-------------------
pullstats <- df_fig3_select_combined_data %>%
  group_by(group) %>%
  do(model = lm(video_count ~ insitu_count, data = .)) %>%
  summarize(group = first(group),
            R2 = summary(model)$r.squared,
            p_value = summary(model)$coefficients[2, 4])
print(pullstats)

##-----------------------------------------------------Plot--------------------
KFMP_figure3 <- ggplot(df_fig3_select_combined_data, 
                       aes(x = insitu_count, 
                           y = video_count)) +
  geom_point(color = ifelse(df_fig3_select_combined_data$group == "Macrocystis Stipe Counts",
                            "#636B2F", #kelp color
                            "#FF5000"), #Patiria color
             alpha = 0.5) + #opacity
  geom_smooth(method = "lm", se = TRUE, aes(color = group)) +
  #1-1 line
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  facet_wrap(~ group, scales = "free") +
  labs(x = expression(italic("In Situ") ~ "Counts"),
       y = "Video Counts") +
  scale_color_manual(values = c("Macrocystis Stipe Counts" = "#636B2F",
                                "Patiria Counts (Adjusted)" = "#FF5000")) +
  #plot a and b labels
  geom_text(data = data.frame(
    group = c("Macrocystis Stipe Counts", "Patiria Counts (Adjusted)"),
    label = c("A", "B"),
    insitu_count = -Inf, video_count = Inf),
    aes(x = insitu_count, y = video_count, label = label),
    hjust = -0.5, #right - left shifting (more - = farther right)
    vjust = 1.1,
    inherit.aes = FALSE,
    size = 5, fontface = "bold", family = "Times New Roman") +
  theme_pubr(base_family = "Times New Roman") +
  theme(legend.position = "none", strip.text = element_blank()) #hide titles

KFMP_figure3

##-----------------------------------------------------Save map----------------
ggsave("KFMP_figure3_20260907.png", plot=KFMP_figure3,
       width=9, height=5)
