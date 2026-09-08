#Sept 7, 2026
#Gwendolyn Donahue
#KFMP Figure 2: Map of sites surveyed for this study

##-----------------------------------------------------Load libraries---------
library(ggplot2)
library(dplyr)
library(sf)
library(ggpubr)

##-----------------------------------------------------Set working directory---
setwd("/Users/gwendolyndonahue/Desktop/Stanford/KFE_2024/KF_DIMES_manuscript/KFMP_Clean_Code_Donahue")

##-----------------------------------------------------Load data set-----------
df_fig2_DIMESsites_noshale <- read.csv("KFMP_df_DIMESsites_InSitu.csv") %>% 
  #exclude Shale Beds from study sites
  filter(site!="shal1")

##-----------------------------------------------------Load base map-----------
stanfordlandmask <- st_read("StanfordEarthworksLandPolygon/data/landmask.shp")

##-----------------------------------------------------Base map adjustments----
#set coordinate reference system (aka CRS) to lat/long normal system
st_crs(stanfordlandmask) <- 4326
#mapping window lat long bounds
xmin <- min(df_fig2_DIMESsites_noshale$longitude) - 0.009
xmax <- max(df_fig2_DIMESsites_noshale$longitude) + 0.009
ymin <- min(df_fig2_DIMESsites_noshale$latitude) - 0.009
ymax <- max(df_fig2_DIMESsites_noshale$latitude) + 0.009
#cropping box for map boundaries
bb <- st_as_sfc(
  st_bbox(c(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
    crs = 4326))
#fix and crop downloaded base map file
stanfordlandmask <- st_transform(stanfordlandmask, 4326)
stanfordlandmask <- st_make_valid(stanfordlandmask)
land_crop <- st_intersection(stanfordlandmask, bb)
#set coordinates for HMS marker
star_hms <- tibble(longitude=-121.90473245248093, latitude=36.620594261608886)

##-----------------------------------------------------Plot--------------------
KFMP_figure2 <- ggplot() +
  #base map of land
  geom_sf(data = land_crop,
          fill = "grey70", 
          color = "grey40", linewidth = 0.4) +
  #site points
  geom_point(data = df_fig2_DIMESsites_noshale,
             aes(longitude, latitude),
             shape = 20, size = 2.4, stroke = 0) +
  #star label for HMS
  geom_point(data = star_hms,
             aes(longitude, latitude),
             shape = 42, color = "white", size = 4.5, stroke = 4.5) +
  coord_sf(
    xlim = c(xmin, xmax),
    ylim = c(ymin, ymax),
    expand = FALSE) +
  #theme appearance info
  theme_pubr(base_family = "Times New Roman") +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
        panel.grid = element_blank(),
        axis.line = element_blank(), aspect.ratio = 0.9) +
  labs(x = "Longitude", y = "Latitude")

##-----------------------------------------------------Save map----------------
ggsave("KFMP_figure2_20260907.png", KFMP_figure2, dpi = 600)