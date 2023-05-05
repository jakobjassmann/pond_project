# Map figure showing the study sites in the pan-Arctic and landscape context
# Jakob J. Assmann jakob.assmann@uzh.ch 4 December 2022

# Dependencies
library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(cowplot)
library(rnaturalearth)

# Load study site outline(s)
cbh_poly <- read_sf("data/drone_time_series/cbh_timeseries/cbh_study_aoi.shp")
cbh_centroid <- st_centroid(cbh_poly)
tlb_poly <- read_sf("data/drone_time_series/tlb_timeseries/tlb_study_aoi.shp")
tlb_centroid <- st_centroid(tlb_poly)
rdg_poly <- read_sf("data/drone_time_series/rdg_timeseries/rdg_study_aoi.shp")
rdg_centroid <- st_centroid(rdg_poly)

## Generate pan-Arctic overview map

# Define Polygon with cut off at 50 deg
bounds_poly <- data.frame(
  x = 0,
  y = 90
) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>%
  st_transform(3413) %>%
  st_buffer(3314693)

# Get outline of contries
countries_poly <- ne_countries(scale = 50, returnclass = "sf") %>%
  st_transform(3413) %>%
  st_intersection(bounds_poly) %>%
  st_union()

# Bring it all together into an overview map
pan_arctic_overview <- ggplot() +
  geom_sf(data = bounds_poly,
          fill = "#82C4F5", 
          colour = "NA", 
          size = 1) +
  geom_sf(data = countries_poly,
          size = 0.2, 
          fill = "grey90", 
          colour = "black") +
  geom_sf(data = cbh_centroid,
          colour = "#DC6027") +
  geom_sf(data = bounds_poly,
          fill = "NA",
          colour = "black",
          size = 1.5) +
  coord_sf(xlim = st_bbox(bounds_poly)[c(1,3)],
           ylim = st_bbox(bounds_poly)[c(2,4)],
           crs = st_crs(bounds_poly)) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_nothing()

## Plot overview of study area

# Load Kytalyk world view imagery from 2022
kytalyk_wv <- rast("Z:/Schaepman/Gr_Schaepman/Kytalyk/Kytalyk_2022_WorldView_Imagery/40cm_original/second_processing/015284154010_01/015284154010_01_P001_MUL/22JUL21020532-M2AS-015284154010_01_P001.TIF")

# Crop imagery
kytalyk_wv_crop <- kytalyk_wv %>%
  select(5,3,2) %>%
  crop(ext(st_bbox(kytalyk_wv)[1] + 2000,
           st_bbox(kytalyk_wv)[1] + 2000 + 5000,
           st_bbox(kytalyk_wv)[2] + 2250,
           st_bbox(kytalyk_wv)[2] + 2250 + ((5000/16) * 6)))

# Stretch RGB values
kytalyk_wv_crop <- kytalyk_wv_crop %>%
  stretch(minq = 0.05,
          maxq = 0.98)

# Plot using ggplot
kytalyk_wv_plot <- ggplot() +
  geom_spatraster_rgb(data = kytalyk_wv_crop,
                      r = 1,
                      g = 2,
                      b = 3,
                      alpha = 0.8) +
  geom_sf(data=cbh_poly,
          fill = NA,
          colour = "#1E1D40",
          linewidth = 2) +
  geom_sf(
    data = tlb_poly,
    fill = NA,
    colour = "#BF625A",
    linewidth = 2) +
  geom_sf(data=rdg_poly,
          fill = NA,
          colour = "#F2A922",
          linewidth = 2) +
  annotate("segment",
    x = ext(kytalyk_wv_crop)[2] - 0.15 * (ext(kytalyk_wv_crop)[2] - ext(kytalyk_wv_crop)[1]), 
    xend =  ext(kytalyk_wv_crop)[2] - 0.15 * (ext(kytalyk_wv_crop)[2] - ext(kytalyk_wv_crop)[1]) + 500, 
    y = ext(kytalyk_wv_crop)[3] + 0.1 * (ext(kytalyk_wv_crop)[4] - ext(kytalyk_wv_crop)[3]),
    yend = ext(kytalyk_wv_crop)[3] + 0.1 * (ext(kytalyk_wv_crop)[4] - ext(kytalyk_wv_crop)[3]),
    linewidth = 2,
    colour = "black"
  ) +
  annotate("text",
    x = ext(kytalyk_wv_crop)[2] - 0.15 * (ext(kytalyk_wv_crop)[2] - ext(kytalyk_wv_crop)[1]) + 250,
    y = ext(kytalyk_wv_crop)[3] + 0.15 * (ext(kytalyk_wv_crop)[4] - ext(kytalyk_wv_crop)[3]),
    label = "500 m",
    colour = "black",
    fontface = "bold",
    size = 5
  ) +
    annotate("text",
      x = st_bbox(cbh_poly)[1],
      y = st_bbox(cbh_poly)[4],
      label = "CBH",
      fontface = "bold",
      hjust = 0,
      vjust = -0.5,
      size = 5,
      colour = "#1E1D40"
    ) +
  annotate("text",
    x = st_bbox(tlb_poly)[1],
    y = st_bbox(tlb_poly)[4],
    label = "TLB",
    fontface = "bold",
    hjust = 0,
    vjust = -0.5,
    size = 5,
    colour = "#BF625A"
  ) +
  annotate("text",
      x = st_bbox(rdg_poly)[1],
      y = st_bbox(rdg_poly)[4],
      label = "RDG",
      fontface = "bold",
      hjust = 0,
      vjust = -0.5,
      size = 5,
      colour = "#F2A922"
    ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_nothing()
kytalyk_wv_plot

## Combine WV plot with overlay map
kytalyk_plot_combined <-
  ggdraw(kytalyk_wv_plot) +
  draw_grob(as_grob(pan_arctic_overview),
            x = 0.9,
            y = 0.8,
            hjust = 0.5,
            vjust  = 0.5,
            scale = 0.4)

# Export plot
save_plot("figures/overview_map.png",
          kytalyk_plot_combined,
          bg = "white",
          base_asp = 16/6)
