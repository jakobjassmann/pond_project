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
cbh_poly <- read_sf("data/drone_data/cbh/cbh_study_aoi.shp")
cbh_centroid <- st_centroid(cbh_poly)
tlb_poly <- read_sf("data/drone_data/tlb/tlb_study_aoi.shp")
tlb_centroid <- st_centroid(tlb_poly)
rdg_poly <- read_sf("data/drone_data/rdg/rdg_study_aoi.shp")
rdg_centroid <- st_centroid(rdg_poly)

## Generate pan-Arctic overview map

# Set crs by rotating NSIDC projection
custom_crs <- st_crs(3413)
cat(custom_crs$wkt)
custom_crs$wkt <- gsub('"Longitude of origin",-45,', '"Longitude of origin",90,', custom_crs$wkt)
cat(custom_crs$wkt)

# Define Polygon with cut off at 50 deg
bounds_poly <- data.frame(
  x = 0,
  y = 90
) %>%
  st_as_sf(coords = c("x", "y"),
           crs = 4326) %>%
  st_transform(custom_crs) %>%
  st_buffer(3314693)

# Get outline of countries
countries_poly <- ne_countries(scale = 50, returnclass = "sf") %>%
  st_transform(custom_crs) %>%
  st_intersection(bounds_poly) %>%
  st_union() 

# Bring it all together into an overview map
pan_arctic_overview <- ggplot() +
  geom_sf(data = bounds_poly,
          fill = "#82C4F5CC", 
          colour = "NA", 
          size = 1) +
  geom_sf(data = countries_poly,
          size = 0.01, 
          fill = "#e5e5e5CC", 
          colour = "#000000CC") +
  geom_sf(data = cbh_centroid,
          shape = 21,
          fill = "#FF0000CC",
          colour = "#000000CC",
          size = 3) +
  geom_sf(data = bounds_poly,
          fill = "NA",
          colour = "#000000CC",
          size = 1.5) +
  coord_sf(xlim = st_bbox(bounds_poly)[c(1,3)],
           ylim = st_bbox(bounds_poly)[c(2,4)],
           crs = st_crs(bounds_poly)) +
  # scale_x_continuous(expand = c(0,0)) +
  # scale_y_continuous(expand = c(0,0)) +
  theme_nothing()

## Plot overview of study area

# Load Kytalyk world view imagery from 2022
kytalyk_planet <- rast("data/planet_data/20220724_013310_99_249b_3B_Visual_clip.tif")

# Crop imagery
kytalyk_planet_crop <- kytalyk_planet %>%
  crop(ext(st_bbox(kytalyk_planet)[1] + 2000,
           st_bbox(kytalyk_planet)[1] + 2000 + 4000,
           st_bbox(kytalyk_planet)[2] + 1750,
           st_bbox(kytalyk_planet)[2] + 1750 + ((4000/3) * 2)))

# Stretch RGB values
kytalyk_planet_crop <- kytalyk_planet_crop %>%
  stretch(minq = 0.0005,
          maxq = 0.9995)
plotRGB(kytalyk_planet_crop)

# Plot using ggplot
kytalyk_wv_plot <- ggplot() +
  geom_spatraster_rgb(data = kytalyk_planet_crop,
                      r = 1,
                      g = 2,
                      b = 3,
                      #alpha = 0.8
                      ) +
  geom_sf(data=cbh_poly,
          fill = NA,
          colour = "#FF369D",
          linewidth = 2) +
  geom_sf(
    data = tlb_poly,
    fill = NA,
    colour = "#19CEE6",
    linewidth = 2) +
  geom_sf(data=rdg_poly,
          fill = NA,
          colour = "#FFE700",
          linewidth = 2) +
  annotate("segment",
    x = ext(kytalyk_planet_crop)[2] - 0.175 * (ext(kytalyk_planet_crop)[2] - ext(kytalyk_planet_crop)[1]), 
    xend =  ext(kytalyk_planet_crop)[2] - 0.175 * (ext(kytalyk_planet_crop)[2] - ext(kytalyk_planet_crop)[1]) + 500, 
    y = ext(kytalyk_planet_crop)[3] + 0.1 * (ext(kytalyk_planet_crop)[4] - ext(kytalyk_planet_crop)[3]),
    yend = ext(kytalyk_planet_crop)[3] + 0.1 * (ext(kytalyk_planet_crop)[4] - ext(kytalyk_planet_crop)[3]),
    linewidth = 2,
    colour = "white"
  ) +
  annotate("text",
    x = ext(kytalyk_planet_crop)[2] - 0.175 * (ext(kytalyk_planet_crop)[2] - ext(kytalyk_planet_crop)[1]) + 250,
    y = ext(kytalyk_planet_crop)[3] + 0.15 * (ext(kytalyk_planet_crop)[4] - ext(kytalyk_planet_crop)[3]),
    label = "500 m",
    colour = "white",
    fontface = "bold",
    size = 5
  ) +
    annotate("text",
      x = st_bbox(cbh_poly)[1],
      y = st_bbox(cbh_poly)[4],
      label = "b",
      fontface = "bold",
      hjust = 0,
      vjust = -0.5,
      size = 7,
      colour = "#FF369D"
    ) +
  annotate("text",
    x = st_bbox(tlb_poly)[1],
    y = st_bbox(tlb_poly)[4],
    label = "c",
    fontface = "bold",
    hjust = 0,
    vjust = -0.5,
    size = 7,
    colour = "#19CEE6"
  ) +
  annotate("text",
      x = st_bbox(rdg_poly)[1],
      y = st_bbox(rdg_poly)[4],
      label = "a",
      fontface = "bold",
      hjust = 0,
      vjust = -0.5,
      size = 7,
      colour = "#FFE700"
    ) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_nothing()

## Combine WV plot with overlay map
kytalyk_plot_combined <-
  ggdraw(kytalyk_wv_plot) +
  draw_grob(as_grob(pan_arctic_overview),
            x = 0.855,
            y = 0.685,
            hjust = 0.5,
            vjust  = 0.4,
            scale = 0.4)

# Export plot
save_plot("figures/overview_map.png",
          kytalyk_plot_combined,
          bg = "white",
          base_asp = 3/2)

