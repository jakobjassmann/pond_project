# Surface volume difference figures for thermokarst and vegetation incursion
# Jakob J. Assmann jakob.assmann@uzh.ch 18 July 2024

# Dependencies
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(cowplot)
library(tidyterra)
library(colorspace)
library(resmush)

# Load ponds and time-series
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")
load("data/pond_polys/pond_time_series.Rda")

# Load dsm 
dsm_rasts <- list.files("data/drone_data",
                        pattern = "tif",
                        recursive = T,
                        full.names = T) %>%
  .[grepl("dsm",.)] %>%
  .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]
# Load predictions
preds_rasts <- list.files("data/drone_data",
                          pattern = "tif",
                          recursive = T,
                          full.names = T) %>%
  .[grepl("preds_filtered/",.)] %>%
  .[!grepl("cbh_2019_preds|tlb_2019_a|tlb_2019_b", .)]

## Thermokarst plot

# Set pond ID
pond_id <- "cbh_066"
site_name <- "cbh"

# Get geometries
pond_bounds <- pond_time_series_ids %>%
  filter(ts_id == pond_id) %>%
  st_buffer(5, endCapStyle = "SQUARE") %>% 
  st_crop(st_bbox(rast(dsm_rasts[grepl(site_name, dsm_rasts)][1]))) %>%
  ext()

# Load preds and dsm for the target year (2014)
preds <- rast(preds_rasts[grepl(paste0("*.", site_name, ".*2014.*"), preds_rasts)])
dsm <- rast(dsm_rasts[grepl(paste0("*.", site_name, ".*2014.*"), dsm_rasts)])

# Load all preds rasters for standardising the dsm
preds_all <- preds_rasts[grepl(site_name, preds_rasts)] %>% rast()

# Crop rasters
preds_crop <- crop(preds, pond_bounds) 
preds_all_crop <- crop(preds_all, pond_bounds)
dsm_crop <- crop(dsm, pond_bounds)  

# Generate a single layer for preds_all
preds_all_crop <- sum(preds_all_crop, na.rm = T)

# Calculate min value for area nevery covered by water
dsm_min <- mask(dsm_crop, preds_all_crop, inverse = T) %>%
  global(., fun=quantile, probs = 0.02, na.rm = T) %>%
  as.numeric()

# Mask dsm for area covered by water in year only and standardise dsm
dsm_crop <- mask(dsm_crop, preds_crop, inverse = T)
dsm_crop <- dsm_crop - dsm_min

# Adjust levels of preds raster to allow for 0-1 alpha plotting
preds_crop <- classify(preds_crop, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))

## Determine geometry of area gained

# Get pond ids for 2014 and 2021
pond_id_2014 <- filter(pond_time_series_ids, ts_id == pond_id) %>% 
  pull(X2014) %>% 
  unlist()
pond_id_2021 <- filter(pond_time_series_ids, ts_id == pond_id) %>% 
  pull(X2021) %>% 
  unlist()

# Get associated geometries
pond_2014 <- filter(ponds, id == pond_id_2014)
pond_2021 <- filter(ponds, id == pond_id_2021)

# Find area gained
area_gained <- st_intersection(pond_2021, pond_2014) %>%
  st_difference(pond_2021, .)

# Calculate inverse of of area_gained in plotting area
area_gained_inv <- st_difference(vect(pond_bounds, crs = "EPSG:32655") %>% 
                                   st_as_sf(),
                                 area_gained)

# Plot DSM
dsm_plot <- ggplot() +
  geom_spatraster(data = dsm_crop)  +
  scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                   limits = c(-0.1, 0.5), oob = scales::squish,
                                   begin = 0.1,
                                   end = 0.9,
                                   na.value = "#82C4F5") +
    geom_sf(data = area_gained, 
          colour = "white", 
          linewidth = 0.5,
          alpha = 1,
          fill = NA) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  # constrain to pond bounds
  coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
           ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4])) +
  theme_nothing() +
  theme(panel.border = element_rect(colour = "white", fill=NA))  

# Surface volume lost plot
svl_plot <- dsm_plot +
  geom_sf(data = area_gained_inv,
          colour = NA, 
          fill = "grey40") +
  geom_sf(data = area_gained, 
          colour = "white", 
          linewidth = 0.5,
          alpha = 1,
          fill = NA) +
  # constrain to pond bounds
  coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
           ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4])) +
  theme(panel.border = element_rect(colour = "white", fill=NA))  

# Add annotations to dsm plot
dsm_plot <- dsm_plot +
  annotate("text", 
           label = "2014",
           colour = "white",
           size = 5,
           hjust = 0,
           vjust = 1,
           fontface = "bold",
           x = pond_bounds[1] + 0.05 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.95 * (pond_bounds[4] - pond_bounds[3])) +
  annotate("text", 
           label = "Pond\n2014",
           colour = "#82C4F5",
           size = 5,
           hjust = 0,
           #fontface = "bold",
           x = pond_bounds[1] + 0.6 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.2 * (pond_bounds[4] - pond_bounds[3]))

# Add annotations to volume lost plot
svl_plot <- svl_plot +
  annotate("text", 
           label = "Volume lost",
           colour = "white",
           size = 5,
           hjust = 0,
           vjust = 1,
           fontface = "bold",
           x = pond_bounds[1] + 0.05 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.95 * (pond_bounds[4] - pond_bounds[3])) +
  annotate("text", 
           label = "Area\ngained\n2021",
           colour = "white",
           hjust = 0,
           size = 5,
           #fontface = "bold",
           x = pond_bounds[1] + 0.60 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.25 * (pond_bounds[4] - pond_bounds[3]))  

# Combine plots into panel
plot_grid(dsm_plot,
          svl_plot)


# Determine map width and height and adjust width if needed
# to get a minimum ration of 0.9
map_width <- pond_bounds[2] - pond_bounds[1]
map_height <- pond_bounds[4] - pond_bounds[3]
if(map_width / map_height < 0.9) map_width <- map_height * 0.9

# Plot map legends (2 maps wide, 1/3 map tall)
(map_legend <- ggplot() +
    annotate("segment", 
             x = 1 * (2/3) * 0.7 * map_width / 2, 
             xend = 1 * (2/3) * 0.7 * map_width / 2,
             y = 1/6 * map_height / 3, 
             yend = 5/6 * map_height / 3,
             colour = "white",
             arrow = arrow(),
             linewidth = 1.5) +
    annotate("text", 
             x = 1 * (2/3) *  1.2 * map_width / 2, 
             y = 0.5 * map_height / 3,
             label = "N", colour = "white",
             fontface = "bold",
             size = 20 / .pt) + 
    annotate("segment", 
             x = 0.5 * 2 * map_width - 5,
             xend = 0.5 * 2 * map_width + 5,
             y = (map_height / 6) * 0.8,
             yend = (map_height / 6) * 0.8,
             colour = "white",
             linewidth = 2) +
    annotate("text",
             x = 0.5 * 2 * map_width, 
             y = (map_height / 6) * 1.3,
             label = "10 m", colour = "white",
             size = 14 / .pt) + 
    annotate("rect", 
             xmin = 2 * 0.7 * map_width,
             xmax = 2 * 0.7 * map_width + 10/3,
             ymin =  (map_height / 6) - 10/6,
             ymax =  (map_height / 6) + 10/6,
             fill = "#82C4F5",
             colour = NA) +
    annotate("text",
             x = 2 * 0.7 * map_width + 5, 
             y = map_height / 6,
             label = "water", colour = "white",
             hjust = 0,
             size = 14 / .pt) + 
    coord_fixed(xlim = c(0, map_width * 2), ylim = c(0, 1/3 * map_height), 
                clip = "off") +
    theme_nothing() +
    theme(plot.background = element_rect(fill = "black", colour = NA),
          panel.background = element_rect(fill = "black", colour = NA)))

# Plot elevation legend (one maps wide, one map tall)
colour_legend <- ggplot() +
  geom_point(aes(x = 1:15, y = 1:15, fill = rep(c(-0.1, 0, 0.5), 5))) +
  scale_colour_manual(values = "white",
                      labels = "pond at start\nof time-series") +
  scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                   limits = c(-0.1, 0.5), 
                                   breaks = seq(-0.1,0.5,0.1),
                                   oob = scales::squish,
                                   begin = 0.1,
                                   end = 0.9,
                                   labels = c("-0.1", "0.0", "0.1", "0.2", "0.3", "0.4" , "0.5+")
  ) +
  guides(fill = guide_colourbar(
    title = "relative elevation [m]",
    title.position = "top",
    title.hjust = 0.5,
    title.vjust = 0.5,
    frame.colour = "white",
    barwidth = unit(2.5, "in")
  )) +
  theme(legend.position = "top",
        legend.key = element_rect(fill = NA, color = NA),
        legend.background = element_rect(fill = NA),
        legend.title = element_text(colour = "white", size = 14),
        legend.text = element_text(colour = "white", size = 14),
        plot.background = element_rect(fill = "black", colour = NA)) 
colour_legend <- ggdraw() +
  draw_plot(ggplot() + 
              coord_fixed(xlim = c(0, 3* map_width), ylim = c(0,  0.8 * map_height)) +
              theme_nothing() +
              theme(plot.background = element_rect(fill = "black", colour = NA),
                    panel.background = element_rect(fill = "black", colour = NA))) +
  draw_grob(get_legend(colour_legend))

# put all legends together and return
# all_legends <- plot_grid(map_legend,
#                          colour_legend,
#                          nrow = 2,
#                          ncol = 1)
# Next combine all plots
(svl_combined_plot <- plot_grid(plot_grid(dsm_plot, svl_plot, ncol = 2), 
                               #all_legends, 
                               map_legend,
                               rel_heights = c(3,1), nrow = 2))

