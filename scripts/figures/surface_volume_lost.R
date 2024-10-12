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
pond_id <- "tlb_025"
site_name <- "tlb"

# Get geometries
pond_bounds <- pond_time_series_ids %>%
  filter(ts_id == pond_id) %>%
  st_buffer(5, endCapStyle = "SQUARE") %>% 
  st_crop(st_bbox(rast(dsm_rasts[grepl(site_name, dsm_rasts)][1]))) %>%
  ext()

# Load preds and dsm for the target years
preds_2014 <- rast(preds_rasts[grepl(paste0("*.", site_name, ".*2014.*"), preds_rasts)])
dsm_2014 <- rast(dsm_rasts[grepl(paste0("*.", site_name, ".*2014.*"), dsm_rasts)])
preds_2021 <- rast(preds_rasts[grepl(paste0("*.", site_name, ".*2021.*"), preds_rasts)])
dsm_2021 <- rast(dsm_rasts[grepl(paste0("*.", site_name, ".*2021.*"), dsm_rasts)])

# Load all preds rasters for standardising the dsm
preds_all <- preds_rasts[grepl(site_name, preds_rasts)] %>% rast()

# Crop rasters
preds_crop_2014 <- crop(preds_2014, pond_bounds) 
preds_crop_2021 <- crop(preds_2021, pond_bounds) 
preds_all_crop <- crop(preds_all, pond_bounds)
dsm_crop_2014 <- crop(dsm_2014, pond_bounds) 
dsm_crop_2021 <- crop(dsm_2021, pond_bounds) 

# Generate a single layer for preds_all
preds_all_crop <- sum(preds_all_crop, na.rm = T)

# Calculate min value for area nevery covered by water
dsm_min_2014 <- mask(dsm_crop_2014, preds_all_crop, inverse = T) %>%
  global(., fun=quantile, probs = 0.02, na.rm = T) %>%
  as.numeric()
dsm_min_2021 <- mask(dsm_crop_2021, preds_all_crop, inverse = T) %>%
  global(., fun=quantile, probs = 0.02, na.rm = T) %>%
  as.numeric()

# Mask dsm for area covered by water in year only and standardise dsm
dsm_crop_2014 <- mask(dsm_crop_2014, preds_crop_2014, inverse = T)
dsm_crop_2014 <- dsm_crop_2014 - dsm_min_2014
dsm_crop_2021 <- mask(dsm_crop_2021, preds_crop_2021, inverse = T)
dsm_crop_2021 <- dsm_crop_2021 - dsm_min_2021

# Adjust levels of preds raster to allow for 0-1 alpha plotting
preds_crop_2014 <- classify(preds_crop_2014, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))
preds_crop_2021 <- classify(preds_crop_2021, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))

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
area_gained <- st_union(pond_2021, pond_2014) %>%
  st_difference(., pond_2014)

# Calculate inverse of of area_gained in plotting area
area_gained_inv <- st_difference(vect(pond_bounds, crs = "EPSG:32655") %>% 
                                   st_as_sf(),
                                 area_gained)

# Plot DSM
dsm_plot <- ggplot() +
  geom_spatraster(data = dsm_crop_2014)  +
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
  theme(panel.border = element_rect(colour = "black", fill=NA))  

# Surface volume lost plot
svl_plot <- dsm_plot +
  geom_sf(data = area_gained_inv,
          colour = NA, 
          fill = "grey70") +
  geom_sf(data = area_gained, 
          colour = "white", 
          linewidth = 0.5,
          alpha = 1,
          fill = NA) +
  # constrain to pond bounds
  coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
           ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4])) 

# Add annotations to dsm plot
dsm_plot <- dsm_plot +
  # annotate("text", 
  #          label = "2014",
  #          colour = "white",
  #          size = 5,
  #          hjust = 0,
  #          vjust = 1,
  #          fontface = "bold",
  #          x = pond_bounds[1] + 0.05 * (pond_bounds[2] - pond_bounds[1]),
  #          y = pond_bounds[3] + 0.95 * (pond_bounds[4] - pond_bounds[3])) +
  annotate("text", 
           label = "Pond\n2014",
           colour = "#82C4F5",
           size = 5,
           hjust = 0,
           fontface = "bold",
           x = pond_bounds[1] + 0.075 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.275 * (pond_bounds[4] - pond_bounds[3]))

# Add annotations to volume lost plot
svl_plot <- svl_plot +
  # annotate("text", 
  #          label = "Volume lost",
  #          colour = "white",
  #          size = 5,
  #          hjust = 0,
  #          vjust = 1,
  #          fontface = "bold",
  #          x = pond_bounds[1] + 0.05 * (pond_bounds[2] - pond_bounds[1]),
  #          y = pond_bounds[3] + 0.95 * (pond_bounds[4] - pond_bounds[3])) +
  annotate("text", 
           label = "Land\narea lost 2021",
           colour = "white",
           hjust = 0,
           size = 5,
           fontface = "bold",
           x = pond_bounds[1] + 0.05 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.25 * (pond_bounds[4] - pond_bounds[3])) +
  annotate("text",
           label = "Surface\n2014",
           colour = "#FF4C5D",
           hjust = 1,
           size = 5,
           fontface = "bold",
           x = pond_bounds[1] + 0.95 * (pond_bounds[2] - pond_bounds[1]),
           y = pond_bounds[3] + 0.8 * (pond_bounds[4] - pond_bounds[3])) 

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
    title = "relative surface elevation [m]",
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

## Plot cross section

transect_coords <- data.frame(x= c(518010, 518017.5), y = c(7859088.5,7859095.5))
transect <- st_as_sf(transect_coords, coords = c("x", "y"), crs = st_crs(dsm_crop_2014)) %>%
  summarise() %>%
  st_cast("LINESTRING")
write_sf(transect, paste0("figures/transect_", pond_id, ".gpkg"))

plot(dsm_crop_2014)
plot(pond_2021, add = T)
#click(dsm_crop_2014,n = 2, xy = T)
plot(transect, add = T, col = "red")
transect_values_2014 <- extract(dsm_crop_2014, transect, touches = FALSE)
transect_values_2014 <- data.frame(
  x = rev(1:length(transect_values_2014[,2])) / length(transect_values_2014[,2]),
  y = transect_values_2014[,2]) %>%
  mutate(pond = case_when(is.na(y) ~ TRUE, 
                          TRUE ~ NA),
          y = case_when(is.na(y) ~ 0, 
                       TRUE ~ y),
         x = x * as.numeric(st_length(transect)))
transect_values_2021 <- extract(dsm_crop_2021, transect, touches = FALSE)
transect_values_2021 <- data.frame(
  x = rev(1:length(transect_values_2021[,2])) / length(transect_values_2021[,2]),
  y = transect_values_2021[,2]) %>%
  mutate(pond = case_when(is.na(y) ~ TRUE, 
                          TRUE ~ NA),
         y = case_when(is.na(y) ~ 0, 
                       TRUE ~ y),
         x = x * as.numeric(st_length(transect)))
transect_plot <- plot_grid(ggplot(transect_values_2014) +
            geom_line(aes(x = x, y = y, colour = y),
                      linewidth = 1) +
            geom_line(aes(x = x, y = y), data = transect_values_2014[!is.na(transect_values_2014$pond),],
                      colour = "#82c4f5", 
                      linewidth = 1) +
            annotate("text", label = "Transect", x = 0, y = Inf,
                       size = 14 / .pt, hjust = 0, vjust = 1.75,
                       fontface = "bold",
                       colour = "white") +
            annotate("text", label = "2014", x = 0, y = Inf,
                     size = 14 / .pt, hjust = 0, vjust = 3.5,
                     fontface = "bold",
                     colour = "white") +
            annotate("text", label = "pond", x = 3, y = -0.11,
                     size = 14 / .pt, colour = "#82c4f5") +
            scale_y_continuous(limits = c(-0.25, 0.5)) +
            scale_colour_continuous_sequential(palette = "inferno", rev = F,
                                               limits = c(-0.1, 0.5), 
                                               breaks = seq(-0.1,0.5,0.1),
                                               oob = scales::squish,
                                               begin = 0.1,
                                               end = 0.9) +
            labs(x = NULL,
                 y = NULL) +
            theme_nothing() +
            theme(legend.position = "none",
                  # axis.line.x  = element_blank(),
                  # axis.text.x = element_blank(),
                  # axis.ticks.x = element_blank(),
                  # axis.title.x = element_blank(),
                  # axis.title.y = element_text(hjust = -0.11),
                  plot.background = element_rect(fill = "black")),
          ggplot(transect_values_2021) +
            geom_line(aes(x = x, y = y, colour = y),
                      linewidth = 1) +
            geom_line(aes(x= x, y = y),
                      linewidth = 0.5,
                      colour = "white",
                      data = transect_values_2014,
                      linetype = "dashed") + 
            geom_line(aes(x = x, y = y), 
                      data = transect_values_2021[!is.na(transect_values_2021$pond),],
                      colour = "#82c4f5", 
                      linewidth = 1) +
            annotate("text", label = "2021", x = 0, y = Inf,
                     size = 14 / .pt, hjust = 0, vjust = 1.75,
                     fontface = "bold",
                     colour = "white") +
            annotate("text", label = "pond", x = 6, y = -0.11,
                     size = 14 / .pt, colour = "#82c4f5") +
            annotate("errorbar", 
                     x = 6.4, 
                     ymin = 0.01, 
                     ymax = mean(transect_values_2014[transect_values_2021$pond,2], na.rm = T) - 0.01,
                     width = 0.4,
                     linewidth = 0.75,
                     color = "white"
            ) +
            annotate("text", 
                     x = 6.4,
                     y = 0.425,
                     label = "drop in elevation",
                     size = 12 /.pt,
                     colour = "white") +
            annotate("segment",
                     x = 6.4, xend = 6.4,
                     y = 0.35, yend = 0.25, arrow = arrow(length = unit(0.1, "inches")),
                     linewidth = 0.75,
                     colour = "white") +
            # annotate("text", label = "transect", 
            #          x = 0, y = -0.18,
            #          size = 12 / .pt, 
            #          hjust = 0, vjust = 0,
            #          colour = "white") +
            # annotate("segment", x = 0, xend = max(transect_values_2021$x),
            #          y = -0.20, yend = -0.20, 
            #          colour = "blue", linewidth = 1) +
            scale_x_continuous(breaks = 0:10) +
            scale_y_continuous(limits = c(-0.25, 0.5)) +
            scale_colour_continuous_sequential(palette = "inferno", rev = F,
                                               limits = c(-0.1, 0.5), 
                                               breaks = seq(-0.1,0.5,0.1),
                                               oob = scales::squish,
                                               begin = 0.1,
                                               end = 0.9) +
            labs(x = NULL,
                 y = NULL) +
            theme_nothing() +
            theme(legend.position = "none",
                  plot.background = element_rect(fill = "black")),
          nrow= 2
          #rel_heights = c(208/242,1)
          )

