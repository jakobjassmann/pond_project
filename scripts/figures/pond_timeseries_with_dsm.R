# Individual pond time-series with DSM
# Jakob J. Assmann jakob.assmann@uzh.ch 9 April 2024

# Dependencies
library(terra)
library(tidyverse)
library(tidyterra)
library(sf)
library(cowplot)
library(colorspace)
library(pbapply)

# Load ponds filtered by size and overlap
ponds <- read_sf("data/pond_polys/pond_polys_filtered_size_intersection.gpkg")

# Add unique id to each ponds across both remaining sites
ponds <- ponds %>%
  mutate(site_spec_id = id) %>%
  mutate(., id = 1:nrow(.)) 

# Generate pond overview maps
save_plot("figures/cbh/map_individual_ponds_cbh.png",
          ggplot(filter(ponds, site == "cbh")) +
  geom_sf(aes(fill = as.factor(intersections)), colour = NA) +
  geom_sf_text(aes(label = id), size = 1) +
  facet_wrap(~year) +
  labs(fill = "n years w/\nintersections") +
  theme_map() +
  theme(panel.border = element_rect(color = "black", fill = NA)), 
  base_height = 12,
  bg = "white")
save_plot("figures/tlb/map_individual_ponds_tlb.png",
          ggplot(filter(ponds, site == "tlb")) +
            geom_sf(aes(fill = as.factor(intersections)), colour = NA) +
            geom_sf_text(aes(label = id), size = 1) +
            facet_wrap(~year) +
            labs(fill = "n years w/\nintersections") +
            theme_map() +
            theme(panel.border = element_rect(color = "black", fill = NA)), 
          base_height = 12,
          bg = "white")
  

# Load unique pond timeseries (see pond_time_series_analysis.R)
load("data/pond_polys/unique_intersections.Rda")

# Load raster data
# Load rgb raster objects
norm_rasts <- list.files("data/drone_data",
                       pattern = "tif",
                       recursive = T,
                       full.names = T) %>%
  .[grepl("norm",.)]
# Load dsm 
dsm_rasts <- list.files("data/drone_data",
                         pattern = "tif",
                         recursive = T,
                         full.names = T) %>%
  .[grepl("dsm",.)]
# Load predictions 
preds_rasts <- list.files("data/drone_data",
                         pattern = "tif",
                         recursive = T,
                         full.names = T) %>%
  .[grepl("preds_filtered_intersection",.)]

# Write helper functions to plot pond time-series
# pond_sf <- unique_ponds_cbh[1,]
# Helper function to generate RGB plot of pond and raster file
plot_pond_rgb <- function(rgb_rast_file, pond_bounds, pond_sf){
  # Load raster
  rgb_rast <- rast(rgb_rast_file)
  
  # crop
  rgb_rast_crop <- crop(rgb_rast, pond_bounds) %>% trim()
  
  # Get year
  year <- gsub(".*([0-9]{4}).*", "\\1", sources(rgb_rast))
  
  # Generate plot
  pond_plot_rgb <- ggplot() +
    geom_spatraster_rgb(data = rgb_rast_crop,
                        max_col_value = 65535,
                        interpolate = F) +
    annotate("text",
             x = ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.05,
             y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.9,
             label = year,
             colour = "white",
             fontface = "bold",
             hjust = 0,
             vjust = 0.5) +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_nothing() +
    theme(panel.border = element_rect(colour = "grey20", fill=NA))
  
  # Add pond outline if available for the year and constrain plot to bounds
  if(sum(grepl(year, pond_sf$year))) {
    pond_plot_rgb <- pond_plot_rgb +       
      geom_sf(data = pond_sf[grepl(year, pond_sf$year),], 
            colour = "white", 
            size = 0.1,
            alpha = 0.5,
            fill = NA) +
    coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
             ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4]))
  }
  
  # Add a scale bar if this is the last plot in the time-series
  if(year == "2021"){
    pond_plot_rgb <- pond_plot_rgb +       
    annotate("rect",
             xmin = ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.55,
             ymin = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.05,
             xmax = (ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.55) +
               round((ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) / 5),
             ymax = (ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.05) +
               round((ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) / 10) * 0.25,
             fill = "white",
             colour = NA) +
    annotate("text",
             x =  (ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.55) +
               round((ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) / 5) * 0.5,
             y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.1,
             label = paste0(round(((ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) / 5)), " m"),
             colour = "white",
             fontface = "bold",
             size = 3,
             hjust = 0.5,
             vjust = 0) +
    annotate("text",
             x =  (ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.55) +
               round((ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) / 5) * 1.3,
             y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.075,
             label = "↑",
             colour = "white",
             fontface = "bold",
             size = 6,
             hjust = 0.5,
             vjust = 0) +
    annotate("text",
             x =  (ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.55) +
               round((ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) / 5) * 1.7,
             y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.075,
             label = "N",
             colour = "white",
             fontface = "bold",
             size = 3,
             hjust = 0.5,
             vjust = 0)
  }
  
  # Return plot as ggplot object with year as title and adjusted borders
  return(pond_plot_rgb)
}

# Helper function to generate DSM plot
plot_pond_dsm <- function(preds_file, dsm_file, pond_bounds, pond_sf){
  
  # Load preds and dsm
  preds <- rast(preds_file)
  dsm <- rast(dsm_file)
  
  # Crop rasters and mask, and standardise dsm
  preds_crop <- crop(preds, pond_bounds) 
  dsm_crop <- crop(dsm, pond_bounds) 
  dsm_crop <- mask(dsm_crop, preds_crop, inverse = T)
  dsm_crop <- dsm_crop - as.numeric(global(dsm_crop, median, na.rm = T))
  
  # Adjust levels of preds raster to allow for 0-1 alpha plotting
  preds_crop <- classify(preds_crop, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))
  
  # Plot DSM
  dsm_plot <- ggplot() +
    geom_spatraster(data = dsm_crop)  +
    scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                     limits = c(-0.5, 0.5), oob = scales::squish,
                                     na.value = "transparent") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_nothing() +
    theme(panel.border = element_rect(colour = "grey20", fill=NA))
  
  # Add predictions if the preds raster is not empty
  if(global(preds_crop, max, na.rm = T) == 1){
    dsm_plot <- dsm_plot +
      geom_spatraster(data = preds_crop, 
                      aes(alpha = after_stat(value)),
                          fill = "#82C4F5") +
                        scale_alpha_continuous(range = c(0,1))
  }
  
  # Add first pond_outline in time-series
  dsm_plot <- dsm_plot + 
    geom_sf(data = filter(pond_sf, year != "2017") %>% slice(1), 
          colour = "white", 
          size = 0.2,
          alpha = 1,
          fill = NA) 
  
  # Return as ggplot object with no margins and a space holder title
  return(dsm_plot)
}

# Helper function to generate one composite plot for a given pond timeseries / combination
composite_plot <- function(combination){
  # get site name
  site_name <- pull(combination, site)
  
  # get pond polyogns 
  ponds_combination <- ponds %>% filter(id %in% unlist(combination$ids_intersecting))
  
  # add 5 m buffer around first occurance in the time-series ignoring 
  # 2017, crop to site area to avoid over spill, cast to ext object
  pond_bounds <- ponds_combination %>%
    filter(year != "2017") %>%
    slice(1) %>% 
    st_buffer(5, endCapStyle = "SQUARE") %>% 
    st_crop(st_bbox(rast(norm_rasts[grepl(site_name, norm_rasts)][1]))) %>%
    ext()
  
  # Get site specific rasters
  norm_rasts_site <- norm_rasts[grepl(site_name, norm_rasts)]
  dsm_rasts_site <- dsm_rasts[grepl(site_name, dsm_rasts)]
  preds_rasts_site <- preds_rasts[grepl(site_name, preds_rasts)]
  
  # Generate rgb plots
  rgb_plots <- map(norm_rasts_site, plot_pond_rgb, pond_bounds = pond_bounds, pond_sf = ponds_combination)
  # Generate dsm plots
  dsm_plots <- map2(preds_rasts_site, dsm_rasts_site, plot_pond_dsm, 
                    pond_bounds = pond_bounds, pond_sf = ponds_combination)
  
  # Combine plots into a single list
  plot_list <- c(rgb_plots, dsm_plots)
  
  # Determine ratio for plotting
  length_x <- pond_bounds[2] - pond_bounds[1]
  length_y <- pond_bounds[4] - pond_bounds[3]
  
  # Prepare output file name
  output_file <- paste0("figures/", site_name, "/individual_ponds/", combination$combination_id, ".png")
  
  # Generate grid from list and save plot
  plot_grid(plotlist = plot_list,
            nrow = 2,
            ncol = length(norm_rasts_site)) %>%
    save_plot(output_file,
              .,
              nrow = 2,
              ncol = length(norm_rasts_site),
              base_height = 1,
              base_asp = length_x / length_y,
              bg = "white")
  
  # Retrun nothing
  return(NULL)
}

# Test: composite_plot(unique_intersections[1,])

## Generate plots for all unique ponds
unique_intersections %>%
  split(., .$combination_id) %>%
  pblapply(., composite_plot, cl = 31)

# Plot scales
(plot_scales <- ggplot() +
    geom_point(aes(x = 1:15, y = 1:15, fill = rep(c(-0.5,0, 0.5), 5))) +
    geom_line(aes(x = 1:15, y = 1:15, 
                  group = 1,
                  colour = "white")) +
    scale_colour_manual(values = "white",
                        labels = "pond in 2014") +
    scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                     limits = c(-0.5, 0.5), oob = scales::squish,
                                     labels = c("-0.5", "", "0", "", "0.5")) +
    guides(fill = guide_colourbar(
      title = "rel. elevation [m]",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      frame.colour = "white",
      barwidth = unit(1.5, "in")
    ),
    colour = guide_legend(
      title = "",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keyheigt = unit(0.5 * 0.66, "in"),
      keywidth = unit(0.5, "in")
    )) +
    theme(legend.position = "bottom",
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_text(colour = "white",
                                      size = 15),
          legend.text = element_text(colour = "white",
                                     size = 15)))
ggdraw(cowplot::get_legend(plot_scales)) %>%
  save_plot("figures/cbh/individual_ponds/legend.png",
            .,
            base_height = 2,
            base_asp = 2.5)
ggdraw(cowplot::get_legend(plot_scales)) %>%
  save_plot("figures/tlb/individual_ponds/legend.png",
            .,
            base_height = 2,
            base_asp = 2.5)



# Check out number of ponds
ponds %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally()

# Check for ponds disappearing between 2014 and 2021
ponds %>%
  filter(site =="cbh", year == "2014_byte") %>%
  st_intersects(filter(ponds, site == "cbh", year == "2021")) %>%
  map(function(x) length(x) == 0) %>%
  unlist() %>%
  sum()
ponds %>%
  filter(site =="tlb", year == "2014") %>%
  st_intersects(filter(ponds, site == "tlb", year == "2021")) %>%
  map(function(x) length(x) == 0) %>%
  unlist() %>%
  sum()
# 
# Check for ponds appearing between 2014 and 2021
ponds %>%
  filter(site =="cbh", year == "2021") %>%
  st_intersects(filter(ponds, site == "cbh", year == "2014_byte")) %>%
  map(function(x) length(x) == 0) %>%
  unlist() %>%
  sum()
ponds %>%
  filter(site =="tlb", year == "2021") %>%
  st_intersects(filter(ponds, site == "tlb", year == "2014")) %>%
  map(function(x) length(x) == 0) %>%
  unlist() %>%
  sum()

# ggplot() + 
#   geom_sf(data = filter(ponds, site =="cbh", year == "2014_byte"), fill = "blue", colour = NA) +
#   geom_sf(data = filter(ponds, site =="cbh", year == "2021"), fill = "red", colour = NA) +
#   theme_map()

# Filter ponds that occur in 2014 and 2021
unique_ponds_cbh <- filter(ponds, 
                           site =="cbh", 
                           year == "2021" | year == "2014_byte") %>%
  mutate(., intersects_with = st_intersects(., .)) %>%
  mutate(unique = sapply(intersects_with, function(x) length(x) == 0)) %>%
  filter(unique | year == "2014_byte") %>%
  select(-intersects_with, -unique)
unique_ponds_tlb <- filter(ponds, 
                           site =="tlb", 
                           year == "2021" | year == "2014") %>%
  mutate(., intersects_with = st_intersects(., .)) %>%
  mutate(unique = sapply(intersects_with, function(x) length(x) == 0)) %>%
  filter(unique | year == "2014") %>%
  select(-intersects_with, -unique)
