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
library(resmush)

# Load ponds and time-series
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")
load("data/pond_polys/pond_time_series.Rda")

# Set variable if plots should be generated to allow sourcing of functions
# from this script
generate_plots <- FALSE

if(generate_plots){
# Generate pond overview maps
save_plot("figures/cbh/map_individual_ponds_cbh.png",
          ggplot(filter(ponds, site == "cbh")) +
  geom_sf(#(fill = as.factor(n_years)), 
    colour = NA, fill = "darkblue") +
  geom_sf_text(aes(label = id), size = 1) +
  facet_wrap(~year) +
  labs(fill = "n years w/\nintersections") +
  theme_map() +
  theme(panel.border = element_rect(color = "black", fill = NA)), 
  base_height = 12,
  bg = "white")
save_plot("figures/tlb/map_individual_ponds_tlb.png",
          ggplot(filter(ponds, site == "tlb")) +
            geom_sf(#aes(fill = as.factor(n_years)), 
              colour = NA, fill = "darkblue") +
            geom_sf_text(aes(label = id), size = 1) +
            facet_wrap(~year) +
            labs(fill = "n years w/\nintersections") +
            theme_map() +
            theme(panel.border = element_rect(color = "black", fill = NA)), 
          base_height = 12,
          bg = "white")

# Generate time-series overview map
save_plot("figures/cbh/pond_time_series_cbh.png",
          ggplot(filter(pond_time_series_ids, site == "cbh")) +
            geom_sf(aes(fill = as.factor(n_years)), 
              colour = NA) +
            geom_sf_text(aes(label = ts_id), size = 1)  +
            labs(fill = "n years w/\nintersections") +
            theme_map() +
            theme(panel.border = element_rect(color = "black", fill = NA)), 
          base_height = 12,
          bg = "white")
save_plot("figures/tlb/pond_time_series_tlb.png",
          ggplot(filter(pond_time_series_ids, site == "tlb")) +
            geom_sf(aes(fill = as.factor(n_years)), 
                    colour = NA) +
            geom_sf_text(aes(label = ts_id), size = 1)  +
            labs(fill = "n years w/\nintersections") +
            theme_map() +
            theme(panel.border = element_rect(color = "black", fill = NA)), 
          base_height = 12,
          bg = "white")
}

# Load raster data
# Load rgb raster objects
norm_rasts <- list.files("data/drone_data",
                       pattern = "tif",
                       recursive = T,
                       full.names = T) %>%
  .[grepl("norm",.)] %>%
  .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]
# Load dsm 
dsm_rasts <- list.files("data/drone_data",
                         pattern = "tif",
                         recursive = T,
                         full.names = T) %>%
  .[grepl("dsm",.)] %>%
  .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]
# Load predictions (not using all just size filtered data here!!!)
preds_rasts <- list.files("data/drone_data",
                         pattern = "tif",
                         recursive = T,
                         full.names = T) %>%
  .[grepl("preds_filtered/",.)] %>%
  .[!grepl("cbh_2019_preds|tlb_2019_a|tlb_2019_b", .)]

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
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0,0)) +
    theme_nothing() +
    theme(panel.border = element_rect(colour = "grey20", fill=NA))
  
  # Add pond outline if available for the year, and constrain plot to bounds
  if(sum(grepl(year, pond_sf$year))) {
    pond_colour <- "#f9ce5a"
    if(year == min(pond_sf$year)) pond_colour <- "white"
    pond_plot_rgb <- pond_plot_rgb +
      # geom_sf(data = pond_sf %>% filter(year != 2017) %>%
      #           arrange(year) %>% 
      #           split(.$year) %>%
      #           .[[1]], 
      #         colour = "white", 
      #         linewidth = 0.5,
      #         alpha = 1,
      #         fill = NA) +   
      geom_sf(data = pond_sf[grepl(year, pond_sf$year),], 
            colour = pond_colour, 
            linewidth = 0.5,
            alpha = 1,
            fill = NA,
            linetype = "solid") +
    coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
             ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4]))
  }
  
  # If year is 2014, add surface elevation caption
  if(year == 2014){
    # Add year
    pond_plot_rgb <- pond_plot_rgb + 
      annotate("text",
               x = ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.05,
               y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.1,
               label = "RGB",
               colour = "white",
               fontface = "bold",
               size = 14 / .pt,
               hjust = 0,
               vjust = 0.5)
  }
  
  # Add year
  pond_plot_rgb <- pond_plot_rgb + 
    annotate("text",
             x = ext(rgb_rast_crop)[1] + (ext(rgb_rast_crop)[2] - ext(rgb_rast_crop)[1]) * 0.05,
             y = ext(rgb_rast_crop)[3] + (ext(rgb_rast_crop)[4] - ext(rgb_rast_crop)[3]) * 0.9,
             label = year,
             colour = "white",
             fontface = "bold",
             size = 5,
             hjust = 0,
             vjust = 0.5)
  
  # Return plot as ggplot object with year as title and adjusted borders
  return(pond_plot_rgb)
}

# Helper function to generate DSM plot
plot_pond_dsm <- function(preds_file, 
                          dsm_file, 
                          pond_bounds, 
                          pond_sf,
                          add_transect = FALSE){
  
  # Load preds and dsm
  preds <- rast(preds_file)
  dsm <- rast(dsm_file)
  
  # Get site name
  site_name <- gsub(".*/(cbh|tlb)/.*", "\\1", sources(preds))
  
  # Get year
  year <- gsub(".*([0-9]{4}).*", "\\1", sources(dsm))
  
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
  
  # Plot DSM
  dsm_plot <- ggplot() +
    geom_spatraster(data = dsm_crop)  +
    scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                     limits = c(-0.1, 0.5), oob = scales::squish,
                                     begin = 0.1,
                                     end = 0.9,
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
    geom_sf(data = pond_sf %>% filter(year != 2017) %>%
              arrange(year) %>% 
              split(.$year) %>%
              .[[1]], 
          colour = "white", 
          linewidth = 0.5,
          alpha = 1,
          fill = NA) +
    # constrain to pond bounds
    coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
             ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4])) 
  
  # If year is 2014, add surface elevation caption
  if(year == 2014){
    # Add year
    dsm_plot <- dsm_plot + 
      annotate("text",
               x = ext(dsm_crop)[1] + (ext(dsm_crop)[2] - ext(dsm_crop)[1]) * 0.05,
               y = ext(dsm_crop)[3] + (ext(dsm_crop)[4] - ext(dsm_crop)[3]) * 0.95,
               label = "Surface",
               colour = "white",
               size = 14 / .pt,
               fontface = "bold",
               hjust = 0,
               vjust = 1) +
      annotate("text",
               x = ext(dsm_crop)[1] + (ext(dsm_crop)[2] - ext(dsm_crop)[1]) * 0.05,
               y = ext(dsm_crop)[3] + (ext(dsm_crop)[4] - ext(dsm_crop)[3]) * 0.775,
               label = "Elevation",
               colour = "white",
               size = 14 / .pt,
               fontface = "bold",
               hjust = 0,
               vjust = 1)
  }
  
  # Add transects if requested
  if(year %in% c(2021) & add_transect){
    transect <- read_sf(paste0("figures/transect_", combination$ts_id, ".gpkg"))
    dsm_plot <- dsm_plot +
      geom_sf(data = transect, colour = "blue",
              linewidth = 1.5)
  }
  
  # Return as ggplot object with no margins and a space holder title
  return(dsm_plot)
}

# Helper function to generate legend and scale bar for rgb plots
# Add a scale bar if this is the last plot in the time-series
legend_rgb <- function(pond_bounds){
  legend_rgb <- ggplot() +
    coord_sf(xlim = c(pond_bounds[1],pond_bounds[2]),
             ylim = c(pond_bounds[3],pond_bounds[4])) +
    annotate("rect",
             xmin = (pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.5) -
               round((pond_bounds[2] - pond_bounds[1]) / 5),
             xmax = (pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.5) +
               round((pond_bounds[2] - pond_bounds[1]) / 5),
             ymin = pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.80,
             ymax = (pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.80) +
               round((pond_bounds[4] - pond_bounds[3]) / 10) * 0.25,
             fill = "white",
             colour = NA) +
    annotate("text",
             x =  (pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.5),
             y = pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.80 +
               round((pond_bounds[4] - pond_bounds[3]) / 10) * 1,
             label = paste0(round(((pond_bounds[2] - pond_bounds[1]) / 5)), " m"),
             colour = "white",
             size = 3,
             hjust = 0.5,
             vjust = 0) +
    annotate("text",
             x =  (pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.45),
             y = pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.25,
             label = "↑",
             colour = "white",
             size = 9,
             hjust = 0.5,
             vjust = 0) +
    annotate("text",
             x =  (pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.55),
             y = pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.25,
             label = "N",
             fontface = "bold",
             colour = "white",
             size = 5,
             hjust = 0.5,
             vjust = 0) +
    theme_nothing() +
    theme(plot.background = element_rect(fill = "black"),
          panel.background = element_rect(fill = "black"))
  
  line <- ggplot() +
    geom_line(aes(x = 1:15, y = 1:15, 
                  group = 1,
                  colour = "white")) +
    scale_colour_manual(values = "white",
                        labels = "pond in given year") +
    guides(colour = guide_legend(
      title = "",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keyheigt = unit(0.5 * 0.66, "in"),
      keywidth = unit(0.25, "in")
    )) +
    theme(legend.position = "top",
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          plot.background = element_rect(fill = "black"))
  legend_rgb <- plot_grid(get_legend(line), legend_rgb, ncol = 1, rel_widths = c(0.5,1)) +
    theme(panel.background = element_rect(fill = "black"))
  
  return(legend_rgb)
}

# Helper function to generate legend for dsm plots (standardised)
legend_dsm <- function(pond_bounds){
  # Generate mock data with legend
  colour_map <- ggplot() +
    geom_point(aes(x = 1:15, y = 1:15, fill = rep(c(-0.1, 0, 0.5), 5))) +
    scale_colour_manual(values = "white",
                        labels = "pond at start\nof time-series") +
    scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                     limits = c(-0.1, 0.5), 
                                     breaks = seq(-0.1,0.5,0.1),
                                     oob = scales::squish,
                                     begin = 0.1,
                                     end = 0.9,
                                     labels = c("-0.1", "0", "0.1", "0.2", "0.3", "0.4" , "0.5+")
    ) +
    guides(fill = guide_colourbar(
      title = "relative surface elevation [m]",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      frame.colour = "white",
      barwidth = unit(1.5, "in")
    )) +
    theme(legend.position = "top",
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_text(colour = "white", size = 8),
          legend.text = element_text(colour = "white", size = 7),
          plot.background = element_rect(fill = "black"))
  
  line <- ggplot() +
    geom_line(aes(x = 1:15, y = 1:15, 
                  group = 1,
                  colour = "white")) +
    scale_colour_manual(values = "white",
                        labels = "pond at start\nof time-series") +
    guides(colour = guide_legend(
      title = "",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keyheigt = unit(0.5 * 0.66, "in"),
      keywidth = unit(0.25, "in")
    )) +
    theme(legend.position = "top",
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          plot.background = element_rect(fill = "black"))
  
  box <- ggplot() +
    geom_col(aes(x = 1:15, y = 1:15, 
                  fill = "water")) +
    scale_fill_manual(values = "#82C4F5",
                        labels = "water") +
    guides(fill = guide_legend(
      title = "",
      title.position = "top",
      title.hjust = 0.5,
      title.vjust = 0.5,
      keyheigt = unit(0.25, "in"),
      keywidth = unit(0.25, "in")
    )) +
    theme(legend.position = "top",
          legend.key = element_rect(fill = NA, color = NA),
          legend.background = element_rect(fill = NA),
          legend.title = element_text(colour = "white"),
          legend.text = element_text(colour = "white"),
          plot.background = element_rect(fill = "black"))
  
  # Pull out legend
  dsm_legend <- plot_grid(plot_grid(get_legend(line),
                          get_legend(box), ncol = 1),
                          get_legend(colour_map),
                          ncol = 1)
  
  # Add to dummy grob
  dsm_legend <- ggdraw(ggplot() +
    coord_sf(xlim = c(st_bbox(pond_bounds)[1],st_bbox(pond_bounds)[3]),
             ylim = c(st_bbox(pond_bounds)[2],st_bbox(pond_bounds)[4])) +
      theme_nothing() +
      theme(plot.background = element_rect("black"))) +
    draw_plot(dsm_legend) +
    theme(legend.title = element_text(size = 6))
  
  # Return legend
  return(dsm_legend)
}

# Helper function for plotting a legend for the manuscript figures
legend_manuscript <- function(pond_bounds, bg_colour = "black"){

  # Plot line legends (2 maps wide, one map tall)
  lines_legend <- ggplot() +
    annotate("segment", x = 10, xend = 40, y = 33, yend = 33, 
             colour = "white", linewidth = 2) +
    annotate("segment", x = 10, xend = 40, y = 66, yend = 66, 
             colour = "#f9ce5a", linewidth = 2) +
    annotate("text", x = 50, y = 33, 
             colour = "white", size = 14 / .pt, hjust = 0,
             label = "pond at start") +
    annotate("text", x = 50, y = 66, 
             colour = "white", size = 14 / .pt, hjust = 0,
             label = "pond in given year") +
    coord_fixed(xlim = c(0,200), ylim = c(0,100), 
                clip = "off") +
    theme_nothing() +
    theme(plot.background = element_rect(fill = bg_colour, colour = NA),
          panel.background = element_rect(fill = bg_colour, colour = NA))
  
  # Determine map width and height and adjust width if needed
  # to get a minimum ration of 0.9
  map_width <- pond_bounds[2] - pond_bounds[1]
  map_height <- pond_bounds[4] - pond_bounds[3]
  if(map_width / map_height < 0.9) map_width <- map_height * 0.9
  
  # Plot map legends (2 maps wide, one map tall)
  (map_legend <- ggplot() +
    annotate("segment", 
             x = 0.7 * map_width / 2, 
             xend = 0.7 * map_width / 2,
             y = map_height / 3, 
             yend = 2 *map_height / 3,
             colour = "white",
             arrow = arrow(),
             linewidth = 1.5) +
      annotate("text", 
               x = 1.2 * map_width / 2, 
               y = 0.5 * map_height,
               label = "N", colour = "white",
               fontface = "bold",
               size = 20 / .pt) + 
      annotate("segment", 
               x =  1.5 * map_width - 5,
               xend = 1.5 * map_width + 5,
               y = (2 * map_height / 3) * 0.9,
               yend = (2 * map_height / 3) * 0.9,
               colour = "white",
               linewidth = 2) +
      annotate("text",
               x = 1.5 * map_width, 
               y = (2 * map_height / 3) * 1.1,
               label = "10 m", colour = "white",
               size = 14 / .pt) + 
      annotate("rect", 
               xmin =  1.5 * map_width - 5,
               xmax = 1.5 * map_width - 5 + 10/3,
               ymin =  0.3 * map_height - 10/6,
               ymax = 0.3 * map_height + 10/6,
               fill = "#82C4F5",
               colour = NA) +
      annotate("text",
               x = 1.5 * map_width, 
               y = 0.3 * map_height,
               label = "water", colour = "white",
               hjust = 0,
               size = 14 / .pt) + 
      coord_fixed(xlim = c(0, map_width * 2), ylim = c(0, map_height), 
                  clip = "off") +
      theme_nothing() +
      theme(plot.background = element_rect(fill = bg_colour, colour = NA),
            panel.background = element_rect(fill = bg_colour, colour = NA)))
  
  # Plot elevation legend (three maps wide, one map tall)
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
          plot.background = element_rect(fill = bg_colour, colour = NA)) 
  colour_legend <- ggdraw() +
    draw_plot(ggplot() + 
                coord_fixed(xlim = c(0, map_width * 2), ylim = c(0, map_height)) +
                theme_nothing() +
                theme(plot.background = element_rect(fill = bg_colour, colour = NA),
                      panel.background = element_rect(fill = bg_colour, colour = NA))) +
    draw_grob(get_legend(colour_legend))
  
  # put all legends together and return
  all_legends <- plot_grid(lines_legend,
                          map_legend,
                          colour_legend,
                          nrow = 1,
                          ncol = 3,
                          rel_widths = c(2,2,3))
  return(all_legends)
}

# Helper function for plotting a legend for the manuscript figures
legend_manuscript2 <- function(pond_bounds, bg_colour = "black"){
  
  # Plot line legends (4 maps wide, 1/3 maps tall)
  lines_legend <- ggplot() +
    annotate("segment", x = 0, 
             xend = 20, y = 1/2 * 100 / 3, 
             yend = 1/2 * 100 / 3, 
             colour = "white", linewidth = 2) +
    annotate("text", x = 30, y = 1/2 * 100 / 3, 
             colour = "white", size = 14 / .pt, hjust = 0,
             label = "pond at start") +
    annotate("segment", x = 140, xend = 160, 
             y = 1/2 * 100 / 3, yend = 1/2 * 100 / 3, 
             colour = "#f9ce5a", linewidth = 2) +
    annotate("text", x = 170, y = 1/2 * 100 / 3, 
             colour = "white", size = 14 / .pt, hjust = 0,
             label = "pond in given year") +
    annotate("segment", x = 325, xend = 345, 
             y = 1/2 * 100 / 3, yend = 1/2 * 100 / 3, 
             colour = "blue", linewidth = 2) +
    annotate("text", x = 355, y = 1/2 * 100 / 3, 
             colour = "white", size = 14 / .pt, hjust = 0,
             label = "transect") +
    coord_fixed(xlim = c(0,400), ylim = c(0,100/3), 
                clip = "off") +
    theme_nothing() +
    theme(plot.background = element_rect(fill = bg_colour, colour = NA),
          panel.background = element_rect(fill = bg_colour, colour = NA))
  
  # Determine map width and height and adjust width if needed
  # to get a minimum ration of 0.9
  map_width <- pond_bounds[2] - pond_bounds[1]
  map_height <- pond_bounds[4] - pond_bounds[3]
  if(map_width / map_height < 0.9) map_width <- map_height * 0.9
  
  # Plot map legends (3 maps wide, 1/2 map tall)
  (map_legend <- ggplot() +
      annotate("segment", 
               x = 0.7 * 1/2 * map_width, 
               xend = 0.7 * 1/2 * map_width,
               y = 1/6 * map_height / 2, 
               yend = 5/6 * map_height / 2,
               colour = "white",
               arrow = arrow(),
               linewidth = 1.5) +
      annotate("text", 
               x = 1.2 * 1/2 * map_width, 
               y = 0.5 * map_height / 2,
               label = "N", colour = "white",
               fontface = "bold",
               size = 20 / .pt) + 
      annotate("segment", 
               x =  1.5 * map_width - 5,
               xend = 1.5 * map_width + 5,
               y = (0.5 * map_height / 2) * 0.7,
               yend = (0.5 * map_height / 2) * 0.7,
               colour = "white",
               linewidth = 2) +
      annotate("text",
               x = 1.5 * map_width, 
               y = (0.5 * map_height / 2) * 1.3,
               label = "10 m", colour = "white",
               size = 14 / .pt) + 
      annotate("rect", 
               xmin =  2.5 * map_width - 5,
               xmax = 2.5 * map_width - 5 + 10/3,
               ymin =  0.5 * (map_height / 2) - 10/6,
               ymax = 0.5 * (map_height / 2) + 10/6,
               fill = "#82C4F5",
               colour = NA) +
      annotate("text",
               x = 2.5 * map_width, 
               y = 1/2 * map_height / 2,
               label = "water", colour = "white",
               hjust = 0,
               size = 14 / .pt) + 
      coord_fixed(xlim = c(0, map_width * 3), ylim = c(0, 1/2 * map_height), 
                  clip = "off") +
      theme_nothing() +
      theme(plot.background = element_rect(fill = bg_colour, colour = NA),
            panel.background = element_rect(fill = bg_colour, colour = NA)))
  
  # Plot elevation legend (three maps wide, one map tall)
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
          plot.background = element_rect(fill = bg_colour, colour = NA)) 
  colour_legend <- ggdraw() +
    draw_plot(ggplot() + 
                coord_fixed(xlim = c(0, map_width * 2), ylim = c(0, map_height)) +
                theme_nothing() +
                theme(plot.background = element_rect(fill = bg_colour, colour = NA),
                      panel.background = element_rect(fill = bg_colour, colour = NA))) +
    draw_grob(get_legend(colour_legend))
  
  # put all legends together and return
  all_legends <- plot_grid(lines_legend,
                           map_legend,
                           nrow = 1,
                           ncol = 2,
                           rel_widths = c(4,3))
  return(all_legends)
}

# Helper function to generate one composite plot for a given pond time series / combination
composite_plot <- function(combination, 
                           save_plot = TRUE, 
                           return_plot = FALSE, 
                           separate_legend = FALSE,
                           manuscript_legend = FALSE,
                           add_transect = FALSE){
  # get site name
  site_name <- pull(combination, site)
  
  # get pond polyogns 
  ponds_combination <- ponds %>% filter(id %in% unlist(combination$combination))
  
  # add 5 m buffer around mean occurrence of pond and convert to ext object
  pond_bounds <- combination %>%
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
                    pond_bounds = pond_bounds, pond_sf = ponds_combination,
                    add_transect = add_transect)
  
  # Check wich legend was requested
  if(manuscript_legend){
    # Manuscript legend (on new row) 
    
    # Combine plots
    plot_list <- c(rgb_plots,
                  dsm_plots)
    
    # Determine ratio for plotting
    length_x <- pond_bounds[2] - pond_bounds[1]
    length_y <- pond_bounds[4] - pond_bounds[3]
    asp_ratio <- length_x / length_y
    if(asp_ratio < 0.9) asp_ratio <- 0.9
    
    # Prepare output file name
    output_file <- paste0("figures/", site_name, "/individual_ponds/", combination$ts_id, ".png")
    
    # Get rel heights from global variable (to allow external manipulation)
    if(!exists(x = "rel_heights", envir = .GlobalEnv)) rel_heights <- c(1,0.5)
    
    # Generate grid from list and save plot if requested
    if(save_plot){
      plot_grid(plot_grid(plotlist = plot_list,
                          nrow = 2,
                          ncol = length(norm_rasts_site)),
                legend_manuscript(pond_bounds, bg_colour = "black"),
                rel_heights = rel_heights,
                nrow = 2,
                ncol = 1) %>%
        save_plot(output_file,
                  .,
                  nrow = 3,
                  ncol = 7,
                  base_height = 1.25,
                  base_asp = asp_ratio,
                  bg = "black")
    }
    
    # Return plot if requested
    if(return_plot){
      # If requested separate plots and legend
      if(separate_legend){
        return(list(plot_grid(plotlist = plot_list,
                              nrow = 2,
                              ncol = length(norm_rasts_site)),
                    legend_manuscript(pond_bounds)))
      } # Otherwise return everything together
      return(     plot_grid(plot_grid(plotlist = plot_list,
                                      nrow = 2,
                                      ncol = length(norm_rasts_site)),
                            legend_manuscript(pond_bounds),
                            rel_heights = rel_heights,
                            nrow = 2,
                            ncol = 1))
    }
  } else{
    # In-row legend
    
    # Combine plots into a single list
    plot_list <- c(rgb_plots, 
                   list(legend_rgb(pond_bounds)), 
                   dsm_plots, 
                   list(legend_dsm(pond_bounds))
    )
    
    # Determine ratio for plotting
    length_x <- pond_bounds[2] - pond_bounds[1]
    length_y <- pond_bounds[4] - pond_bounds[3]
    asp_ratio <- length_x / length_y
    if(asp_ratio < 0.9) asp_ratio <- 0.9
    
    # Prepare output file name
    output_file <- paste0("figures/", site_name, "/individual_ponds/", combination$ts_id, ".png")
    
    # Generate grid from list and save plot if requested
    if(save_plot){
      plot_grid(plotlist = plot_list,
                nrow = 2,
                ncol = length(norm_rasts_site) + 1) %>%
        save_plot(output_file,
                  .,
                  nrow = 2,
                  ncol = length(norm_rasts_site) + 1,
                  base_height = 2,
                  base_asp = asp_ratio,
                  bg = "black")
    }
    
    # Return plot if requested
    if(return_plot){
      # If requested separate plots and legend
      if(separate_legend){
        return(list(plot_grid(plotlist = plot_list[c(-(length(norm_rasts_site) + 1),-((2*length(norm_rasts_site)) + 2))],
                              nrow = 2,
                              ncol = length(norm_rasts_site)),
                    plot_list[[length(norm_rasts_site) + 1]],
                    plot_list[[(2*length(norm_rasts_site)) + 2]]))
      } # Otherwise return everything together
      return(plot_grid(plotlist = plot_list,
                       nrow = 2,
                       ncol = length(norm_rasts_site) + 1))
    }
  }
  # Otherwise return nothing
  return(NULL)
}

# Test: composite_plot(pond_time_series_ids %>% filter(ts_id == "cbh_031"), manuscript_legend = T)

if(generate_plots){
  # Remove previous plots (if they exist)
  list.files("figures/cbh/individual_ponds/", full.names = T) %>% file.remove()
  list.files("figures/tlb/individual_ponds/", full.names = T) %>% file.remove()
  
  ## Generate plots for all unique ponds
  pond_time_series_ids %>%
    split(., .$ts_id) %>%
    pblapply(., composite_plot, manuscript_legend = T, cl = 31)
}



