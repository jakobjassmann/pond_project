# Generate time-series plots for CBH dsm diff time-series
# Jakob J. Assmann jakob.assmann@uzh.ch 29 August 2022

# Dependencies
library(terra)
library(tidyverse)
library(tidyterra)
library(cowplot)
library(viridisLite)
library(colorspace)

# Load rgb raster objects
cbh_norm <- list.files("data/drone_time_series/cbh/cbh_norm/",
                            pattern = "tif",
                            full.names = T) 
# Load dsm 
cbh_dsm <- list.files("data/drone_time_series/cbh/cbh_dsm/",
                           pattern = "tif",
                           full.names = T) 

# Load predictions 
cbh_preds <- list.files("data/drone_time_series/cbh/cbh_preds/",
                      pattern = "tif",
                      full.names = T) 

# Set pond boundaries
pond_bounds <- get_pond_bounds(19)

# set water colour 
water_colour <- "#82C4F5"

plot_pond_ts <- function(pond_bounds, rast_files, preds_files, dsm_files, prefix,
                      pond_2014_rgb = NA,
                      pond_2014_dsm = NA){
  # Status
  cat("Plotting pond", pond_bounds[,1], "\n")
  
  # Visualise normalised rasters in RBG
  cat("Visualising rgb rasters...\n")
  rgb_plots <- map(rast_files, function(x){
    # Load raster
    x <- rast(x)
    
    # Convert ext object
    pond_bounds <- ext(as.numeric(pond_bounds[-1]))
    
    # crop
    x_crop <- crop(x, pond_bounds)
    
    # Get year
    year <- gsub(".*([0-9]{4}).*", "\\1", x@ptr$filenames)
    
    # Generate plot
    pond_plot_rgb <- ggplot() +
      geom_spatraster_rgb(data = x_crop,
                          max_col_value = 1,
                          interpolate = F) +
      annotate("text",
               x = pond_bounds[1] + (pond_bounds[2] - pond_bounds[1]) * 0.05,
               y = pond_bounds[3] + (pond_bounds[4] - pond_bounds[3]) * 0.9,
               label = year,
               colour = "white",
               fontface = "bold",
               hjust = 0,
               vjust = 0.5) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      theme_nothing() +
      theme(panel.border = element_rect(colour = "grey20", fill=NA))
    
    # Add 2014 outline if requested
    if(sum(!is.na(pond_2014_rgb))){
      cat("HELLO\n")
      pond_plot_rgb <- pond_plot_rgb +
        geom_sf(data = pond_2014_rgb, 
                colour = "white", 
                size = 0.2,
                fill = NA)
    }

        # Return plot as ggplot object with year as title and adjusted borders
    return(pond_plot_rgb)
  })
  
  # Add a scale bar to the last pont
  rgb_plots[[7]] <- rgb_plots[[7]] +       
    annotate("rect",
             xmin = pond_bounds[,2] + (pond_bounds[,3] - pond_bounds[,2]) * 0.55,
             ymin = pond_bounds[,4] + (pond_bounds[,5] - pond_bounds[,4]) * 0.05,
             xmax = (pond_bounds[,2] + (pond_bounds[,3] - pond_bounds[,2]) * 0.55) +
               round((pond_bounds[,3] - pond_bounds[,2]) / 5),
             ymax = (pond_bounds[,4] + (pond_bounds[,5] - pond_bounds[,4]) * 0.05) +
               round((pond_bounds[,3] - pond_bounds[,2]) / 10) * 0.25,
             fill = "white",
             colour = NA) +
    annotate("text",
             x =  (pond_bounds[,2] + (pond_bounds[,3] - pond_bounds[,2]) * 0.55) +
               round((pond_bounds[,3] - pond_bounds[,2]) / 5) * 0.5,
             y = pond_bounds[,4] + (pond_bounds[,5] - pond_bounds[,4]) * 0.1,
             label = paste0(round(((pond_bounds[,3] - pond_bounds[,2]) / 5)), " m"),
             colour = "white",
             fontface = "bold",
             size = 3,
             hjust = 0.5,
             vjust = 0) +
    annotate("text",
             x =  (pond_bounds[,2] + (pond_bounds[,3] - pond_bounds[,2]) * 0.55) +
               round((pond_bounds[,3] - pond_bounds[,2]) / 5) * 1.3,
             y = pond_bounds[,4] + (pond_bounds[,5] - pond_bounds[,4]) * 0.075,
             label = "↑",
             colour = "white",
             fontface = "bold",
             size = 6,
             hjust = 0.5,
             vjust = 0) +
  annotate("text",
           x =  (pond_bounds[,2] + (pond_bounds[,3] - pond_bounds[,2]) * 0.55) +
             round((pond_bounds[,3] - pond_bounds[,2]) / 5) * 1.7,
           y = pond_bounds[,4] + (pond_bounds[,5] - pond_bounds[,4]) * 0.075,
           label = "N",
           colour = "white",
           fontface = "bold",
           size = 3,
           hjust = 0.5,
           vjust = 0)
  
  # Visualise projections with distinct colours
  cat("Visualising  dsm...\n")
  dsm_plots <- map2(preds_files, dsm_files, function(preds, dsm){

    # Load preds and dsm
    preds <- rast(preds)
    dsm <- rast(dsm)
    
    # Convert ext object
    pond_bounds <- ext(as.numeric(pond_bounds[-1]))
    
    # Crop rasters and mask, and standardise dsm
    preds_crop <- crop(preds, pond_bounds)
    dsm_crop <- crop(dsm, pond_bounds) 
    dsm_crop <- mask(dsm_crop,classify(preds_crop, matrix(c(1, 1, 2, NA), byrow = T, ncol = 2)))
    dsm_crop <- dsm_crop - as.numeric(global(dsm_crop, median, na.rm = T))
  
    # Plot
    dsm_plot <- ggplot() +
      geom_spatraster(data = dsm_crop) +
      geom_spatraster(data = preds_crop,
                      aes(alpha = after_stat(value)),
                      fill = water_colour) +
      scale_alpha_continuous(range = c(0,1)) +
      scale_fill_continuous_sequential(palette = "inferno", rev = F,
                                       limits = c(-0.5, 0.5), oob = scales::squish) +
      scale_x_continuous(expand = c(0,0)) +
      scale_y_continuous(expand = c(0,0)) +
      labs(fill = "Altitude") +
      guides(fill = "none", #guide_colourbar(),
             alpha = "none") +
      theme_nothing() +
      theme(panel.border = element_rect(colour = "grey20", fill=NA))
    
    # Add outline of pond in 2014 if it exists
    if(sum(!is.na(pond_2014_dsm))){
      dsm_plot <- dsm_plot +
        geom_sf(data = pond_2014_dsm, 
                colour = "white", 
                size = 0.2,
                fill = NA)
    }
    # Return as ggplot object with no margins and a space holder title
    return(dsm_plot)
  })
  
  # Combine plots into a list
  cat("Combining plots...\n")
  plot_list <- c(rgb_plots, dsm_plots)
  
  # Determine ratio for plotting
  length_x <- as.numeric(pond_bounds)[3] - as.numeric(pond_bounds)[2]
  length_y <- as.numeric(pond_bounds)[5] - as.numeric(pond_bounds)[4]
  
  # Generate grid from list and save plot
  cat("Writing file...\n")
  plot_grid(plotlist = plot_list,
            nrow = 2,
            ncol = 7) %>%
    save_plot(paste0("figures/", prefix, "/individual_ponds/pond_",
                     pond_bounds[1], "_time_series.png"),
              .,
              nrow = 2,
              ncol = 7,
              base_height = 1,
              base_asp = length_x / length_y,
              bg = "white")
  
  # Return Null
  cat("Done.\n")
  return(NULL)
}

# Plot scale 
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
                                   size = 15))
  )
ggdraw(cowplot::get_legend(plot_scales)) %>%
  save_plot("figures/cbh/individual_ponds/legend.png",
            .,
            base_height = 2,
            base_asp = 2.5)
