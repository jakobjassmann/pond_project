# Time-series figure
# In addtion to the animated time-series, I wanted to generate a panel figure
# showing the time-series in a static way, for presentations and the publication
# Jakob J. Assmann jakob.assmann@uzh.ch 2 December 2021

# Dependencies
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(ggplot2)
library(cowplot)

# Load rgb imagery
cbh_rgb <- list.files("data/drone_time_series/cbh/cbh_norm/",
                      pattern = "tif",
                      full.names = T) 
# Load predictions
cbh_preds <- list.files("data/drone_time_series/cbh/cbh_preds/",
                      pattern = "tif",
                      full.names = T) 

# Load and prep training data
cbh_training <- read_sf("data/training/cbh_two_class_polys.shp")
cbh_training <- set_names(cbh_training,
  paste0(c("", "", rep("x", 7), ""), names(cbh_training)))
cbh_training$class <- factor(cbh_training$class,
                             levels = c("water", "other"),
                             ordered  = T)

# Set years
years <- c(2014, 2016:2021)

# Plot RGB imagery
rgb_plots <- map(1:length(years),
                 function(index){
                   # Status
                   cat("Plotting", years[index], "\n")
                   # Get rast object from file name
                   rgb_rast <- rast(cbh_rgb[index])
                   # Retrieve extent
                   rgb_ext <- ext(rgb_rast)
                   # Plot rgb image
                   rgb_plot <- ggplot() +
                     geom_spatraster_rgb(
                       data = rgb_rast,
                       interpolate = F,
                       max_col_value = 1) +
                     annotate("text",
                              x = rgb_ext[1] + (rgb_ext[2] - rgb_ext[1]) * 0.1,
                              y = rgb_ext[3] + (rgb_ext[4] - rgb_ext[3]) * 0.9,
                              label = years[index],
                              fontface = "bold",
                              colour = "white",
                              hjust = 0) +
                     scale_x_continuous(expand = c(0,0)) +
                     scale_y_continuous(expand = c(0,0)) +
                     theme_nothing() +
                     theme(panel.border = element_rect(colour = "grey20", fill=NA))
                   return(rgb_plot)
                 })
# Arrange and save rgb plots
plot_grid(plotlist = rgb_plots,
          nrow = 2,
          ncol = 4) %>%
  save_plot("figures/cbh/whole_area/whole_area_rgb.png",
            .,
           nrow = 2,
           ncol = 4,
           base_height = 2,
           base_asp = (ext(rast(cbh_rgb[[1]]))[2] -ext(rast(cbh_rgb[[1]]))[1]) / 
             (ext(rast(cbh_rgb[[1]]))[2] - ext(rast(cbh_rgb[[1]]))[1]))

# Plot RGB imagery with training data
training_plots <- map(1:length(years),
                 function(index){
                   # Status
                   cat("Plotting", years[index], "\n")
                   # Get rast object from file name
                   rgb_rast <- rast(cbh_rgb[index])
                   # Retrieve extent
                   rgb_ext <- ext(rgb_rast)
                   # Get traingin data
                   year_col <- as.character(paste0("x", years[index]))
                   trainning_sf <- cbh_training[
                     st_drop_geometry(chb_training[,2+index]) == 1,]
                   
                   # Plot rgb image
                   preds_plot <- ggplot() +
                     geom_spatraster_rgb(
                       data = rgb_rast,
                       interpolate = F,
                       max_col_value = 1) +
                     geom_sf(data = trainning_sf,
                             aes(fill = class),
                             colour = NA) +
                     scale_fill_manual(values = c("#82C4F5", "#EB1E95")) +
                     annotate("text",
                              x = rgb_ext[1] + (rgb_ext[2] - rgb_ext[1]) * 0.1,
                              y = rgb_ext[3] + (rgb_ext[4] - rgb_ext[3]) * 0.9,
                              label = years[index],
                              fontface = "bold",
                              colour = "white",
                              hjust = 0) +
                     scale_x_continuous(expand = c(0,0)) +
                     scale_y_continuous(expand = c(0,0)) +
                     theme_nothing() +
                     theme(panel.border = element_rect(colour = "grey20", fill=NA))
                   return(preds_plot)
                 })

# Arrange and save training plots
plot_grid(plotlist = training_plots,
          nrow = 2,
          ncol = 4) %>%
  save_plot("figures/cbh/whole_area/whole_area_training.png",
            .,
            nrow = 2,
            ncol = 4,
            base_height = 2,
            base_asp = (ext(rast(cbh_rgb[[1]]))[2] -ext(rast(cbh_rgb[[1]]))[1]) / 
              (ext(rast(cbh_rgb[[1]]))[2] - ext(rast(cbh_rgb[[1]]))[1]))

# Get legend
training_plot_with_legend <- training_plots[[1]] + 
  labs(fill = "training class") +
  guides(fill = guide_legend(
    title.position = "top",
    title.hjust = 0.5,
    title.vjust = 0.5
  )) +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = NA, colour = NA),
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white"))
save_plot(
  "figures/cbh/whole_area/training_legend.png",
  ggdraw(get_legend(training_plot_with_legend))
)
  
  
# Plot RGB imagery with predictions
preds_plots <- map(1:length(years),
                 function(index){
                   # Status
                   cat("Plotting", years[index], "\n")
                   # Get rast objects from file names
                   rgb_rast <- rast(cbh_rgb[index])
                   preds_rast <- rast(cbh_preds[index])
                   # Retrieve extent
                   rgb_ext <- ext(rgb_rast)
                   # Plot rgb image
                   rgb_plot <- ggplot() +
                     geom_spatraster_rgb(
                       data = rgb_rast,
                       interpolate = F,
                       max_col_value = 1) +
                     geom_sf(data = st_as_sf(as.polygons(rgb_rast, extent = T)),
                             fill = "grey10", alpha = 0.5) +
                     geom_spatraster(data = preds_rast,
                                     aes(alpha = after_stat(value)),
                                     fill = "#82C4F5") +
                     scale_alpha_continuous(range = c(0,1)) +
                     annotate("text",
                              x = rgb_ext[1] + (rgb_ext[2] - rgb_ext[1]) * 0.1,
                              y = rgb_ext[3] + (rgb_ext[4] - rgb_ext[3]) * 0.9,
                              label = years[index],
                              fontface = "bold",
                              colour = "white",
                              hjust = 0) +
                     scale_x_continuous(expand = c(0,0)) +
                     scale_y_continuous(expand = c(0,0)) +
                     guides() +
                     theme_nothing() +
                     theme(panel.border = element_rect(colour = "white", fill=NA))
                   return(rgb_plot)
                 })
# Arrange and save preds plots
plot_grid(plotlist = preds_plots,
          nrow = 2,
          ncol = 4) %>%
  save_plot("figures/cbh/whole_area/whole_area_preds.png",
            .,
            nrow = 2,
            ncol = 4,
            base_height = 2,
            base_asp = (ext(rast(cbh_rgb[[1]]))[2] -ext(rast(cbh_rgb[[1]]))[1]) / 
              (ext(rast(cbh_rgb[[1]]))[2] - ext(rast(cbh_rgb[[1]]))[1]))

# Plot predictions only
preds_only_plots <- map(1:length(years),
                   function(index){
                     # Status
                     cat("Plotting", years[index], "\n")
                     # Get rast objects from file names
                     rgb_rast <- rast(cbh_rgb[index])
                     preds_rast <- rast(cbh_preds[index])
                     # Retrieve extent
                     rgb_ext <- ext(rgb_rast)
                     # Plot rgb image
                     rgb_plot <- ggplot() +
                       # geom_spatraster_rgb(
                       #   data = rgb_rast,
                       #   interpolate = F,
                       #   max_col_value = 1) +
                       geom_spatraster(data = preds_rast,
                                       aes(alpha = after_stat(value)),
                                       fill = "#82C4F5") +
                       scale_alpha_continuous(range = c(0,1)) +
                       annotate("text",
                                x = rgb_ext[1] + (rgb_ext[2] - rgb_ext[1]) * 0.1,
                                y = rgb_ext[3] + (rgb_ext[4] - rgb_ext[3]) * 0.9,
                                label = years[index],
                                fontface = "bold",
                                colour = "white",
                                hjust = 0) +
                       scale_x_continuous(expand = c(0,0)) +
                       scale_y_continuous(expand = c(0,0)) +
                       guides() +
                       theme_nothing() +
                       theme(panel.border = element_rect(colour = "white", fill=NA))
                     return(rgb_plot)
                   })
# Arrange and save preds plots
plot_grid(plotlist = preds_only_plots,
          nrow = 2,
          ncol = 4) %>%
  save_plot("figures/cbh/whole_area/whole_area_preds_only.png",
            .,
            nrow = 2,
            ncol = 4,
            base_height = 2,
            base_asp = (ext(rast(cbh_rgb[[1]]))[2] -ext(rast(cbh_rgb[[1]]))[1]) / 
              (ext(rast(cbh_rgb[[1]]))[2] - ext(rast(cbh_rgb[[1]]))[1]))
