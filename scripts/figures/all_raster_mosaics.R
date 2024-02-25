# Mosaic all rasters

# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(pbapply)

# get rasters files
norm_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/norm/",.)]
rgb_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/rgb/",.)] %>%
  .[!(grepl("cbh_2014\\.tif", .))]

# Helper function to plot rasters
plot_raster <- function(rast_file, max_val){
  #cat(rast_file, "\n")
  # rast_file <- norm_files[1]
  ggplot() +
    geom_spatraster_rgb(data = rast(rast_file), max_col_value = max_val) +
    theme_nothing() +
    theme(panel.border = element_rect(colour = "grey20", fill=NA))
}

# Plot all rgb rasters
pblapply(rgb_files, plot_raster, max_val = 255) %>%
  plot_grid(plotlist = .) %>%
  save_plot("figures/all_rgb_rasters.png", plot = ., bg = "black",
            base_height = 12, base_asp = 16/9)


# Plot all norm rasters
pblapply(norm_files, plot_raster, max_val = 65535) %>%
  plot_grid(plotlist = .) %>%
  save_plot("figures/all_norm_rasters.png", plot = ., bg = "black",
            base_height = 12, base_asp = 16/9)
  