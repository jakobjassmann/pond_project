# Mosaic all rasters

# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(pbapply)

# get rasters files
norm_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/norm/",.)] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))]
rgb_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/rgb/",.)] %>%
  .[!(grepl("cbh_2014\\.tif", .))] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))]

# Helper function to plot rasters
plot_raster <- function(rast_file, max_val){
  # cat(rast_file, "\n")
  # rast_file <- norm_files[1]
  ggplot() +
    geom_spatraster_rgb(data = rast(rast_file), max_col_value = max_val) +
    ggtitle(rast_file %>% 
      gsub(".*([0-9]{4}.*)\\.tif", "\\1", .) %>%
      gsub("2014_byte", "2014", .)
    ) +
    theme_nothing() +
    theme(plot.title = element_text(vjust = 0.5, size = 25))
    # theme(panel.border = element_rect(colour = "grey20", fill=NA))
}
#plot_raster(norm_files[1], max_val = 65535)
# Plot all rgb rasters
# pblapply(rgb_files, plot_raster, max_val = 255) %>%
#  plot_grid(plotlist = .) %>%
#  save_plot("figures/all_rgb_rasters.png", plot = ., bg = "black",
#            base_height = 12, base_asp = 16/9)


# Plot all norm rasters
# pblapply(norm_files, plot_raster, max_val = 65535) %>%
#  plot_grid(plotlist = .) %>%
#  save_plot("figures/all_norm_rasters.png", plot = ., bg = "black",
#            base_height = 12, base_asp = 16/9)

## Plot CBH time-series
cbh_files <- norm_files %>%
  .[grepl("cbh", .)] 
cbh_plots <- cbh_files %>%
  pblapply(., plot_raster, max_val = 65535)
# Add scale bar to last plot
cbh_ext <- ext(rast(norm_files %>% .[grepl("cbh", .)] %>% .[1]))
cbh_width <- cbh_ext[2] - cbh_ext[1]
cbh_height <- cbh_ext[4] - cbh_ext[3]
cbh_plots[[8]] <- cbh_plots[[8]] +
  annotate("segment",
    x = cbh_ext[1] + cbh_width * 0.1, 
    xend = cbh_ext[1] + cbh_width * 0.1 + 100,
    y = cbh_ext[3] + cbh_height * 0.1,
    yend = cbh_ext[3] + cbh_height * 0.1,
    linewidth = 3,
    colour = "white"
  ) + 
  annotate("text",
  x = cbh_ext[1] + cbh_width * 0.1 + (100 / 2) ,
  y= cbh_ext[3] + cbh_height * 0.2,,
  colour = "white",
  size = 8,
  label = "100 m")
plot_grid(
  plotlist = cbh_plots,
  nrow = 2,
  ncol = 4
) %>%
  save_plot("figures/cbh/cbh_norm_timeseries.png", .,
    nrow = 2,
    ncol = 4,
    base_asp = cbh_width / cbh_height,
  bg = "white")
