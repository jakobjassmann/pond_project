library(terra)
library(tidyverse)
library(cowplot)


# Load normalised raster objects
dtlb_norm <- list.files("data/drone_time_series/dtlb/dtlb_norm/",
                           pattern = "tif",
                           full.names = T) 
cbh_norm <- list.files("data/drone_time_series/cbh/cbh_norm/",
                            pattern = "tif",
                            full.names = T) 
# Load projections
dtlb_preds <- list.files("data/drone_time_series/dtlb/dtlb_preds/",
                           pattern = "tif",
                           full.names = T) 
cbh_preds <- list.files("data/drone_time_series/cbh/cbh_preds/",
                           pattern = "tif",
                           full.names = T) 

# Define collour pallettes
dtlb_pallette <- c("#A6874E", "#F2BE22", 
                   "#30A5BF", "#000000", "#185359")[3:5]
cbh_pallette <- c("#A6874E", "#F2BE22", 
                   "#30A5BF", "#000000", "#185359")

dtlb_ponds <- list(c(1, 518361.930, 518396.845,
                        7858896.776, 7858929.276),
                   c(2, 518362.718,518390.130,
                       7858827.869,7858849.313),
                   c(3, 518243.213,518264.529,
                       7858875.675,7858894.272))

cbh_ponds <- list(c(1, 517518.698, 517560.526,
                       7858819.126, 7858859.336),
                  c(2, 517589.856, 517638.226,
                       7858881.095, 7858918.439),
                  c(3, 517476.282, 517536.193,
                       7858702.464, 7858736.867))

plot_pond <- function(target_ext, rast_files, preds_files, class_pallette,
                      prefix){
# Status
cat("Plotting pond", target_ext[1], "for site:", prefix, "\n")

# Visualise normalised rasters in RBG
cat("Visualising normalised rasters...\n")
rast_rgb <- map(rast_files, function(x){
  # Load raster
  x <- rast(x)

  # Convert ext object
  target_ext <- ext(target_ext[-1])

  # Get year
  year <- gsub(".*([0-9]{4}).*", "\\1", x@ptr$filenames)

  # Open connetion to tempfile
  temp_file <- tempfile()
  png(temp_file, width = 271 * 2, height = 291 * 2)

  # Crop and plot raster
  x_crop <- crop(x, target_ext)
  plotRGB(x_crop, scale = 1, stretch = "linear")

  # Close connetion to tempfile
  dev.off()

  # Return plot as ggplot object with year as title and adjusted borders
  return(ggplot() +
   draw_image(temp_file) +
   labs(title = year) + 
   theme_minimal() +
   theme(axis.line.x = element_blank(),
         axis.line.y = element_blank(),
         plot.title = element_text(hjust = 0.5),
         plot.margin = unit(c(5,0,0,0), "mm"),
         axis.text = element_blank(), 
         axis.ticks.length = unit(0, "mm"),))
})

# Visualise projections with distinct colours
cat("Visualising  projection rasters...\n")
preds_plots <- map(preds_files, function(x){
  # Load raster
  x <- rast(x)

  # Convert ext object
  target_ext <- ext(target_ext[-1])
  
  # Open connection to tempfile
  temp_file <- tempfile()
  png(temp_file, width = 271 * 2, height = 291 * 2)

  # Crop and exprot plot of the projections raster
  x_crop <- crop(x, target_ext) 
  class_pallette <- class_pallette[unique(x_crop)[,1]]
  plot(x_crop,col = class_pallette,
      axes = F, mar = c(0, 0, 0, 0), legend = F)

  # Close connection to the file
  dev.off()

  # Return as ggplot object with no margins and a space holder title
  return(ggplot() +
          draw_image(temp_file) +
          labs(title = " ") + 
          theme_minimal() +
          theme(axis.line.x = element_blank(),
          axis.line.y = element_blank(),
          axis.text = element_blank(), 
          axis.ticks.length = unit(0, "mm"),
          plot.margin = unit(c(5,0,0,0), "mm")))
})

# Combine plots into a list
cat("Combining plots...\n")
plot_list <- c(rast_rgb, preds_plots)

# Determine ratio for plotting
length_x <- target_ext[3] - target_ext[2]
length_y <- target_ext[5] - target_ext[4]

# Generate grid from list and save plot
plot_grid(plotlist = plot_list,
          nrow = 2) %>%
  save_plot(paste0("figures/", prefix, "/individual_ponds/pond_",
                   target_ext[1], "_time_series.png"),
            .,
            nrow = 3,
            ncol = 6,
            base_asp = length_x / length_y + 0.2,
            bg = "white")
  
  # Return Null
  cat("Done.\n")
  return(NULL)
}

# Plot ponds
map(dtlb_ponds, plot_pond, dtlb_norm, dtlb_preds, dtlb_pallette, "dtlb")
map(cbh_ponds, plot_pond, cbh_norm, cbh_preds, cbh_pallette, "cbh")
