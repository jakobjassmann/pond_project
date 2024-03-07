# Quick script to min max normalize all drone bands 
# Jakob J. Assmann jakob.assmann@uzh.ch 04 August 2022

# Dependencies
library(terra)
library(tidyverse)

# Read list of raster files
raster_files <- list.files("data/drone_time_series/cbh/cbh_rgb",
                           full.names = T)

# make output dir
output_dir <- "data/drone_time_series/cbh/cbh_norm/"
dir.create(output_dir)

# Helper function to normalize raster
norm_raster <- function(raster_file){
  cat("Processing:", raster_file, "\n")
  cat("Calculating global stats...\n")
  raster_object <- rast(raster_file)
  bands_min <- global(raster_object, min)
  bands_max <- global(raster_object, max)
  cat("Normalizing Band 1 (R):\n")
  raster_object[[1]] <- (raster_object[[1]] - bands_min[1,]) / (bands_max[1,] - bands_min[1,])
  cat("Normalizing Band 2 (G):\n")
  raster_object[[2]] <- (raster_object[[2]] - bands_min[2,]) / (bands_max[2,] - bands_min[2,])
  cat("Normalizing Band 3 (B):\n")
  raster_object[[3]] <- (raster_object[[3]] - bands_min[3,]) / (bands_max[3,] - bands_min[3,])
  #cat("Cropping to aoi extent:\n")
  #raster_object <- crop(raster_object, vect("data/study_aois/cbh.shp"))
  cat("Writing raster...\n")
  writeRaster(raster_object, 
              gsub(".*/(.*_[0-9]{4}).*", 
                   paste0(output_dir, "\\1_norm.tif"), 
                   raster_file),
              overwrite = T)
  cat("Done.\n")
  return(NULL)
}

map(raster_files, norm_raster)
