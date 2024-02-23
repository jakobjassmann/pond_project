# Calculate RCC and BCC (GCC if needed) rasters for all files
# Jakob J. Assmann jakob.assmann@uzh.ch 24 January 2024

library(terra)
library(dplyr)
library(pbapply)

# Create directories
dir.create("data/drone_time_series/cbh_timeseries/rcc/")
dir.create("data/drone_time_series/tlb_timeseries/rcc/")
dir.create("data/drone_time_series/rdg_timeseries/rcc/")
dir.create("data/drone_time_series/cbh_timeseries/bcc/")
dir.create("data/drone_time_series/tlb_timeseries/bcc/")
dir.create("data/drone_time_series/rdg_timeseries/bcc/")

# Get list of norm rasters to calculate rcc and bcc for
raster_files <- list.files("data/drone_time_series", pattern = ".tif", full.names = T, recursive = T) %>%
  .[grepl("norm", .)]

# Calculate rcc and bcc rasters
pblapply(raster_files, function(rast_file){
  # Load raster and set name
  norm_raster <- rast(rast_file)
  names(norm_raster) <- c("R", "G", "B", "alpha")
  
  # Calculate rcc (gcc) and bcc
  rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(rcc) <- "rcc"
  #gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  #names(gcc) <- "gcc"
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(bcc) <- "bcc"
  
  # Write out files
  writeRaster(rcc, gsub("norm", "rcc", rast_file), overwrite = T)
  writeRaster(bcc, gsub("norm", "bcc", rast_file), overwrite = T)
  
  return(NULL)
}, cl = 31)
