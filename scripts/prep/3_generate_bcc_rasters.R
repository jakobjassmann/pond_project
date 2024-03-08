# Calculate RCC and BCC (GCC if needed) rasters for all files
# Jakob J. Assmann jakob.assmann@uzh.ch 24 January 2024

library(terra)
library(dplyr)
library(pbapply)

# Create directories for normalised bcc
dir.create("data/drone_data/cbh/bcc/")
dir.create("data/drone_data/tlb/bcc/")
dir.create("data/drone_data/rdg/bcc/")
# Create directories for raw bcc
dir.create("data/drone_data/cbh/bcc_raw/")
dir.create("data/drone_data/tlb/bcc_raw/")
dir.create("data/drone_data/rdg/bcc_raw/")

# Get list of norm rasters to calculate bcc for
raster_files <- list.files("data/drone_data", pattern = ".tif", full.names = T, recursive = T) %>%
  .[grepl("norm", .)]

# Calculate bcc rasters
pblapply(raster_files, function(rast_file){
  # Load raster and set name
  norm_raster <- rast(rast_file)
  names(norm_raster) <- c("R", "G", "B", "alpha")
  
  # Calculate rcc (gcc) and bcc
  #rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  #names(rcc) <- "rcc"
  #gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  #names(gcc) <- "gcc"
  
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(bcc) <- "bcc"
  
  # Write out files
  #writeRaster(rcc, gsub("norm", "rcc", rast_file), overwrite = T)
  writeRaster(bcc, gsub("norm", "bcc", rast_file), overwrite = T)
  
  return(NULL)
}, cl = 7)

# Get list of raw raster to calcualte bcc for
raster_files <- list.files("data/drone_data", pattern = ".tif", full.names = T, recursive = T) %>%
  .[grepl("rgb.*\\.tif$", .)] %>%
  .[!grepl("native", .)]

# Calculate bcc rasters
pblapply(raster_files, function(rast_file){
  # Load raster and set name
  rgb_raster <- rast(rast_file)
  names(rgb_raster) <- c("R", "G", "B", "alpha")
  
  # Calculate rcc (gcc) and bcc
  #rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  #names(rcc) <- "rcc"
  #gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  #names(gcc) <- "gcc"
  
  bcc <- rgb_raster[["B"]] / (rgb_raster[["R"]] + rgb_raster[["G"]] + rgb_raster[["B"]])
  names(bcc) <- "bcc"
  
  # Write out files
  #writeRaster(rcc, gsub("norm", "rcc", rast_file), overwrite = T)
  writeRaster(bcc, gsub("rgb", "bcc_raw", rast_file), overwrite = T)
  
  return(NULL)
}, cl = 7)
