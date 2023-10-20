# Generate focal neighbourhood data from nomralised drone rasters 
# Jakob J Assmann jakob.assmann(at)uzh.ch 5 October 2023

# Dependencies
library(tidyverse)
library(terra)
library(pbapply)

# Get norm raster file paths
cbh_norm <- list.files("data/drone_time_series/cbh_timeseries/norm",
                       full.names = T)
tlb_norm <- list.files("data/drone_time_series/tlb_timeseries/norm",
                       full.names = T)
rdg_norm <- list.files("data/drone_time_series/rdg_timeseries/norm",
                       full.names = T)

# Write function to generate focal raster 
generate_focal <- function(rast_file, target_stat, window){
  # Load raster
  norm_raster <- rast(rast_file)
  # Calculate rcc and bcc
  names(norm_raster) <- c("R", "G", "B", "alpha")
  rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(rcc) <- "rcc"
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  
  # Generate output dir path
  out_dir_rcc <- paste0(gsub("(.*)/norm.*", "\\1", rast_file),
                    "/focal_rcc_", target_stat, "_", window)
  out_dir_bcc <- paste0(gsub("(.*)/norm.*", "\\1", rast_file),
                        "/focal_bcc_", target_stat, "_", window)
  # Generater output dir (if needed)
  dir.create(out_dir_rcc, showWarnings = F)
  dir.create(out_dir_bcc, showWarnings = F)
  # generate focal raster and write to a file
  focal(rcc, window, fun = target_stat, na.rm = T, na.policy = "omit",
        filename = paste0(out_dir_rcc, "/",
                          gsub(".*/norm/([a-z]{3}_[0-9]{4}.*)\\.tif", "\\1", rast_file),
                          "_rcc_", target_stat, "_", window, ".tif"),
        overwrite = T)
  focal(bcc, window, fun = target_stat, na.rm = T, na.policy = "omit",
        filename = paste0(out_dir_bcc, "/",
                          gsub(".*/norm/([a-z]{3}_[0-9]{4}.*)\\.tif", "\\1", rast_file),
                          "_bcc_", target_stat, "_", window, ".tif"),
        overwrite = T)
  # Return null
  return(NULL)
}

# Generate focal rasters sd with window sizes 3 and 9
pblapply(c(cbh_norm),
         target_stat = "sd",
         window = 3,
         FUN = generate_focal,
         cl = 9)
pblapply(c(cbh_norm),
         target_stat = "sd",
         window = 9,
         FUN = generate_focal,
         cl = 9)

pblapply(c(tlb_norm),
         target_stat = "sd",
         window = 9,
         FUN = generate_focal,
         cl = 9)

pblapply(c(rdg_norm),
         target_stat = "sd",
         window = 9,
         FUN = generate_focal,
         cl = 9)
