# Calculate BCC and GCC rasters for all files
# Jakob J. Assmann jakob.assmann@uzh.ch 24 January 2024

# Dependencies
library(terra)
library(dplyr)
library(pbapply)
library(parallel)

# Perpare parallel envrionment
# On Windows
# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, {
#   library(terra)
#   library(dplyr)
# })
# On Unix
cl <- detectCores() - 1

# Create directories for normalised bcc and gcc
dir.create("data/drone_data/cbh/bcc/")
dir.create("data/drone_data/tlb/bcc/")
dir.create("data/drone_data/rdg/bcc/")
dir.create("data/drone_data/cbh/gcc/")
dir.create("data/drone_data/tlb/gcc/")
dir.create("data/drone_data/rdg/gcc/")
# Create directories for raw bcc
dir.create("data/drone_data/cbh/bcc_raw/")
dir.create("data/drone_data/tlb/bcc_raw/")
dir.create("data/drone_data/rdg/bcc_raw/")

# Get list of norm rasters to calculate bcc for
raster_files <- list.files("data/drone_data", pattern = ".tif", full.names = T, recursive = T) %>%
  .[grepl("norm", .)]

# Calculate bcc rasters
pblapply(raster_files, function(rast_file) {
  # Load raster and set name
  norm_raster <- rast(rast_file)
  names(norm_raster) <- c("R", "G", "B", "alpha")

  # Calculate bcc
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(bcc) <- "bcc"

  writeRaster(bcc, gsub("norm", "bcc", rast_file), overwrite = T)

  return(NULL)
}, cl = cl)

# Calculate gcc rasters
pblapply(raster_files, function(rast_file) {
  # Load raster and set name
  norm_raster <- rast(rast_file)
  names(norm_raster) <- c("R", "G", "B", "alpha")

  # Calculate gcc
  gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(gcc) <- "gcc"

  # Write out file
  writeRaster(gcc, gsub("norm", "gcc", rast_file), overwrite = T)

  return(NULL)
}, cl = cl)

# Get list of raw raster to calcualte bcc for
raster_files <- list.files("data/drone_data", pattern = ".tif", full.names = T, recursive = T) %>%
  .[grepl("rgb.*\\.tif$", .)] %>%
  .[!grepl("native", .)]

# Calculate bcc rasters
pblapply(raster_files, function(rast_file) {
  # Load raster and set name
  rgb_raster <- rast(rast_file)
  names(rgb_raster) <- c("R", "G", "B", "alpha")

  # Calculate rcc (gcc) and bcc
  # rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  # names(rcc) <- "rcc"
  # gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  # names(gcc) <- "gcc"

  bcc <- rgb_raster[["B"]] / (rgb_raster[["R"]] + rgb_raster[["G"]] + rgb_raster[["B"]])
  names(bcc) <- "bcc"

  # Write out files
  # writeRaster(rcc, gsub("norm", "rcc", rast_file), overwrite = T)
  writeRaster(bcc, gsub("rgb", "bcc_raw", rast_file), overwrite = T)

  return(NULL)
}, cl = cl)

# Shut down cluster on Windows
# stopCluster(cl)