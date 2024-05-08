# Pepare files for cloud-optimised hosting

library(tidyverse)
library(sf)
library(terra)
library(pbapply)
library(parallel)

# Prepare environment (if needed)
cl <- makeCluster(7)
clusterEvalQ(cl, library(terra))

# Make folders
dir.create("data/web_data/")
dir.create("data/web_data/cogs")

# Get norm rasters for the time-series (excluding within-year duplicates)
norm_files <- list.files("data/drone_data/", "tif", recursive = T, full.names = T) %>%
    .[grepl("norm", .)] %>%
    .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]

# Convert to 8bit RGB rasters and write to disk
norm_files[1] %>%
    pblapply(function(x) {
        norm_rast_8bit <- round(rast(x)[[-4]] / (65535 / 255))
        out_file <- gsub("(.*norm/)(.*)", "data/web_data/cogs/\\2", x)
        writeRaster(norm_rast_8bit, out_file, overwrite = TRUE)
        return(NULL)
    }, cl = cl)

# Convert to cog raster
norm_8bit <- list.files("data/web_data/cogs", full.names = TRUE)
pblapply(norm_8bit, function(source_file) {
    dest_file <- gsub("(.*[0-9]{4})(.*)(\\.tif)", "\\1_cog\\3", source_file)
    gdal_utils("warp",
        source = source_file,
        destination = dest_file,
        options = c(
            "-of", "COG",
            "-co", "TILING_SCHEME=GoogleMapsCompatible",
            "-co", "COMPRESS=DEFLATE",
            "-co", "NUM_THREADS=7"
        )
    )
    file.remove(source_file)
    return(NULL)
})

# Clean up environment
stopCluster(cl)
