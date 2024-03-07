# Generate DSM difference rasters for the CBH time-series
# Jakob J. Assmann jakob.assmann@uzh.ch 29 AUgust 2022

# Dependencies
library(terra)

# Get list of files
dsm_files <- list.files("data/drone_time_series/cbh_2/cbh_dsm",
    full.names = T)

# load 2014 as the comparison raster and pop it from list
dsm_2014 <- rast(dsm_files[1])
dsm_files <- dsm_files[-1]

# Create output directors
dir.create("data/drone_time_series/cbh_2/cbh_dsm_diff")

# Calculate difference to 2014
Map(function(dsm_file){
    dsm_object <- rast(dsm_file)
    dsm_diff <- dsm_2014 - dsm_object
    writeRaster(dsm_diff, gsub(".*/(.*_[0-9]{4}).*", 
        paste0("data/drone_time_series/cbh_2/cbh_dsm_diff/", "\\1_dsm_diff.tif"),
        dsm_file),
        overwrite = T)
    return(NULL)
}, dsm_files)
