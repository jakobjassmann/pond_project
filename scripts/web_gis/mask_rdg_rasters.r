# Produce transparent version of rdg rasters
# Jakob J. Assmann jakob.assmann@uzh.ch 10 May 2025

# Dependencies
library(terra)
library(sf)
library(tidyverse)

# Load rdg rasters
rdg_norm <- list.files("data/drone_data/rdg", 
    pattern = "tif",
    full.names = T,
    recursive = T) %>%
.[grepl("norm", .)]

# Load site boundary
aoi <- vect("data/drone_data/rdg/rdg_study_aoi.shp")

# Create output dir
dir.create("data/drone_data/rdg/web_gis")
# Mask rasters
map(rdg_norm, function(x){
    out_file <- gsub("norm", "web_gis", x)
    masked_rast <- mask(rast(x), aoi, filename = out_file, overwrite = T)
})
