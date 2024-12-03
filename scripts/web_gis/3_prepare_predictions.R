## Script to generate compressed mosaics of the predictions for the web-gis
# Jakob J. Assmann 3 December 2024

library(terra)
library(tidyverse)

# Prepare directory
dir.create("data/web_data/preds")

# Generating mosaiced predicitons for 2014
preds_filtered <- list.files("data/drone_data/", "*.preds_filtered.tif", recursive = T, full.names = T)

# Load and mosaic rasters
c("2014", 2016:2021) %>%
  lapply(function(year){
    cat(year, "\n")
    preds_mosaic <- preds_filtered[grepl(year, preds_filtered)] %>%
      lapply(rast) %>%
      lapply(function(x) subst(x, from = 0, to = NA)) %>%
      do.call(terra::mosaic, .) %>%
      as.polygons() %>%
      project("epsg:4326")
    writeVector(preds_mosaic, paste0("data/web_data/preds/", year,".geojson"),
                overwrite = T)
  })
