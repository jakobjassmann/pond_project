# Size filter for pond to improve classification
# Jakob J. Assmann jakob.assmann@uzh.ch 15 December 2023

# Dependencies
library(sf)
library(tidyverse)
library(terra)
library(pbapply)
library(units)
library(tidyterra)
library(ggplot2)

# Load training data
cbh_polys <- read_sf("data/training/cbh_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) 
tlb_polys <- read_sf("data/training/tlb_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) 
rdg_polys <- read_sf("data/training/rdg_training.gpkg") %>%
  mutate(., id = 1:nrow(.))
# Get area of smallest pond in training
cbh_polys$area <- cbh_polys %>% 
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
tlb_polys$area <- tlb_polys %>% 
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
rdg_polys$area <- rdg_polys %>% 
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
min(cbh_polys$area)
min(tlb_polys$area[tlb_polys$area!=0])
min(rdg_polys$area)
# => minimum area = 1m2

# Get file names to prediction rasters
preds_rasters <- c(
  list.files("data/drone_time_series/cbh_timeseries/preds", full.names = T),
  list.files("data/drone_time_series/tlb_timeseries/preds", full.names = T),
  list.files("data/drone_time_series/rdg_timeseries/preds", full.names = T))

# Write helper function to remove all water areas smaller than 1 m2
filter_size <- function(pred_raster_file){
  #pred_raster_file <- preds_rasters[2]
  
  # Load raster
  pred_raster <- rast(pred_raster_file)
  
  # Set non-water values to NA
  pred_raster <- classify(pred_raster, matrix(c(1,NA,
                                                2,1,
                                                NaN, NA), nrow = 3, byrow = T))
  
  # Convert to polygons
  water_polys <- as.polygons(pred_raster)%>% st_as_sf()
  
  # Cast multipolygon into individual polys
  water_polys <- water_polys %>% st_cast("POLYGON")
  
  # calculate area for each poylgon
  water_polys$area <-st_area(water_polys)
  
  # filter out all ponts smaller than 1 m2
  water_polys <- water_polys %>% filter(area >= set_units(5, "m^2"))
  
  # 
  
}