# Script to prepare the pond-time-series geometries for the Web GIS
# Jakob J. Assmann 14 May 2025 jakob.assmann@uzh.ch

# Dependencies
library(tidyverse)
library(sf)

# Load pond time-series geometeries
ts_geoms <- read_sf("data/pond_polys/pond_time_series.gpkg")
ts_geoms$url <- paste0("https://pondproject.s3.eu-central-1.amazonaws.com/pond-time-series/", ts_geoms$ts_id, ".png")

ts_geoms <- st_transform(ts_geoms, crs = 4326)
st_crs(ts_geoms)
# Export as geojson
file.remove("data/web_data/pond_time_series.geojson")
write_sf(ts_geoms, "data/web_data/pond_time_series.geojson")
