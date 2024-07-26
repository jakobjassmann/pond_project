# Quick script to summarise area of interest statistics
# Jakob J. Assmann jakob.assmann@uzh.ch 10 July 2024

# Dependencies
library(tidyverse)
library(sf)
library(gt)

# load aois
cbh <- read_sf("data/drone_data/cbh/cbh_study_aoi.shp")
tlb <- read_sf("data/drone_data/tlb/tlb_study_aoi.shp")
rdg <- read_sf("data/drone_data/rdg/rdg_study_aoi.shp")

# Corner point coordinates
cbh %>% st_transform(4326) %>% st_coordinates() %>% data.frame() %>% gt()
tlb %>% st_transform(4326) %>% st_coordinates() %>% data.frame() %>% gt()
rdg %>% st_transform(4326) %>% st_coordinates() %>% data.frame() %>% gt()

# bbox dimension: x
st_bbox(cbh)["xmax"] - st_bbox(cbh)["xmin"]
st_bbox(tlb)["xmax"] - st_bbox(tlb)["xmin"]
st_bbox(rdg)["xmax"] - st_bbox(rdg)["xmin"]

# bbox dimension: y
st_bbox(cbh)["ymax"] - st_bbox(cbh)["ymin"]
st_bbox(tlb)["ymax"] - st_bbox(tlb)["ymin"]
st_bbox(rdg)["ymax"] - st_bbox(rdg)["ymin"]


# Area
st_area(cbh) / 10000 # high
st_area(tlb) / 10000 # med
st_area(rdg) / 10000 # low

# Centroid
st_centroid(st_transform(cbh, 4326)) %>% st_geometry() # high
st_centroid(st_transform(tlb, 4326)) %>% st_geometry() # med
st_centroid(st_transform(rdg, 4326)) %>% st_geometry() # low

            