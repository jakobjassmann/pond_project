# Quick script to set the study AoIs for the pond project
# Jakob J. Assmann jakob.assmann@uzh.ch 2 August 2022

# Dependencies
library(tidyverse)
library(sf)

# Load geometries
drone_availability_all <- read_sf("G:/My Drive/Postdoc Zurich/Projects/Pond Project/drone_data_availability/drone_data_availability_all.shp")

# Filter out 2021 data
drone_2021 <- filter(drone_availability_all, year == 2021)

# Get lakebed centroid and add boundary to make up 300 m x 300 m
lakebed <- drone_2021 %>%
  filter(grepl("TLB.*rgb", drone_2021$name)) %>%
  st_centroid() %>%
  st_transform(crs = 32655) %>%
  st_buffer(150, endCapStyle = "SQUARE") 

plot(st_geometry(drone_2021 %>%
                   st_transform(crs = 32655)))
plot(st_geometry(lakebed),add = T, col = "red")
write_sf(lakebed, "G:/My Drive/Postdoc Zurich/Projects/Pond Project/study_aois/lakebed.shp")     

# Get cloudberry hills centroid, add 50 m in X and 100 m in Y
# then add boundary to make up 300 m x 300 m
cbh <- drone_2021 %>%
  filter(grepl("CBH.*rgb", drone_2021$name)) %>%
  st_centroid() %>%
  st_transform(crs = 32655) %>%
  st_coordinates() + c(60, 125) %>%
  matrix(nrow = 1)
cbh <- as_tibble(cbh) %>%
  st_as_sf(coords = c("X", "Y"), crs = 32655) %>%
  st_buffer(150, endCapStyle = "SQUARE")

plot(st_geometry(drone_2021 %>%
                   st_transform(crs = 32655)))
plot(st_geometry(cbh), add = T, col = "red")
write_sf(cbh, "data/study_aois/cbh.shp")

# EOF