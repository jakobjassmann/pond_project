# Quick script to generate standardised RGB data
# Jakob J. Assmann jakob.assmann@uzh.ch 3 August 2022

# Dependencies
library(terra)
library(sf)
library(tidyverse)

# Load list of images
raster_files <- list.files(pattern = "tif$", "data/drone_time_series/cbh/cbh_original",
                           full.names = T)

# Load tiepoints
tie_points <- read_sf("data/drone_time_series/cbh/cbh_tiepoints/cbh_tiepoints.shp") %>%
  filter(grepl("tiepoint", .$name))  %>%
  mutate(x = st_coordinates(.)[,1],
         y = st_coordinates(.)[,2]) %>%
  st_drop_geometry()


# Set truths as poins in year 2021
truths <- tie_points %>% 
  filter(year == 2021) %>%
  mutate(x_new = x, y_new = y) %>%
  select(-x, -y, -year)

# make output dir
dir.create("data/drone_time_series/cbh/cbh_warped")

# Define a quick helper function to warp image based on the year
warp_with_gcps <- function(year_interest) {
  # Get path to raster file
  raster_file <- raster_files[grepl(as.character(year_interest), raster_files)]
  
  # Subset GCPs
  gcps <- tie_points %>% 
    filter(year == year_interest)
  
  # Get pixel coordinates for GCPs
  year_rast <- rast(raster_file)
  pixel_coords <- rowColFromCell(year_rast, 
                                 cellFromXY(year_rast, as.matrix(gcps[, c("x","y")])))[,c(2,1)]
  colnames(pixel_coords) <- c("x_old", "y_old")
  #pixel_coords[,1] <- ncol(year_rast) - pixel_coords[,1]  
  gcps <- bind_cols(gcps, pixel_coords)
  rm(year_rast)
  
  # Generate string of GCPs for GDAL translate 
  gcp_string <- 
    gcps %>%
    select(-x, -y, -year) %>%
    full_join(truths) %>%
    split(.$name) %>%
    map(function(x) paste(as.data.frame(select(x, -name)), collapse = " ")) %>%
    as.vector() %>%
    paste(collapse = " -gcp ") %>%
    paste("-gcp", .)
  
  # Set tempfile name
  temp_file <- gsub("(data/drone_time_series/cbh/)(.*)_original/.*",
                    paste0("\\1\\2_warped/\\2_", year_interest, "_temp.tif"),
                    raster_file)
  # Add GCPs to tempfile
  shell(paste0("C:/OSGeo4W/OSGeo4W.bat gdal_translate ",
               gcp_string, " ",
               raster_file, " ",
               temp_file))
  
  # Set final output file name
  out_file <- gsub("(data/drone_time_series/cbh/)(.*)_original/.*",
                    paste0("\\1\\2_warped/\\2_", year_interest, "_warped.tif"),
                                  raster_file)
  
  # Warp image
  shell(paste0("C:/OSGeo4W/OSGeo4W.bat gdalwarp ",
               " -tps -r near -t_srs EPSG:32655 -overwrite ",
               temp_file, " ",
               out_file))
             
  # Remove temp file
  file.remove(temp_file)
  
  return(NULL)
}

# apply to all years
c("2014",
  "2017",
  "2018",
  "2019",
  "2020") %>%
  map(warp_with_gcps)

# Read in tiepoint before and after warping
tie_points <- read_sf("data/drone_time_series/cbh/cbh_tiepoints/cbh_tiepoints.shp")
tie_points_post_warp <- read_sf("data/drone_time_series/cbh/cbh_tiepoints/cbh_tiepoints_warped.shp") 

# Double check naming is all good
tie_points %>% group_by(year) %>% tally()
tie_points_post_warp %>% group_by(year) %>% tally()
tie_points %>% group_by(name) %>% tally()
tie_points_post_warp %>% group_by(name) %>% tally()

# Calculate mean distances before and after
tie_points %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    mean(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  mean()
tie_points_post_warp %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    mean(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  mean()

# Calculate min distances before and after
tie_points %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    min(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  min()
tie_points_post_warp %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    min(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  min()

# Calculate max distances before and after
tie_points %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    max(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  max()
tie_points_post_warp %>% 
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    mean(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  max()

# Test differences of re0marking 2021
tie_points %>% 
  filter(year == 2021) %>%
  mutate(go = 1) %>%
  bind_rows(tie_points_post_warp %>% 
              filter(year == 2021) %>%
              mutate(go = 2)) %>%
  arrange(name) %>%
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, go == 1)
    tiepoints <- filter(tiepoints, go != 1)
    st_distance(point_2021, tiepoints)
  }) %>%
  unlist() %>%
  mean()
tie_points %>% 
  filter(year == 2021) %>%
  mutate(go = 1) %>%
  bind_rows(tie_points_post_warp %>% 
              filter(year == 2021) %>%
              mutate(go = 2)) %>%
  arrange(name) %>%
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, go == 1)
    tiepoints <- filter(tiepoints, go != 1)
    st_distance(point_2021, tiepoints)
  }) %>%
  unlist() %>%
  min()
tie_points %>% 
  filter(year == 2021) %>%
  mutate(go = 1) %>%
  bind_rows(tie_points_post_warp %>% 
              filter(year == 2021) %>%
              mutate(go = 2)) %>%
  arrange(name) %>%
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, go == 1)
    tiepoints <- filter(tiepoints, go != 1)
    st_distance(point_2021, tiepoints)
  }) %>%
  unlist() %>%
  max()
# => My marking accuracy is approx within 1 GSD

# Finally, check whether there was an accuracy imporvement for 2020
tie_points %>% 
  filter(year %in% c(2020, 2021)) %>%
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    mean(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  mean()
tie_points_post_warp %>% 
  filter(year %in% c(2020, 2021)) %>%
  group_by(name) %>%
  group_split() %>%
  map(function(...){
    tiepoints <- st_as_sf(...)
    point_2021 <- filter(tiepoints, year == 2021)
    tiepoints <- filter(tiepoints, year != 2021)
    mean(st_distance(point_2021, tiepoints))
  }) %>% 
  unlist() %>%
  mean()
# There was hardly an improvement, let's stick to the orginal data.

# Copy files to a new folder for processing
dir.create("data/drone_time_series/cbh/cbh_ready/")
file.copy("data/drone_time_series/cbh/cbh_original/cbh_2020.tif",
 "data/drone_time_series/cbh/cbh_ready/cbh_2020.tif")
file.copy("data/drone_time_series/cbh/cbh_original/cbh_2021.tif",
 "data/drone_time_series/cbh/cbh_ready/cbh_2021.tif")
ready_files <- list.files("data/drone_time_series/cbh/cbh_warped/")
map(ready_files, function(x) {
  file.copy(paste0("data/drone_time_series/cbh/cbh_warped/", x),
            paste0("data/drone_time_series/cbh/cbh_ready/", x))
} )
