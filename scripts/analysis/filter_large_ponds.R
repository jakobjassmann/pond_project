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
library(cowplot)

## Identify minimum pond area in training dataset
# Load training data
cbh_polys <- read_sf("data/training_polygons/cbh_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geom))
tlb_polys <- read_sf("data/training_polygons/tlb_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))
rdg_polys <- read_sf("data/training_polygons/rdg_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))

# Get area of smallest pond in training
cbh_polys$area <- cbh_polys  %>%
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
tlb_polys$area <- tlb_polys %>%
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
rdg_polys$area <- rdg_polys %>%
  split(., 1:nrow(.)) %>% map(st_area) %>% unlist()
min(cbh_polys$area)
min(tlb_polys$area)
min(rdg_polys$area)
# => minimum area = 1m2

# Get file names to prediction rasters
preds_rasters <- c(
  list.files("data/drone_data/cbh/preds/", full.names = T),
  list.files("data/drone_data/tlb/preds/", full.names = T),
  list.files("data/drone_data/rdg/preds/", full.names = T))

# Helper function to get pond polygons
get_pond_polys <- function(pred_file, site = NA){
  #pred_file <- preds_rasters[2]

  # Load raster or reassign object for further handling if raster is provided
  if(is.character(pred_file)){
    cat(pred_file, "\n")
    pred_rast <- rast(pred_file)
    site <- gsub(".*/(cbh|tlb|rdg)/.*", "\\1", pred_file)
    year <- gsub(".*(20[0-9][0-9].*)_preds.*.tif", "\\1\\2", pred_file)
  } else if("SpatRaster" %in% class(pred_file)) {
    cat("Getting ponds for composite...\n")
    pred_rast <- pred_file
    site <- site
    year <- "composite"
  }

  # Set non-water values to NA
  pred_rast <- classify(pred_rast, matrix(c(0,NA,
                                            1,1,
                                            NaN, NA), nrow = 3, byrow = T))
  
  # Convert to polygons and cast as sf object
  water_polys <- as.polygons(pred_rast)%>% st_as_sf()
  
  # Cast multipolygon into individual polygons
  water_polys <- water_polys %>% st_cast("POLYGON")
  
  # Return NULL if empty
  if(nrow(water_polys) == 0) return(NULL)
  
  # calculate area for each polygon
  water_polys$area <-st_area(water_polys)
  
  # Add metadata colums
  water_polys$site <- site
  water_polys$year <- year
  
  # return sf object
  return(water_polys)
}

# Get polygons for all sites
pond_polys <- pblapply(preds_rasters, get_pond_polys, cl = 31) %>% bind_rows()

# Number of pond polys above a threshold
pond_polys_n_by_size <- pond_polys %>% 
  group_by(site, year) %>% 
  st_drop_geometry() %>% 
  summarise(n_total = n(), 
            larger_than_1m2 = sum(area >= set_units(1, "m^2")),
            larger_than_2m2 = sum(area >= set_units(2, "m^2")),
            larger_than_3m2 = sum(area >= set_units(3, "m^2")),
            larger_than_5m2 = sum(area >= set_units(5, "m^2")),
            larger_than_6m2 = sum(area >= set_units(6, "m^2"))) 
pond_polys_n_by_size %>% print(n= 100)
write_csv(pond_polys_n_by_size, 
          "tables/pond_polys_n_by_size.csv")

# Filter out all ponds smaller than 1 m2 (approx 500 pixels)
pond_polys_filtered_size <- pond_polys %>% filter(area >= set_units(1, "m^2"))

# Save filtered polygons to file
write_sf(pond_polys_filtered_size, "data/pond_polys/pond_polys_filtered_size.gpkg")
# pond_polys_filtered_size <- read_sf("data/pond_polys/pond_polys_filtered_size.gpkg")

# # Remove erroneous rdg data outside the area of interest
# rdg_aoi <- read_sf("data/drone_time_series/rdg_timeseries/rdg_study_aoi.shp")
# pond_polys_filtered_size <- bind_rows(
#   pond_polys_filtered_size %>% filter(site != "rdg"),
#   pond_polys_filtered_size %>% filter(site == "rdg") %>%
#     st_contains(rdg_aoi,.) %>% unlist() %>%
#     slice(pond_polys_filtered_size %>% filter(site == "rdg"), .)
# )

# Plot polygons that were kept by the filtering
pond_polys_filtered_size %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site) %>%
  lapply(function(site_polys){
    site_polys %>% 
      split(.,.$year) %>%
      lapply(function(site_year_polys){
        ggplot() +
          geom_sf(data = site_year_polys, colour ="NA", fill = "blue") +
          theme_nothing()
      }) %>%
      plot_grid(plotlist = .)
  })

# Helper function to turn polygons into raster
pond_polys_to_raster <- function(pond_polys_sf, rast_files = preds_rasters){
    # pond_polys_sf <- pond_polys %>% filter(site == "cbh", year == 2016)
  
    # Load matching raster
    pred_raster <- rast_files[grepl(unique(pond_polys_sf$site), rast_files)] %>%
      .[grepl(unique(pond_polys_sf$year), .)] %>% rast()
    
    # Convert polygons to raster
    pred_rast <- rasterize(vect(pond_polys_sf), pred_raster)
    
    # return raster
    return(pred_rast)
}

# Write out polys filtered by size (as rasters)
# Generate filtered rasters
pond_polys_filtered_size %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  pblapply(function(polys_site_year){
    # Set site and year of interest
    site_interest <- unique(polys_site_year$site)
    year_interest <- unique(polys_site_year$year)
    # Generate raster
    site_rast <- pond_polys_to_raster(polys_site_year, preds_rasters)
    # Generate dir
    dir.create(paste0("data/drone_data/", site_interest, "/preds_filtered"), showWarnings = F)
    writeRaster(site_rast, 
                paste0("data/drone_data/", site_interest, "/preds_filtered/", 
                       site_interest, "_", year_interest, "_preds_filtered.tif"), 
                overwrite = T)
    return(NULL)
  }, 
  cl = 31)

# Copy empty rasters for the rdg years without ponds
list.files("data/drone_data/rdg/preds", full.names = T) %>%
  .[!grepl("2017", .)] %>%
  file.copy(. , gsub("preds", "preds_filtered", .), overwrite = T)


### Identify pond id based on max extend across the time-series (excluding 2017)

## Calculate max area composites excluding 2017

# Prepare prediction raster meta data
preds_rasters_meta <- tibble(file = preds_rasters) %>%
  mutate(site = gsub(".*(cbh|rdg|tlb).*", "\\1", file),
         year = gsub(".*_([0-9]{4})_.*", "\\1", file)) %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  # for 2019, keep only the observation closest to the mean date of observation
  # These are cbh 2019_b and tlb 2019_c (23 July 2019)
  filter(!(site_year %in% c("cbh_2019", "tlb_2019_a", "tlb_2019_b"))) %>%
  # Arrange by site and year
  arrange(site, year) %>%
  # Remove 2017 as this is an outlier year
  filter(year != 2017) %>%
  # Exclude rdg as time-series is empty except for 2017
  filter(site != "rdg")

# Generate cross time-series composite polygons and assign ids
pond_time_series_ids <- preds_rasters_meta %>%
  split(.$site) %>%
  map(function(meta_sub){
    cat("Generating max value composite for:", unique(meta_sub$site), "\n")
    # Load rasters
    meta_sub %>% 
      pull(file) %>% 
      rast() %>% 
      # Max value composite (0,1 rasters)
      app(., max) %>%
      # Get polygons using helper function
      get_pond_polys(., site = unique(meta_sub$site)) %>%
      # Keep only ponds larger than 1 m2
      filter(area >= set_units(1, "m^2")) %>%
      # roughly arrange top-left to bottom-right corner
      mutate(min_x = st_bbox(.)[1], min_y = st_bbox(.)[2]) %>%
      arrange(min_y, min_y) %>%
      select(-min_y, -min_x) %>%
      # Add id column
      mutate(ts_id = paste0(unique(.$site), "_",
                            formatC(1:nrow(.), width = 3, flag = "0")))
  }) %>%
  bind_rows() %>% 
  # Remove max column (value of max composite raster)
  select(-max)

# Warning: terra sometimes does not recognise touching geometries
# Let's check!

# Get self-intersections
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(., intersects = st_intersects(.) %>% map(function(x) x))

# Calculate number of intersections per polygon
pond_time_series_ids$n_intersects <- pond_time_series_ids$intersects %>% 
  map(function(x) length(x)) %>% unlist()

# Calculate number of intersections large than one
sum(pond_time_series_ids$n_intersects > 1) 
# The problem is indeed the case for 184 of the identified ponds.

# Split tibble and treat ponds with intersections separately
pond_time_series_ids_unique <- pond_time_series_ids %>%
  filter(n_intersects == 1) %>%
  select(-intersects, n_intersects)
pond_time_series_ids_inter <- filter(pond_time_series_ids,
                                     n_intersects > 1)

# Sort intersections
pond_time_series_ids_inter$intersects <- pond_time_series_ids_inter$intersects %>%
  map(sort)

# Find unique ones (excluding contained of sets)
unique_interects <- unique(pond_time_series_ids_inter$intersects)

# For each unique combination, check how often it is contained in 
# all other combinations, including itself then keep only those
# that are truely uniuqe
unique_interects[
  sapply(unique_interects, function(x) {
    n_contained <- sapply(unique_interects, function(y) all(x %in% y)) %>%
    sum()
    n_contained > 1
    })] <- NULL 

# Merge remaining contained combinations
pond_time_series_ids_inter <- map(unique_interects, function(x) {
  summarise(pond_time_series_ids[unlist(x),], 
  area = sum(area),
  site = unique(site),
  year = unique(year),
  ts_id = ts_id[1])
}) %>% bind_rows()

# Re-merge with unique polygons
pond_time_series_ids <- bind_rows(pond_time_series_ids_unique,
  pond_time_series_ids_inter) %>%
  arrange(ts_id)

# Double check: 
# Get self-intersections
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(., intersects = st_intersects(.) %>% map(function(x) x))

# Calculate number of intersections per polygon
pond_time_series_ids$n_intersects <- pond_time_series_ids$intersects %>% 
  map(function(x) length(x)) %>% unlist()

# Calculate number of intersections large than one
sum(pond_time_series_ids$n_intersects > 1) 
# 2 remain!

# merge last two remaining polygons 
pond_time_series_ids <- bind_rows(
  summarise(pond_time_series_ids[which(pond_time_series_ids$n_intersects > 1),], 
    area = sum(area),
    site = unique(site),
    year = unique(year),
    ts_id = ts_id[1]),
  pond_time_series_ids[which(pond_time_series_ids$n_intersects == 1),]) %>%
  arrange(ts_id)

# Quick visual check
pond_time_series_ids %>%
  split(.$site) %>%
  map(function(x) {
                   ggplot(x) +
                     geom_sf(fill = "lightblue", colour = NA) +
                     geom_sf_text(aes(label = ts_id), size = 1) +
                     theme_map()})

## Prepare other yearly ponds for matching

# Restrict time-series analysis to one scene per year
ponds_for_time_series <- pond_polys_filtered_size %>% 
  # for 2019, keep only the observation closest to the mean date of observation
  # These are cbh 2019_b and tlb 2019_c (23 July 2019)
  mutate(site_year = paste0(site, "_", year)) %>%
  filter(!(site_year %in% c("cbh_2019", "tlb_2019_a", "tlb_2019_b"))) %>%
  select(-site_year)

# Convert year column to integer
ponds_for_time_series <- ponds_for_time_series %>%
  mutate(year = as.integer(gsub(".*([0-9]{4}).*", "\\1", year)))

# Assign ids to individual ponds
ponds_for_time_series <- ponds_for_time_series %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  map(function(ponds){
    ponds %>%
      mutate(id = paste0(unique(.$site_year), "_", formatC(1:nrow(.), width = 4, flag = "0")))
  }) %>%
  # Re combine dataframe 
  bind_rows()

# reset row names
row.names(ponds_for_time_series) <- NULL

### Determine intersecting ponds for each year
# pond_ts <- pond_time_series_ids[1,]
pond_time_series_ids <- pond_time_series_ids %>%
  split(1:nrow(.)) %>%
  pblapply(function(pond_ts){
    # Status
    cat(pond_ts$ts_id, "\n")
    # Get years in time-series
    years_in_ts <- ponds_for_time_series %>% 
      filter(site == pond_ts$site) %>% 
      pull(year) %>% 
      unique()
    # Find overlapping polygons in each year
    intersecting_ponds <- map(years_in_ts, function(current_year){
      cat(current_year, "\n")
      # Filter ponds for current year
      ponds_year <- ponds_for_time_series %>% 
        filter(year == current_year & site == pond_ts$site)
      # Get intersecting indices
      intersect_index <- st_intersects(pond_ts, ponds_year) %>%
        unlist()
      # Check whether intersection is empty
      if(length(intersect_index == 0)) intersecting_ids <- NA
      # Determine intersecting pond ids
      intersecting_ids <- ponds_year[intersect_index,]$id
      tibble(year = current_year,
             intersecting_ids = list(intersecting_ids))
    }) %>% bind_rows()
    # Tally years with intersection
    n_years <- sum(sapply(intersecting_ponds$intersecting_ids, 
                        function(x) length(x) > 0))
    combination = intersecting_ponds$intersecting_ids %>% 
      unlist(recursive = T) %>% 
      na.omit()
    # Reformat tibble and append
    pond_ts <- pivot_wider(intersecting_ponds, 
                           names_from = year, 
                           values_from = intersecting_ids) %>%
      cbind(pond_ts,.) %>%
      mutate(n_years = n_years,
             combination = list(combination))
    # Return updated pond_ts object
    return(pond_ts)
  }, cl = 31) %>% 
  bind_rows() %>% 
  # Remove time-series with less than three years of observations
  filter(n_years >= 3)

# Quick check of key stats
pond_time_series_ids %>%
  st_drop_geometry() %>%
  group_by(site) %>%
  tally()
ggplot(pond_time_series_ids) + 
  geom_histogram(aes(x = n_years), binwidth = 1) + 
  facet_wrap(~site)

# Re-assign time-series ids going from 1:n(ids)
# roughly arrange top-left to bottom-right corner
pond_time_series_ids <- pond_time_series_ids %>%
  split(.$site) %>%
  map(function(ponds){
    ponds %>%
      # Roughly arrange top-left to bottom-right
      mutate(min_x = st_bbox(.)[1], min_y = st_bbox(.)[2]) %>%
      arrange(min_y, min_y) %>%
      select(-min_y, -min_x) %>%
      # Add id column
      mutate(ts_id = paste0(unique(.$site), "_", formatC(1:nrow(.), width = 3, flag = "0")))
  }) %>% bind_rows()
    
# Write out files
save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")
write_sf(ponds_for_time_series, "data/pond_polys/ponds_for_time_series.gpkg")

# Time-series for GIS use
write_sf(select(pond_time_series_ids, ts_id, site, year, area, geometry), "data/pond_polys/pond_time_series.gpkg")

# Generate filtered rasters from time-series 
ponds_for_time_series %>%
  filter(site != "rdg") %>%
  split(., .$site_year) %>%
  pblapply(function(polys_site){
    # Set site and year of interest
    site_interest <- unique(polys_site$site)
    year_interest <- unique(polys_site$year)
    # Filter out ponds not included in time-series
    year_selector <- paste0("X", year_interest)
    ts_ids_site_year <- pond_time_series_ids %>%
      filter(site == site_interest) %>%
      pull(!!year_selector) %>%
      unlist(recursive = T)
    polys_site <- polys_site %>%
      filter(id %in% ts_ids_site_year)
    # Generate raster
    site_rast <- pond_polys_to_raster(polys_site, preds_rasters)
    # Generate dir
    dir.create(paste0("data/drone_data/", site_interest, "/preds_filtered_time_series"), showWarnings = F)
    writeRaster(site_rast,
                paste0("data/drone_data/", site_interest, "/preds_filtered_time_series/",
                       site_interest, "_", year_interest, ".tif"),
                overwrite = T)
    return(NULL)
  },
  cl = 31)


