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
  list.files("data/drone_time_series/cbh_timeseries/preds_thresh/", full.names = T),
  list.files("data/drone_time_series/tlb_timeseries/preds_thresh/", full.names = T),
  list.files("data/drone_time_series/rdg_timeseries/preds_thresh/", full.names = T))

# # Apply majority filter to reduce noise in predictions
# dir.create("data/drone_time_series/cbh_timeseries/preds_filtered/")
# dir.create("data/drone_time_series/tlb_timeseries/preds_filtered/")
# dir.create("data/drone_time_series/rdg_timeseries/preds_filtered/")
# 
# pblapply(preds_rasters, function(pred_file){
#   year_interest <- gsub(".*/[a-z]{3}_[a-z]{3}_([0-9]{4}.*)_preds\\.tif", "\\1", pred_file)
#   site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", pred_file)
#   cat("Majority filter for predictions from:", site_interest, "and", year_interest, "\n")
#   pred_rast <- rast(pred_file)
#   pred_filterd <- focal(pred_rast, 
#                         w = 3, 
#                         fun = "modal",
#                         na.policy = "omit",
#                         na.remove = T)
#   cat("Writing raster...\n")
#   writeRaster(pred_filterd,
#               filename = paste0("data/drone_time_series/", site_interest, "_timeseries/preds_filtered/", site_interest, "_", year_interest, "_preds_filtered.tif"),
#               overwrite = T
#   )
#   return(NULL)
# }, cl = 31)
# 
# 
# # Get file names for filtered rasters
# preds_rasters_filtered <- c(
#   list.files("data/drone_time_series/cbh_timeseries/preds_filtered", full.names = T),
#   list.files("data/drone_time_series/tlb_timeseries/preds_filtered", full.names = T),
#   list.files("data/drone_time_series/rdg_timeseries/preds_filtered", full.names = T))
# 
# # Plot rasters for comparison
# plot(rast(preds_rasters[2]))
# plot(rast(preds_rasters_filtered[2]))
# plot(crop(rast(preds_rasters[2]), ext(517519,517558,7858823,7858859))) 
# plot(crop(rast(preds_rasters_filtered[2]), ext(517519,517558,7858823,7858859)))


# Helper funciton to get pond polygons
get_pond_polys <- function(pred_file){
  #pred_file <- preds_rasters[2]
  cat(pred_file, "\n")
  
  # Load raster
  pred_rast <- rast(pred_file)
  
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
  water_polys$site <- gsub(".*/(cbh|tlb|rdg)_timeseries/.*", "\\1", pred_file)
  water_polys$year <- gsub(".*(20[0-9][0-9].*)_preds.*.tif", "\\1\\2", pred_file)
  
  # return sf object
  return(water_polys)
}

# Get polygons for all sites
pond_polys <- pblapply(preds_rasters, get_pond_polys, cl = 31) %>% bind_rows()

# Number of pond polys above a threshold
pond_polys %>% 
  group_by(site, year) %>% 
  st_drop_geometry() %>% 
  summarise(n = n(), 
            more_than_1m2 = sum(area >= set_units(1, "m^2")),
            more_than_2m2 = sum(area >= set_units(2, "m^2")),
            more_than_3m2 = sum(area >= set_units(3, "m^2")),
            more_than_5m2 = sum(area >= set_units(5, "m^2")),
            more_than_6m2 = sum(area >= set_units(6, "m^2"))) %>% print(n= 100)

# Filter out all ponds smaller than 1 m2 (approx 500 pixels)
pond_polys_filtered_size <- pond_polys %>% filter(area >= set_units(1, "m^2"))

# Remove erroneous rdg data outside the area of interest
rdg_aoi <- read_sf("data/drone_time_series/rdg_timeseries/rdg_study_aoi.shp")
pond_polys_filtered_size <- bind_rows(
  pond_polys_filtered_size %>% filter(site != "rdg"),
  pond_polys_filtered_size %>% filter(site == "rdg") %>%
    st_contains(rdg_aoi,.) %>% unlist() %>%
    slice(pond_polys_filtered_size %>% filter(site == "rdg"), .)
)
# Helper function to plot polygons that were filtered
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
  pblapply(function(polys_site){
    # Set site and year of interest
    site_interest <- unique(polys_site$site)
    year_interest <- unique(polys_site$year)
    # Generate raster
    site_rast <- pond_polys_to_raster(polys_site, preds_rasters)
    # Generate dir
    dir.create(paste0("data/drone_time_series/", site_interest, "_timeseries/preds_filtered"), showWarnings = F)
    writeRaster(site_rast, 
                paste0("data/drone_time_series/", site_interest, "_timeseries/preds_filtered/", 
                       site_interest, "_", year_interest, ".tif"), 
                overwrite = T)
    return(NULL)
  }, 
  cl = 31)

# Write function to filter polygons within time-series
filter_polys_overlap <- function(site_year_df){
  # dummy site and year
  # year_interest <- "2019_a"
  # site_interest <- "tlb"
  year_interest <- site_year_df %>% pull(year)
  calendar_year <- gsub(".*([0-9]{4}).*", "\\1", year_interest)
  site_interest <- site_year_df %>% pull(site)
  cat("\n", site_interest, year_interest, "\n")
  
  # Filter size-filtered pond polygons for site
  pond_polys_site <- pond_polys_filtered_size %>% 
    filter(site == site_interest) %>%
    ungroup() # remove any previous groupings, just in case
  
  # Get years in time-series and remove years different to the year of interest
  # and others in the same calendar year
  years <- unique(pond_polys_site$year)
  other_years <- years[!grepl(calendar_year, years)] 
    #%>% .[!grepl("2017",.)] # remove 2017
  
  # Pull out polygons for year of interest
  polys_year_interest <- pond_polys_site %>% 
    filter(year == year_interest) %>% 
    mutate(id = 1:nrow(.)) # add id
  
  # Visual check
  # ggplot(polys_year_interest) + 
  #   geom_sf(aes(fill = as.character(id)), colour = NA) + 
  #   theme_minimal() +
  #   theme(legend.position = "none")
  
  # Determine overlap of pond polygons for year of interest with other years
  polys_year_filtered <- pond_polys_site %>% # Filter out polygons for year of interest
    filter((year %in% other_years)) %>%
    group_by(year) %>% # Group and split other years
    group_split() %>% 
    # Calculate overlap of polygons for year of interest with polygons in other years
    map(function(polys){
      data.frame(id = polys_year_interest$id,
                 year = polys_year_interest$year,
                 overlaps = apply(st_overlaps(polys_year_interest, polys, sparse = F), 1, any))
    }) %>% 
    bind_rows() %>%
    # Group by id and calculate total number of years with overlap
    group_by(id) %>%
    summarise(overlaps = sum(overlaps)) %>%
    # Att overlaps column to polygons for year of interest
    full_join(polys_year_interest, ., by = c("id" = "id"))
  
  # Visualise for control
  # ggplot(aes(fill = as.character(overlaps)), data = polys_year_filtered) +
  #   geom_sf(colour = NA) +
  #   theme_minimal()

  # Retain only those polygons that have overlapping polygons in at least 3 other years
  polys_year_filtered <- polys_year_filtered %>% filter(overlaps >= 3)
  
  # Visualise for control
  # ggplot(aes(fill = as.character(overlaps)), data = polys_year_filtered) +
  #   geom_sf(colour = NA) +
  #   theme_minimal()
   
  # Clean up metadata and return filtered ponds
  polys_year_filtered %>%
    select(-bcc) %>%
    mutate(id = 1:nrow(.)) %>%
    return()
}

# Apply filter to pond polygons based on overlap
pond_polys_filtered_overlap <- pond_polys_filtered_size %>%
  st_drop_geometry() %>%
  distinct(site, year) %>%
  remove_rownames() %>%
  filter(site != "rdg") %>%
  split(., 1:nrow(.)) %>%
  pblapply(filter_polys_overlap) %>%
  bind_rows() %>%
  bind_rows(filter(pond_polys_filtered_size, site == "rdg"))
  
# Generate figures to document filtering
pond_polys_filtered_overlap %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  pblapply(function(pond_polys){
    # Set site and year of interest
    site_interest <- unique(pond_polys$site)
    year_interest <- unique(pond_polys$year)
    
    # Find matching original prediction raster
    pred_file <- preds_rasters[grepl(site_interest, preds_rasters)] %>%
      .[grepl(year_interest, .)]
    # Load raster
    pred_rast <- rast(pred_file)
        # Set non-water values to NA
    pred_rast <- classify(pred_rast,  matrix(c(0,NA,
                                               1,1,
                                               NaN, NA), nrow = 3, byrow = T))
    # Convert to categorical
    levels(pred_rast) <- data.frame(1, "water")
    
    # Find matching ponds filtered by size only
    ponds_polys_size_only <- pond_polys_filtered_size %>%
      filter(site == site_interest, year == year_interest)
      
    # Generate plot
    filter_plot <- ggplot() +
        geom_spatraster(data = pred_rast) +
        geom_sf(data = ponds_polys_size_only, colour ="NA", fill = "blue") +
        geom_sf(data = pond_polys, colour ="NA", fill = "darkblue") +
        scale_fill_manual(values = c("red"), na.value = NA) +
      labs(title = paste(site_interest, year_interest)) +
        theme_map() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5))
    
    # Save plot
    dir.create(paste0("figures/", site_interest, "/filtered/"), showWarnings = F)
    save_plot(filename = paste0("figures/", site_interest, "/filtered/", site_interest, "_", year_interest, ".png"), 
              filter_plot,
              base_asp = 1.2, 
              bg = "white")
    
    # Return null
    return(NULL)
    }, cl = 31) 

# Generate filtered rasters
pond_polys_filtered_overlap %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  pblapply(function(polys_site){
    # Set site and year of interest
    site_interest <- unique(polys_site$site)
    year_interest <- unique(polys_site$year)
    # Generate raster
    site_rast <- pond_polys_to_raster(polys_site, preds_rasters)
    # Generate dir
    dir.create(paste0("data/drone_time_series/", site_interest, "_timeseries/preds_filtered_overlap"), showWarnings = F)
    writeRaster(site_rast, 
                paste0("data/drone_time_series/", site_interest, "_timeseries/preds_filtered_overlap/", 
                       site_interest, "_", year_interest, ".tif"), 
                overwrite = T)
    return(NULL)
  }, 
  cl = 31)

# Save polygons
dir.create("data/pond_polys")
write_sf(pond_polys_filtered_size, "data/pond_polys/pond_polys_filtered_size.gpkg")
write_sf(pond_polys_filtered_overlap, "data/pond_polys/pond_polys_filtered_size_overlap.gpkg")
