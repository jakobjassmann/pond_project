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
  water_polys$site <- gsub(".*/(cbh|tlb|rdg)/.*", "\\1", pred_file)
  water_polys$year <- gsub(".*(20[0-9][0-9].*)_preds.*.tif", "\\1\\2", pred_file)
  
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

# Function to filter polygons within time-series based on intersection
filter_polys_intersection <- function(site_year_df){
  # dummy site and year
  # year_interest <- "2017"
  # site_interest <- "cbh"
  year_interest <- site_year_df %>% pull(year)
  site_interest <- site_year_df %>% pull(site)
  cat("\n", site_interest, year_interest, "\n")
  
  # Filter size-filtered pond polygons for site
  pond_polys_site <- pond_polys_filtered_size %>% 
    # for 2019, keep only the observation closest to the mean date of observation
    # These are cbh 2019_b and tlb 2019_c (23 July 2019)
    mutate(site_year = paste0(site, "_", year)) %>%
    filter(!(site_year %in% c("cbh_2019", "tlb_2019_a", "tlb_2019_b"))) %>%
    select(-site_year) %>%
    filter(site == site_interest) %>%
    ungroup() # remove any previous groupings, just in case
  
  # Get years in time-series and remove years different to the year of interest
  # and others in the same calendar year
  years <- unique(pond_polys_site$year)
  other_years <- years[!grepl(year_interest, years)] 
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
    # Calculate intersection of polygons for year of interest with polygons in other years
    map(function(polys){
      data.frame(id = polys_year_interest$id,
                 year = polys_year_interest$year,
                 # Check whether ther is any intersection in year being processed for any given polygon
                 intersects = apply(st_intersects(polys_year_interest, polys, sparse = F), 1, any))
    }) %>% 
    bind_rows() %>%
    # Group by id and calculate total number of years with intersections
    group_by(id) %>%
    summarise(intersections = sum(intersects)) %>%
    # Att overlaps column to polygons for year of interest
    full_join(polys_year_interest, ., by = c("id" = "id"))
  
  # Visualise for control
  # ggplot(aes(fill = as.character(intersections)), data = polys_year_filtered) +
  #   geom_sf(colour = NA) +
  #   geom_sf_text(aes(label = id)) +
  #   theme_minimal()

  # Retain only those polygons that have intersecting polygons in at least 3 other years
  polys_year_filtered <- polys_year_filtered %>% filter(intersections >= 3)
  
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

# Apply filter to pond polygons based on intersection
pond_polys_filtered_intersection <- pond_polys_filtered_size %>%
  st_drop_geometry() %>%
  distinct(site, year) %>%
  remove_rownames() %>%
  filter(site != "rdg") %>%
  # for 2019, keep only the observation closest to the mean date of observation
  # These are cbh 2019_b and tlb 2019_c (23 July 2019)
  mutate(site_year = paste0(site, "_", year)) %>%
  filter(!(site_year %in% c("cbh_2019", "tlb_2019_a", "tlb_2019_b"))) %>%
  select(-site_year) %>%
  split(., 1:nrow(.)) %>%
  pblapply(filter_polys_intersection) %>%
  bind_rows() %>%
  bind_rows(filter(pond_polys_filtered_size, site == "rdg"))

# Generate an overview table of size and number
pond_polys_filtered_intersection %>%
  st_drop_geometry() %>%
  remove_rownames() %>%
  group_by(site, year) %>% tally() %>%
  write_csv("tables/ponds_filtered_size_intersection.csv")
  
# Generate figures to document filtering
norm_rasters <- list.files("data/drone_data", "tif", full.names = T, recursive = T) %>%
  .[grepl("norm",. )]
pond_polys_filtered_intersection %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  pblapply(function(pond_polys_site_year){
    # Set site and year of interest
    site_interest <- unique(pond_polys_site_year$site)
    year_interest <- unique(pond_polys_site_year$year)
    
    # Find matching norm raster
    norm_file <- norm_rasters[grepl(site_interest, norm_rasters)] %>%
      .[grepl(year_interest, .)]
    # Load raster
    norm_rast <- rast(norm_file)
    
    # # Find matching original prediction raster
    # pred_file <- preds_rasters[grepl(site_interest, preds_rasters)] %>%
    #   .[grepl(year_interest, .)]
    # # Load raster
    # pred_rast <- rast(pred_file)
    #     # Set non-water values to NA
    # pred_rast <- classify(pred_rast,  matrix(c(0,NA,
    #                                            1,1,
    #                                            NaN, NA), nrow = 3, byrow = T))
    # Convert to categorical
    #levels(pred_rast) <- data.frame(1, "water")
    
    # Find matching ponds filtered by size only
    ponds_polys_size_only <- pond_polys_filtered_size %>%
      filter(site == site_interest, year == year_interest)
      
    # Generate plot
    filter_plot <- plot_grid(
      # Plot norm raster
      ggplot() +
        geom_spatraster_rgb(data = norm_rast, max_col_value = 65535) +
        labs(title = paste(site_interest, year_interest)) +
        theme_map() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5)),
      # Plot pond predictions
      ggplot() +
      geom_spatraster_rgb(data = norm_rast, max_col_value = 65535) +
        #geom_spatraster(data = pred_rast) +
        geom_sf(data = ponds_polys_size_only, colour ="NA", fill = "#E0C20D") +
        geom_sf(data = pond_polys_site_year, colour ="NA", fill = "magenta") +
        #scale_fill_manual(values = c("#00DEE0"), na.value = NA) +
      labs(title = "ponds: 1+ m2 (yellow) - persistent (magenta)") +
        theme_map() +
        theme(legend.position = "none",
              plot.title = element_text(hjust = 0.5)))
    
    # Save plot
    dir.create(paste0("figures/", site_interest, "/ponds_filtered/"), showWarnings = F)
    save_plot(filename = paste0("figures/", site_interest, "/ponds_filtered/", site_interest, "_", year_interest, ".png"), 
              filter_plot,
              base_asp = 2 * (ext(norm_rast)[2] - ext(norm_rast)[1]) / 
                (ext(norm_rast)[4] - ext(norm_rast)[3]), 
              base_height = 6,
              bg = "white")
    
    # Return null
    return(NULL)
    }, cl = 31) 

# Generate filtered rasters based on intersection
# dir.create("data/drone_data/cbh/preds_filtered_intersection")
# dir.create("data/drone_data/tlb/preds_filtered_intersection")
pond_polys_filtered_intersection %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  split(., .$site_year) %>%
  pblapply(function(polys_site){
    # Set site and year of interest
    site_interest <- unique(polys_site$site)
    year_interest <- unique(polys_site$year)
    # Generate raster
    site_rast <- pond_polys_to_raster(polys_site, preds_rasters)
    # Generate dir
    dir.create(paste0("data/drone_data/", site_interest, "/preds_filtered_intersection"), showWarnings = F)
    writeRaster(site_rast, 
                paste0("data/drone_data/", site_interest, "/preds_filtered_intersection/", 
                       site_interest, "_", year_interest, ".tif"), 
                overwrite = T)
    return(NULL)
  }, 
  cl = 31)

# Save polygons
dir.create("data/pond_polys")
write_sf(pond_polys_filtered_size, "data/pond_polys/pond_polys_filtered_size.gpkg")
# pond_polys_filtered_size <- read_sf("data/pond_polys/pond_polys_filtered_size.gpkg")
write_sf(pond_polys_filtered_intersection, "data/pond_polys/pond_polys_filtered_size_intersection.gpkg")

