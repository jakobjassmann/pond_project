# This script generates pond centroid locations for all ponds larger than 1 m2
# Jakob J. Assmann 25 November 2022 jakob.assmann@uzh.ch

# Dependencies
library(tidyverse)
library(terra)
library(sf)
library(parallel)
library(pbapply)
library(ggplot2)
library(cowplot)

# Get list of water rasters
preds_files <- list.files("data/drone_time_series/cbh/cbh_preds/",
                          full.names = T)

# Prepare Cluster
cl <- makeCluster(7)
clusterEvalQ(cl, library(terra))

# Polygonyze and pack 
pond_polys <- preds_files %>% 
  pblapply(function(rast_file){
    water_rast <- rast(rast_file)
    polys <- as.polygons(water_rast)
    polys <- wrap(polys)
  }, cl = cl) 
# Unpack polys and convert to sf
pond_polys <- pond_polys %>%
  map(vect) %>%
  map(st_as_sf) %>%
  map(st_cast, to = "POLYGON")

# Clean and process ponds larger than 1 m2
pond_polys <- pond_polys %>%
  map(function(ponds){
  ponds %>% 
    # Remove all non pond regions
    filter(lyr1 == 2) %>%
    # Calculate area
    mutate(area = as.numeric(st_area(.))) %>%
    # Remove all ponds with less than 1 m2
    filter(area >= 1)
})

# Create output dir
dir.create("data/drone_time_series/cbh/cbh_pond_polys")

# Save polygons
mapply(function(poly, source){
  write_sf(poly,
           gsub("(.*)cbh_preds/(cbh_[0-9]{4}_).*", "\\1cbh_pond_polys/\\2pond_polys.shp", source),
           delete_dsn = T)
},
poly = pond_polys,
source = preds_files)

# Plot pond area and number over time
pond_stats <- pond_polys %>%
  map(function(ponds){
    data.frame(area = sum(ponds$area),
               n_ponds = nrow(ponds))
  }) %>% bind_rows()
pond_stats$year <- gsub(".*([0-9]{4}).*", "\\1", preds_files)

# Generate overviewplots
plot_grid(
  # Number of ponds > 1m2
  ggplot(pond_stats) + 
    geom_line(aes(x = year, y = n_ponds, group = 1)) +
    geom_point(aes(x = year, y = n_ponds)) +
    labs( x= "", y = "Number of ponds > 1 m2") +
    theme_cowplot(),
  # Total area of ponds > 1m2
  ggplot(pond_stats) + 
    geom_line(aes(x = year, y = area, group = 1)) +
    geom_point(aes(x = year, y = area)) +
    labs( x= "", y = "Sum area of ponds > 1 m2") +
    theme_cowplot()
)

# Add year as names to pond list
years <- gsub(".*([0-9]{4}).*", "\\1", preds_files)
names(pond_polys) <- years

# Identify ponds that exist across all years
pond_polys_all_years <- map(years, function(year){
  # Split pond dataset into target year and other years
  ponds_year <- pond_polys[[year]]
  ponds_other_years <- map(years[years != year], function(x) getElement(pond_polys, x)) %>%
    set_names(years[years != year])
  # Intersect target year ponds with all other years and process into logical vector
  intersections <- map(ponds_other_years, function(ponds){
                       st_intersects(ponds_year, ponds) %>%
                         map(function(x){
                           if(length(x) >= 1){
                             TRUE
                           }
                           else if(is_empty(x)){
                             FALSE
                           } else {
                             FALSE
                           }
                         }) %>% unlist()
    })
  # Bind list of logical vectors into a data frame
  intersections <- do.call(cbind, intersections)
  # Subset ponds in target year that intersect with ponds in all other years
  ponds_intersecting <- ponds_year[rowSums(intersections) == 6,]
  return(ponds_intersecting)
})

# Save ponds
save(pond_polys_all_years, file = "data/pond_polys_all_years.Rda")

map(pond_polys_all_years, function(x) plot(st_geometry(x)))
