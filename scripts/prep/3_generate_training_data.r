# Extract training data for water and other surfaces
# Jakob J. Assmann jakob.assmann@uzh.ch 7 March 2024

# Dependencies
library(terra)
library(tidyverse)
library(sf)
library(caret)
library(ggplot2)
library(cowplot)
library(parallel)
library(pbapply)

# Load training polys
cbh_polys <- read_sf("data/training_polygons/cbh_training.gpkg") %>% 
  mutate(geometry = geom) %>%
  st_drop_geometry() %>% 
  st_as_sf() %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))
cbh_polys[cbh_polys$year == "2019_a",]$year <- "2019" 
tlb_polys <- read_sf("data/training_polygons/tlb_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))
rdg_polys <- read_sf("data/training_polygons/rdg_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))

# Load list of raster files
raster_files_norm <- list.files("data/drone_data/", pattern = "tif",
                               full.names = T, recursive = T) %>% 
  .[grepl("norm",.)] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))]
raster_files_bcc <- list.files("data/drone_data/", pattern = "tif",
             full.names = T, recursive = T) %>% 
  .[grepl("bcc",.)] %>%
  .[!grepl("raw", .)] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))]

# Write quick helper function to grab training pixels for each year and site
get_training_vals <- function(raster_file){
  # Status
  cat("\nExtracting", raster_file, "\n")
  
  # Load raster
  norm_raster <- rast(raster_file)
  
  # Get year
  year_interest <- gsub(".*([0-9]{4}_?.*)\\..*","\\1", raster_file)
  
  # Get site
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", raster_file)
  
  # Load corresponding bcc raster
  raster_bcc <- raster_files_bcc[grepl(site_interest, raster_files_bcc)] %>%
    .[grepl(year_interest, .)] %>%
    rast()
    
  # Subset polys for year
  polys <- get(paste0(site_interest, "_polys"))
  polys <- filter(polys, year == year_interest)
  #polys <- polys[pull(polys, paste0(year_interest)) == 1, ]
  polys$ID <- 1:nrow(polys)
  
  # Extract training data
  training_vals_norm <- terra::extract(norm_raster, vect(polys))[, 1:4]
  training_vals_bcc <- terra::extract(raster_bcc, vect(polys))[, 1:2]
  
  # Adjust column names of data frame
  colnames(training_vals_norm) <- c("ID", "R", "G", "B")
  colnames(training_vals_bcc) <- c("ID", "bcc")
 
  # bind training values
  training_vals_all <- cbind(training_vals_norm, 
                             select(training_vals_bcc, -ID))
  
  # Combine training data with metadata (incl. class info) of the polys
  final_training <- polys %>%
    st_drop_geometry() %>%
    select(ID, id, class) %>%
    full_join(training_vals_all) %>%
    select(-ID) %>%
    mutate(year = year_interest) %>%
    mutate(site = site_interest)
  
  # Return training dataset for year
  return(final_training)
} 

# Map function over years, re-arrange and turn class vector into a factor
training_all <- pblapply(raster_files_norm, get_training_vals) %>% bind_rows()
training_all <- arrange(training_all, class)
training_all$class <- as.factor(training_all$class)
training_all$row_id <- 1:nrow(training_all)

# Summary stats
training_all %>% group_by(site, class) %>%
  summarise(n())

# Save training data
write_csv(training_all, file = "data/training_data/training_all_df.csv")

# Generate summary tables for supplementary materials
# Polygon summaries 
bind_rows(cbh_polys %>% st_drop_geometry() %>% group_by(year, class) %>% 
  summarise(n()) %>% pivot_wider(names_from = class, values_from = `n()`) %>%
  mutate(Site = "high", `Mosaic ID` = paste0("cbh", "_", year)) %>% 
    ungroup() %>% select(-year) %>% relocate(Site, `Mosaic ID`),
tlb_polys %>% st_drop_geometry() %>% group_by(year, class) %>% 
  summarise(n()) %>% pivot_wider(names_from = class, values_from = `n()`) %>%
  mutate(Site = "medium", `Mosaic ID` = paste0("tlb", "_", year)) %>% 
  ungroup() %>% select(-year) %>% relocate(Site, `Mosaic ID`),
rdg_polys %>% st_drop_geometry() %>% group_by(year, class) %>%
  summarise(n()) %>% pivot_wider(names_from = class, values_from = `n()`) %>%
  mutate(Site = "low", `Mosaic ID` = paste0("rdg", "_", year)) %>% 
  ungroup() %>% select(-year) %>% relocate(Site, `Mosaic ID`)) %>%
  full_join(
training_all %>% mutate(`Mosaic ID` = paste0(site, "_", year)) %>%
  group_by(`Mosaic ID`, class) %>%
  tally() %>%
  pivot_wider(names_from = class, values_from = n) %>%
  rename(other_n = other, water_n = water) ) %>% 
  write_csv("tables/training_annotations.csv")
