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
cbh_polys <- read_sf("data/training/cbh_training.gpkg") %>% 
  mutate(geometry = geom) %>%
  st_drop_geometry() %>% 
  st_as_sf() %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))
#cbh_polys[cbh_polys$year == "2019",]$year <- "2019_a" 
tlb_polys <- read_sf("data/training/tlb_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))
rdg_polys <- read_sf("data/training/rdg_training.gpkg") %>%
  mutate(., id = 1:nrow(.)) %>%
  filter(!st_is_empty(geometry))

# Load list of raster files
raster_files_cbh <- list.files("data/drone_time_series/cbh_timeseries/norm/",
                               full.names = T
)
raster_files_tlb <- list.files("data/drone_time_series/tlb_timeseries/norm/",
                               full.names = T)
raster_files_rdg <- list.files("data/drone_time_series/rdg_timeseries/norm/",
                               full.names = T) %>%
  .[!(grepl("2016", .) | grepl("2019_b", .))]
raster_files_focal <- c(
  list.files("data/drone_time_series/", pattern = "tif",
             full.names = T, recursive = T) %>% .[grepl("focal_bcc",.)],
  list.files("data/drone_time_series/", pattern = "tif",
             full.names = T, recursive = T) %>% .[grepl("focal_rcc",.)]
)

# Combine norm rasters into a list
raster_files_all <- c(
  raster_files_cbh,  raster_files_tlb, raster_files_rdg
)

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
  
  # Load corresponding focal rasters
  focal_raster_rcc_sd <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("rcc", .)] %>%
    .[grepl("sd", .)] %>%
    rast()
  focal_raster_bcc_sd <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("bcc", .)] %>%
    .[grepl("sd", .)] %>%
    rast()
  focal_raster_rcc_mean <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("rcc", .)] %>%
    .[grepl("mean", .)] %>%
    rast()
  focal_raster_bcc_mean <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("bcc", .)] %>%
    .[grepl("mean", .)] %>%
    rast()
  
  # Subset polys for year
  polys <- get(paste0(site_interest, "_polys"))
  polys <- filter(polys, year == year_interest)
  #polys <- polys[pull(polys, paste0(year_interest)) == 1, ]
  polys$ID <- 1:nrow(polys)
  
  # Extract training data
  training_vals_norm <- terra::extract(norm_raster, vect(polys))[, 1:4]
  training_vals_focal_rcc_sd <- terra::extract(focal_raster_rcc_sd, vect(polys))[, 1:2]
  training_vals_focal_bcc_sd <- terra::extract(focal_raster_bcc_sd, vect(polys))[, 1:2]
  training_vals_focal_rcc_mean <- terra::extract(focal_raster_rcc_mean, vect(polys))[, 1:2]
  training_vals_focal_bcc_mean <- terra::extract(focal_raster_bcc_mean, vect(polys))[, 1:2]
  
  # Adjust column names of data frame
  colnames(training_vals_norm) <- c("ID", "R", "G", "B")
  colnames(training_vals_focal_rcc_sd) <- c("ID", gsub(".*(rcc.*)\\.tif", "\\1", sources(focal_raster_rcc_sd)))
  colnames(training_vals_focal_bcc_sd) <- c("ID", gsub(".*(bcc.*)\\.tif", "\\1", sources(focal_raster_bcc_sd)))
  colnames(training_vals_focal_rcc_mean) <- c("ID", gsub(".*(rcc.*)\\.tif", "\\1", sources(focal_raster_rcc_mean)))
  colnames(training_vals_focal_bcc_mean) <- c("ID", gsub(".*(bcc.*)\\.tif", "\\1", sources(focal_raster_bcc_mean)))
  
  # bind training values
  training_vals_all <- cbind(training_vals_norm, 
                             select(training_vals_focal_rcc_sd, -ID),
                             select(training_vals_focal_bcc_sd, -ID),
                             select(training_vals_focal_rcc_mean, -ID),
                             select(training_vals_focal_bcc_mean, -ID))
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
training_all <- pblapply(raster_files_all, get_training_vals) %>% bind_rows()
training_all <- arrange(training_all, class)
training_all$class <- as.factor(training_all$class)
training_all$row_id <- 1:nrow(training_all)

# Add gcc and bcc to data frame
training_all <- training_all %>%
  mutate(gcc = G / (R + G + B),
         bcc = B / (R + G + B),
         rcc = R / (R + G + B)) %>%
  na.omit()

# Save training data
save(training_all, file = "data/training/training_all_df.csv")