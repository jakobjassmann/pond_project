# Quick script to assess how the different classes group in the colour space
# Jakob J. Assmann jakob.assmann@uzh.ch

library(terra)
library(tidyverse)
library(sf)
library(ggplot2)

# Load training polys
cbh_polys <- read_sf("data/training/cbh_polys.shp") 

# Load list of raster files 
raster_files_norm <- list.files("data/drone_time_series/cbh/cbh_norm/",
                           full.names = T)
raster_files_focal_mean <- list.files("data/drone_time_series/cbh/cbh_focal_mean/",
                                full.names = T)
raster_files_focal_sd <- list.files("data/drone_time_series/cbh/cbh_focal_sd/",
                                      full.names = T)

# Set years on record
years <- c(2014, 2017, 2018, 2019, 2020, 2021)

# Write quick helper function to grab pixels
get_training_vals <- function(year_interest){
  # Load rasters
  norm_raster <- rast(raster_files_norm[grepl(year_interest, raster_files_norm)])
  mean_raster <- rast(raster_files_focal_mean[grepl(year_interest, raster_files_focal_mean)])
  sd_raster <- rast(raster_files_focal_sd[grepl(year_interest, raster_files_focal_sd)])
  # Subset polys
  polys <- cbh_polys[pull(cbh_polys, paste0("X", year_interest)) == 1,]
  polys$id <- 1:nrow(polys)
  # Extract training data
  training_vals_norm <- terra::extract(norm_raster, vect(polys))[,1:4]
  training_vals_focal_mean <- terra::extract(mean_raster, vect(polys))[,2:4]
  training_vals_focal_sd <- terra::extract(sd_raster, vect(polys))[,2:4]
  colnames(training_vals_norm) <- c("id", "R", "G", "B")
  colnames(training_vals_focal_mean) <- c("R_mean", "G_mean", "B_mean")
  colnames(training_vals_focal_sd) <- c("R_sd", "G_sd", "B_sd")
  final_training <- polys %>% st_drop_geometry() %>%
    select(id, class) %>%
    full_join(bind_cols(training_vals_norm,
                        training_vals_focal_mean,
                        training_vals_focal_sd))
  return(final_training)
} 
cbh_training <- map(years, get_training_vals) %>% bind_rows()
cbhtraining <- arrange(cbh_training, class)
cbh_training$class <- as.factor(cbh_training$class)
cbh_training <- na.omit(cbh_training)
cbh_training_thin <- cbh_training %>%
group_by(class) %>%
sample_n(1000)

cbh_pcr <- prcomp(cbh_training_thin %>% ungroup() %>% select(-class), scale = TRUE,
                    center = TRUE, retx = T)
library(ggfortify)
autoplot(cbh_pcr, data = cbh_training_thin, colour = "class")
ggplot(cbh_training_thin) +
  geom_point(aes(x = G/ (B + R + G), y = B / (B + R + G), colour = class))

ggplot(cbh_training_thin) +
  geom_boxplot(aes(x = B / (B + R + G), group = class, colour = class))


sample_rast <- function(rast_file, n){
  rast_obj <- rast(rast_file)
  cell_samples <- data.frame(cell_id = sample(1:ncell(rast_obj), n))
  cell_samples$coords <- xyFromCell(rast_obj, cell_samples$cell_id)
  cell_samples$is_water <- cell_samples %>% split(.$cell_id) %>%
    map(function(x){
      coords <- x$coords
      buffer <- ext(sort(c(coords + 5, coords - 5)))
      plotRGB(crop(rast_obj, buffer))
      plot(vect(coords, crs = crs(rast_obj)), add = T)
      is_water <- -1
      while(!(as.numeric(is_water) == 1 | as.numeric(is_water) == 0)){
        is_water <- readline(prompt = "Is water? [1 or 0]:")
        if(!(as.numeric(is_water) == 1 | as.numeric(is_water) == 0)) cat("Error! Invalid entry, try again.")
      }
      return(is_water)
    }) 
  cell_samples$rast_file <- rast_file
  return(cell_samples)
}

train_2014 <- sample_rast("data/drone_time_series/cbh_2/cbh_raw/cbh_2014_rgb.tif",
n = 5)
