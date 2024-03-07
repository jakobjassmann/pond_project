# Quick scrip to prep some training data
# Jakob J. Assmann jakob.assmann@uzh.ch 4 August 2022

library(terra)
library(tidyverse)
library(sf)
library(ranger)
library(caret)
library(randomForest)

# Load training polys
thaw_lakebed_polys <- read_sf("data/training/cbh_polys.shp") 

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
  polys <- thaw_lakebed_polys[pull(thaw_lakebed_polys, paste0("X", year_interest)) == 1,]
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
thaw_lakebed_training <- map(years, get_training_vals) %>% bind_rows()
thaw_lakebed_training <- arrange(thaw_lakebed_training, class)
thaw_lakebed_training$class <- as.factor(thaw_lakebed_training$class)
thaw_lakebed_training <- na.omit(thaw_lakebed_training)

# Split into training and validation
thaw_lakebed_training$id <- 1:nrow(thaw_lakebed_training)
training <- thaw_lakebed_training %>%
  group_by(class) %>%
  slice_sample(prop = 0.8)
validation <- filter(thaw_lakebed_training, !(id %in% training$id)) 

# # Train random forest model
# tuneGrid <- expand.grid(mtry = c(2), #:3)
#                         splitrule = c("gini"), #, "extratrees"),
#                         min.node.size = c(3)) #, 3, 5)) 
# rf_fit <- train(class ~ .,
#                 data = select(training, -id),
#                 method = "ranger",
#                 trControl = trainControl(method = "repeatedcv", 
#                                          repeats = 3, 
#                                          classProbs = TRUE, 
#                                          summaryFunction = defaultSummary),
#                 tuneGrid = tuneGrid,
#                 importance = "permutation",
#                 metric = "Accuracy")
# 
# summary(rf_fit)
# varImp(rf_fit)$importance %>% arrange(desc(Overall))
rf_fit <- randomForest(class ~ .,
             data = select(training, -id))

# Validate on test set
test_preds <- predict(rf_fit, newdata = validation,
                      type = "response")
confusionMatrix(data = test_preds, validation$class)

# Save the model
save(rf_fit, file = "data/models/cbh_ranger.Rda")

# Load model if needed
#load("data/models/cbh_ranger.Rda")

# Let's see how that looks like in space
dir.create("data/drone_time_series/cbh/cbh_preds/")
preds_rasters <- map(years, function(year_interest){
  cat("Running projections for", year_interest, ":\n")
  cat("Preparing data...\n")
  norm_raster <- rast(raster_files_norm[grepl(year_interest, raster_files_norm)])
  mean_raster <- rast(raster_files_focal_mean[grepl(year_interest, raster_files_focal_mean)])
  sd_raster <- rast(raster_files_focal_sd[grepl(year_interest, raster_files_focal_sd)])
  names(norm_raster) <- c("R", "G", "B", "alpha")
  names(mean_raster) <- c("R_mean", "G_mean", "B_mean", "alpha_mean")
  names(sd_raster) <- c("R_sd", "G_sd", "B_sd", "alpha_sd")
  norm_raster <- crop(norm_raster, mean_raster)
  predictors <- c(norm_raster, mean_raster, sd_raster)
  cat("Predicting values...\n")
  preds <-  terra::predict(predictors, rf_fit)
  cat("Writing raster...\n")
  writeRaster(preds, 
              filename =  gsub(".*/(.*_[0-9]{4}).*", 
                               "data/drone_time_series/cbh/cbh_preds/\\1_preds.tif", 
                               raster_files_norm[grepl(year_interest, raster_files_norm)]),
              overwrite = T)
  cat(year_interest, "done.\n")
  return(preds)
  })
