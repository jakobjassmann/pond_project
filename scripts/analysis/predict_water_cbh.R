# Quick scrip to prep some training data
# Jakob J. Assmann jakob.assmann@uzh.ch 4 August 2022

library(terra)
library(tidyverse)
library(sf)
library(ranger)
library(caret)
library(randomForest)
library(ggplot2)
library(cowplot)
library(spatialEco)

# Load training polys
cbh_polys <- read_sf("data/training/cbh_two_class_polys.shp") 

# Load list of raster files 
raster_files_norm <- list.files("data/drone_time_series/cbh/cbh_norm/",
                           full.names = T)
#raster_files_focal_mean <- list.files("data/drone_time_series/cbh/cbh_focal_mean/",
#                                full.names = T)
#raster_files_focal_sd <- list.files("data/drone_time_series/cbh/cbh_focal_sd/",
#                                      full.names = T)

# Set years on record
years <- c(2014, 2016, 2017, 2018, 2019, 2020, 2021)

# Write quick helper function to grab training pixels for each year
get_training_vals <- function(year_interest){
  cat("Extracting", year_interest, "\n")
  # Load raster
  norm_raster <- rast(raster_files_norm[grepl(year_interest, raster_files_norm)])
  # Subset polys for year
  polys <- cbh_polys[pull(cbh_polys, paste0(year_interest)) == 1,]
  polys$ID <- 1:nrow(polys)
  # Extract training data
  training_vals_norm <- terra::extract(norm_raster, vect(polys))[,1:4]
  # Adjust column names of data frame
  colnames(training_vals_norm) <- c("ID", "R", "G", "B")
  # Combine training data with metadata (incl. class info) of the polys 
  final_training <- polys %>% st_drop_geometry() %>%
    select(ID, id, class) %>%
    full_join(training_vals_norm) %>%
        select(-ID) %>% 
    mutate(year = year_interest)
  # Return training dataset for year
  return(final_training)
} 
# Map function over years, re-arrange and turn class vector into a factor
cbh_training <- map(years, get_training_vals) %>% bind_rows()
cbh_training <- arrange(cbh_training, class)
cbh_training$class <- as.factor(cbh_training$class)

# Balance the dataset by random subsetting
set.seed(29)
cbh_training_all <- cbh_training
cbh_training <- cbh_training %>% group_by(year) %>%
    sample_n(10000)

# Add gcc and bcc to data frame
cbh_training <- cbh_training %>%
  mutate(gcc = G / (R + G + B),
         bcc = B / (R + G + B),
         rcc = R / (R + G + B))

# Calculate separability
sep_stats <- bind_rows(separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(R), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(R)) %>%
                         select(-R, -R.1),
                       separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(G), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(G)) %>%
                         select(-G, -G.1),
                       separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(B), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(B)) %>%
                         select(-B, -B.1),
                       separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(rcc), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(rcc)) %>%
                         select(-rcc, -rcc.1),
                       separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(gcc), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(gcc)) %>%
                         select(-gcc, -gcc.1),
                       separability(filter(cbh_training, class == "water") %>% ungroup() %>% select(bcc), 
                                    filter(cbh_training, class == "other") %>% ungroup() %>% select(bcc)) %>%
                         select(-bcc, -bcc.1),)
sep_stats$band <- row.names(sep_stats)

# Look at group and variable relationships
plot_grid(
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = R, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "R"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1),
              fontface = "bold") +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = G, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "G"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = B, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "B"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = rcc, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "rcc"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1,
                  fontface = "bold")) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = gcc, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "gcc"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(cbh_training) +
    geom_violin(aes(x = class, y = bcc, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "bcc"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1,
                  fontface = "bold")) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  nrow = 2,
  labels = paste0(letters[1:6], ")")) %>%
    save_plot("figures/cbh/sepparation_all.png",
    .,
    nrow = 2,
    ncol = 2,
    base_asp = 1.2,
    bg = "white")

### Plot seperability between bands and years 
plot_sep_band <- function(band = "R"){
  sep_year <- cbh_training %>% 
    ungroup() %>% 
    split(.$year) %>% 
    map(function(x){
      separability(filter(x, class == "water") %>% ungroup() %>% dplyr::select(!!band), 
                   filter(x, class == "other") %>% ungroup() %>% dplyr::select(!!band)) %>%
        mutate(year = unique(x$year))
    }) %>% bind_rows()
  
  cbh_training$band_to_plot <- pull(cbh_training, !!band)
  save_plot(paste0("figures/cbh/sepparation_", band, "_year.png"),
            ggplot(cbh_training) +
              geom_violin(aes(x = class, y = band_to_plot, fill = class)) +
              geom_text(data = sep_year, 
                        aes(x = 2.5, y = 1, 
                            label = paste("TD =", 
                                          formatC(TD, format = "f", digits = 2)),
                            hjust = 1,
                            fontface = "bold")) +
              scale_y_continuous(limits = c(0,1)) +
              labs(y = band) +
              facet_wrap(vars(as.character(year)), scales ="free") +
              theme_cowplot() +
              theme(legend.position = "none",
                    strip.background = element_rect(fill = "white")),
            bg = "white",
            base_asp = 1.3,
            base_height = 6)
  return(NULL)
}
c("R", "G", "B", "rcc", "gcc", "bcc") %>% map(plot_sep_band)

# Split into training and validation
cbh_training$id <- 1:nrow(cbh_training)
training <- cbh_training %>%
  group_by(year, class) %>%
  slice_sample(prop = 0.8)
validation <- filter(cbh_training, !(id %in% training$id)) 


# Train random forest model
rf_fit <- randomForest(class ~ rcc + bcc + R + year,
             data = select(training, -id))

# Validate on test set
test_preds <- predict(rf_fit, newdata = validation,
                      type = "response")
confusionMatrix(data = test_preds, validation$class)

# Accuracy per year
acc_per_year <- function(year_interest){
  validation_data <- validation %>% filter(year == year_interest)
  test_preds <- predict(rf_fit, newdata = validation_data,
                        type = "response")
  data.frame(
    year = year_interest,
    accuracy = confusionMatrix(data = test_preds, validation_data$class)$overall[1],
    nsample = nrow(validation_data))
}
map(years, acc_per_year) %>% bind_rows()

# Save the model
save(rf_fit, file = "data/models/cbh_rf_with2016.Rda")

# Load model if needed
#load("data/models/cbh_ranger.Rda")
# load("data/models/cbh_rf_with2016.Rda")

# Let's see how that looks like in space
dir.create("data/drone_time_series/cbh/cbh_preds/")
preds_rasters <- map(years, function(year_interest){
  cat("Running projections for", year_interest, ":\n")
  cat("Preparing data...\n")
  norm_raster <- rast(raster_files_norm[grepl(year_interest, raster_files_norm)])
  names(norm_raster) <- c("R", "G", "B", "alpha")
  # names(mean_raster) <- c("R_mean", "G_mean", "B_mean", "alpha_mean")
  # names(sd_raster) <- c("R_sd", "G_sd", "B_sd", "alpha_sd")
  # norm_raster <- crop(norm_raster, mean_raster)
  
  # Calculate rcc, gcc and bcc
  rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] +norm_raster[["B"]])
  names(rcc) <- "rcc"
  gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] +norm_raster[["B"]])
  names(gcc) <- "gcc"
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] +norm_raster[["B"]])
  names(bcc) <- "bcc"
  
  # generate year raster (as factor)
  year <- gcc
  year[] <- rep(year_interest, ncell(gcc))
  names(year) <- "year"

  # Collate predictors
  predictors <- c(norm_raster, rcc, gcc, bcc, year)
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
