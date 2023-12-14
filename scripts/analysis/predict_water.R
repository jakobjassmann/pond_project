# Script to generate training data, evaluate seperability and generate predictions
# Jakob J. Assmann jakob.assmann@uzh.ch 11 May 2023

library(terra)
library(tidyverse)
library(sf)
library(caret)
library(randomForest)
library(ggplot2)
library(cowplot)
library(spatialEco)
library(dplyr)
library(purrr)
library(parallel)
library(pbapply)

# Load training polys
cbh_polys <- read_sf("data/training/cbh_training.gpkg") %>%
  mutate(., id = 1:nrow(.))
#cbh_polys[cbh_polys$year == "2019",]$year <- "2019_a" 
tlb_polys <- read_sf("data/training/tlb_training.gpkg") %>%
  mutate(., id = 1:nrow(.))
rdg_polys <- read_sf("data/training/rdg_training.gpkg") %>%
  mutate(., id = 1:nrow(.))

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
  list.files("data/drone_time_series/cbh_timeseries/focal_bcc_sd_9",
    full.names = T
  ),
  list.files("data/drone_time_series/cbh_timeseries/focal_rcc_sd_9",
    full.names = T
  ),
    list.files("data/drone_time_series/tlb_timeseries/focal_bcc_sd_9",
    full.names = T
  ),
  list.files("data/drone_time_series/tlb_timeseries/focal_rcc_sd_9",
    full.names = T
  ),
    list.files("data/drone_time_series/rdg_timeseries/focal_bcc_sd_9",
    full.names = T
  ),
  list.files("data/drone_time_series/rdg_timeseries/focal_rcc_sd_9",
    full.names = T
  )
)

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
  focal_raster_rcc <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("rcc", .)] %>%
    rast()
  focal_raster_bcc <- raster_files_focal[grepl(site_interest, raster_files_focal)] %>%
    .[grepl(year_interest, .)] %>%
    .[grepl("bcc", .)] %>%
    rast()
  
  # Subset polys for year
  polys <- get(paste0(site_interest, "_polys"))
  polys <- filter(polys, year == year_interest)
  #polys <- polys[pull(polys, paste0(year_interest)) == 1, ]
  polys$ID <- 1:nrow(polys)
  
  # Extract training data
  training_vals_norm <- terra::extract(norm_raster, vect(polys))[, 1:4]
  training_vals_focal_rcc <- terra::extract(focal_raster_rcc, vect(polys))[, 1:2]
  training_vals_focal_bcc <- terra::extract(focal_raster_bcc, vect(polys))[, 1:2]
  
  # Adjust column names of data frame
  colnames(training_vals_norm) <- c("ID", "R", "G", "B")
  colnames(training_vals_focal_rcc) <- c("ID", gsub(".*(rcc.*)\\.tif", "\\1", sources(focal_raster_rcc)))
  colnames(training_vals_focal_bcc) <- c("ID", gsub(".*(bcc.*)\\.tif", "\\1", sources(focal_raster_bcc)))

  # bind training values
  training_vals_all <- cbind(training_vals_norm, 
  select(training_vals_focal_rcc, -ID),
  select(training_vals_focal_bcc, -ID))
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

# Balance the dataset by random subsetting
set.seed(29)
training_original <- training_all 
# First cbh (10k per year and class)
training_all <- training_original %>%
  filter(site == "cbh") %>%
  group_by(year, site, class) %>%
  na.omit() %>%
  sample_n(5000)
# Second tlb (5k per year and class)
training_all <- training_original %>%
  filter(site == "tlb") %>%
  group_by(year, site, class) %>%
  na.omit() %>%
  sample_n(5000) %>%
  bind_rows(training_all)
# Third rdg (5k per year and class)
training_all <- training_original %>%
  filter(site == "rdg", class != "water") %>%
  group_by(year, site, class) %>%
  na.omit() %>%
  sample_n(5000) %>%
  bind_rows(training_all)
# Check final distribution of classes (expected: unbalanced due to rdg!)
training_all %>% group_by(class) %>% tally()
# Add all water training data from rdg
training_all <- training_original %>%
  filter(site == "rdg", class == "water") %>%
  group_by(year, site, class) %>%
  na.omit() %>%
  bind_rows(training_all)
# Check again
(training_balance <- training_all %>% group_by(class) %>% tally())
# Balance out water class using a random sample across all years and sites
# from remaining samples
training_all <- training_original %>%
  filter(!(row_id %in% training_all$row_id)) %>%
  filter(class == "water") %>%
  ungroup() %>%
  na.omit() %>%
  sample_n(training_balance$n[1] - training_balance$n[2]) %>%
  group_by(year, site, class) %>%
  bind_rows(training_all)
# Final check
(training_balance <- training_all %>% group_by(class) %>% tally())


# Add gcc and bcc to data frame
training_all <- training_all %>%
  mutate(gcc = G / (R + G + B),
         bcc = B / (R + G + B),
         rcc = R / (R + G + B)) %>%
  na.omit()

# Calculate separability
sep_stats <- bind_rows(
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(R),
    filter(training_all, class == "other") %>% ungroup() %>% select(R)
  ) %>%
    select(-R, -R.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(G),
    filter(training_all, class == "other") %>% ungroup() %>% select(G)
  ) %>%
    select(-G, -G.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(B),
    filter(training_all, class == "other") %>% ungroup() %>% select(B)
  ) %>%
    select(-B, -B.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(rcc),
    filter(training_all, class == "other") %>% ungroup() %>% select(rcc)
  ) %>%
    select(-rcc, -rcc.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(gcc),
    filter(training_all, class == "other") %>% ungroup() %>% select(gcc)
  ) %>%
    select(-gcc, -gcc.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(bcc),
    filter(training_all, class == "other") %>% ungroup() %>% select(bcc)
  ) %>%
    select(-bcc, -bcc.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(rcc_sd_9),
    filter(training_all, class == "other") %>% ungroup() %>% select(rcc_sd_9)
  ) %>%
    select(-rcc_sd_9, -rcc_sd_9.1),
  separability(
    filter(training_all, class == "water") %>% ungroup() %>% select(bcc_sd_9),
    filter(training_all, class == "other") %>% ungroup() %>% select(bcc_sd_9)
  ) %>%
    select(-bcc_sd_9, -bcc_sd_9.1)
)
sep_stats$band <- row.names(sep_stats)

# Look at group and variable relationships
plot_grid(
  ggplot(training_all) +
    geom_violin(aes(x = class, y = R, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "R"), 
              aes(x = 2.5, y = 65535, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1),
              fontface = "bold") +
    scale_y_continuous(limits = c(0,65535)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(training_all) +
    geom_violin(aes(x = class, y = G, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "G"), 
              aes(x = 2.5, y = 65535, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,65535)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(training_all) +
    geom_violin(aes(x = class, y = B, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "B"), 
              aes(x = 2.5, y = 65535, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,65535)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(training_all) +
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
  ggplot(training_all) +
    geom_violin(aes(x = class, y = gcc, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "gcc"), 
              aes(x = 2.5, y = 1, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1)) +
    scale_y_continuous(limits = c(0,1)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  ggplot(training_all) +
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
      ggplot(training_all) +
    geom_violin(aes(x = class, y = rcc_sd_9, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "rcc_sd_9"), 
              aes(x = 2.5, y = 0.3, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1,
                  fontface = "bold")) +
    scale_y_continuous(limits = c(0,0.3)) +
    theme_cowplot() +
    theme(legend.position = "none"),
      ggplot(training_all) +
    geom_violin(aes(x = class, y = bcc_sd_9, fill = class)) +
    geom_text(data = sep_stats %>% filter(band == "bcc_sd_9"), 
              aes(x = 2.5, y = 0.5, 
                  label = paste("TD =", 
                                formatC(TD, format = "f", digits = 2)),
                  hjust = 1,
                  fontface = "bold")) +
    scale_y_continuous(limits = c(0,0.5)) +
    theme_cowplot() +
    theme(legend.position = "none"),
  nrow = 2,
  labels = paste0(letters[1:8], ")")) %>%
    save_plot("figures/sepparation_all.png",
    .,
    nrow = 2,
    ncol = 4,
    base_asp = 1.2,
    bg = "white")

### Plot seperability between bands, years
plot_sep_band <- function(band = "R"){
  sep_year <- training_all %>% 
    ungroup() %>% 
    split(.$year) %>% 
    map(function(x){
      separability(filter(x, class == "water") %>% ungroup() %>% dplyr::select(!!band), 
                   filter(x, class == "other") %>% ungroup() %>% dplyr::select(!!band)) %>%
        mutate(year = unique(x$year))
    }) %>% bind_rows()
  
  training_all$band_to_plot <- pull(training_all, !!band)
  if(band %in% c("R", "G", "B")) training_all$band_to_plot <- training_all$band_to_plot / 65535
  save_plot(paste0("figures/sepparation_", band, "_year.png"),
            ggplot(training_all) +
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
c("R", "G", "B", "rcc", "gcc", "bcc", "rcc_sd_9", "bcc_sd_9") %>% map(plot_sep_band)

# Split into training and validation
training_all$id <- 1:nrow(training_all)
training <- training_all %>%
  group_by(year, class) %>%
  slice_sample(prop = 0.8)
validation <- filter(training_all, !(id %in% training$id)) 


# Train random forest model
rf_fit <- randomForest(class ~ rcc + bcc + R + rcc_sd_9 + bcc_sd_9,
             data = select(training, -id))

# Validate on test set
test_preds <- predict(rf_fit, newdata = validation,
                      type = "response")
file_connection <- file("tables/rf_confusion_matrix.txt")
confusionMatrix(data = test_preds, validation$class) %>%
  print() %>%
  capture.output() %>%
  writeLines(file_connection)
close(file_connection)

# Accuracy per year
acc_per_year <- function(year_interest){
  validation_data <- validation %>% filter(year == year_interest)
  test_preds <- predict(rf_fit, newdata = validation_data,
                        type = "response")
  data.frame(
    year = year_interest,
    accuracy = confusionMatrix(data = test_preds, validation_data$class)$overall[1],
    nsample_water = nrow(validation_data %>% filter(class == "water")),
    nsample_other = nrow(validation_data %>% filter(class == "other")))
}
map(unique(validation$year), acc_per_year) %>%
  bind_rows() %>%
  remove_rownames() %>%
  print() %>%
  write_csv("tables/rf_acc_per_year.csv")

# Accuracy by site
acc_per_site <- function(site_interest){
  validation_data <- validation %>% filter(site == site_interest)
  test_preds <- predict(rf_fit, newdata = validation_data,
                        type = "response")
  data.frame(
    site = site_interest,
    accuracy = confusionMatrix(data = test_preds, validation_data$class)$overall[1],
    nsample_water = nrow(validation_data %>% filter(class == "water")),
    nsample_other = nrow(validation_data %>% filter(class == "other")))
}
map(unique(validation$site), acc_per_site) %>%
  bind_rows() %>%
  remove_rownames() %>%
  print() %>%
  write_csv("tables/rf_acc_per_site.csv")

# Save the model
save(rf_fit, file = "data/models/rf_2023-12-14.Rda")

# Load model if needed
load("data/models/rf_2023-12-14.Rda")

# Let's see how that looks like in space
dir.create("data/drone_time_series/cbh_timeseries/preds/")
dir.create("data/drone_time_series/tlb_timeseries/preds/")
dir.create("data/drone_time_series/rdg_timeseries/preds/")

pblapply(raster_files_all, function(rast_file) {
        year_interest <- gsub(".*/([a-z]{3}_[0-9]{4}.*)\\.tif", "\\1", rast_file)
        site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rast_file)
        cat("Running projections for", site_interest, "and", year_interest, ":\n")
        cat("Preparing data...\n")
        norm_raster <- rast(rast_file)
        names(norm_raster) <- c("R", "G", "B", "alpha")

        # Calculate rcc, gcc and bcc
        rcc <- norm_raster[["R"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
        names(rcc) <- "rcc"
        gcc <- norm_raster[["G"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
        names(gcc) <- "gcc"
        bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
        names(bcc) <- "bcc"

        # Load focal rasters
        focal_bcc <- rast(paste0("data/drone_time_series/", site_interest,
                                 "_timeseries/focal_bcc_sd_9/", year_interest, 
                                 "_bcc_sd_9.tif"))
        names(focal_bcc) <- "bcc_sd_9"
        focal_rcc <- rast(paste0("data/drone_time_series/", site_interest,
                                 "_timeseries/focal_rcc_sd_9/", year_interest, 
                                 "_rcc_sd_9.tif"))
        names(focal_rcc) <- "rcc_sd_9"
        
        # Collate predictors
        predictors <- c(norm_raster, rcc, gcc, bcc, focal_bcc, focal_rcc)
        cat("Predicting values...\n")
        preds <- terra::predict(predictors, rf_fit)
        cat("Writing raster...\n")
        writeRaster(preds,
            filename = paste0("data/drone_time_series/", site_interest, "_timeseries/preds/", site_interest, "_", year_interest, "_preds.tif"),
            overwrite = T
        )
        cat(year_interest, "done.\n")
        return(NULL)
    }, cl = 31)
