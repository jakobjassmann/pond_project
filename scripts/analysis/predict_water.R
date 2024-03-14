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


# Load training data
training <- read_csv("data/training_data/training_all_df.csv")

## Simple BCC threshold
# Plot distribution of BCC
# (estimated threshold 5% percentile of water class)
ggplot(training) +
  geom_histogram(aes(x= bcc)) +
  theme_cowplot()
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(xintercept = quantile(training$bcc[training$class == "water"], 0.05)) +
  theme_cowplot()
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(xintercept = quantile(training$bcc[training$class == "water"], 0.05)) +
  facet_wrap(vars(site)) +
  theme_cowplot()
ggplot(training %>% mutate(site_year = paste0(site, "_", year))) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(xintercept = quantile(training$bcc[training$class == "water"], 0.05)) +
  facet_wrap(vars(site_year)) +
  theme_cowplot()
# -> a global threshold will not work well!!

# Check whether class is a factor (if not adjust)
if(!is.factor(training$class)) training$class <- factor(training$class, 
                                                        levels = c("other", "water"))
levels(training$class)

# Generate vector of thresholds values to be tested
threshold_vals <- seq(0,1,0.01)

# split training data by site
training_cbh <- filter(training, site == "cbh")
training_tlb <- filter(training, site == "tlb")
training_rdg <- filter(training, site == "rdg")

# Helper function to generate predictions for a set of thresholds
gen_preds_thres <- function(threshold_val, training_data = training) {
  preds <- data.frame(
    threshold_val = threshold_val,
    class = training_data$bcc >= threshold_val
  ) %>%
    mutate(class = factor(case_when(class == 1 ~ "water",
                                    class == 0 ~ "other"), 
                          levels = c("other", "water")))
  
  cf_matrix <- confusionMatrix(preds$class, training_data$class, positive = "water")
  data.frame(
    threshold = threshold_val, 
    acc = cf_matrix$overall["Accuracy"],
    fpr = 1 - cf_matrix$byClass["Specificity"],
    sens = cf_matrix$byClass["Sensitivity"],
    spec = cf_matrix$byClass["Specificity"],
    sens_spec = cf_matrix$byClass["Specificity"] + cf_matrix$byClass["Sensitivity"]
  )
}

## Generate predictions for range of thresholds 

# Generate predictions - global threshold
threshold_preds_all <- pblapply(threshold_vals, 
                                gen_preds_thres, 
                                cl = 31) %>% 
  bind_rows()

# Generate predictions - cbh threshold 
threshold_preds_cbh <- pblapply(threshold_vals, 
                                gen_preds_thres, 
                                training_data = training_cbh, 
                                cl = 31) %>% 
  bind_rows()

# Generate predictions - tlb threshold 
threshold_preds_tlb <- pblapply(threshold_vals, gen_preds_thres, training_data = training_tlb, cl = 31) %>% bind_rows()

# Generate predictions - rdg threshold 
threshold_preds_rdg <- pblapply(threshold_vals, gen_preds_thres, training_data = training_rdg, cl = 31) %>% bind_rows()

# Generate predictions - each raster individually
thresholds_site <- training %>% 
  mutate(site_year = paste0(site,"_",year)) %>%
  split(., .$site_year) %>%
  lapply(function(training_data_site){
    cat("Generating predictions for", unique(training_data_site$site_year), "\n")
    if(nrow(training_data_site %>% filter(class == "water")) == 0) threshold_vals <- 1
    pblapply(threshold_vals, gen_preds_thres, training_data = training_data_site, cl = 31) %>% 
      bind_rows() %>%
      mutate(site_year = unique(training_data_site$site_year))
  }) %>% 
  bind_rows() %>%
  na.omit()

## Assess performance
# Get threshold with highest accuracy
best_thresh_all <- threshold_preds_all[threshold_preds_all$acc == max(threshold_preds_all$acc),][1,] 
best_thresh_cbh <- threshold_preds_cbh[threshold_preds_cbh$acc == max(threshold_preds_cbh$acc),][1,] %>% mutate (site = "cbh")
best_thresh_tlb <- threshold_preds_tlb[threshold_preds_tlb$acc == max(threshold_preds_tlb$acc),][1,] %>% mutate (site = "tlb")
best_thresh_rdg <-  threshold_preds_rdg[threshold_preds_rdg$acc == max(threshold_preds_rdg$acc),][1,] %>% mutate (site = "rdg")
best_thresh_site_year <- thresholds_site %>%
  split(., .$site_year) %>%
  lapply(., function(x){ 
    x[x$acc == max(x$acc),][1,]
  }) %>%
  bind_rows()
best_thresh_site_year <- bind_rows(
  best_thresh_site_year,
  training %>% 
    mutate(site_year = paste0(site,"_",year)) %>%
    distinct(site_year) %>%
    filter(grepl("rdg", site_year)) %>%
    filter(!grepl("2017", site_year)) %>%
    mutate(threshold = 1)
) %>% arrange(site_year)

## Plot distributions and thresholds
# Global threshold
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = threshold), data = best_thresh_all) +
  geom_text(aes(x = threshold + 0.1, y = -0.5, label = threshold), 
            data = best_thresh_all) +
  facet_wrap(vars(site)) +
  theme_cowplot()
# Does not worke well -> as expected

# Site specific threshold
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = threshold), data = bind_rows(best_thresh_cbh, best_thresh_rdg, best_thresh_tlb)) +
  geom_text(aes(x = threshold + 0.1, y = -0.5, label = threshold), 
            data = bind_rows(best_thresh_cbh, best_thresh_rdg, best_thresh_tlb)) +
  facet_wrap(vars(site)) +
  theme_cowplot()
# Loks better but not great either 

# Site-year specific threhold
(bcc_norm_with_thresh_plot <- ggplot(training %>% mutate(site_year = paste0(site,"_", year))) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = threshold), data = best_thresh_site_year) +
  geom_text(aes(x = threshold - 0.07, y = -Inf, label = round(threshold,2)), 
            data = best_thresh_site_year, vjust = -0.5) +
  facet_wrap(vars(site_year), scales = "free") +
  scale_colour_manual(values = c("#C00000", "#00B0F0")) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(expand = c(0.2,0.2)) +
  labs(x = "bcc", y = "density") +
  theme_cowplot() +  
  theme(strip.background = element_rect(fill = NA)) )
save_plot("figures/bcc_all_norm_sep_with_thresh.png", bcc_norm_with_thresh_plot,
          base_height = 12, base_asp = 16/9, bg = "white")

# Plot ROC curves
(roc_curves_site <- ggplot(thresholds_site) +
  geom_line(aes(x = fpr, y = sens)) +
  geom_point(aes(x= fpr, y = sens), color = "red", data = best_thresh_site_year, size = 3) +
  facet_wrap(vars(site_year), scales = "free") +
  labs(x = "false positive rate", y = "sensitivity") +
  theme_cowplot() +
  theme(strip.background = element_rect(fill = NA)))
save_plot("figures/roc_curves_site.png", roc_curves_site,
          base_height = 12, base_asp = 16/9, bg = "white")

# Write out threshold table
best_thresh_site_year %>%
  mutate(site = gsub(".*(cbh|tlb|rdg).*", "\\1", site_year),
         year = gsub(".*[a-z]{3}_([0-9]{4}.*)", "\\1", site_year)) %>%
  select(-site_year) %>%
  mutate(across(acc:sens_spec, ~ round(.x, 2))) %>%
  remove_rownames() %>%
  write_csv("tables/bcc_thersholds_site.csv")

## Generate projections based on threshold

# Get list of bcc raster files
raster_files_bcc <- list.files("data/drone_data/", pattern = "tif",
                               full.names = T, recursive = T) %>% 
  .[grepl("bcc",.)] %>%
  .[!grepl("raw", .)] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))]

# Prepare folders
dir.create("data/drone_data/cbh/preds/")
dir.create("data/drone_data/tlb/preds/")
dir.create("data/drone_data/rdg/preds/")

# Generate prediction rasters
pblapply(raster_files_bcc, function(rast_file) {
  year_interest <- gsub(".*/[a-z]{3}_([0-9]{4}.*)\\.tif", "\\1", rast_file)
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rast_file)
  cat("Running projections for", site_interest, "and", year_interest, ":\n")
  cat("Preparing data...\n")
  
  # Load data
  bcc <- rast(rast_file)
  names(bcc) <- "bcc"
  
  # If site is rdg -> mask NA values outside the aoi
  if(site_interest == "rdg") {
    norm_raster <- list.files("data/drone_data/", pattern = "\\.tif", 
                            full.names = T, recursive = T) %>%
      .[grepl("norm", .)] %>%
      .[grepl(site_interest, .)] %>%
      .[grepl(year_interest, .)] %>%
      rast()
    bcc <- mask(bcc, norm_raster[[4]], maskvalues = 0)
  }
  
  cat("Predicting values...\n")
  best_thresh <- best_thresh_site_year %>%
    filter(site_year == paste0(site_interest, "_", year_interest))
  threshold <- best_thresh$threshold
  
  # Calculate predictions
  preds <- bcc >= threshold
  
  cat("Writing raster...\n")
  writeRaster(preds,
              filename = paste0("data/drone_data/", site_interest, 
                                "/preds/", 
                                site_interest, "_", year_interest, 
                                "_preds.tif"),
              overwrite = T
  )
  cat(year_interest, "done.\n")
  return(NULL)
}, cl = 31)
