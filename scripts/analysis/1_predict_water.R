# Script to generate training data, evaluate seperability and generate predictions
# Jakob J. Assmann jakob.assmann@uzh.ch 11 May 2023

library(terra)
library(tidyverse)
library(sf)
library(caret)
library(ggplot2)
library(cowplot)
library(dplyr)
library(purrr)
library(parallel)
library(pbapply)

## Prepare training data

# Load training data
training <- read_csv("data/training_data/training_all_df.csv")

# Check whether class is a factor (if not adjust)
if(!is.factor(training$class)) training$class <- factor(training$class, 
                                                        levels = c("other", "water"))
levels(training$class)

## Test simple BCC threshold (running not required for remainder of script)
# Plot distribution of BCC & estimated threshold 5% percentile of water class
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


## Identifiy BCC threshold values

# Generate vector of thresholds values to be tested
threshold_vals <- seq(0,1,0.01)

# split training data by site
training_cbh <- filter(training, site == "cbh")
training_tlb <- filter(training, site == "tlb")
training_rdg <- filter(training, site == "rdg")

# Helper function to generate predictions for a set of thresholds
gen_preds_thres <- function(threshold_val, training_data = training, cv = FALSE) {
  # Predictions based on threshold using ALL data
  preds <- data.frame(
    threshold_val = threshold_val,
    class = training_data$bcc >= threshold_val
  ) %>%
    # Label classes
    mutate(class = factor(case_when(class == 1 ~ "water",
                                    class == 0 ~ "other"), 
                          levels = c("other", "water")))
  
  # Determine confusion matrix
  cf_matrix <- confusionMatrix(preds$class, training_data$class, positive = "water")
  
  # Format results
  results <- data.frame(
    threshold = threshold_val, 
    acc = cf_matrix$overall["Accuracy"],
    fpr = 1 - cf_matrix$byClass["Specificity"],
    sens = cf_matrix$byClass["Sensitivity"],
    spec = cf_matrix$byClass["Specificity"],
    sens_spec = cf_matrix$byClass["Specificity"] + cf_matrix$byClass["Sensitivity"]
  )
  
  # If there is only rdg data (not enought water obs) do not continue
  if(all(training_data$site == "rdg")) return(results)
  
  # For all other sites - check whether to run CV?
  if(!cv) return(results)

  # Subsample 10k pixels and assign 8-fold split (1250 pixels per group)
  training_cv <- training_data %>%
    group_by(class) %>%
    sample_n(10000) %>%
    mutate(., k = sample(rep(1:8, 1250),10000))
  
  # Generate predictions and validate for k-subsets (leave one)
  cv_res <- lapply(1:5, function(k_current){
    # Get subset
    training_k <- filter(training_cv,  k != k_current)
    # Run predictions on subset
    preds <- data.frame(
      threshold_val = threshold_val,
      class = training_k$bcc >= threshold_val
    ) %>%
      # Label classes
      mutate(class = factor(case_when(class == 1 ~ "water",
                                      class == 0 ~ "other"), 
                            levels = c("other", "water")))
    # Determine accuracy, sensitivity and specificity
    acc <- sum(preds$class == training_k$class) / length(training_k$class)
    sens <- sum((preds$class == training_k$class)[training_k$class == "water"]) /
      sum(training_k$class == "water")
    spec <- sum((preds$class == training_k$class)[training_k$class == "other"]) /
      sum(training_k$class == "other")
    # Return acc
    return(data.frame(acc = acc,
                      sens = sens,
                      spec = spec))
  }) %>% bind_rows()
  
  # Add cv stat to output
  results <- results %>% mutate(
    k_cv = 8,
    acc_cv_mean = mean(cv_res$acc),
    acc_cv_se = sd(cv_res$acc) / 8,
    acc_cv_sens = mean(cv_res$sens),
    acc_cv_spec = mean(cv_res$spec)
  )
  
  # Return results
  return(results)
}

##  Prepare parallel environment
# Windows
# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, {
#   library("caret")
#   library("dplyr")
#   library("terra")})
# clusterEvalQ(cl, set.seed(45))
# Unix
cl <- detectCores() - 1
set.seed(45)

## Generate predictions for the whole range of thresholds 

# Generate predictions - global threshold identification
threshold_preds_all <- pblapply(threshold_vals, 
                                gen_preds_thres,
                                training_data = training,
                                cl = cl) %>% 
  bind_rows()

# Generate predictions - cbh site-specific threshold identification
threshold_preds_cbh <- pblapply(threshold_vals, 
                                gen_preds_thres, 
                                training_data = training_cbh, 
                                cl = cl) %>% 
  bind_rows()

# Generate predictions - tlb site-specific threshold identification
threshold_preds_tlb <- pblapply(threshold_vals,
                                gen_preds_thres,
                                training_data = training_tlb,
                                cl = cl) %>% bind_rows()

# Generate predictions - rdg site-specific threshold identification
threshold_preds_rdg <- pblapply(threshold_vals,
                                gen_preds_thres,
                                training_data = training_rdg,
                                cl = cl) %>% bind_rows()

# Generate predictions - raster-specific threshold identification
thresholds_site <- training %>% 
  # filter(site == "tlb" & year == "2016") %>%
  # Add site-year identifier to training data
  mutate(site_year = paste0(site,"_",year)) %>%
  # Apply across each site-year combination
  split(., .$site_year) %>%
  lapply(function(training_data_site){
    # Status
    cat("Generating predictions for", unique(training_data_site$site_year), "\n")
    # If no water training data (rdg site) are available, set threshold to 1
    if(nrow(training_data_site %>% filter(class == "water")) == 0) threshold_vals <- 1
    # Else generate predictions for each threshold, including "n-fold" validation
    training_all <- pblapply(threshold_vals, 
                             gen_preds_thres, 
                             training_data = training_data_site,
                             cv = TRUE,
                             cl = cl) %>% 
      bind_rows() %>%
      # add site year identifier
      mutate(site_year = unique(training_data_site$site_year))
    # Return results
    return(training_all)
  }) %>% 
  bind_rows() 

## Choose threshold based on maximum sensitivity + specificity

# Get threshold with highest accuracy using all data
best_thresh_all <- threshold_preds_all[threshold_preds_all$acc == max(threshold_preds_all$acc),][1,] 

# Get threshold with highest accuracy for each site
best_thresh_cbh <- threshold_preds_cbh[threshold_preds_cbh$acc == max(threshold_preds_cbh$acc),][1,] %>% mutate (site = "cbh")
best_thresh_tlb <- threshold_preds_tlb[threshold_preds_tlb$acc == max(threshold_preds_tlb$acc),][1,] %>% mutate (site = "tlb")
best_thresh_rdg <-  threshold_preds_rdg[threshold_preds_rdg$acc == max(threshold_preds_rdg$acc),][1,] %>% mutate (site = "rdg")

# Get threshold with highest accuracy for each raster (site-year combination)
best_thresh_site_year <- thresholds_site %>%
  split(., .$site_year) %>%
  lapply(., function(x){ 
    if(grepl("rdg", unique(x$site_year)) & unique(x$site_year) != "rdg_2017_b") return(x)
    if(unique(x$site_year) == "rdg_2017_b") return(x[x$acc == max(x$acc),][1,])
    # Flag values where max acc does not match max acc_cv_mean
    if(x$threshold[x$acc == max(x$acc)] != x$threshold[x$acc_cv_mean == max(x$acc_cv_mean)]){
      warning(paste0("Thresholds ",  
                     x$threshold[x$acc == max(x$acc)], " (max acc - all) and ",
                     x$threshold[x$acc_cv_mean == max(x$acc_cv_mean)], " (max mean_acc - cv) ",
                     "do not match for ", unique(x$site_year), "!"))
    }
    # Get max sens_spec threshold value for predictions
    x[x$acc == max(x$acc),][1,]
  }) %>%
  bind_rows()

## Plot distributions and thresholds
# Global threshold
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = threshold), data = best_thresh_all) +
  geom_text(aes(x = threshold + 0.1, y = -0.5, label = threshold), 
            data = best_thresh_all) +
  facet_wrap(vars(site)) +
  theme_cowplot()
# Does not work well -> as expected

# Site specific threshold
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = threshold), data = bind_rows(best_thresh_cbh, best_thresh_rdg, best_thresh_tlb)) +
  geom_text(aes(x = threshold + 0.1, y = -0.5, label = threshold), 
            data = bind_rows(best_thresh_cbh, best_thresh_rdg, best_thresh_tlb)) +
  facet_wrap(vars(site)) +
  theme_cowplot()
# Looks a kit better but not great either 

# Site-year specific threshold
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
  mutate(across(threshold:sens_spec, ~ round(.x, 2)),
         across(acc_cv_mean, ~ round(.x, 3)),
         across(acc_cv_se, ~ round(.x, 5)),
         across(acc_cv_sens:acc_cv_spec, ~ round(.x, 3))) %>%
  remove_rownames() %>%
  write_csv("tables/bcc_thersholds_site.csv")

# Test sensitivity of threshold to sample size. 

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

# Export thershold variable to cluster if working on Windows
# clusterExport(cl, varlist = "best_thresh_site_year")

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
}, cl = cl)

# Stop cluster on Windows
# stopCluster(cl)
