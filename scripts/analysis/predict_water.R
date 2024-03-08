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
training <- read_csv("data/training/training_all_df.csv")

## Simple BCC thershold
# Plot distribution of BCC
ggplot(training) +
  geom_histogram(aes(x= bcc)) +
  theme_cowplot()
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(xintercept = 0.359) +
  theme_cowplot()
ggplot(training) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(xintercept = 0.359) +
  facet_wrap(vars(site)) +
  theme_cowplot()

# Generate vector of thersholds
thershold_vals <- seq(0,1,0.01)
levels(training_all$class)

training_cbh <- filter(training_all, site == "cbh")
training_tlb <- filter(training_all, site == "tlb")
training_rdg <- filter(training_all, site == "rdg")

# Helper function to generate predictions for a set of thresholds
gen_preds_thres <- function(thershold_val, training_data = training_all) {
  preds <- data.frame(
    thershold_val = thershold_val,
    class = training_data$bcc >= thershold_val
  ) %>%
    mutate(class = factor(case_when(class == 1 ~ "water",
                                    class == 0 ~ "other"), 
                          levels = c("other", "water")))
  
  cf_matrix <- confusionMatrix(preds$class, training_data$class, positive = "water")
  data.frame(
    thershold = thershold_val, 
    acc = cf_matrix$overall["Accuracy"],
    fpr = 1 - cf_matrix$byClass["Specificity"],
    sens = cf_matrix$byClass["Sensitivity"],
    spec = cf_matrix$byClass["Specificity"],
    sens_spec = cf_matrix$byClass["Specificity"] + cf_matrix$byClass["Sensitivity"]
  )
}

# Generate predictions
thershold_preds_all <- pblapply(thershold_vals, gen_preds_thres, cl = 31) %>% bind_rows()
thershold_preds_cbh <- pblapply(thershold_vals, gen_preds_thres, training_data = training_cbh, cl = 31) %>% bind_rows()
thershold_preds_tlb <- pblapply(thershold_vals, gen_preds_thres, training_data = training_tlb, cl = 31) %>% bind_rows()
thershold_preds_rdg <- pblapply(thershold_vals, gen_preds_thres, training_data = training_rdg, cl = 31) %>% bind_rows()

# Get minimum threshold with more than 95% specificity
best_thersh_all <- thershold_preds_all[thershold_preds_all$acc == max(thershold_preds_all$acc),][1,] 
best_thersh_cbh <- thershold_preds_cbh[thershold_preds_cbh$acc == max(thershold_preds_cbh$acc),][1,] %>% mutate (site = "cbh")
best_thersh_tlb <- thershold_preds_tlb[thershold_preds_tlb$acc == max(thershold_preds_tlb$acc),][1,] %>% mutate (site = "tlb")
best_thersh_rdg <-  thershold_preds_rdg[thershold_preds_rdg$acc == max(thershold_preds_rdg$acc),][1,] %>% mutate (site = "rdg")

# Generate predictions for each raster individually
thresholds_site <- training_all %>% 
  mutate(site_year = paste0(site,"_",year)) %>%
  split(., .$site_year) %>%
  lapply(function(training_data_site){
    cat("Generating predictions for", unique(training_data_site$site_year), "\n")
    if(nrow(training_data_site %>% filter(class == "water")) == 0) thershold_vals <- 1
    pblapply(thershold_vals, gen_preds_thres, training_data = training_data_site, cl = 31) %>% 
      bind_rows() %>%
      mutate(site_year = unique(training_data_site$site_year))
  }) %>% 
  bind_rows() %>%
  na.omit()

# Get max accuracy thresholds           
best_thersh_site_year <- thresholds_site %>%
  split(., .$site_year) %>%
  lapply(., function(x){ 
    x[x$acc == max(x$acc),][1,]
  }) %>%
  bind_rows()
best_thersh_site_year <- bind_rows(
  best_thersh_site_year,
  training_all %>% 
    mutate(site_year = paste0(site,"_",year)) %>%
    distinct(site_year) %>%
    filter(grepl("rdg", site_year)) %>%
    filter(!grepl("2017", site_year)) %>%
    mutate(thershold = 1)
) %>% arrange(site_year)

# Plot distributions and thersholds
ggplot(training_all) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = thershold), data = bind_rows(best_thersh_cbh, best_thersh_rdg, best_thersh_tlb)) +
  geom_text(aes(x = thershold + 0.1, y = -0.5, label = thershold), 
            data = bind_rows(best_thersh_cbh, best_thersh_rdg, best_thersh_tlb)) +
  facet_wrap(vars(site)) +
  theme_cowplot()

(bcc_norm_with_thresh_plot <- ggplot(training_all %>% mutate(site_year = paste0(site,"_", year))) +
  geom_density(aes(x= bcc, colour = class)) +
  geom_vline(aes(xintercept = thershold), data = best_thersh_site_year) +
  geom_text(aes(x = thershold - 0.07, y = -Inf, label = round(thershold,2)), 
            data = best_thersh_site_year, vjust = -0.5) +
  facet_wrap(vars(site_year), scales = "free") +
  scale_colour_manual(values = c("#C00000", "#00B0F0")) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(expand = c(0.2,0.2)) +
  labs(x = "bcc", y = "density") +
  theme_cowplot())
save_plot("figures/bcc_all_norm_sep_with_thresh.png", bcc_norm_with_thresh_plot,
          base_height = 12, base_asp = 16/9, bg = "white")
# Plot ROC curve
ggplot(thershold_preds_all) +
  geom_line(aes(x = fpr, y = sens)) +
  annotate("point", x= best_thersh_all$fpr, best_thersh_all$sens, color = "red") +
  theme_cowplot()
ggplot(thershold_preds_cbh) +
  geom_line(aes(x = fpr, y = sens)) +
  annotate("point", x= best_thersh_cbh$fpr, best_thersh_cbh$sens, color = "red") +
  theme_cowplot()
ggplot(thershold_preds_tlb) +
  geom_line(aes(x = fpr, y = sens)) +
  annotate("point", x= best_thersh_tlb$fpr, best_thersh_tlb$sens, color = "red") +
  theme_cowplot()
ggplot(thershold_preds_rdg) +
  geom_line(aes(x = fpr, y = sens)) +
  annotate("point", x= best_thersh_rdg$fpr, best_thersh_rdg$sens, color = "red") +
  theme_cowplot()

# Generate projections based on threshold
# Let's see how that looks like in space
dir.create("data/drone_time_series/cbh_timeseries/preds_thresh/")
dir.create("data/drone_time_series/tlb_timeseries/preds_thresh/")
dir.create("data/drone_time_series/rdg_timeseries/preds_thresh/")

pblapply(raster_files_all, function(rast_file) {
  year_interest <- gsub(".*/[a-z]{3}_([0-9]{4}.*)\\.tif", "\\1", rast_file)
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rast_file)
  cat("Running projections for", site_interest, "and", year_interest, ":\n")
  cat("Preparing data...\n")
  norm_raster <- rast(rast_file)
  names(norm_raster) <- c("R", "G", "B", "alpha")
  
  # Calculate and bcc
  bcc <- norm_raster[["B"]] / (norm_raster[["R"]] + norm_raster[["G"]] + norm_raster[["B"]])
  names(bcc) <- "bcc"
  
  cat("Predicting values...\n")
  # if(site_interest == "cbh") thershold <- best_thersh_cbh$thershold
  # if(site_interest == "rdg") thershold <- best_thersh_rdg$thershold
  # if(site_interest == "tlb") thershold <- best_thersh_tlb$thershold
  # thershold <- best_thersh_all$thershold
  best_thersh <- best_thersh_site_year %>%
    filter(site_year == paste0(site_interest, "_", year_interest))
  thershold <- best_thersh$thershold
  
  # Calculate predicitons
  preds <- bcc >= thershold
  
  cat("Writing raster...\n")
  writeRaster(preds,
              filename = paste0("data/drone_time_series/", site_interest, "_timeseries/preds_thresh/", site_interest, "_", year_interest, "_preds_thres.tif"),
              overwrite = T
  )
  cat(year_interest, "done.\n")
  return(NULL)
}, cl = 31)
