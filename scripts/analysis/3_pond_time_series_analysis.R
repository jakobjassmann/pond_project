# Pond Time-Series Analysis (stability, surface area variation, thermokarst and veg incursion)
# Jakob J. Assmann jakob.assmann@uzh.ch 23 May 2024

# Dependencies
library(terra)
library(tidyverse)
library(tidyterra)
library(sf)
library(cowplot)
library(colorspace)
library(pbapply)

# Load ponds and time-series
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")
load("data/pond_polys/pond_time_series.Rda")

# Remove ponds that have been experimentaly manipulated
manipulated_ponds <- read_csv("data/pond_polys/experiment_ponds.csv")
pond_time_series_ids <- filter(pond_time_series_ids,
                               !(ts_id %in% manipulated_ponds$ts_id))

# Initial histogram for number of years present in each pond time-series 
ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = n_years), binwidth = 1) +
  facet_wrap(~site) +
  theme_cowplot()

# Stable ponds
### Here we consider ponds as stable if the following conditions are met:
### 1) present in at least n-1 (= 6) years of the time-series (allow one for detection error)
### 2) coefficient of variation of area through the time-series is less than 10% (ignoring 2017)

# Calculate the CV for each unique intersections
pond_time_series_ids$cv <- pond_time_series_ids %>%
  split(., .$ts_id) %>%
  sapply(function(combination){
    # Calculate sum of area for each year, excluding 2017
    area <- ponds %>%
      filter(year != 2017) %>%
      filter(id %in% combination$combination[[1]]) %>%
      st_drop_geometry() %>%
      group_by(year) %>%
      summarise(area = sum(area)) %>%
      pull(area)
    # Return SD excluding 2017
    return(sd(area) / mean(area))
  })

# Show distributions of CV ponds with more than 6 years in the time-series
cv_hist <- ggplot(pond_time_series_ids %>% filter(n_years >= 6)) +
  geom_histogram(aes(x= cv), breaks = seq(0,2.2,0.1)) +
  geom_vline(aes(xintercept = mean(cv)), colour = "red") +
  geom_text(aes(x = cv, label = round(cv,2)), 
            y = Inf, 
            hjust = - 0.1,
            vjust = 1.5,
            colour = "red",
            data = pond_time_series_ids %>% 
              filter(n_years >= 6) %>%
              st_drop_geometry() %>% 
              group_by(site) %>%
              summarise(cv = mean(cv))) +
  scale_y_continuous(limits = c(0,25)) +
  labs(x = "Area change relative to mean (CV)", y= "Number of Ponds") +
  facet_wrap(~site) +
  theme_cowplot()
save_plot("figures/mean_area_change_cv.png", cv_hist, bg = "white")
# add stability indicator
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(stable = case_when(n_years >= 6 & cv <= 0.1 ~ "stable",
                            TRUE ~ "unstable"))
pond_time_series_ids %>% filter(stable == "stable") %>% pull(ts_id)

# Export dataset of unique intersections 
save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")
load("data/pond_polys/pond_time_series.Rda")

## Identify bank degreadation and vegetation incursion
# combination <- pond_time_series_ids %>% filter(ts_id == "cbh_262")

# Define helper function to calculate dsm differences at beginning and end 
# of time-series
get_volume_diff <- function(combination){
  # Get site from ts object (combination)
  site_name <- combination$site
  
  # Get associated pond polygons for the combination of ponds across years
  ponds_combi <- filter(ponds, id %in% unlist(combination$combination)) 
  
  # Set start and end year (here standardised to 2014 and 2017)
  year_min <- 2014 # min(ponds_combi$year)
  year_max <- 2021 # max(ponds_combi$year)
  
  
  # Load dsm rasters
  dsm_rasts <- list.files("data/drone_data",
                          pattern = "tif",
                          recursive = T,
                          full.names = T) %>%
    .[grepl("dsm",.)] %>%
    .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)] %>%
    .[grepl(site_name,.)] %>%
    .[grepl(paste0(year_min, "|", year_max), .)]
  
  # Load preds rasters (filtered by minimum mapping unit only)
  preds_rasts <- list.files("data/drone_data",
                            pattern = "tif",
                            recursive = T,
                            full.names = T) %>%
    .[grepl("preds_filtered/",.)] %>%
    .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)] %>%
    .[grepl(site_name,.)]
  
  # Determine pond boundary by buffering all ponds by 10 m, then cropping
  # to extent of a dsm raster (so that boundaries don't extend beyond it)
  pond_bounds <- combination %>%
    st_buffer(10, endCapStyle = "SQUARE") %>% 
    st_crop(st_bbox(rast(dsm_rasts[1]))) %>%
    ext()
  
  # Load and crop predictions (size filtered only) across whole time-series
  preds_all_crop <- crop(sum(rast(preds_rasts), na.rm = T), pond_bounds)
  
  # Standardise DSMs for calculation of relative differences
  dsm_standardised <- dsm_rasts %>%
    map(function(x){
      # Get year
      current_year <- gsub(".*([0-9]{4}).*", "\\1", x)
      # Crop to area of interest
      dsm_rast <- rast(x) %>%
        crop(., pond_bounds)
      # Calculate min value masking maximum water extend across all years (incl. 2017)
      dsm_min <- mask(dsm_rast, preds_all_crop, inverse = T) %>%
        global(., fun = min, 
               #probs = 0.02, 
               na.rm = T) %>%
        as.numeric()
      # Standardise and return
      dsm_std <- dsm_rast - dsm_min
      # Get water surface for year
      water_surf <- preds_rasts[grepl(current_year, preds_rasts)] %>%
        rast() %>%
        crop(., pond_bounds)
      # Mask water surface in current year setting it to 0
      dsm_std  <- mask(dsm_std,
                       water_surf,
                       updatevalue = 0,
                       inverse = T)
      # Return standardised and masked raster
      return(dsm_std)
    })
  
  # Determine years in time-series
  years_all <- as.numeric(gsub(".*([0-9]{4}).*", "\\1", dsm_rasts))
  
  # Assign years to standardised dsm objects
  names(dsm_standardised) <- years_all
  
  # Calculate volume difference between min and max year 
  pond_max_year <- ponds_combi %>% filter(year == year_max) %>% st_union()
  cells_max_year <- terra::extract(dsm_standardised[names(dsm_standardised) == year_min][[1]],
                              vect(pond_max_year)) %>%
    na.omit()
  
  # If cells were lost, calculate average volume loss per area, else return 0
  if(length(cells_max_year) > 0) volume_loss <- sum(cells_max_year[,2] * 0.12**2)
  if(length(cells_max_year) == 0) volume_loss <- 0
  
  # Convert to m2
  land_area_loss <- (length(cells_max_year[,2]) * 0.12**2)
    
    
  # Calculate volume difference between max and min year 
  pond_min_year <- ponds_combi %>% filter(year == year_min) %>% st_union()
  cells_min_year <- terra::extract(dsm_standardised[names(dsm_standardised) == year_max][[1]],
                              vect(pond_min_year)) %>%
    na.omit()
  
  # If cells were gained, calculate average volume gain per area, else return 0
  if(length(cells_min_year) > 0) volume_gain <- sum(cells_min_year[,2] * 0.12**2)
  if(length(cells_min_year) == 0) volume_gain <- 0
  
  # Convert to m2
  land_area_gain <- (length(cells_min_year[,2]) * 0.12**2)
  
  
  # Total change in volume of area ever occupied by pond
  dsm_diff <- dsm_standardised[names(dsm_standardised) == year_max][[1]] -
    dsm_standardised[names(dsm_standardised) == year_min][[1]]
  dsm_diff_cells <- terra::extract(dsm_diff,
                                   vect(combination))
  
  # Confert to m2
  total_volume_change <- sum(dsm_diff_cells[,2] * 0.12**2)
  
  # return sum of total volume lost per cell change on average per year
  return(tibble(ts_id = combination$ts_id, 
                land_area_loss, 
                volume_loss, 
                mean_volume_loss_per_m2 = volume_loss / land_area_loss,
                land_area_gain,
                volume_gain,
                mean_volume_gain_per_m2 = volume_gain / land_area_gain,
                total_volume_change,
                mean_total_change_per_m2 = total_volume_change / as.numeric(combination$area))) # max(volume_loss_per_year$volume_loss))
}

# Test volume difference function for one pond
get_volume_diff(pond_time_series_ids %>% filter(ts_id == "cbh_009"))

# Get volume difference for all combinations
pond_time_series_ids <- pond_time_series_ids %>%
           split(., 1:nrow(.)) %>%
           pblapply(get_volume_diff, cl = 31) %>%
  bind_rows() %>%
  full_join(pond_time_series_ids, .)


pond_time_series_ids <- pond_time_series_ids %>%
  mutate(mean_volume_loss_per_m2 = case_when(is.nan(mean_volume_loss_per_m2) ~0,
                                             TRUE ~mean_volume_loss_per_m2))

pond_time_series_ids <- pond_time_series_ids %>%
  mutate(mean_volume_gain_per_m2 = case_when(is.nan(mean_volume_gain_per_m2) ~0,
                                             TRUE ~mean_volume_gain_per_m2))

# Plot volume loss per m2 of lost land area
(ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_volume_loss_per_m2), binwidth = 0.025) +
  geom_vline(xintercept= 0.1, colour = "red") +
  annotate("text", label = "detection threshold", 
           x= 0.1, y = Inf, hjust = - 0.1, vjust = 1.5, colour = "red") +
  labs(x = "Mean volume loss m3 / m2", y = "Number of Ponds") +
  facet_wrap(vars(site)) +
  theme_cowplot()) %>%
  save_plot("figures/mean_volume_loss_per_m2.png", .,
            base_height = 5, bg = "white")

# Plot volume gain per m2 of lost land area
(ggplot(pond_time_series_ids) +
    geom_histogram(aes(x = mean_volume_gain_per_m2), binwidth = 0.025) +
    geom_vline(xintercept= 0.1, colour = "red") +
    annotate("text", label = "detection threshold", 
             x= 0.1, y = Inf, hjust = - 0.1, vjust = 1.5, colour = "red") +
    labs(x = "Mean volume gain m3 / m2", y = "Number of Ponds") +
    facet_wrap(vars(site)) +
    theme_cowplot()) %>%
  save_plot("figures/mean_volume_gain_per_m2.png", .,
            base_height = 5, bg = "white")

# Next we define that losing more than 0.1 m3 per / m 2 of volume => degradation
# This is equivalent to a loss of 10 cm of soil covering 1 m2 
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(degradation = case_when(mean_volume_loss_per_m2  >=  0.10
                                 ~ "degradation",
                                 TRUE ~ "no_degradation"))

# Get a quick overview of how many ponds with detected degradation at each site
pond_time_series_ids %>% filter(degradation == "degradation") %>% 
  group_by(site) %>%
  st_drop_geometry() %>% tally()

# Write out statistics to file
save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")

# Optional code to look into the ponds in detal
# pond_time_series_ids %>% filter(degradation == "degradation") %>%
#   pull(ts_id)
# pond_time_series_ids %>% filter(degradation == "degradation") %>%
#   st_drop_geometry() %>%
#   select(ts_id, cv, volume_diff) %>%
#   tibble() %>%
#   print(n = 142)
# pond_time_series_ids %>% filter(ts_id == "tlb_013") %>% st_drop_geometry()

# Optional code for quality control of this assessment  
# cbh_test <- pond_time_series_ids %>% filter(degradation == "degradation", site == "cbh") %>%
#   st_drop_geometry() %>%
#   select(ts_id, cv, volume_diff) %>%
#   tibble() %>%
#   pull(ts_id)
# 
# pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% tally()
# pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% pull(ts_id)
# 
# cbh_truth <- readxl::read_xlsx("data/training_data/degradation_ponds.xlsx") %>% 
#   #filter(ts_id %in% (pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% pull(ts_id))) %>%
#   filter(grepl("cbh", ts_id)) %>%
#   pull(1)
# # Detection rate (sensitivity)
# sum(unlist(map(cbh_truth, function(x) x %in% cbh_test))) / length(cbh_truth)
# # Proportion false positives (specificity)
# sum(unlist(map(cbh_test, function(x) x %in% cbh_truth))) / length(cbh_test)
# 
# filter(pond_time_series_ids, ts_id %in% cbh_truth) %>% select(ts_id, mean_volume_loss_per_m2) %>% st_drop_geometry()
# 
# pond_time_series_ids %>% filter(ts_id == "cbh_050") %>% select(ts_id, cv, volume_diff, n_years)


### Detect veg invasion

# combination <- pond_time_series_ids %>% filter(ts_id == "cbh_079")

# Define helper function to retrieve gcc differences for terrain that was 
# occupied by water at the beginning of a pond's time series
get_gcc_diff <- function(combination){
  # Get site
  site_name <- combination$site
  
  # Get pond polygons for combination 
  ponds_combi <- filter(ponds, id %in% unlist(combination$combination)) 
  
  # Determine start and end year, excluding 2017
  year_min <- min(ponds_combi$year[!grepl("2017", ponds_combi$year)])
  year_max <- max(ponds_combi$year[!grepl("2017", ponds_combi$year)])
  
  # Get start and end ponds
  pond_start <- ponds_combi %>% filter(year == year_min)
  pond_end <- ponds_combi %>% filter(year == year_max)
  
  # Load GCC for end year
  gcc_end <- list.files("data/drone_data/", "tif", recursive = T, full.names = T) %>%
    .[grepl("gcc", .)] %>%
    .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)] %>%
    .[grepl(site_name, .)] %>%
    .[grepl(year_max, .)] %>%
    rast()
  
  # Extract mean gcc values for end of time-series
  cells_new <- terra::extract(mask(gcc_end, pond_end, inverse = T), 
                              pond_start) %>%
    na.omit() %>%
    .[,2]
  # If cells were lost, calculate average vgcc, else return 0
  if(length(cells_new) > 0) gcc_mean <- mean(cells_new)
  if(length(cells_new) == 0) gcc_mean <- 0
 
  # Return result
  return(gcc_mean)
}
# pond_time_series_ids %>% filter(ts_id == "cbh_070") %>% get_gcc_diff()

# Retrieve gcc differences for all ponds using the helper function
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(gcc_diff = pond_time_series_ids %>%
           split(., 1:nrow(.)) %>%
           pblapply(get_gcc_diff, cl = 31) %>% unlist())

# Plot histogram of gcc differences
hist(pond_time_series_ids$gcc_diff)

# Determine veg incursion based on volue gain (detection threshold = 0.1 m3 / m2)
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(veg_intrusion = case_when(mean_volume_gain_per_m2 > 0.1 ~ "veg_intrusion",
                                   TRUE ~ "no_veg_intrusion"))

# For ponds with vegetation incursion compare gcc and volume gain
pond_time_series_ids %>% filter(veg_intrusion == "veg_intrusion") %>%
  st_drop_geometry() %>%
  select(ts_id, gcc_diff, mean_volume_gain_per_m2)

# Tally up number of ponds with veg gain
pond_time_series_ids %>% filter(veg_intrusion == "veg_intrusion") %>%
  st_drop_geometry() %>%
  group_by(site) %>%
  tally()

# Save changes to pond time series object
save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")
