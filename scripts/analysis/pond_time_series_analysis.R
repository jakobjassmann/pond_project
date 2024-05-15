## Analysis of ponds dynamicism - bank degradation etc. 

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

# Generate histogram for number of intersections 
ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = n_years), binwidth = 1) +
  facet_wrap(~site) +
  theme_cowplot()

# Check ponds present for only three years
pond_time_series_ids %>% filter(n_years == 3) %>% 
  st_drop_geometry() %>%
  group_by(site, n_years) %>%
  tally()
filter(ponds, id %in% unlist(pond_time_series_ids %>% 
                               filter(n_years == 3) %>% 
                               pull(combination))) %>%
  group_by(site) %>%
  group_map(function(x, y, ...){
    ggplot() + 
      geom_sf(data = ponds %>% filter(site == y$site)) +
      geom_sf(data = x, colour = "red")
  }) %>%
  plot_grid(plotlist = .)
pond_time_series_ids %>%
  filter(n_years == 3) %>%
  split(1:nrow(.)) %>%
  map(function(x){ 
    filter(ponds, id %in% unlist(x$combination)) %>%
      {ggplot(data = .) +
          geom_sf() +
          facet_wrap(~year)}
  })
# Look legit!

# Stable ponds
### Here I consider ponds as stable if the following conditions are met:
### 1) present in at least (n-1) years of the time-series (allow one for detection error)
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
      # Calculate sum of area for each year
      group_by(year) %>%
      summarise(area = sum(area)) %>%
      pull(area)
    return(sd(area) / mean(area))
  })

# Show distributions of CV ponds with more than 6 years in the time-series
cv_hist <- (ggplot(pond_time_series_ids %>% filter(n_years >= 6)) +
  geom_histogram(aes(x= cv), breaks = seq(0,2.2,0.1)) +
  geom_vline(aes(xintercept = mean(cv)), colour = "red") +
  geom_text(aes(x = cv, label = round(cv,2)), y = Inf, hjust = - 0.1, vjust = 1.5, colour = "red",
            data = pond_time_series_ids %>% filter(n_years >= 6) %>%
              st_drop_geometry() %>% group_by(site) %>% summarise(cv = mean(cv))) +
  scale_y_continuous(limits = c(0,20)) +
  labs(x = "Area change relative to mean (CV)", y= "Number of Ponds") +
  facet_wrap(~site) +
  theme_cowplot())
save_plot("figures/mean_area_change_cv.png", cv_hist, bg = "white")
# add stability indicator
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(stable = case_when(n_years >= 6 & cv <= 0.1 ~ "stable",
                            TRUE ~ "unstable"))
pond_time_series_ids %>% filter(stable == "stable") %>% pull(ts_id)

# Export dataset of unique intersections 
save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")
load("data/pond_polys/pond_time_series.Rda")
## Identify bank degreadation
# Following steps:
# 1) Determine area covered by pond throught time-series
# 2) Identify beginning and end of time-series
# 3) Calculate the loss in "height" per area from the DSM for the difference of
#    the total pond area covered by the time-series less the area at the beginning of the pond

pond_time_series_ids 
combination <- pond_time_series_ids %>% filter(ts_id == "cbh_262")

get_volume_diff <- function(combination){
  # Get site
  site_name <- combination$site
  
  # Get pond polygons for combination 
  ponds_combi <- filter(ponds, id %in% unlist(combination$combination)) 
  
  # Determine start and end year
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
  
  # Load preds rasters (not filtered by intersection)
  preds_rasts <- list.files("data/drone_data",
                            pattern = "tif",
                            recursive = T,
                            full.names = T) %>%
    .[grepl("preds_filtered/",.)] %>%
    .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)] %>%
    .[grepl(site_name,.)]
  
  # Determine pond boundary
  # add 5 m buffer around first occurance in the time-series ignoring 
  # 2017, crop to site area to avoid over spill, cast to ext object
  pond_bounds <- combination %>%
    st_buffer(10, endCapStyle = "SQUARE") %>% 
    st_crop(st_bbox(rast(dsm_rasts[1]))) %>%
    ext()
  
  # Get union across all ponds, including 2017
  # ponds_union <- filter(ponds, id %in% unlist(combination$combination)) %>%
  #   st_union()
  
  # Load and crop predictions (size filtered only) across whole time-series
  preds_all_crop <- crop(sum(rast(preds_rasts), na.rm = T), pond_bounds)
  
  # # Load and crop prediction for 2014 
  # preds_start <- preds_rasts %>% 
  #   .[grepl("2014",. )] %>% 
  #   rast() %>%
  #   crop(., pond_bounds)
  
  # Standardise DSMs 
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
  
  # Assign to standardised dsms
  names(dsm_standardised) <- years_all
  
  # Calculate volume difference between min and max year 
  pond_max_year <- ponds_combi %>% filter(year == year_max) %>% st_union()
  cells_max_year <- terra::extract(dsm_standardised[names(dsm_standardised) == year_min][[1]],
                              vect(pond_max_year)) %>%
    na.omit()
  # If cells were lost, calculate average volume loss per area, else return 0
  if(length(cells_max_year) > 0) volume_loss <- sum(cells_max_year[,2] * 0.12**2)
  if(length(cells_max_year) == 0) volume_loss <- 0
  land_area_loss <- (length(cells_max_year[,2]) * 0.12**2)
    
    
  # Calculate volume difference between max and min year 
  pond_min_year <- ponds_combi %>% filter(year == year_min) %>% st_union()
  cells_min_year <- terra::extract(dsm_standardised[names(dsm_standardised) == year_max][[1]],
                              vect(pond_min_year)) %>%
    na.omit()
  # If cells were gained, calculate average volume gain per area, else return 0
  if(length(cells_min_year) > 0) volume_gain <- sum(cells_min_year[,2] * 0.12**2)
  if(length(cells_min_year) == 0) volume_gain <- 0
  # calculate area change
  land_area_gain <- (length(cells_min_year[,2]) * 0.12**2)
  
  
  # Total change in volume of area ever occupied by pond
  dsm_diff <- dsm_standardised[names(dsm_standardised) == year_max][[1]] -
    dsm_standardised[names(dsm_standardised) == year_min][[1]]
  dsm_diff_cells <- terra::extract(dsm_diff,
                                   vect(combination))
  total_volume_change <- sum(dsm_diff_cells[,2] * 0.12**2)
  
  # # Calculate annual loss of terrain (for all years with following years) except 2016 (flooded in 2017)
  # volume_loss_per_year <- map(years_all[!(years_all %in% c(2016, 2021))],
  #                             function(current_year){
  #                               cat(current_year, "\n")
  #                               if(current_year == 2014){
  #                                 next_year <- 2016
  #                               } else {
  #                                 next_year <- current_year + 1
  #                               }
  #                               # Get pond surfaces
  #                               # pond_current <- ponds %>% filter(year == current_year) %>% st_union()
  #                               pond_next <- ponds_combi %>% filter(year == next_year) %>% st_union()
  # 
  #                               # Check whether there was a pond
  #                               if(length(pond_next) > 0){
  #                                 # Extract difference for current year
  #                                 cells_new <- terra::extract(dsm_standardised[names(dsm_standardised) == current_year][[1]],
  #                                                             vect(pond_next)) %>%
  #                                   na.omit()
  # 
  #                                 # If cells were gained, calculate average volume loss per area else return 0
  #                                 if(length(cells_new) > 0) volume_loss <- sum(cells_new[,2] * 0.12**2) / (length(cells_new) * 0.12**2)
  #                                 if(length(cells_new) == 0) volume_loss <- 0
  # 
  #                               } else {
  # 
  #                                 volume_loss <- 0
  #                               }
  #                               # Return as tibble
  #                               return(tibble(year = current_year,
  #                                             volume_loss = volume_loss))
  #                             }) %>% bind_rows()
  # mask water in start year
  
  
  # # Load dsm for 2021
  # dsm_crop_end <- dsm_rasts %>% 
  #   .[grepl("2021",. )] %>% 
  #   rast() %>%
  #   crop(., pond_bounds)
  # 
  # # Calculate median value for area never covered by water
  # dsm_end_median <- mask(dsm_crop_end, preds_all_crop, inverse = T) %>%
  #   global(., median, na.rm = T) %>%
  #   as.numeric()
  # 
  # # Standardise dsm by median for first year in time-series
  # dsm_crop_end <- dsm_crop_end - dsm_end_median
  # 
  # # mask water in start year
  # dsm_crop_end <- mask(dsm_crop_end, 
  #                        crop(rast(preds_rasts[grepl("2021", preds_rasts)]), pond_bounds), inverse = T)
  # 
  # # Calculate difference
  # dsm_diff <- dsm_crop_end - dsm_crop_start
  
  # Extract cells where pond extended to at maximum extent (excluding 2017) and those
  # that were not originally covered by water at end of time-series
  # cells_new <- terra::extract(dsm_standardised[[1]], vect(combination)) %>%
  #   na.omit()
  # 
  # # Calculate volume change across maximum extend area (excluding 2017)
  # volume_diff <- sum(cells_new[,2] * 0.12**2)
  
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

# plot(dsm_crop_start)
# plot(st_geometry(combination), add = T)
# plot(ponds_union, add = T)
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


quantile(pond_time_series_ids$mean_volume_loss_per_m2, 0.75, na.rm = T)
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


quantile(pond_time_series_ids$mean_volume_gain_per_m2, 0.75)
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

quantile(pond_time_series_ids$mean_total_change_per_m2, 0.75)
ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_total_change_per_m2), binwidth = 10) +
  geom_vline(aes(xintercept= quantile(mean_total_change_per_m2, 0.75))) +
  theme_cowplot()

# Losing more than 10 m3 per / m 2 of volume => degradation
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(degradation = case_when(mean_volume_loss_per_m2  >=  0.11
                                 ~ "degradation",
                                 TRUE ~ "no_degradation"))

pond_time_series_ids %>% filter(degradation == "degradation") %>% 
  group_by(site) %>%
  st_drop_geometry() %>% tally()
pond_time_series_ids %>% filter(degradation == "degradation") %>%
  pull(ts_id)
pond_time_series_ids %>% filter(degradation == "degradation") %>%
  st_drop_geometry() %>%
  select(ts_id, cv, volume_diff) %>%
  tibble() %>%
  print(n = 142)

pond_time_series_ids %>% filter(ts_id == "tlb_013") %>% st_drop_geometry()
  
cbh_test <- pond_time_series_ids %>% filter(degradation == "degradation", site == "cbh") %>%
  st_drop_geometry() %>%
  select(ts_id, cv, volume_diff) %>%
  tibble() %>%
  pull(ts_id)

pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% tally()
pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% pull(ts_id)

cbh_truth <- readxl::read_xlsx("data/training_data/degradation_ponds.xlsx") %>% 
  #filter(ts_id %in% (pond_time_series_ids %>% filter(n_years >= 7) %>% st_drop_geometry() %>% pull(ts_id))) %>%
  filter(grepl("cbh", ts_id)) %>%
  pull(1)
# Detection rate (sensitivity)
sum(unlist(map(cbh_truth, function(x) x %in% cbh_test))) / length(cbh_truth)
# Proportion false positives (specificity)
sum(unlist(map(cbh_test, function(x) x %in% cbh_truth))) / length(cbh_test)

filter(pond_time_series_ids, ts_id %in% cbh_truth) %>% select(ts_id, mean_volume_loss_per_m2) %>% st_drop_geometry()

pond_time_series_ids %>% filter(ts_id == "cbh_050") %>% select(ts_id, cv, volume_diff, n_years)

save(pond_time_series_ids, file = "data/pond_polys/pond_time_series.Rda")

### Detect veg invasion

# combination <- pond_time_series_ids %>% filter(ts_id == "cbh_079")
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
pond_time_series_ids <- pond_time_series_ids %>%
  mutate(gcc_diff = pond_time_series_ids %>%
           split(., 1:nrow(.)) %>%
           pblapply(get_gcc_diff, cl = 31) %>% unlist())
hist(pond_time_series_ids$gcc_diff)
quantile(pond_time_series_ids$gcc_diff, 0.68)

pond_time_series_ids <- pond_time_series_ids %>%
  mutate(veg_intrusion = case_when(mean_volume_gain_per_m2 > 0.1 ~ "veg_intrusion",
                                   TRUE ~ "no_veg_intrusion"))
pond_time_series_ids %>% filter(veg_intrusion == "veg_intrusion") %>%
  st_drop_geometry() %>%
  select(ts_id, gcc_diff, mean_volume_gain_per_m2)

pond_time_series_ids %>% filter(veg_intrusion == "veg_intrusion") %>%
  st_drop_geometry() %>%
  group_by(site) %>%
  tally()
