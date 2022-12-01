# Analysis of ponds continous across landscape 
# Jakob J. Assmann jakob.assmann@uzh.ch 25 November 2022

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)
library(terra)

# Read in continous ponds
load("data/drone_time_series/cbh/cbh_continous_ponds/cbh_continous_ponds.Rda")

# Assess how many ponds there are in each year
map(pond_polys_all_years, nrow)

# 2021 has the larges number of ponds that exist intersect with all other
# data sets, there is 51 of them. I think we should use them as our baseline.

# Export them for visual control
#dir.create("data/drone_time_series/cbh/cbh_continous_ponds/")
write_sf(pond_polys_all_years[[7]],
         "data/drone_time_series/cbh/cbh_continous_ponds/continous_ponds_2021.shp")

# Generate dedicated object for year of interest and other years excluding 2017
ponds_2021 <- pond_polys_all_years[[7]]
ponds_other_years <- pond_polys_all_years[c(1:2, 4:6)]

# Work without ponds that have combined in the past years!
# Get umber of overlapping ponds in all other years
years <- c(2014, 2016, 2018:2020)
n_intersections <- map(1:5, function(x){
  data.frame(
    1:nrow(ponds_2021),
    map(st_intersects(ponds_2021, ponds_other_years[[x]]),
      function(x) data.frame(length(x))) %>% unlist()) %>%
    set_names(c("pond_id", paste0("n_ints_", years[x])))
}) %>% reduce(full_join)

# Remove ponds that have combined from 2021
pond_ids_of_interest <- n_intersections$pond_id[apply(n_intersections[,2:6], 1, sum) == 5]
ponds_of_interest <- ponds_2021[pond_ids_of_interest,]

# Remove ponds not of interest from other years and re-arrange
ponds_other_years <- map(ponds_other_years,
    function(ponds){
      ponds[st_intersects(ponds_of_interest, ponds) %>% unlist(),]
    })

# Work without ponds that have split in the past years
ponds_other_years_with_double_matches <- map(1:5, function(x){
  data.frame(
    1:nrow(ponds_other_years[[x]]),
    map(st_intersects(ponds_other_years[[x]], ponds_2021),
        function(x) data.frame(length(x))) %>% unlist()) %>%
    set_names(c("pond_id", paste0("n_ints_", years[x])))
}) %>% reduce(full_join) %>%
  select(-contains("2017"))

# Remove ponds that have split before 2021 from other ponds
pond_ids_of_interest <- ponds_other_years_with_double_matches$
  pond_id[apply(ponds_other_years_with_double_matches[,2:6], 1, sum) == 5]
ponds_other_years <- map(ponds_other_years,
                         function(x) x[pond_ids_of_interest,])
ponds_of_interest <- ponds_of_interest[pond_ids_of_interest,]

# Calculate centroids
ponds_other_years_centroids <- map(ponds_other_years,
    function(ponds){
      st_centroid(st_geometry(ponds))
    })
ponds_of_interest_centroids <- st_centroid(ponds_of_interest)

# Calculate shift of centroids
centroid_shifts <- map(1:5, 
    function(year_index){
      # Get year and other pond centroids
      year <- years[year_index]
      pond_centroids <- ponds_other_years_centroids[[year_index]]
      # Calculate distance between each centroid of interest and other centroids
      map(1:nrow(ponds_of_interest_centroids),
          function(index){
            # Calculate distance to matching ponds and return as data frame
            data.frame(pond_id = index,
                       distance = st_distance(ponds_of_interest_centroids[index,], 
                                   pond_centroids[index])
                       ) %>%
              set_names(c("pond_id", year))
          }) %>%
        bind_rows()
    }) %>%
  reduce(full_join)

# Get summary stats
centroid_shifts$mean <- centroid_shifts[,-1] %>%
  apply(1, mean) 
quantile(centroid_shifts$mean, c(0.1, 0.45, 0.55, 0.8))

# Define function to plot pond outline over time
plot_pond <- function(pond_id, legend = T, scale_plot = F){
  # pond_id <- 9
  
  # Plot pond
  return_plot <- ggplot() +
    geom_sf(data = ponds_other_years[[1]][pond_id,],
            aes(fill = years[1],
                colour = years[1]),
            alpha = 0.25) +
    geom_sf(data = ponds_other_years[[2]][pond_id,],
            aes(fill = years[2],
                colour = years[2]),
            alpha = 0.25) +
    geom_sf(data = ponds_other_years[[3]][pond_id,],
            aes(fill = years[3],
                colour = years[3]),
            alpha = 0.25) +
    geom_sf(data = ponds_other_years[[4]][pond_id,],
            aes(fill = years[4],
                colour = years[4]),
            alpha = 0.25) +
    geom_sf(data = ponds_other_years[[5]][pond_id,],
            aes(fill = years[5],
                colour = years[5]),
            alpha = 0.25) +
    geom_sf(data = ponds_of_interest[pond_id,],
            fill = "red", colour = "red",
            alpha = 0.25) +
    scale_fill_continuous() +
    scale_colour_continuous() +
    theme_map()
  if(!legend){
    return_plot <- return_plot + theme(legend.position = "none")
  }
  if(scale_plot){
    return_plot <- return_plot +
      coord_sf(
        xlim = c(st_coordinates(st_centroid(ponds_of_interest[pond_id,]))[1] - 20,
                 st_coordinates(st_centroid(ponds_of_interest[pond_id,]))[1] + 20),
        ylim = c(st_coordinates(st_centroid(ponds_of_interest[pond_id,]))[2] - 20,
                 st_coordinates(st_centroid(ponds_of_interest[pond_id,]))[2] + 20)
      )
  }
  return(return_plot)
}

# Get pond ids at the extremes

# Pond with big shifts:
centroid_shifts[which(centroid_shifts$mean >= 1.8),]
map(c(9, 19, 20), plot_pond) %>% plot_grid(plotlist = ., nrow = 4, ncol = 4)
# No 9 pond is the one with the biggest shift

# Ponds with small shifts:
centroid_shifts[which(centroid_shifts$mean <= 0.3),]
# No 7 has by far the least shift

# Pond with medium shifts:
centroid_shifts[which(centroid_shifts$mean >= 1.02 & centroid_shifts$mean <= 1.23),]
# Pond 23 seems to fall bang on in the middle


# plot(st_geometry(ponds_other_years[[5]][23,]), col = "red")
# plot(st_geometry(ponds_of_interest[23,]), col = "blue", add = T)
# plot(st_geometry(ponds_of_interest_centroids[23,]), col = "white", add = T)
# plot(st_geometry(ponds_other_years_centroids[[5]][23]), col = "black", add = T)

plot(st_geometry(ponds_of_interest), col = "blue")
plot(st_geometry(ponds_of_interest_centroids[13,]), col = "red", add = T)

# Get maximum pond extent in dataset 
bind_rows(ponds_other_years) %>%
  bind_rows(ponds_of_interest) %>%
  split(1:nrow(.)) %>%
  map(function(pond){
    data.frame(
      width = st_bbox(pond)$xmax - st_bbox(pond)$xmin,
      height = st_bbox(pond)$ymax - st_bbox(pond)$ymin
    )
  }) %>% 
  bind_rows() %>%
  summarize(max_width = max(width),
            max_height = max(height))




map(ponds_other_years,
    function(x){ 
      plot(st_geometry(ponds_of_interest), border = "red")
      plot(st_geometry(x), add = T)
      readline()
      })

map(1:25, plot_pond, legend = F, scale = T) %>% 
  plot_grid(plotlist = ., 
            nrow = 5, ncol = 5,
            labels = 1:25) %>%
  save_plot("figures/cbh/ponds_change_overive.png",
            .,
            bg = "white",
            base_height = 12,
            base_asp = 1)

### Plot ponds with DSM

# define helper function to obtain boundary box around plot
get_pond_bounds <- function(pond_id){
  # Get maximum extent across years (excluding 2017)
  bounds <- map(append(ponds_other_years,
                       list(ponds_of_interest)),
                function(ponds){
                  data.frame(
                    xmin = st_bbox(ponds[pond_id,])[1],
                    xmax = st_bbox(ponds[pond_id,])[3],
                    ymin = st_bbox(ponds[pond_id,])[2],
                    ymax = st_bbox(ponds[pond_id,])[4]
                  )
                }) %>%
    bind_rows() %>%
    summarize(xmin = min(xmin),
              xmax = max(xmax),
              ymin = min(ymin),
              ymax = max(ymax)) %>%
    mutate(pond_id = pond_id) %>%
    relocate(pond_id)
  # Add 25% buffer
  bounds <- bounds + data.frame(0, 
                                (bounds$xmax - bounds$xmin) * -0.25, 
                                (bounds$xmax - bounds$xmin) * 0.25, 
                                (bounds$ymax - bounds$ymin) * -0.25, 
                                (bounds$ymax - bounds$ymin) * 0.25)
  # Finnally check whether bounds to not exceed area of interest
  study_bounds <- st_bbox(st_read("data/study_aois/cbh.shp"))
  if(bounds[,2] < study_bounds[1]) bounds[,2] <- study_bounds[1]
  if(bounds[,3] > study_bounds[3]) bounds[,3] <- study_bounds[3]
  if(bounds[,4] < study_bounds[2]) bounds[,4] <- study_bounds[2]
  if(bounds[,5] > study_bounds[4]) bounds[,5] <- study_bounds[4]
  # Return result
  return(bounds)
}

# Loat functions to plot pond time series
source("scripts/figures/plot_ponds_with_dsm.R")

# Plot ponds of interest
map(1:25,
    function(x){
      plot_pond_ts(get_pond_bounds(x),
                cbh_norm,
                cbh_preds,
                cbh_dsm,
                "cbh",
                pond_2014 = ponds_other_years[[1]][x,])
    })


# calculate are of ponds that do not change
