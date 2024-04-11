# Dependencies
library(terra)
library(tidyverse)
library(tidyterra)
library(sf)
library(cowplot)
library(colorspace)
library(pbapply)

# Load ponds filtered by size and overlap
ponds <- read_sf("data/pond_polys/pond_polys_filtered_size_intersection.gpkg")

# Check out number of ponds per year
ponds %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally()

# drop rdg ponds from analysis
ponds <- filter(ponds, site != "rdg")

# Add unique id to each ponds across both remaining sites
ponds <- ponds %>%
  mutate(site_spec_id = id) %>%
  mutate(., id = 1:nrow(.)) 

# Get combinations of intersecting ponds across year
intersections <- ponds %>%
  filter(site != "rdg") %>%
  group_by(site, year) %>%
  group_map(function(x, y, ...){
    cat(unlist(y), "\n")
    other_ponds <- filter(ponds, site == y$site, year != y$year)
    intersecting_ids <- tibble(
      id_pond = x$id, 
      ids_intersecting = st_intersects(x, other_ponds) %>%
      map(function(a) other_ponds$id[a])) %>%
      mutate(ids_intersecting = map2(id_pond, ids_intersecting, function(a,b) c(a,b))) %>%
      select(-id_pond)
    return(tibble(site = y$site,
           year = y$year, 
           intersecting_ids = intersecting_ids$ids_intersecting))
  }) %>% 
  bind_rows()

# Find unique combinations in intersections (i.e, individual pond time-series)
unique_intersections <- intersections %>%
  group_by(site) %>%
  group_map(function(x,y){
    tibble(site = y$site,
           ids_intersecting = map(x$intersecting_ids, sort) %>% 
              unique()
           ) %>% 
      mutate(., combination_id = 1:nrow(.)) %>%
      mutate(combination_id = paste0(site, "_", formatC(combination_id, w = 3, flag = "0")))
    }) %>%
  bind_rows() 

# Check stats for unique time-series:
unique_intersections %>% st_drop_geometry() %>% group_by(site) %>% tally()

# Find number of years that a pond is present in each time-series
unique_intersections <- unique_intersections %>%
  mutate(n_years_present = sapply(ids_intersecting, length))

# Check out whether there are any unexpected outliers
unique_intersections %>% filter(n_years_present <= 3) %>% 
  group_by(site, n_years_present) %>%
  tally()
  
# There are 5 ponds that have less than three overlapping counter parts in the
# time-series
filter(ponds, id %in% pull(unique_intersections %>% 
         filter(n_years_present < 3) %>% 
         mutate(ids = unlist(ids_intersecting)), ids)) %>%
  group_by(site) %>%
  group_map(function(x, y, ...){
    ggplot() + 
      geom_sf(data = ponds %>% filter(site == y$site)) +
      geom_sf(data = x, colour = "red")
    }) %>%
  plot_grid(plotlist = .)
# These seem to be ephemeral (often large-ish) ponds that might have overlapped
# with multiple smaller ponds, but these themselves did occur only once or twice in the time-series. 

# Remove these outliers
unique_intersections <- unique_intersections %>% filter(n_years_present >= 3)

# Generate histogram for number of intersections 
ggplot(unique_intersections) +
  geom_histogram(aes(x = n_years_present), breaks = seq(2.5,8.5, 1)) +
  scale_x_continuous(breaks = seq(3,8,1)) +
  facet_wrap(~site) +
  theme_cowplot()

# Check ponds present for only three years
unique_intersections %>% filter(n_years_present == 3) %>% 
  group_by(site, n_years_present) %>%
  tally()
unlist(unique_intersections %>%
         filter(site == "cbh") %>%
         filter(n_years_present == 3) %>% 
         pull(ids_intersecting))
unlist(unique_intersections %>%
         filter(site == "tlb") %>%
         filter(n_years_present == 3) %>% 
         pull(ids_intersecting))
filter(ponds, id %in% unlist(unique_intersections %>% 
                             filter(n_years_present == 3) %>% 
                             pull(ids_intersecting))) %>%
  group_by(site) %>%
  group_map(function(x, y, ...){
    ggplot() + 
      geom_sf(data = ponds %>% filter(site == y$site)) +
      geom_sf(data = x, colour = "red")
  }) %>%
  plot_grid(plotlist = .)
# Looks legit

# Stable ponds
### Here I consider ponds as stable if the following conditions are met:
### 1) present in at least (n-1) years of the time-series (allow one for detection error)
### 2) coefficient of variation of area throught time-series is less than 25% (ignoring 2017)

# Calculate the CV for each unique intersections
unique_intersections$cv <- unique_intersections %>%
  split(., .$combination_id) %>%
  sapply(function(combination){
    area <- ponds %>% 
      filter(year != 2017) %>%
      filter(id %in% combination$ids_intersecting[[1]]) %>%
      pull(area)
    return(sd(area) / mean(area))
  })

# Show distributions of CV for "stable ponds"
ggplot(unique_intersections %>% filter(n_years_present >= 7)) +
  geom_histogram(aes(x= cv), breaks = seq(0,3,.25)) +
  geom_vline(xintercept = 0.25, colour = "red") +
  annotate("text", x = 0.25, y = Inf, hjust = - 0.1, vjust = 1.5, label = "cv = 0.25", colour = "red") +
  facet_wrap(~site) +
  theme_cowplot()
ggplot(unique_intersections %>% filter(n_years_present >= 7 & cv <= 0.25)) +
  geom_histogram(aes(x= cv))

# add stability indicator
unique_intersections <- unique_intersections %>%
  mutate(stable = case_when(n_years_present >= 7 & cv <= 0.25 ~ "stable",
                            TRUE ~ "unstable"))

# Export dataset of unique intersections 
save(unique_intersections, file = "data/pond_polys/unique_intersections.Rda")


