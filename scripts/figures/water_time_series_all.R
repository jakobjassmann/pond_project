# Water time-series plots

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)

# Load ponds
pond_polys_filtered_size <- read_sf("data/pond_polys/pond_polys_filtered_size.gpkg")

# Calculate total water area
water_area <- pond_polys_filtered_size %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  summarize(area = sum(area)) %>%
  mutate(calendar_year = gsub("([0-9]{4}).*", "\\1", year))

# water_area_overlap <- pond_polys_filtered_size_overlap %>%
#   st_drop_geometry() %>%
#   group_by(site, year) %>%
#   summarize(area = sum(area)) %>%
#   mutate(calendar_year = gsub("([0-9]{4}).*", "\\1", year))

# Load site AOIs and calculate area
aois <- bind_rows(
  read_sf("data/drone_data/cbh/cbh_study_aoi.shp") %>%
    mutate(site = "cbh"),
  read_sf("data/drone_data/rdg/rdg_study_aoi.shp") %>%
    mutate(site = "rdg"),
  read_sf("data/drone_data/tlb/tlb_study_aoi.shp") %>%
    mutate(site = "tlb")
) %>%
  select(-FID) %>%
  mutate(area = st_area(geometry))

# Calculate water proportion 
water_prop <- water_area %>%
  split(.$site) %>%
  lapply(function(x){
    site_interest <- unique(x$site)
    x %>%
      mutate(prop = area / (filter(aois, site == site_interest) %>% pull(area))) %>%
      mutate(prop = as.numeric(prop))
  }) %>% bind_rows()

# Save as csv
write_csv(water_prop, "tables/annual_water_prpop.csv")

# water_prop_overlap <- water_area_overlap %>%
#   split(.$site) %>%
#   lapply(function(x){
#     site_interest <- unique(x$site)
#     x %>%
#       mutate(prop = area / (filter(aois, site == site_interest) %>% pull(area))) %>%
#       mutate(prop = as.numeric(prop))
#   }) %>% bind_rows()

# Plot water proportion
water_prop_plot <- ggplot() +  
  geom_line(aes(x = as.numeric(calendar_year), 
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>% 
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              filter(year >= 2016) %>%
              summarise(prop_mean = mean(prop)),
              linewidth = 2) +
  geom_line(aes(x = as.numeric(calendar_year), 
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>% 
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              filter(year <= 2016) %>%
              summarise(prop_mean = mean(prop)),
              linetype = "dotted",
              linewidth = 2) +
    geom_point(aes(x = as.numeric(calendar_year), 
                 y = prop * 100, 
                 fill = site),
                 data = water_prop,
                 shape = 21,
                 stroke = 1,
                 size = 3,
                 alpha = 0.75) +
  labs(x = "", y = "Surface Water (%)") +
  scale_colour_manual(values = c("#FF369D", "#FFE700", "#19CEE6")) +
  scale_fill_manual(values = c("#FF369D", "#FFE700", "#19CEE6")) +
  scale_y_continuous(breaks = seq(0,20, 5), limits = c(0,20)) +
  scale_x_continuous(breaks = seq(2014,2021,1)) +
  theme_cowplot(16) +
  theme(legend.position = "none")
save_plot("figures/water_proportion_all.png", 
            water_prop_plot, 
            bg = "white",
            base_asp = 3 / 2)

# (ggplot() +
#     geom_line(aes(x = as.numeric(calendar_year), y = prop_mean * 100, colour = site, group = site),
#               data= water_prop_overlap %>% ungroup() %>% group_by(site, calendar_year) %>% 
#                 summarise(prop_mean = mean(prop))) +
#     geom_point(aes(x = as.numeric(calendar_year), y = prop * 100, colour = site),
#                data = water_prop_overlap) +
#     labs(x = "", y = "water proportion (%)") +
#     scale_y_continuous(breaks = seq(0,8, 2), limits = c(0,8)) +
#     scale_x_continuous(breaks = seq(2014,2021,1)) +
#     theme_cowplot()) %>%
#   save_plot("figures/water_proportion_all_overlap.png", ., bg = "white")

# # Add linear models to plots
# (ggplot() +
#     geom_line(aes(x = as.numeric(calendar_year), y = prop_mean * 100, colour = site, group = site),
#               data= water_prop_overlap %>% ungroup() %>% group_by(site, calendar_year) %>% 
#                 summarise(prop_mean = mean(prop))) +
#     geom_point(aes(x = as.numeric(calendar_year), y = prop * 100, colour = site),
#                data = water_prop_overlap) +
#     geom_smooth(aes(x = as.numeric(calendar_year), y = prop * 100,  group = site),
#                 method = "lm", data = water_prop_overlap) +
#     labs(x = "", y = "water proportion (%)") +
#     scale_y_continuous(breaks = seq(0,8, 2), limits = c(0,8)) +
#     scale_x_continuous(breaks = seq(2014,2021,1)) +
#     theme_cowplot()) %>%
#   save_plot("figures/water_proportion_all_overlap_with_lm.png", ., bg = "white")



