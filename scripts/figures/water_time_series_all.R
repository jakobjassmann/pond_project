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

# Add years with 0 water for "rdg"
water_prop <- water_prop %>%
  bind_rows(
    tibble(site = "rdg", 
           year = c(2014, 2018, "2019_a", 2020, 2021),
           calendar_year = as.character(c(2014, 2018:2021)),
           area = 0,
           prop = 0)
  )
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

# Calculate coefficients of variation for each time-series
cv_site <- water_prop %>%
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              summarise(prop_mean = mean(prop)) %>%
            group_split() %>%
            map(function(x){
                return(tibble(site = unique(x$site),
                              cv_all = sd(x$prop_mean) / mean(x$prop_mean),
                              cv_no2017 = sd(filter(x, as.numeric(calendar_year) != 2017)$prop_mean) / mean(filter(x, as.numeric(calendar_year) != 2017)$prop_mean)))
            }) %>% bind_rows() %>%
            mutate(cv_all = round(cv_all * 100),
                   cv_no2017 = round(cv_no2017 * 100))
# Set CV for rdg without 2017 to 0 (returned NaN as devision through Null would be reuqired)
cv_site$cv_no2017[2] <- 0

# Plot water proportion
water_prop_plot <- ggplot() +
  # Line for rdg between 2014 and 2017
  geom_line(aes(x = as.numeric(calendar_year), 
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>%
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              filter(site == "rdg", calendar_year <= 2017) %>%
              summarise(prop_mean = mean(prop)),
            linetype = "dotted",
            linewidth = 2) +
  # Line for rdg between after 2017
  geom_line(aes(x = as.numeric(calendar_year),
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>%
              ungroup() %>%
              group_by(site, calendar_year) %>%
              filter(site == "rdg", calendar_year >= 2017) %>%
              summarise(prop_mean = mean(prop)),
            linewidth = 2) +
  # Line for tlb and cbh between 2014 and 2016
  geom_line(aes(x = as.numeric(calendar_year), 
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>% 
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              filter(year <= 2016, site != "rdg") %>%
              summarise(prop_mean = mean(prop)),
              linetype = "dotted",
              linewidth = 2) +
  # Line for tlb and cbh after 2016
  geom_line(aes(x = as.numeric(calendar_year), 
                y = prop_mean * 100, 
                colour = site,
                group = site),
            data = water_prop %>% 
              ungroup() %>% 
              group_by(site, calendar_year) %>%
              filter(year >= 2016, site != "rdg") %>%
              summarise(prop_mean = mean(prop)),
              linewidth = 2) +
  # Observations for all sites and years
  geom_point(aes(x = as.numeric(calendar_year),
                 y = prop * 100,
                 fill = site),
             data = water_prop %>%
               bind_rows(),
            shape = 21,
            stroke = 1,
            size = 3,
            alpha = 0.75) +
  labs(x = "", y = "Surface Water (%)") +
  scale_colour_manual(values = c("#FF369D", "#FFE700", "#19CEE6")) +
  scale_fill_manual(values = c("#FF369D", "#FFE700", "#19CEE6")) +
  scale_y_continuous(breaks = seq(0,20, 5), limits = c(0,20)) +
  scale_x_continuous(breaks = seq(2014,2021,1)) +
  # Annotate with cv values
  annotate("rect", xmin = 2018.2, xmax = 2021.3, 
            ymin = 14, ymax = 20,
            fill = "NA",
            colour = "black") +
  annotate("rect", xmin = 2018.3, xmax = 2018.6, 
            ymin = 18.6, ymax = 19.4,
            fill = "#FF369D",
            colour = "black") +
  annotate("rect", xmin = 2018.3, xmax = 2018.6, 
            ymin = 16.6, ymax = 17.4,
            fill = "#FFE700",
            colour = "black") +
  annotate("rect", xmin = 2018.3, xmax = 2018.6, 
            ymin = 14.6, ymax = 15.4,
            fill = "#19CEE6",
            colour = "black") +
  annotate("text", x = 2018.7, y = 19, 
           label = paste0("CV: ", cv_site[1,2], "% (", cv_site[1,3], "%)"),
           colour = "black",
           hjust = 0,
           size =  14/ .pt) +
  annotate("text", x = 2018.7, y = 17, 
           label = paste0("CV: ", cv_site[3,2], "% (", cv_site[3,3], "%)"),
           colour = "black",
           hjust = 0,
           size =  14/ .pt) +  
  annotate("text", x = 2018.7, y = 15, 
           label = paste0("CV: ", cv_site[2,2], "% (", formatC(cv_site[2,3][[1]], width = 1), "%)"),
           colour = "black",
           hjust = 0,
           size =  14 / .pt) +
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



