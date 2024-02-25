# Number of ponds time-series plots

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)

# Load ponds
pond_polys_filtered_size <- read_sf("data/pond_polys/pond_polys_filtered_size.gpkg")
pond_polys_filtered_size_overlap <- read_sf("data/pond_polys/pond_polys_filtered_size_overlap.gpkg")

# Tally number of ponds
number_of_ponds <- bind_rows(pond_polys_filtered_size %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally() %>%
  mutate(filter = "size_only"),
pond_polys_filtered_size_overlap %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally() %>%
  mutate(filter = "size_and_overlap"))

# Plot bar chart
(ggplot(number_of_ponds) +
  geom_col(aes(x = year, y = n, fill = filter), position = "dodge") +
  facet_wrap(vars(site), scales = "free_x") +
  labs(x = "", y = "number of ponds") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))) %>%
  save_plot("figures/number_of_ponds_filtering_method.png", ., bg = "white",
            base_height = 6)
