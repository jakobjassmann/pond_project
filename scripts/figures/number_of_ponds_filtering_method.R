# Number of ponds time-series plots

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)

# Load ponds
pond_polys_filtered_size <- read_sf("data/pond_polys/pond_polys_filtered_size.gpkg")
load("data/pond_polys/pond_time_series.Rda")
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")

# Tally number of ponds
number_of_ponds <- bind_rows(pond_polys_filtered_size %>%
  mutate(site_year = paste0(site, "_", year)) %>%
  filter(!(site_year %in% c("cbh_2019", "tlb_2019_a", "tlb_2019_b"))) %>%
  mutate(year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", year))) %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally() %>%
  mutate(filter = "ponds > 1m2 (1-2 years)"),
  ponds %>%
    filter(id %in% (pond_time_series_ids$combination %>% 
             unlist(recursive = T) %>%
             unique())) %>%
  st_drop_geometry() %>%
  group_by(site, year) %>%
  tally() %>%
  mutate(filter = "ponds > 1m2 (3-7 years)"),
  tibble(site = "rdg",
         year = rep(c(2014,2016,2018,2019,2020,2021),2),
         n = 0,
         filter = c(rep("ponds > 1m2 (1-2 years)", 6), rep("ponds > 1m2 (3-7 years)", 6))
         )
  )

# Plot bar chart
(ggplot(number_of_ponds) +
  geom_col(aes(x = year, y = n, fill = filter), position = "dodge") +
  facet_wrap(vars(site), scales = "free_x") +
  labs(x = "", y = "number of ponds") +
  theme_cowplot() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))) %>%
  save_plot("figures/number_of_ponds_filtering_method.png", ., bg = "white",
            base_height = 6)

# What is the average length of a pond time-series?
(ggplot(pond_time_series_ids) +
    geom_histogram(aes(x = n_years), binwidth = 1) +
    scale_y_continuous(limits = c(0,120)) +
    labs(x = "Number of years present", y = "Count") +
    geom_vline(xintercept = 5.5, colour = "red") +
    geom_text(aes(label = perc),
             x = 5.5, y = Inf, 
             hjust = - 0.1, vjust = 1.5,
             colour = "red", 
             data = tibble(site = c("cbh", "tlb"),
                           perc = c("--> 35%", "--> 42%"))) +
    geom_text(aes(label = perc),
              x = 3.75, y = Inf, 
              hjust = -0.2, vjust = 1.5,
              colour = "red", 
              data = tibble(site = c("cbh", "tlb"),
                            perc = c("75% <--", "58% <--"))) +
    facet_wrap(vars(site)) +
    theme_cowplot()) %>%
  save_plot("figures/n_years_pond_ts.png", ., bg = "white")

# Get stats for the above
sum(pond_time_series_ids$n_years >= 6) / nrow(pond_time_series_ids)

pond_time_series_ids %>% group_by(site) %>%
  st_drop_geometry() %>%
  tally()

pond_time_series_ids %>% group_by(site) %>%
  filter(n_years >= 6) %>%
  st_drop_geometry() %>%
  tally()
