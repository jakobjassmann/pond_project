# Visualisation of the surface water area cover over time for cbh2
# Jakob J. Assmann jakob.assmann@uzh.ch 5 October 2022

# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(cowplot)


# Get list of files
cbh_preds <- list.files("data/drone_time_series/cbh_timeseries/preds/",
                        pattern = "tif",
                        full.names = T) 

# Calculate area per year
area_water_per_year <- map(cbh_preds, function(x){
  rast_obj <- rast(x)
  data.frame(
    year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", x)),
    area = length(cells(rast_obj, 2)[[1]]) * prod(res(rast_obj)),
    total_area = ncell(rast_obj) * prod(res(rast_obj)))
}) %>% bind_rows()
write_csv(area_water_per_year,
          "data/drone_time_series/cbh_timeseries/area_water_by_year.csv")

# # Set time per year
# time_of_year <- data.frame(
#   date = as.POSIXct(
#     c("2021-07-19",
#       "2020-07-24",
#       "2019-07-12",
#       "2018-07-20",
#       "2017-07-16",
#       "2016-08-18",
#       "2016-08-12",
#       "2016-08-12",
#       "2014-08-10")))
# time_of_year$year <- as.numeric(format(time_of_year$date, "%Y"))
# time_of_year$doy <- as.numeric(format(time_of_year$date, "%j"))

# Check correlation between water area nad day of year
cor(time_of_year %>% group_by(year) %>%
      summarise(max_doy = max(doy)) %>%
      pull(max_doy), area_water_per_year$area)
# and max difference
max(max_doy$max_doy) - min(max_doy$max_doy)

# Visualise time-series 
time_series <- ggplot(area_water_per_year) +
  geom_line(aes(x = year, y = area, group = 1),
            data = filter(area_water_per_year, year != 2014),
            linewidth = 1.5) +
  geom_line(aes(x = year, y = area, group = 1),
            data = filter(area_water_per_year, year <= 2016),
            linetype = "dashed",
            linewidth = 1.5) +
  geom_errorbar(aes(x = year, ymin = area * 0.9,
                    ymax = area * 1.1),
                width = 0.2,
                size = 1,
                colour = "#DC6027") +
  geom_point(aes(x = year, y = area),
             size = 1.5) +
  scale_x_continuous(limits = c(2013.7, 2021.3), 
                     breaks = 2014:2021) +
  scale_y_continuous(limits = c(0,25000)) +
  labs(x = "Year", y = "Open Water [m2]") +
  theme_cowplot(20)
save_plot("figures/cbh/time_series_open_water.png",
          time_series, 
          bg = "white",
          base_asp = 1.8)

# Same but as proportion (for UWW250)
time_series <- ggplot(area_water_per_year) +
  geom_line(aes(x = year, y = 100*(area/total_area), group = 1),
            data = filter(area_water_per_year, year != 2014),
            linewidth = 1.5,
            colour = "#1E1D40") +
  geom_line(aes(x = year, y = 100*(area/total_area), group = 1),
            data = filter(area_water_per_year, year <= 2016),
            linetype = "dashed",
            linewidth = 1.5,
            colour = "#1E1D40") +
  geom_errorbar(aes(x = year, ymin = (100*(area/total_area)) * 0.8,
                    ymax = (100*(area/total_area)) * 1.2),
                width = 0.2,
                linewidth = 1,
                colour = "#1E1D40") +
  geom_point(aes(x = year, y = 100*(area/total_area)),
             size = 1.5) +
  scale_x_continuous(limits = c(2013.7, 2021.3), 
                     breaks = 2014:2021) +
  scale_y_continuous(limits = c(0,12),
                     breaks = seq(0,12,2)) +
  labs(x = "Year", y = "Proportion Water (%)") +
  theme_cowplot(20)
save_plot("figures/cbh/time_series_open_water_prop.png",
          time_series, 
          bg = "white",
          base_asp = 1.8)

# Visualise day of observation
time_of_observation <- ggplot(time_of_year) +
  geom_point(aes(x = year, y = doy)) +
  geom_line(data = time_of_year %>% filter(year == 2016), aes(x = year, y = doy)) +
  geom_segment(data = time_of_year %>%
              mutate(ymin = 177),
            aes(x = year, xend = year, y = ymin, yend = doy),
            linetype = "dashed", 
            size = 0.1) +
  geom_hline(yintercept = 182, linetype = "dashed") +
  geom_hline(yintercept = 213, linetype = "dashed") +
  scale_x_continuous(limits = c(2014, 2021), breaks = 2014:2021) +
  scale_y_continuous(limits = c(177, 231)) +
  annotate("text", x = 2021, y = 182 + 1, label = "1 Jul", hjust = 0) +
  annotate("text", x = 2021, y = 213 + 1, label = "1 Aug", hjust = 0) +
  labs(x = "Year", y = "Day of Drone Survey (doy)") +
  coord_flip() +
  theme_cowplot()
save_plot("figures/cbh/day_of_drone_survey.png",
          time_of_observation, bg = "white")

