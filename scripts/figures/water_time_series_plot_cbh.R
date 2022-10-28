# Visualisation of the surface water area cover over time for cbh2
# Jakob J. Assmann jakob.assmann@uzh.ch 5 October 2022

# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(cowplot)


# Get list of files
cbh_preds <- list.files("data/drone_time_series/cbh_2/cbh_preds/",
                        pattern = "tif",
                        full.names = T) 

# Calculate area per year
area_water_pre_year <- map(cbh_preds, function(x){
  rast_obj <- rast(x)
  data.frame(
    year = as.numeric(gsub(".*([0-9]{4}).*", "\\1", x)),
    area = length(cells(rast_obj, 2)[[1]]) * prod(res(rast_obj)))
}) %>% bind_rows()

# Set time per year
time_of_year <- data.frame(
  date = as.POSIXct(
    c("2021-07-19",
      "2020-07-24",
      "2019-07-12",
      "2018-07-20",
      "2017-07-16",
      # "2016-08-18",
      # "2016-08-12",
      # "2016-08-12",
      "2014-08-10")))
time_of_year$year <- as.numeric(format(time_of_year$date, "%Y"))
time_of_year$doy <- as.numeric(format(time_of_year$date, "%j"))

# Visualise time-series 
time_series <- ggplot(area_water_pre_year) +
  geom_point(aes(x = year, y = area)) +
  geom_line(aes(x = year, y = area, group = 1),
            data = filter(area_water_pre_year, year != 2014)) +
  geom_line(aes(x = year, y = area, group = 1),
            data = filter(area_water_pre_year, year <= 2017),
            linetype = "dashed") +
  scale_x_continuous(limits = c(2014, 2021), breaks = 2014:2021) +
  scale_y_continuous(limits = c(0,25000)) +
  labs(x = "Year", y = "Open Water [m2]") +
  theme_cowplot()
save_plot("figures/cbh_2/time_series_open_water.png",
          time_series, bg = "white")

# Visualise day of observation
time_of_observation <- ggplot(time_of_year) +
  geom_point(aes(x = year, y = doy)) +
  geom_hline(yintercept = 182, linetype = "dashed") +
  geom_hline(yintercept = 213, linetype = "dashed") +
  scale_x_continuous(limits = c(2014, 2021), breaks = 2014:2021) +
  scale_y_continuous(limits = c(177,229)) +
  annotate("text", x = 2021, y = 182 + 1, label = "1 Jul", hjust = 0) +
  annotate("text", x = 2021, y = 213 + 1, label = "1 Aug", hjust = 0) +
  labs(x = "Year", y = "Day of Drone Survey (doy)") +
  coord_flip() +
  theme_cowplot()
save_plot("figures/cbh_2/day_of_drone_survey.png",
          time_of_observation, bg = "white")

