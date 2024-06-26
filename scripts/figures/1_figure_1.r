# Source and arrange figure 1
# This script relies on the following oder scripts:
# - water_time_series_all.R
# - climate_analaysis.R
# - all_raster_mosaics.R (to be run separately)
# - sutdy_site_map_figure.R (to be run separately)
# Jakob J. Assmann jakobjassmann@gmail.com 20 May 2024

# Dependencies
library(tidyverse)
library(cowplot)

# Source surface water time-series and climate analysis scripts
source("scripts/figures/water_time_series_all.R")
source("scripts/analysis/climate_analysis.R")

# Define 8% margin based on plot size of 4 inch
marings_perc <- list(t = 4 * 0.06,
     r = 4 * (3/2) * 0.06,
     b = 4 * 0.06,
     l = 4 * (3/2) * 0.06)

# Create combined plot
{ggdraw() + draw_plot(
  plot_grid(
    plot_grid(
      ggdraw() + draw_image("figures/overview_map.png", scale = 0.92),
      ggdraw() + draw_image("figures/preds_plot_all.png", scale = 0.92),
      nrow = 1,
      labels = letters[1:2],
      label_size = 16
    ),
    plot_grid(water_prop_plot + theme(plot.margin = margin(marings_perc, unit = "inch")),
              climate_study + theme(plot.margin = margin(marings_perc, unit = "inch")), 
              nrow = 1,
              labels = letters[3:4],
              label_size = 16,
              align = "h"),
    nrow = 2)) +
  # +
  # draw_image("figures/icons/water_proportion.png",
  # scale = 0.1, x = 0.1, y = 0.42) + 
  draw_image("figures/icons/thermometer.png",
  scale = 0.1, x = 0.105, y = - 0.08) +
  draw_image("figures/icons/snow.png",
  scale = 0.1, x = 0.37, y = - 0.08)
  } %>%
  save_plot("figures/1_figure_1.png", 
                   .,
                   base_height = 4,
                   base_asp = 3/2,
                   nrow = 2,
                   ncol = 2,
                   bg = "white")

