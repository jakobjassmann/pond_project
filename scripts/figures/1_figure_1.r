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

plot_grid(plotlist = list(water_prop_plot, climate_study),
          nrow = 2)

# Create combined plot
plot_grid(
          plot_grid(ggdraw() + draw_image("figures/overview_map.png"),
              ggdraw() + draw_image("figures/preds_plot_all.png"),
              nrow = 2,
              label_colour = "white",
              label_size = 18,
              labels = c("a", "c")),
          plot_grid(water_prop_plot,
                    climate_study, 
                    nrow = 2, 
                    labels = c("b", "d"),
                    label_size = 18,
                    align = "v"),
          byrow = FALSE) %>%
  save_plot("figures/1_figure_1.png", 
                   .,
                   base_height = 4,
                   base_asp = 3/2,
                   nrow = 2,
                   ncol = 2,
                   bg = "white")
