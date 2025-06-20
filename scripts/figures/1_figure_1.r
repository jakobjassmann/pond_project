# Source and arrange figure 1
# This script relies on the following oder scripts:
# - water_time_series_all.R
# - climate_analaysis.R
# - all_raster_mosaics.R (to be run separately)
# - sutdy_site_map_figure.R (to be run separately)
# Jakob J. Assmann jakob.assmann@uzh.ch 20 May 2024

# Dependencies
library(tidyverse)
library(cowplot)

# Source surface water time-series and climate analysis scripts
source("scripts/analysis/4_water_time_series_all.R")
#source("scripts/analysis/climate_analysis.R")

# Define 8% margin based on plot size of 4 inch
marings_perc <- list(t = 4 * 0.06,
     r = 4 * (3/2) * 0.06,
     b = 4 * 0.06,
     l = 4 * (3/2) * 0.06)
# And reduce l and r margins for the double row plot
marings_perc2 <- list(t = 0,
                     r = 4 * (3/2) * 0.06,
                     b = 0,
                     l = 4 * (3/2) * 0.06)

# Create combined plot
{ggdraw() + draw_plot(
  plot_grid(
    plot_grid(
      ggdraw() + draw_image("figures/overview_map.png", scale = 1) +
        theme(plot.margin = margin(marings_perc, unit = "inch")),
      water_prop_plot + theme(plot.margin = margin(marings_perc, unit = "inch")),
      nrow = 1,
      labels = c("(a)", "(b)"),
      label_size = 18,
      hjust = 0
    ),
    ggdraw() + draw_image("figures/preds_plot_all.png", scale = 1) +
      theme(plot.margin = margin(marings_perc2, unit = "inch")),
    nrow = 2,
    labels = c("", "(c)"),
    hjust = 0,
    label_size = 18))
  } %>%
  save_plot("figures/1_figure_1.png", 
                   .,
                   base_height = 4,
                   base_asp = 3/2,
                   nrow = 2,
                   ncol = 2,
                   bg = "white")

