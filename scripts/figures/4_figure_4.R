# Pond Project Figure 3
# Jakob J. Assmann jakob.assmann@uzh.ch 30 May 2024
# Note: this script requires "pond_time_series_analysis.R" to be run

# Dependencies
library(gpplot)
library(tidyverse)
library(sf)
library(cowplot)
library(gt)

# Load annotated pond-time-series dataset
load("data/pond_polys/pond_time_series.Rda")

# Source pond time series plotting functions
source("scripts/figures/pond_timeseries_with_dsm.R")

# Overide buggy get legend if needed
get_legend <- function(plot){
  legends <- get_plot_component(plot, "guide-box", return_all = T)
  legends_new <- list()
  for(i in 1:length(legends)){
    if(!("zeroGrob" %in% class(legends[[i]]))) legends_new <- c(legends_new, list(legends[[i]]))
  }
  do.call(cbind, legends_new)
}

# Generate time splot for cbh_049
pond_plot <- composite_plot(pond_time_series_ids %>% filter(ts_id == "cbh_049"),
               save_plot = F,
               return_plot = T,
               separate_legend = T)


# Set side colours
rdg_col <- "#FFE700"
cbh_col <- "#FF369D"
tlb_col <- "#19CEE6"
site_col <- c(tlb_col, cbh_col)

# Update site names for plotting 
pond_time_series_ids <- mutate(pond_time_series_ids,
                               site_plot = case_when(site == "cbh" ~ "high",
                                                     site == "tlb" ~ "med",
                                                     site == "rdg" ~ "low",
                                                     TRUE ~ site)) %>%
  mutate(site_plot = factor(site_plot, levels = c("low", "med", "high")))



# Plot histograms for volume gain in each pond time series
volume_gained_hist <- ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_volume_gain_per_m2, fill = site_plot), 
                 binwidth = 0.025, 
                 colour = "white") +
  geom_segment(aes(x = 0.1, xend = 0.1,
                   y = -Inf, yend = height),
               colour = "darkblue",
               data = pond_time_series_ids %>% 
                 st_drop_geometry() %>%
                 group_by(site_plot) %>%
                 slice(1) %>%
                 mutate(height = case_when(site_plot == "med" ~ 40,
                                           site_plot == "high" ~ 90))) +
  geom_text(aes(x = 0.1, 
                y = height,
                label = "detection threshold"), 
            hjust = - 0.1,
            vjust = 1,
            colour = "darkblue",
            size = 14 / .pt,
            data = pond_time_series_ids %>% 
              st_drop_geometry() %>%
              group_by(site_plot) %>%
              slice(1) %>%
              mutate(height = case_when(site_plot == "med" ~ 40,
                                        site_plot == "high" ~ 90))) +
  geom_text(aes(x = Inf, y = Inf,
                label = paste0("Site: ", site_plot),
                colour = site_plot),
            data = pond_time_series_ids %>%
              st_drop_geometry() %>% 
              distinct(site_plot),
            fontface = "bold",
            hjust = 1.2, vjust = 1.5,
            size = 14 / .pt) +
  labs(x = "Mean volume gained per surface area lost (m³ / m²)", y = "Number of Ponds") +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  scale_x_continuous(limits = c(-0.1, 0.4)) +
  scale_y_continuous(limits = c(0, 100)) +
  facet_wrap(~site_plot, scales = "free_y") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))

# Composite plot
plot_grid(pond_plot[[1]],
          plot_grid(volume_gained_hist, 
                    plot_grid(pond_plot[[2]], 
                              pond_plot[[3]], 
                              nrow = 2, 
                              ncol = 1), 
                    ncol = 2, rel_widths = c(6,1)), 
          nrow = 2,
          ncol = 1,
          rel_heights = c(0.825, 1)) %>%
  save_plot("figures/4_figure_4.png",
            .,
            ncol = 1,
            nrow = 2,
            base_asp = 3,
            bg = "black")
