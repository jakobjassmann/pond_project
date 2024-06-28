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

# Override buggy cowplot::get_legend if needed
get_legend <- function(plot){
  legends <- get_plot_component(plot, "guide-box", return_all = T)
  legends_new <- list()
  for(i in 1:length(legends)){
    if(!("zeroGrob" %in% class(legends[[i]]))) legends_new <- c(legends_new, list(legends[[i]]))
  }
  if(length(legends_new) > 0){
    do.call(cbind, legends_new)
  } else{
    return(legends[[1]])
  }
}
(test_plot <- ggplot()+
  geom_point(aes(x = 1:10, y = 1:10, 
                 colour = 1:10, 
                 size = 1:10),
                 shape = 21)) +
  guides(colour = guide_colourbar(position = "bottom"),
         size = guide_legend(position = "top"))
ggdraw() + draw_grob(get_legend(test_plot))
get_legend(ggplot())

# Calculate summary stats for pond change
# Number of ponds where thermokarst was detected
(pond_time_series_ids %>% 
  st_drop_geometry() %>%
  filter(mean_volume_loss_per_m2 >= 0.1) %>% 
  nrow())
# Percentage of ponds where thermokarst was detected
((pond_time_series_ids %>% 
    st_drop_geometry() %>%
    filter(mean_volume_loss_per_m2 >= 0.1) %>% 
    nrow()) / nrow(pond_time_series_ids)) %>%
  round(2)
# Per site
pond_time_series_ids %>% 
  st_drop_geometry() %>%
  group_by(site) %>%
  filter(mean_volume_loss_per_m2 >= 0.1) %>% 
  tally() %>% 
  mutate(per = n / pull(tally(group_by(st_drop_geometry(pond_time_series_ids), site)), n)) %>%
  mutate(per = round(per, 2))

# Generate time splot for cbh_049
pond_plot <- composite_plot(pond_time_series_ids %>% filter(ts_id == "tlb_013"),
               save_plot = F,
               return_plot = T,
               separate_legend = F,
               manuscript_legend = T)


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
(volume_lost_hist <- ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_volume_loss_per_m2, fill = site_plot), 
                 binwidth = 0.025, 
                 colour = "grey20") +
  geom_segment(aes(x = 0.1, xend = 0.1,
                   y = -Inf, yend = height),
               colour = "darkblue",
               data = pond_time_series_ids %>% 
                 st_drop_geometry() %>%
                 group_by(site_plot) %>%
                 slice(1) %>%
                 mutate(height = case_when(site_plot == "med" ~ 50,
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
              mutate(height = case_when(site_plot == "med" ~ 50,
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
  labs(x = "Volume lost per area gained (m³ / m²)", y = "Number of Ponds") +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  # scale_x_continuous(limits = c(-0.05, 0.55)) +
  scale_y_continuous(limits = c(0, 250)) +
  facet_wrap(~site_plot, scales = "free_y") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")))


# Composite plot
plot_grid(
  ggdraw() + 
            draw_plot(ggplot() + 
                        theme_nothing() + 
                        theme(plot.background = element_rect("black"),
                              panel.background = element_rect("black")),
                      scale = 0.94) +
            draw_plot(pond_plot, scale = 0.94), 
  ggdraw() + draw_plot(volume_lost_hist, scale = 0.94),
          nrow = 2,
          ncol = 1,
          labels = letters[1:2],
          vjust = 1.6) %>%
  save_plot("figures/3_figure_3.png",
            .,
            ncol = 1,
            nrow = 2,
            base_asp = (7*15.76)/(3*14.2),
            bg = "white")

