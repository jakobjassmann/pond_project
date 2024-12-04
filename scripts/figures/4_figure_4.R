# Pond Project Figure 3
# Jakob J. Assmann jakob.assmann@uzh.ch 30 May 2024
# Note: this script requires "pond_time_series_analysis.R" to be run

# Dependencies
library(ggplot2)
library(tidyverse)
library(sf)
library(cowplot)
library(gt)

# Load annotated pond-time-series dataset
load("data/pond_polys/pond_time_series.Rda")

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

# Source surface volume gained plot
source("scripts/figures/surface_volume_gained.R")

# Source pond time series plotting functions
source("scripts/figures/pond_timeseries_with_dsm.R")

# Calculate summary stats for pond change
# Number of ponds where thermokarst was detected
(pond_time_series_ids %>% 
    st_drop_geometry() %>%
    filter(mean_volume_gain_per_m2 >= 0.1) %>% 
    nrow())
# Percentage of ponds where thermokarst was detected
((pond_time_series_ids %>% 
    st_drop_geometry() %>%
    filter(mean_volume_gain_per_m2 >= 0.1) %>% 
    nrow()) / nrow(pond_time_series_ids)) %>%
  round(2)
# Per site
pond_time_series_ids %>% 
  st_drop_geometry() %>%
  group_by(site) %>%
  filter(mean_volume_gain_per_m2 >= 0.1) %>% 
  tally() %>% 
  mutate(per = n / pull(tally(group_by(st_drop_geometry(pond_time_series_ids), site)), n)) %>%
  mutate(per = round(per, 2))
# Percentage of ponds with either vegetation incursion or thermocarst
((pond_time_series_ids %>% 
    st_drop_geometry() %>%
    filter(mean_volume_gain_per_m2 >= 0.1 | mean_volume_loss_per_m2 >= 0.1) %>% 
    nrow()) / nrow(pond_time_series_ids)) %>%
  round(2)

# overwrite legend function to updated legend
legend_manuscript <- legend_manuscript2
# and set rel_heights
rel_heights <- c(2,1/2)

# Generate time splot for cbh_049
pond_plot <- composite_plot(pond_time_series_ids %>% filter(ts_id == "cbh_049"),
               save_plot = F,
               return_plot = T,
               separate_legend = F,
               manuscript_legend = T,
               add_transect = T,
               add_caption = T)

# Pond statistics
pond_time_series_ids %>% 
  filter(ts_id == "cbh_049") %>%
  st_drop_geometry() %>%
  select(mean_volume_loss_per_m2, mean_volume_gain_per_m2)


# Set site colours
rdg_col <- "#FFE700"
cbh_col <- "#FF369D"
tlb_col <- "#19CEE6"
site_col <- c(tlb_col, cbh_col)

# Update site names for plotting 
pond_time_series_ids <- mutate(pond_time_series_ids,
                               site_plot = case_when(site == "cbh" ~ "high",
                                                     site == "tlb" ~ "medium",
                                                     site == "rdg" ~ "low",
                                                     TRUE ~ site)) %>%
  mutate(site_plot = factor(site_plot, levels = c("low", "medium", "high")))



# Plot histograms for volume gain in each pond time series
volume_gained_hist <- ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_volume_gain_per_m2, fill = site_plot), 
                 binwidth = 0.025, 
                 colour = "grey20") +
  annotate("segment", x = 0.1, xend = 0.1,
           y = -Inf, yend = 110,
           colour = "darkblue") +
  annotate("text", 
           x = 0.1,
           y = 110,
           label = "colonisation threshold\n < 18% (n = 84)",
           hjust = - 0.1,
           vjust = 1,
           colour = "darkblue",
           size = 14 / .pt) +
  # geom_segment(aes(x = 0.1, xend = 0.1,
  #                  y = -Inf, yend = height),
  #              colour = "darkblue",
  #              data = pond_time_series_ids %>% 
  #                st_drop_geometry() %>%
  #                group_by(site_plot) %>%
  #                slice(1) %>%
  #                mutate(height = case_when(site_plot == "med" ~ 40,
  #                                          site_plot == "high" ~ 90))) +
  # geom_text(aes(x = 0.1, 
  #               y = height,
  #               label = "detection threshold"), 
  #           hjust = - 0.1,
  #           vjust = 1,
  #           colour = "darkblue",
  #           size = 14 / .pt,
  #           data = pond_time_series_ids %>% 
  #             st_drop_geometry() %>%
  #             group_by(site_plot) %>%
  #             slice(1) %>%
  #             mutate(height = case_when(site_plot == "med" ~ 40,
  #                                       site_plot == "high" ~ 90))) +
  geom_text(aes(x = 0.2, y = height,
                label = paste0("Site: ", site_plot),
                colour = site_plot),
            data = pond_time_series_ids %>%
              st_drop_geometry() %>% 
              distinct(site_plot) %>%
              mutate(height = c(135,150)),
            fontface = "bold",
            hjust = 0, vjust = 1,
            size = 14 / .pt) +
  # annotate("curve",
  #          x = 0.15, y = 25, xend = Inf, yend = 50,
  #          arrow = arrow(length = unit(0.03, "npc")),
  #          curvature = -0.3,
  #) +
  labs(x = "Mean gain in surface elevation (m)\n 2014 vs 2021", y = "Number of Ponds") +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  # scale_x_continuous(limits = c(-0.1, 0.4)) +
  scale_y_continuous(limits = c(0, 150)) +
  # facet_wrap(~site_plot, scales = "free_y") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white"))



# Composite plot
plot_grid(
  ggdraw() + 
    draw_plot(ggplot() + 
                theme_nothing() + 
                theme(plot.background = element_rect("black"),
                      panel.background = element_rect("black")),
              scale = 0.94) +
    draw_plot(pond_plot, scale = 0.94), 
  ggdraw() + draw_plot(
    plot_grid(
      volume_gained_hist,
      plot_grid(
        transect_plot,
        colour_legend,
        rel_heights = c(1.5 + 1.5 * 1/3, 1),
        nrow = 2,
        labels = c("c", ""),
        label_size = 18 * 1 / 0.94,
        vjust = c(1.1, 1.8),
        hjust = 1.6),
      ncol = 2,
      rel_widths = c(4,3)),
    scale = 0.94),
  nrow = 2,
  ncol = 1,
  labels = letters[1:2],
  label_size = 18 * 1,
  vjust = c(1.6, 1.6),
  rel_heights = c(2 + 1/2, 2.5 + 1.5 * 1/3)) %>%
  save_plot("figures/4_figure_4.png",
            .,
            ncol = 1,
            nrow = 1,
            base_height = 2 * 3.71,
            base_asp = (7*(pond_bounds[2]-pond_bounds[1])) /
              ((2+ 1/2 + 2.5 + 1.5 * 1/3) * (pond_bounds[4]-pond_bounds[3])),
            bg = "white")

# Plot histograms for volume gain in each pond time series for each site for 
# Fig. S5
(volume_gained_hist_site <- ggplot(pond_time_series_ids) +
  geom_histogram(aes(x = mean_volume_gain_per_m2, fill = site_plot), 
                 binwidth = 0.025, 
                 colour = "grey20") +
  annotate("segment", x = 0.1, xend = 0.1,
           y = -Inf, yend = 80,
           colour = "darkblue") +
  # annotate("text", 
  #          x = 0.1,
  #          y = 110,
  #          label = "detection threshold\n< 18% (n = 84)",
  #          hjust = - 0.1,
  #          vjust = 1,
  #          colour = "darkblue",
  #          size = 14 / .pt) +
  geom_segment(aes(x = 0.1, xend = 0.1,
                   y = -Inf, yend = height),
               colour = "darkblue",
               data = pond_time_series_ids %>%
                 st_drop_geometry() %>%
                 group_by(site_plot) %>%
                 slice(1) %>%
                 mutate(height = case_when(site_plot == "medium" ~ 100,
                                           site_plot == "high" ~ 100))) +
  geom_text(aes(x = 0.1,
                y = height,
                label = paste0("colonis. threshold\n < ", perc, "% (n = ", n, ")")),
            hjust = - 0.1,
            vjust = 1,
            colour = "darkblue",
            size = 14 / .pt,
            data = pond_time_series_ids %>%
              st_drop_geometry() %>%
              group_by(site_plot) %>%
              slice(1) %>%
              mutate(height = case_when(site_plot == "medium" ~ 100,
                                        site_plot == "high" ~ 100),
                     perc = case_when(site_plot == "medium" ~ 37,
                                        site_plot == "high" ~ 12),
                     n = case_when(site_plot == "medium" ~ 40,
                                        site_plot == "high" ~ 43))) +
  geom_text(aes(x = Inf, y = height,
                label = paste0("Site: ", site_plot),
                colour = site_plot),
            data = pond_time_series_ids %>%
              st_drop_geometry() %>% 
              distinct(site_plot) %>%
              mutate(height = c(150,150)),
            fontface = "bold",
            hjust = 1, vjust = 1,
            size = 14 / .pt) +
  # annotate("curve",
  #          x = 0.15, y = 25, xend = Inf, yend = 50,
  #          arrow = arrow(length = unit(0.03, "npc")),
  #          curvature = -0.3,
  # ) +
  labs(x = "Mean gain in surface elevation (m)\n 2014 vs 2021", y = "Number of Ponds") +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  scale_x_continuous(limits = c(-0.1, 0.4)) +
  scale_y_continuous(limits = c(0, 150)) +
  facet_wrap(~site_plot, scales = "free_y") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank()) +
  theme(plot.background = element_rect(fill = "white"),
        panel.background = element_rect(fill = "white")))
save_plot("figures/6_figure_S5.png", 
          volume_gained_hist_site,
          nrow = 1,
          ncol = 2,
          base_asp = 1,
          bg = "white")
