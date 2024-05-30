# Figure 2 - CV of time-series for individual ponds and area change
# Jakob J. Assmann jakob.assmann@uzh.ch 23 May 2024

# Note: requires pond_time_series_analysis.R to be completely run

# Dependencies
library(tidyverse)
library(sf)
library(cowplot)

# Load annotated pond-time-series dataset
load("data/pond_polys/pond_time_series.Rda")

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

# # Load assoiated ponds
# ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")

# Calculate proportion of time-series shorter or longer than 6 years
perc_lt6 <- pond_time_series_ids %>%
    group_by(site) %>%
    group_split() %>%
    map(function(x) tibble(site = unique(x$site),
                           site_plot = unique(x$site_plot),
                           perc = 100 * sum(x$n_years < 6) / nrow(x))) %>%
    bind_rows() %>% 
    mutate(perc = paste0(round(perc), "% < ")) %>%
  mutate(height = case_when(site == "tlb" ~ 50,
                            site == "cbh" ~ 105))
perc_gt6 <- pond_time_series_ids %>%
    group_by(site) %>%
    group_split() %>%
    map(function(x) tibble(site = unique(x$site),
                           site_plot = unique(x$site_plot),
                           perc = 100* sum(x$n_years >= 6) / nrow(x))) %>%
    bind_rows() %>%
    mutate(perc = paste0(" > ", round(perc), "%")) %>%
  mutate(height = case_when(site == "tlb" ~ 50,
                            site == "cbh" ~ 105))

# Histogram of time-series length
ts_length_hist <- 
  ggplot(pond_time_series_ids %>%
           mutate(height = case_when(site == "tlb" ~ 50,
                                     site == "cbh" ~ 105))) +
  geom_histogram(aes(x = n_years, fill = site_plot), binwidth = 1) +
  scale_y_continuous(limits = c(0,120)) +
  labs(x = "Number of years pond present in time-series", y = "Number of Ponds") +
  geom_segment(x = 5.5, xend = 5.5, 
               y = -Inf, mapping = aes(yend = height),
               colour = "darkblue") +
  geom_text(aes(label = perc, y = height),
            x = 5.5,  
            hjust = 0, vjust = 1.5,
            size = 14 / .pt,
            colour = "darkblue", 
            data = perc_gt6) +
  geom_text(aes(label = perc, y = height),
            x = 5.5,
            hjust = 1, vjust = 1.5,
            size = 14 / .pt,
            colour = "darkblue", 
            data = perc_lt6) +  
  geom_text(aes(x = Inf, y = Inf,
                label = paste0("Site: ", site_plot),
                colour = site_plot),
            data = pond_time_series_ids %>%
              st_drop_geometry() %>% 
              distinct(site_plot),
            fontface = "bold",
            hjust = 1.2, vjust = 1.5,
            size = 14 / .pt) +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  facet_wrap(~site_plot, scales = "free") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank())
ts_length_hist

# Histogram of CV for each time-series (excluding 2017)
cv_hist <- ggplot(pond_time_series_ids %>%
                    filter(n_years >= 3) %>%
                    mutate(height = case_when(site == "tlb" ~ 25,
                                              site == "cbh" ~ 47.5))) +
  geom_histogram(aes(x= cv * 100, fill = site_plot), breaks = seq(0,2,0.1) * 100) +
  geom_segment(aes(x = mean(cv) * 100, xend = mean(cv) * 100,
                   y = -Inf, yend = height),
               colour = "darkblue") +
  geom_text(aes(x = cv * 100, 
                label = paste0("mean = ", round(cv * 100), "%"),
                y = height), 
            hjust = - 0.1,
            vjust = 1.5,
            colour = "darkblue",
            size = 14 / .pt,
            data = pond_time_series_ids %>% 
              filter(n_years >= 3) %>%
              st_drop_geometry() %>% 
              group_by(site_plot) %>%
              summarise(cv = mean(cv)) %>%
              mutate(height = case_when(site_plot == "med" ~ 25,
                                        site_plot == "high" ~ 47.5))) +
  geom_text(aes(x = Inf, y = Inf,
                label = paste0("Site: ", site_plot),
                colour = site_plot),
            data = pond_time_series_ids %>%
              st_drop_geometry() %>% 
              distinct(site_plot),
            fontface = "bold",
            hjust = 1.2, vjust = 1.5,
            size = 14 / .pt) +
  scale_x_continuous(limits = c(0, 200)) +
  scale_y_continuous(limits = c(0, 50)) +
  scale_colour_manual(values = site_col) +
  scale_fill_manual(values = site_col) +
  labs(x = "Variability in pond area relative to mean - CV (%)", y= "Number of Ponds") +
  facet_wrap(~site_plot, scales = "free") +
  theme_cowplot() + 
  theme(legend.position = "none",
        strip.background = element_blank(),
        strip.text = element_blank())


# Combine both plots into one figure
combined_plot <- ggdraw() +
  draw_plot(
    plot_grid(ts_length_hist,
              cv_hist,
              nrow = 2, ncol = 1,
              align = "hv", 
              labels = letters[1:2])) +
  draw_image("figures/icons/pond_occurence.png",
             scale = 0.1, x = -0.345, y = 0.435) +
  draw_image("figures/icons/pond_area_change.png",
             scale = 0.1, x = -0.345, y = -0.065)

# Save plot
save_plot("figures/2_figure_2.png", 
          combined_plot, 
          nrow = 2, 
          ncol = 1, 
          base_asp = 2,
          bg = "white")

