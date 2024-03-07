# Short script to generate focal value data for the drone rasters
# Jakob J. Assmann jakob.assmann@uzh.ch 

# Dependencies
library(raster)
library(sp)
library(tidyverse)
library(gstat)
library(rgdal)
library(ggplot2)
library(cowplot)

# Read raster
dtlb_2021 <- brick("data/drone_time_series/dtlb/dtlb_norm/dtlb_2021_norm.tif")
names(dtlb_2021) <- c("R", "G", "B", "alpha")
dtlb_2021_spdf <- as(dtlb_2021, "SpatialPixelsDataFrame")

# Subsample ( 1 million points should be enough)
dtlb_2021_spdf <- dtlb_2021_spdf[sample(1:nrow(dtlb_2021_spdf), 10^6),]

# Sample varioogram for the three channels
dtlb_2021_vario_R <- variogram(R ~ 1,
                                      dtlb_2021_spdf,
                                      width = 0.12,
                                      verbose = T)
save(dtlb_2021_vario_R, file = "data/drone_time_series/variograms/dtlb_2021_vario_R.Rda")
dtlb_2021_vario_G <- variogram(R ~ 1,
                                      dtlb_2021_spdf,
                                      width = 0.12,
                                      verbose = T)
save(dtlb_2021_vario_G, file = "data/drone_time_series/variograms/dtlb_2021_vario_G.Rda")
dtlb_2021_vario_B <- variogram(R ~ 1,
                                      dtlb_2021_spdf,
                                      width = 0.12,
                                      verbose = T)
save(dtlb_2021_vario_B, file = "data/drone_time_series/variograms/dtlb_2021_vario_B.Rda")

dtlb_2021_vario_R_model <- fit.variogram(dtlb_2021_vario_R, model = vgm("Sph"))
dtlb_2021_vario_G_model <- fit.variogram(dtlb_2021_vario_G, model = vgm("Sph"))
dtlb_2021_vario_B_model <- fit.variogram(dtlb_2021_vario_B, model = vgm("Sph"))

# Quick visualisation
dtlb_2021_vario_R_plot <- ggplot(dtlb_2021_vario_R) +
  geom_point(aes(x = dist, y = gamma),
             alpha = 1,
             size = 0.1) +
  geom_col(aes(x = dist, y = -exp(np / max(dtlb_2021_vario_R$np)) / 1000)) +
  geom_line(aes(x = dist, y = gamma), 
            data = variogramLine(dtlb_2021_vario_R_model, maxdist = max(dtlb_2021_vario_R$dist)),
            colour = "blue") +
  geom_vline(xintercept = dtlb_2021_vario_R_model$range[2], colour = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -exp(10000 / max(dtlb_2021_vario_R$np)) / 1000,
             linetype = "dashed") +
  annotate("text", x = dtlb_2021_vario_R_model$range[2],
           y = max(dtlb_2021_vario_R$gamma) * 0.1,
           colour = "blue",
           label = paste0(" range = ", 
                          round(dtlb_2021_vario_R_model$range[2], 2),
                          " m"),
           hjust = 0) +
  labs(title = "Thaw Lakebed 2021 Red Channel",
       x = "Distance (m)",
       y = "Semivariance (γ)") +
  scale_x_continuous(breaks = seq(0, max(dtlb_2021_vario_R$dist), 10)) +
  theme_cowplot()

dtlb_2021_vario_G_plot <- ggplot(dtlb_2021_vario_G) +
  geom_point(aes(x = dist, y = gamma),
             alpha = 1,
             size = 0.1) +
  geom_col(aes(x = dist, y = -exp(np / max(dtlb_2021_vario_G$np)) / 1000)) +
  geom_line(aes(x = dist, y = gamma), 
            data = variogramLine(dtlb_2021_vario_G_model, maxdist = max(dtlb_2021_vario_G$dist)),
            colour = "blue") +
  geom_vline(xintercept = dtlb_2021_vario_G_model$range[2], colour = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -exp(10000 / max(dtlb_2021_vario_G$np)) / 1000,
             linetype = "dashed") +
  annotate("text", x = dtlb_2021_vario_G_model$range[2],
           y = max(dtlb_2021_vario_G$gamma) * 0.1,
           colour = "blue",
           label = paste0(" range = ", 
                          round(dtlb_2021_vario_G_model$range[2], 2),
                          " m"),
           hjust = 0) +
  labs(title = "Thaw Lakebed 2021 Green Channel",
       x = "Distance (m)",
       y = "Semivariance (γ)") +
  scale_x_continuous(breaks = seq(0, max(dtlb_2021_vario_G$dist), 10)) +
  theme_cowplot()

dtlb_2021_vario_B_plot <- ggplot(dtlb_2021_vario_B) +
  geom_point(aes(x = dist, y = gamma),
             alpha = 1,
             size = 0.1) +
  geom_col(aes(x = dist, y = -exp(np / max(dtlb_2021_vario_B$np)) / 1000)) +
  geom_line(aes(x = dist, y = gamma), 
            data = variogramLine(dtlb_2021_vario_B_model, maxdist = max(dtlb_2021_vario_B$dist)),
            colour = "blue") +
  geom_vline(xintercept = dtlb_2021_vario_B_model$range[2], colour = "blue") +
  geom_hline(yintercept = 0) +
  geom_hline(yintercept = -exp(10000 / max(dtlb_2021_vario_B$np)) / 1000,
             linetype = "dashed") +
  annotate("text", x = dtlb_2021_vario_B_model$range[2],
           y = max(dtlb_2021_vario_B$gamma) * 0.1,
           colour = "blue",
           label = paste0(" range = ",
                          round(dtlb_2021_vario_B_model$range[2], 2),
                          " m"),
           hjust = 0) +
  labs(title = "Thaw Lakebed 2021 Blue Channel",
       x = "Distance (m)",
       y = "Semivariance (γ)") +
  scale_x_continuous(breaks = seq(0, max(dtlb_2021_vario_B$dist), 10)) +
  theme_cowplot()

save_plot("data/drone_time_series/variograms/dtlb_varios.png",
          plot_grid(dtlb_2021_vario_R_plot,
                    dtlb_2021_vario_G_plot,
                    dtlb_2021_vario_B_plot,
                    nrow = 3),
          nrow = 3, 
          base_asp = 1.9)
