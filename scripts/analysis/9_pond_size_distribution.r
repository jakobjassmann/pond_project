# Distribution of pond size
# Jakob J. Assmann jakob.assmann@uzh.ch 22 May 2025

# Dependencies
library(tidyverse)
library(sf)
library(ggplot2)
library(cowplot)

# Load ponds
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")

# Calculate mean min and max area
mean(ponds$area)
min(ponds$area)
max(ponds$area)

# Visualise
ggplot(ponds, aes(x = area)) +
geom_histogram() +
theme_cowplot()
