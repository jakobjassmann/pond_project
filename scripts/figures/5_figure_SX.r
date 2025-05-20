
# 

# Dependencies
library(tidyverse)
library(sf)
library(pbapply)
library(parallel)
library(ggplot2)
library(cowplot)
library(sfheaders)

# Set up parallel environment
cl <- detectCores() -1
# on Windows
#cl <- makeCluster(cl)

# Load ponds
ponds <- read_sf("data/pond_polys/ponds_for_time_series.gpkg")

# Test method by filling "holes" in perimeter
test <- st_geometry(ponds[1,])
plot(test)
plot(sfheaders::sf_remove_holes(test), col = "red", add = T)

# Apply across all ponds (takes ~15 min on my computer)
ponds$prec_veg <- ponds %>% split(.$id) %>% pblapply(
    function(x){
        pond_geom <- sf::st_geometry(x)
        pond_geom_no_holes <- sfheaders::sf_remove_holes(pond_geom)
        return(1-(as.numeric(sf::st_area(pond_geom))/as.numeric(sf::st_area(pond_geom_no_holes))))
    }, cl = cl
) %>% unlist()

# Check max proportion non-water in perimeter
sum(ponds$prec_veg > 0.3)
# 0
max(ponds$prec_veg)
#  0.2908908

# Plot histogram
hist_fig <- ggplot(ponds, aes(x = prec_veg)) +
geom_histogram(binwidth = 0.025) +
scale_y_continuous(limits = c(0,3000)) +
labs(x = "Fraction of non-water pixels in perimeter", y = "Number of Ponds") +
theme_cowplot() 
save_plot("figures/8_figure_S8.png", hist_fig, bg = "white")

stopCluster(cl)
