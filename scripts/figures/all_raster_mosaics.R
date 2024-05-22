# Mosaic all rasters

# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(sf)
library(pbapply)

# get rasters files
norm_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/norm/",.)] %>%
  .[!(grepl("rdg_2016", .) | grepl("rdg_2019_b", .))] %>%
  .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]
preds_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/preds_filtered/",.)] %>%
  .[!grepl("cbh_2019\\.|tlb_2019_a|tlb_2019_b", .)]

# Helper function to plot rasters
plot_raster <- function(rast_file,
                        max_val = 65535,
                        add_scale = FALSE,
                        add_preds = TRUE){
  cat(rast_file, "\n")

  # Get aoi outline
  aoi_geom <- read_sf(paste0("data/drone_data/",
                             gsub(".*(rdg|cbh|tlb).*", "\\1", rast_file),
                             "/",
                             gsub(".*(rdg|cbh|tlb).*", "\\1", rast_file),
                             "_study_aoi.shp"))

  # Check whether an empty date string was supplied as a file argument
  # If yes create an empty plot with the year
  if(rast_file %in% c("rdg_2015", "rdg_2016",
                      "cbh_2015",
                      "tlb_2015")) {
    rast_object <-  gsub(".*(rdg|cbh|tlb).*", "\\1", rast_file) %>%
                        grepl(., norm_files) %>%
                        norm_files[.] %>% 
                        .[1] %>%
                        rast() %>% 
                        .[[1]]
    rast_plot <-  ggplot() +
      geom_spatraster(data = rast_object,
                      fill = "black") +
      geom_sf(data = aoi_geom,
               colour = "black",
               fill = "black",
               linewidth = 2) +
      annotate("text",
               x = -Inf,
               y = - Inf, 
               label = rast_file %>% 
                 gsub(".*([0-9]{4}).*", "\\1", .),
               hjust = -0.25,
               vjust = -0.75,
               size = 18 /.pt,
               colour = "grey") +
      annotate("text",
               x = (ext(rast_object)[1]+ext(rast_object)[2])/2,
               y =  (ext(rast_object)[3]+ext(rast_object)[4])/2,
               label = "No Data",
               size = 16 / .pt,
               hjust = 0.5,
               vjust = 0.5,
               colour = "grey") +
               theme_nothing() 
    return(rast_plot)
  }

  # Set site colour
  site_colour <- c(
    "#FFE700",
    "#FF369D",
    "#19CEE6")[c(grepl("rdg", rast_file),
                 grepl("cbh", rast_file), 
                grepl("tlb", rast_file))]
  # Base plot of norm raster
  norm_rast <- rast(rast_file)
  if(grepl("rdg", rast_file)) {
    norm_rast[norm_rast[[4]] == 0 ] <- NA
  }
  rast_plot <- ggplot() +
    geom_spatraster_rgb(data = norm_rast,
                        max_col_value = max_val,
                        alpha = 1) +
    geom_sf(data = aoi_geom,
            colour = site_colour,
            fill = NA,
            linewidth = 2) +
    theme_nothing()

  # Add predicitons if requested
  if(add_preds){
    pred_file <- preds_files[grepl(gsub(".*/(.*)\\.tif", "\\1", rast_file), preds_files)]
    pred_rast <- as.factor(rast(pred_file))
    if(!(length(pred_file) == 0 | levels(pred_rast)[[1]][1, 2] == "0")) {
      rast_plot <- rast_plot +
        geom_spatraster(data = pred_rast) +
        scale_fill_manual(values = "#00C4F5",
                          na.value = "transparent")
    }
  }

  # Add scale bar if requested
  if(add_scale){
    rast_ext <- ext(rast(rast_file))
    rast_dims_xy <- c(rast_ext[2] - rast_ext[1], rast_ext[4] - rast_ext[3])
    rast_plot <- rast_plot +
      annotate("segment",
        x = rast_ext[1] + rast_dims_xy[1] * 0.1,
        xend = rast_ext[1] + rast_dims_xy[1] * 0.1 + 100,
        y = rast_ext[3] + rast_dims_xy[2] * 0.1,
        yend = rast_ext[3] + rast_dims_xy[2]  * 0.1,
        linewidth = 3,
        colour = "white"
    ) + 
    annotate("text",
             x = rast_ext[1] + rast_dims_xy[1] * 0.1 + (100 / 2),
             y = rast_ext[3] + rast_dims_xy[2]  * 0.2,
             colour = "white",
             size = 14/.pt,
             label = "100 m")
  }

  # Add year
  rast_plot <- rast_plot  +
    annotate("text",
      x = -Inf,
      y = - Inf, 
      label = rast_file %>%
        gsub(".*([0-9]{4}).*\\.tif", "\\1", .),
      hjust = -0.25,
      vjust = -0.75,
      size = 18 / .pt,
      colour = "white"
    )
  # Return plot
  return(rast_plot)
}

#plot_raster(norm_files %>% .[grepl("rdg_2014", .)])
#plot_raster("rdg_2015")

rast_plots <- list(norm_files %>% .[grepl("rdg_2014", .)],
                   "rdg_2016",
                   norm_files %>% .[grepl("rdg_2018", .)],
                   norm_files %>% .[grepl("rdg_2020", .)],
                   "rdg_2015",
                   norm_files %>% .[grepl("rdg_2017", .)],
                   norm_files %>% .[grepl("rdg_2019", .)],
                   norm_files %>% .[grepl("rdg_2021", .)],
                   norm_files %>% .[grepl("cbh_2014", .)],
                   norm_files %>% .[grepl("cbh_2016", .)],
                   norm_files %>% .[grepl("cbh_2018", .)],
                   norm_files %>% .[grepl("cbh_2020", .)],
                   "cbh_2015",
                   norm_files %>% .[grepl("cbh_2017", .)],
                   norm_files %>% .[grepl("cbh_2019", .)],
                   norm_files %>% .[grepl("cbh_2021", .)],
                   norm_files %>% .[grepl("tlb_2014", .)],
                   norm_files %>% .[grepl("tlb_2016", .)],
                   norm_files %>% .[grepl("tlb_2018", .)],
                   norm_files %>% .[grepl("tlb_2020", .)],
                   "tlb_2015",
                   norm_files %>% .[grepl("tlb_2017", .)],
                   norm_files %>% .[grepl("tlb_2019", .)],
                   norm_files %>% .[grepl("tlb_2021", .)]) %>%
                   map(plot_raster)
plot_grid(plotlist = rast_plots,
          ncol = 6, nrow = 4,
          byrow = FALSE,
          rel_widths = c(1, 1, 1.313, 1.313, 1.094, 1.094)) %>%
  save_plot("figures/preds_plot_all.png", .,
            bg = "black",
            base_asp = 3 / 2,
            base_height = 6)
