# Quick script to assess the open water and wet sedge fractions in the  Pond 
# Project drone time-series 
# Jakob J. Assmann jakob.assmann@uzh.ch 19 August 2021

# Dependencies
library(tidyverse)
library(terra)
library(cowplot)

# set paths to rasters
dtlb_pred_files <- list.files("data/drone_time_series/dtlb/dtlb_preds",
    pattern = "tif", 
    full.names = T)
cbh_pred_files <- list.files("data/drone_time_series/cbh/cbh_preds",
    pattern = "tif", 
    full.names = T)

# Define class key-value pairs
dtlb_key <- data.frame(
    class = c("open_water", "other", "wet_sedges"),
    value = 1:3,
    colour = c("#30A5BF", "#000000", "#185359")
)
cbh_key <- data.frame(
    class = c("brown_water", "brown_sedges", 
        "open_water", "other", "wet_sedges"),
    value = 1:5,
    colour = c("#A6874E", "#F2BE22", 
        "#30A5BF", "#000000", "#185359")
) # spare dark blue #0B2B40

# write function to calculate area of classes for raster and key-value pairs
get_class_area <- function(rast_file, class_vals){
    rast_object <- rast(rast_file)
    data.frame(
        year = gsub(".*([0-9]{4}).*", "\\1", rast_object@ptr$filenames),
        class = class_vals$class,
        area = map(class_vals$value, function(x){
            length(cells(rast_object, x)[[1]]) * prod(res(rast_object)) %>%
            unlist()
        }) %>% unlist()
    )
}

# Apply over time_series
dtlb_area <- map(dtlb_pred_files, get_class_area, class_vals = dtlb_key) %>%
    bind_rows()
cbh_area <- map(cbh_pred_files, get_class_area, class_vals = cbh_key) %>%
    bind_rows()

# Visualise results
dtlb_area_plot <- filter(dtlb_area, class != "other") %>%
    ggplot(aes(x = as.numeric(year), y = area, colour = class)) +
        geom_line() +
        geom_point() +
        labs(x = "Year", y = "Area [m²]",
            colour = "Class") +
        scale_color_manual(
            values = dtlb_key$colour[match(levels(factor(filter(dtlb_area, class != "other")$class)),
                                    dtlb_key$class)]) +
        theme_cowplot()
save_plot("figures/dtlb/dtlb_area_change.png",
    dtlb_area_plot,
    bg = "white")

cbh_area_plot <- filter(cbh_area, class != "other") %>%
    ggplot(aes(x = as.numeric(year), y = area, colour = class)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(x = "Year", y = "Area [m2]",
            colour = "Class") +
        scale_colour_manual(
            values = cbh_key$colour[match(levels(factor(filter(cbh_area, class != "other")$class)),
                                    cbh_key$class)]) +
        theme_cowplot()
save_plot("figures/cbh/cbh_area_change.png",
    cbh_area_plot,
    bg = "white")
