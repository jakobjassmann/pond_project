# Vertical accuracy assessment (around GCPs)
# Jakob J. Assmann 21 May 2025 jakob.assmann@uzh.ch

# Dependencies
library(tidyverse)
library(sf)
library(terra)
library(ggplot2)
library(cowplot)

## Load data ----

# Load ground control points
cbh_test_points <- st_read("data/drone_data/cbh/cbh_test_points.shp") %>%
    mutate(name = gsub("text", "test", name))
tlb_test_points <- st_read("data/drone_data/tlb/tlb_test_points.shp")
tlb_test_points <- tlb_test_points %>%
    mutate(year = yeat) %>%
    select(-yeat)
test_points <- bind_rows(cbh_test_points, tlb_test_points) %>%
    filter(year %in% c(2014, 2021)) %>%
    mutate(site = substr(name, 1, 3))

# Load dsm
dsm_rasts <- list.files("data/drone_data",
    pattern = "tif",
    recursive = T,
    full.names = T
) %>%
    .[grepl("dsm", .)] %>%
    .[grepl("cbh_2014|cbh_2021|tlb_2014|tlb_2021", .)]

# Load predictions
preds_rasts <- list.files("data/drone_data",
    pattern = "tif",
    recursive = T,
    full.names = T
) %>%
    .[grepl("preds_filtered/", .)] %>%
    .[grepl("cbh_2014|cbh_2021|tlb_2014|tlb_2021", .)]

## Define helper functions
get_diff <- function(points) {
    site_name <- unique(points$site)
    bounds <- points %>%
        st_buffer(10, endCapStyle = "SQUARE") %>%
        st_union() %>%
        st_crop(st_bbox(rast(dsm_rasts[grepl(site_name, dsm_rasts)][1]))) %>%
        st_as_sf() %>%
        ext()

    # Load preds and dsm for 2014
    preds_2014 <- rast(preds_rasts[grepl(paste0("*.", site_name, ".*2014.*"), preds_rasts)])
    dsm_2014 <- rast(dsm_rasts[grepl(paste0("*.", site_name, ".*2014.*"), dsm_rasts)])
    # Load preds and dsm for 2021
    preds_2021 <- rast(preds_rasts[grepl(paste0("*.", site_name, ".*2021.*"), preds_rasts)])
    dsm_2021 <- rast(dsm_rasts[grepl(paste0("*.", site_name, ".*2021.*"), dsm_rasts)])

    # Load all preds rasters for standardising the dsm
    preds_all <- preds_rasts[grepl(site_name, preds_rasts)] %>% rast()

    # Crop rasters
    preds_crop_2014 <- crop(preds_2014, bounds)
    preds_crop_2021 <- crop(preds_2021, bounds)
    preds_all_crop <- crop(preds_all, bounds)
    dsm_crop_2014 <- crop(dsm_2014, bounds)
    dsm_crop_2021 <- crop(dsm_2021, bounds)

    # Generate a single layer for preds_all
    preds_all_crop <- sum(preds_all_crop, na.rm = T)

    # Calculate min value for area nevery covered by water
    dsm_min_2014 <- mask(dsm_crop_2014, preds_all_crop, inverse = T) %>%
        global(.,
            fun = min, # quantile, probs = 0.02,
            na.rm = T
        ) %>%
        as.numeric()
    dsm_min_2021 <- mask(dsm_crop_2021, preds_all_crop, inverse = T) %>%
        global(.,
            fun = min, # quantile, probs = 0.02,
            na.rm = T
        ) %>%
        as.numeric()

    # Mask dsm for area covered by water in year only and standardise dsm
    dsm_crop_2014 <- mask(dsm_crop_2014, preds_crop_2014, inverse = T)
    dsm_crop_2014 <- dsm_crop_2014 - dsm_min_2014
    dsm_crop_2021 <- mask(dsm_crop_2021, preds_crop_2021, inverse = T)
    dsm_crop_2021 <- dsm_crop_2021 - dsm_min_2021

    # Adjust levels of preds raster to allow for 0-1 alpha plotting
    preds_crop_2014 <- classify(preds_crop_2014, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))
    preds_crop_2021 <- classify(preds_crop_2021, matrix(c(NaN, 0, 1, 1), byrow = T, ncol = 2))

    # Calculate difference in height
    diff_height <- extract(dsm_crop_2014, points[2, ])[, 2] - extract(dsm_crop_2021, points[1, ])[, 2]

    # Return data frame
    data.frame(
        name = unique(points$name),
        site = unique(points$site),
        diff_height = diff_height
    )
}

## Calculate height difference for all test points
height_diff <- test_points %>%
    split(test_points$name) %>%
    lapply(get_diff) %>%
    bind_rows() %>%
    mutate(site = case_when(
        site == "cbh" ~ "high",
        site == "tlb" ~ "medium"
    ))

# Calculate summary stats
height_diff_sum <- height_diff %>%
    group_by(site) %>%
    summarise(
        mean = mean(diff_height),
        min = min(diff_height),
        max = max(diff_height),
        se = sd(diff_height) / sqrt(length(diff_height))
    )

# Global mean
mean(height_diff$diff_height)
# -0.05785923
sd(height_diff$diff_height)/sqrt(length(height_diff$diff_height))
# 0.02609936

# Visualsie
vertical_plot <- ggplot(height_diff, aes(y = diff_height, x = site)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_point(data = height_diff_sum, aes(y = mean), size = 2) +
    geom_errorbar(data = height_diff_sum,
        aes(ymin = mean - se, ymax = mean + se, x = site), width = 0.25, inherit.aes = F) +
    geom_point(aes(colour = site), size = 2) +
    scale_colour_manual(values = c("#FF369D", "#19CEE6")) +
    scale_y_continuous(limits = c(-0.3, 0.3), breaks = c(-0.3, -0.2, -0.1, 0, 0.1, 0.2, 0.3)) +
    labs(x = "", y = "Vertical difference 2021-2014 [m]") +
    theme_cowplot() +
    theme(legend.position = "none")
save_plot("figures/8_figure_S8.png", vertical_plot,
    base_asp = 1.6, bg = "white"
)
