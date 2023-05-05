# Quick script to assess quality of geolocation across the time-series
# Jakob J. Assmann May 3 2023 jakob.assmann@uzh.ch

# Dependencies
library(sf)
library(ggplot2)
library(cowplot)
library(tidyverse)
library(tidyterra)
library(terra)
library(tidyterra)

# Load test point annotations
cbh_test_points <- st_read("data/drone_time_series/cbh_timeseries/cbh_test_points.shp")
tlb_test_points <- st_read("data/drone_time_series/tlb_timeseries/tlb_test_points.shp")
tlb_test_points <- tlb_test_points %>%
    mutate(year = yeat) %>%
    select(-yeat)
rdg_test_points <- st_read("data/drone_time_series/rdg_timeseries/rdg_test_points.shp") %>%
    st_transform(crs = st_crs(cbh_test_points))

# Check completness
cbh_test_points %>%
    st_drop_geometry() %>%
    group_by(name) %>%
    tally()
tlb_test_points %>%
    st_drop_geometry() %>%
    group_by(name) %>%
    tally()
rdg_test_points %>%
    st_drop_geometry() %>%
    group_by(name) %>%
    tally()
cbh_test_points %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    tally()
tlb_test_points %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    tally()
rdg_test_points %>%
    st_drop_geometry() %>%
    group_by(year) %>%
    tally()

# Callculate mean difference
cbh_test_points %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
tlb_test_points %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
rdg_test_points %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
# Calculate marking accuracy (years 2020 and 2021)
cbh_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
tlb_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
rdg_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        data.frame(
            name = unique(x$name),
            mean_distance = mean(st_distance(x)[1, ][-1])
        )
    }) %>% 
    bind_rows()
# Use average between the two years as the "true location" and claculated distance for each year
cbh_ref_location <- cbh_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        centroids <- st_centroid(summarise(x))
        centroids$name <- unique(x$name)
        return(centroids)
    }) %>% 
    bind_rows()
tlb_ref_location <- tlb_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        centroids <- st_centroid(summarise(x))
        centroids$name <- unique(x$name)
        return(centroids)
    }) %>% 
    bind_rows()
rdg_ref_location <- rdg_test_points %>%
    filter(year %in% c("2020", "2021")) %>%
    group_by(name) %>%
    group_split() %>%
    lapply(function(x) {
        centroids <- st_centroid(summarise(x))
        centroids$name <- unique(x$name)
        return(centroids)
    }) %>% 
    bind_rows()
# Calculate distance to reference in each year
cbh_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- cbh_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows()
tlb_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- tlb_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows()
rdg_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- rdg_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows()
# Determine average across all points
cbh_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- cbh_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))
tlb_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- tlb_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))
rdg_test_points %>%
    filter(!(year %in% c("2020", "2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- rdg_ref_location %>% filter(name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))
# Repeat for just 2021 average across all points
cbh_test_points %>%
    filter(!(year %in% c("2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- cbh_test_points %>% filter(year == "2021", name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))
tlb_test_points %>%
    filter(!(year %in% c("2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- tlb_test_points %>% filter(year == "2021", name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))
rdg_test_points %>%
    filter(!(year %in% c("2021"))) %>%
    group_by(name, year) %>%
    group_split() %>%
    lapply(function(x) {
        ref_loc <- rdg_test_points %>% filter(year == "2021", name %in% unique(x$name))
        print(st_distance(x, ref_loc))

        data.frame(
            name = unique(x$name),
            year = unique(x$year),
            dist_to_ref = st_distance(x, ref_loc)
        )
    }) %>% 
    bind_rows() %>%
        group_by(year) %>%
        summarise(mean_dist_to_ref = mean(dist_to_ref))

# Prepare final figure
# all distances in reference to 2021 then plot min may and mean

# Helper function
geolocation_accuracy <- bind_rows(
    cbh_test_points,
    tlb_test_points,
    rdg_test_points
) %>%
    mutate(year = case_when(year == "2019" ~ "2019_a", TRUE ~ year)) %>%
    mutate(site = gsub("(^[a-z]{3}).*", "\\1", name)) %>%
        group_by(name) %>%
        group_split() %>%
        lapply(function(x){
            ref_2021 <- filter(x, year == "2021")
            other_years <- filter(x, year != "2021")
            other_years %>%
                group_by(year) %>%
                group_split() %>%
                lapply(function(y) {
                    data.frame(
                        site = unique(x$site),
                        name = y$name,
                        year = y$year,
                        dist_to_2021 = st_distance(y, ref_2021)
                    )
                }) %>%
                bind_rows()
        }) %>%
        bind_rows() %>%
            group_by(site, year) %>%
            summarise(
                dist_to_2021_min = min(dist_to_2021),
                dist_to_2021_mean = mean(dist_to_2021), 
                dist_to_2021_max = max(dist_to_2021)
            )

# Plot the data
geolocation_per_site <- geolocation_accuracy %>% 
group_by(site) %>%
group_split() %>%
lapply(., function(x){
    # Set site colour
    site_col <- case_when(
        unique(x$site) == "cbh" ~ "#1E1D40",
        unique(x$site) == "tlb" ~ "#BF625A",
        unique(x$site) == "rdg" ~ "#F2A922")
    site_plot <- ggplot(x, aes(x = year, 
    y = as.numeric(dist_to_2021_mean), group = site)) +
        geom_point(colour = site_col) +
        geom_errorbar(
            aes(
                ymin = as.numeric(dist_to_2021_min),
                ymax = as.numeric(dist_to_2021_max)
            ),
            width = 0.5,
            colour = site_col) +
        geom_line(colour = site_col) +
            labs(
                title = unique(x$site),
                x = "",
                y = "Distance to 2021 [m]"
            ) +
        geom_hline(yintercept = 3 * 0.12, linetype = "dashed") +
            scale_y_continuous(limits = c(0, 2.5)) +
            theme_cowplot()
    if (unique(x$site) == "rdg") {
        site_plot <- site_plot + annotate("text", x = length(unique(x$year)), y = 3 * 0.12 + 0.15, label = "3x GSD")
    }
    return(site_plot)
})
# Arrange grid and save
plot_grid(geolocation_per_site[[1]],
geolocation_per_site[[3]],
geolocation_per_site[[2]],
 labels = c("a)", "b)", "c)"), nrow = 1) %>%
    save_plot("figures/geolocation_accuracy.png", .,
        base_asp = 1.6,
    ncol = 3,    bg = "white"
    )


# Quick plots that illustrate the marking process
# Pick on random testpoint form each site
points_to_plot <- bind_rows(
    cbh_test_points,
    tlb_test_points,
    rdg_test_points
) %>%
    mutate(year = case_when(year == "2019" ~ "2019_a", TRUE ~ year)) %>%
    mutate(site = gsub("(^[a-z]{3}).*", "\\1", name)) %>%
    group_by(site) %>%
        sample_n(1) %>% pull(name)

# Helper function to plot point and time_series
plot_point <- function(point_name) {
    # Get all point coordinates
    point_coordinates <- bind_rows(
        cbh_test_points,
        tlb_test_points,
        rdg_test_points
    ) %>%
        filter(name == point_name)
    # Get site name
    site_name <- gsub("^([a-z]{3}).*", "\\1", point_name)
    # Load rasters
    drone_rast <- list.files(paste0("data/drone_time_series/", site_name, "_timeseries/norm"),
        pattern = "tif",
        full.names = T
    ) %>% lapply(rast)
    # Get 2021 plot and derive extent (5 m buffer)
    point_2021 <- filter(point_coordinates, grepl("2021", year))
    point_2021_buffered <- st_buffer(point_2021, 5)
    # Set aoi extent
    aoi_ext <- ext(
        st_bbox(point_2021_buffered)[1],
        st_bbox(point_2021_buffered)[3],
        st_bbox(point_2021_buffered)[2],
        st_bbox(point_2021_buffered)[4]
    )
    # Crop drone rasters
    drone_rast_crop <- lapply(drone_rast, function(x) crop(x, aoi_ext))
    # Plot points
    point_plot <- seq_along(point_coordinates$year) %>%
        lapply(function(x) {
            ggplot() +
                geom_spatraster_rgb(
                    data = drone_rast_crop[[x]],
                    max_col_value = 65535
                ) +
                geom_sf(
                    data = point_coordinates[x, ],
                    colour = "magenta",
                    size = 5
                ) +
                theme_nothing()
        }) %>%
        plot_grid(plotlist = ., nrow = 1)
    # return plot
    return(point_plot)
}

# Generate plots
point_plots <- lapply(points_to_plot, plot_point) 

# Save plots
point_plots %>%
    plot_grid(plotlist = ., nrow = 3) %>%
    save_plot("figures/geolocation_test_plots_examples.png", ., base_asp = 9, nrow = 3)
