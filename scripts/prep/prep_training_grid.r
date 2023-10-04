# Script to prepare grid for training data production
# Jakob J. Assmann 4 October 2023

# Dependencies
library(tidyverse)
library(sf)
library(terra)

# Load rasters
cbh <- read_sf("data/drone_time_series/cbh_timeseries/cbh_study_aoi.shp")
tlb <- read_sf("data/drone_time_series/tlb_timeseries/tlb_study_aoi.shp")
rdg <- read_sf("data/drone_time_series/rdg_timeseries/rdg_study_aoi.shp")

# Calculate width and height of each area
cat(
    "CBH widht", st_bbox(cbh)[3] - st_bbox(cbh)[1],
    "height", st_bbox(cbh)[4] - st_bbox(cbh)[2]
)
cat(
    "tlb widht", st_bbox(tlb)[3] - st_bbox(tlb)[1],
    "height", st_bbox(tlb)[4] - st_bbox(tlb)[2]
)
cat(
    "rdg widht", st_bbox(rdg)[3] - st_bbox(rdg)[1],
    "height", st_bbox(rdg)[4] - st_bbox(rdg)[2]
)

# All sites are roughly 500 m x 400 m
# => Reallistically I can do about 25 annotaions per year
# => 100 m x 100 m cells

# Generate polygons grid CBH
cbh_centroid <- st_centroid(cbh) %>% st_coordinates()
cbh_grid_ext <- ext(
    (cbh_centroid[1] - 25) - 5 * 50, (cbh_centroid[1] + 25) + 5 * 50,
    (cbh_centroid[2] - 25) - 5 * 50, (cbh_centroid[2] + 25) + 5 * 50
)
cbh_grid <- rast(cbh_grid_ext, nrows = 5, ncols = 5, crs = crs(cbh)) %>%
    as.polygons() %>%
    st_as_sf()
plot(cbh_grid)
plot(cbh, add = T, col = "red")
write_sf(cbh_grid, "data/training/cbh_grid.gpkg")

# Generate polygons grid tlb
tlb_centroid <- st_centroid(tlb) %>% st_coordinates()
tlb_grid_ext <- ext(
    (tlb_centroid[1] - 25) - 5 * 50, (tlb_centroid[1] + 25) + 5 * 50,
    (tlb_centroid[2] - 25) - 5 * 50, (tlb_centroid[2] + 25) + 5 * 50
)
tlb_grid <- rast(tlb_grid_ext, nrows = 5, ncols = 5, crs = crs(tlb)) %>%
    as.polygons() %>%
    st_as_sf()
plot(tlb_grid)
plot(tlb, add = T, col = "red")
write_sf(tlb_grid, "data/training/tlb_grid.gpkg")

# Generate polygons grid rdg
rdg_centroid <- st_centroid(rdg) %>% st_coordinates()
rdg_grid_ext <- ext(
    (rdg_centroid[1] - 25) - 5 * 50, (rdg_centroid[1] + 25) + 5 * 50,
    (rdg_centroid[2] - 25) - 5 * 50, (rdg_centroid[2] + 25) + 5 * 50
)
rdg_grid <- rast(rdg_grid_ext, nrows = 5, ncols = 5, crs = crs(rdg)) %>%
    as.polygons() %>%
    st_as_sf()
plot(rdg_grid)
plot(rdg, add = T, col = "red")
write_sf(rdg_grid, "data/training/rdg_grid.gpkg")

# The plan will be to mark all cells for 2014 (in space)
# Then mark 3 cells per year.
# Here we generate the coordinates of these:
years <- c(2014,2016:2021)
sites <- c("cbh", "tlb", "rdg")
set.seed(21)
sample_combos <- map(sites, function(site) {
    map2(years, site, function(y, site) {
        data.frame(site = site, year = y, cells = sample(1:25, 3))
    })
}) %>% bind_rows()
write_csv(sample_combos, "data/training/grid_cells_to_annotate.csv")
# Cell numbering starting top-left

# generate some extra data for the outlier sites
# CHH 2019_b
sample(1:25, 3)

# too little sample data in 2016 -> generate one more random cell
sample(1:25,1)
