# Script to prepare regular grids to help with manual training data annotations
# !! The grid files are also included as version controlled files 
# !! in this repository: /data/training_polygons/*_grid.gpkg
# !! This script does not need to be run and ist kept for legacy reasons only. 
# Jakob J. Assmann 4 October 2023

# Dependencies
library(tidyverse)
library(sf)
library(terra)

# Load rasters
cbh <- read_sf("data/drone_data/cbh/cbh_study_aoi.shp")
tlb <- read_sf("data/drone_data/tlb/tlb_study_aoi.shp")
rdg <- read_sf("data/drone_data/rdg/rdg_study_aoi.shp")

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
write_sf(cbh_grid, "data/training_polygons/cbh_grid.gpkg")

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
write_sf(tlb_grid, "data/training_polygons/tlb_grid.gpkg")

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
write_sf(rdg_grid, "data/training_polygons/rdg_grid.gpkg")

## !!! Below is the legacy code for generating a random sample of cells to
## annotate for each site-year combination. This approach was used for the
## inital set of polygons to reduce bias in the annotations. However, 
## this approach did not work well for all drone rasters and further training 
## polygons were then added iteratively by targeting cells with poor 
## performance of the classifier. 

# First
# Generate a random sample of three cells per sit-year combination to annotate
# Then fill in as needed.

# Get sample of 3 cells per year-site combination
years <- c(2014,2016:2021)
sites <- c("cbh", "tlb", "rdg")
set.seed(21)
sample_combos <- map(sites, function(site) {
    map2(years, site, function(y, site) {
        data.frame(site = site, year = y, cells = sample(1:25, 3))
    })
}) %>% bind_rows()
write_csv(sample_combos, "data/training_polygons/grid_cells_to_annotate.csv")
# Cell numbering starting top-left

# generate some extra data for the poorly performing cbh site
# CHH 2019_b
sample(1:25, 3)

# too little sample data in cbh 2016 -> generate one more random cell
sample(1:25,1)

# Generate three addtional ones per year
sample_combos <- map(sites, function(site) {
    map2(years, site, function(y, site) {
        data.frame(site = site, year = y, cells = sample(1:25, 3))
    })
}) %>%
    bind_rows() %>%
        arrange(site, year)
write_csv(sample_combos, "data/training_polygons/grid_cells_to_annotate_2.csv")

# one more cell for cbh 2016 as one was double counted
sample(1:25,1)

# one more cell for cbh 2018 as one contained no water
sample(1:25,1) # double counted
sample(1:25,1)

# two more cells for cbh 2019 as the two new suggested ones don't contain water
sample(1:25,2)
# Generate four more sells for 2019_b
sample(1:25, 4) # one double
sample(1:25, 1) # two more cells with water required
sample(1:25,2)

# For tlb and rdg, I generate 6 grid cells per year straight away
# that way we can avoid duplicates and don't have to re-generate again and again
# Generate three addtional ones per year
set.seed(23)
site <- "tlb"
years <- c("2014", "2016", "2017_b", "2018", "2019_a", "2019_b", "2020", "2021")
sample_combos <- map2(years, site, function(y, site) {
        data.frame(site = site, year = y, cells = sample(1:25, 6))
    }) %>%
    bind_rows() %>%
        arrange(site, year)
write_csv(sample_combos, "data/training_polygons/grid_cells_to_annotate_tlb.csv")

site <- "rdg"
years <- c("2014", "2017", "2018", "2019_a", "2019_c", "2020", "2021")
sample_combos <- map2(years, site, function(y, site) {
        data.frame(site = site, year = y, cells = sample(1:25, 6))
    }) %>%
    bind_rows() %>%
        arrange(site, year)
write_csv(sample_combos, "data/training_polygons/grid_cells_to_annotate_rdg.csv")

#  tlb 2019_a did not have enought water (few ponds) generating some extra cells here
sample(c(3, 4, 11, 16, 17), 2)

# Too little water in tlb 2021 -> add one more sample of one of the following cells
sample(c(3, 4, 16, 17),1)

# rdg 2014 requirs cell 25 but this is empty -> generate a new one
sample(c(1:25), 1)

# rdg 2017 requirs cell 25 but this is empty -> generate a new one
sample(c(1:25), 1) # 22 already given
sample(c(1:25), 1)

# rdg 2018 requirs cell 1 but this is empty -> generate a new one
sample(c(1:25), 1)

# rdg 2019 requirs cell 1 and 25 but this these are empty -> generate a new ones
sample(c(1:25), 1)
sample(c(1:25), 1)

# rdg 2020 requirs cell 25 but this is empty -> generate a new one
sample(c(1:25), 1)

# One more cell with lots of water required for tlb 2019_a
# and one addtional one
set.seed(34)
sample(c(3,4,16,17),1)

# Furhter cells were added iteratively, resulting in the final set of 
# training annotations as outlined in Table S3. 