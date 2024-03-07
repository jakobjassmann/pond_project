# Quick try to generate an emperical line bcc classification
# Jakob J. Assmann jakob.assmann@uzh.ch 30 Janaury 2023

# Dependencies
library(tidyverse)
library(sf)
library(pbapply)
library(terra)
library(ggplot2)
library(cowplot)

# Load training polygons
training_polys <- bind_rows(read_sf("data/training/cbh_training.gpkg") %>% mutate(geometry = geom) %>% st_drop_geometry() %>% st_as_sf(),
                            read_sf("data/training/tlb_training.gpkg"),
                            read_sf("data/training/rdg_training.gpkg")) %>% 
  mutate(., 
         id = paste0(site, "_", 1:nrow(.))) %>%
  filter(!st_is_empty(geometry))

# Get list of rgb rasters
rgb_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("rgb",.)]

# Helper function to calculate bcc
calc_bcc <- function(rgb_file, suffix ="_raw"){
  # Get year and site
  year_interest <- gsub(".*/[a-z]{3}_([0-9]{4}.*)\\.tif", "\\1", rgb_file)
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rgb_file)
  # Load rgb raster
  rgb_rast <- rast(rgb_file)
  # Assign band names
  names(rgb_rast) <- c("R", "G", "B", "alpha")
  # Calculate bcc
  bcc <- rgb_rast[["B"]] / (rgb_rast[["R"]] + rgb_rast[["G"]] + rgb_rast[["B"]])
  # Generate folder if needed
  dir.create(paste0("data/drone_time_series/", site_interest, "_timeseries/bcc_raw/"), showWarnings = FALSE)
  writeRaster(bcc,
              filename = paste0("data/drone_time_series/", site_interest, "_timeseries/bcc", suffix, "/", site_interest, "_", year_interest, ".tif"),
              overwrite = T)
  # Return NULL
  return(NULL)
}

# Generate bcc_raw rasters for all raster files
pblapply(rgb_files, calc_bcc, cl = 31)

# Remove cbh 2014 byte
file.remove("data/drone_time_series/cbh_timeseries/bcc_raw/cbh_2014.tif")

## Determine empirical line based on training data

# Get list of bcc rasters
bcc_raw_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("bcc_raw",.)]


# Helper function to extract bcc values
extract_bcc_vals <- function(rast_file){
  # Status
  cat("\nExtracting", rast_file, "\n")
  
  # Load raster
  bcc_raster <- rast(rast_file)
  
  # Get year
  year_interest <- gsub(".*([0-9]{4}_?.*)\\..*","\\1", rast_file)
  
  # Get site
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rast_file)
  
  # Skip year if not available
  if(!paste0(site_interest, year_interest) %in% unique(paste0(training_polys$site,training_polys$year))) return(NULL)
  
  # Subset polys for year
  polys <- filter(training_polys, 
                  site == site_interest,
                  year == year_interest)
  polys$ID <- 1:nrow(polys)
  
  # Extract training data
  bcc_vals <- terra::extract(bcc_raster, vect(polys))[, 1:2]
  
  # Adjust column names of data frame
  colnames(bcc_vals) <- c("ID", "bcc")
  
  # Combine training data with metadata (incl. class info) of the polys
  extracted_vals <- polys %>%
    st_drop_geometry() %>%
    select(ID, id, class) %>%
    full_join(bcc_vals) %>%
    select(-ID) %>%
    mutate(year = year_interest) %>%
    mutate(site = site_interest)
  
  # Return training dataset for year
  return(extracted_vals)
}

# Extract bcc for all files
bcc_raw_vals <- pblapply(bcc_raw_files, extract_bcc_vals, cl = 12) %>% bind_rows()

# Plot bcc values by year
(bbc_raw_plot <- ggplot(bcc_raw_vals) +
  geom_density(aes(x = bcc, colour = class)) +
  facet_wrap(~paste0(site, "_", year), scales = "free") +
  scale_colour_manual(values = c("#C00000", "#00B0F0")) +
  scale_x_continuous(limits = c(0,1)) +
  scale_y_continuous(expand = c(0.2,0.2)) +
  theme_cowplot())
save_plot("figures/training_all_raw_sep.png", bbc_raw_plot,
          base_height = 12, base_asp = 16/9, bg = "white")

ggplot(bcc_raw_vals %>% filter(site == "cbh", year == "2017")) +
  geom_density(aes(x = bcc, colour = class)) +
  facet_wrap(~paste0(site, "_", year), scales = "free") +
  geom_vline(xintercept = 0.38) +
  scale_x_continuous(limits = c(0,1)) +
  theme_classic()

# Calculate min and max and 99% quantiles per year
bcc_raw_vals %>%
  group_by(site, year) %>%
  summarise(min = min(bcc) %>% round(3),
            low_05 = quantile(bcc, 0.01) %>% round(3),
            max = max(bcc) %>% round(3),
            up_05 = quantile(bcc, 0.99) %>% round(3)) %>%
  print(n = 100)

# Rescale values per year
bcc_raw_vals <- bcc_raw_vals %>%
  group_by(site, year) %>%
  group_map(function(bcc_tbl, grps){
    sum_stats <- summarise(bcc_tbl,
              min = min(bcc) %>% round(3),
              low_01 = quantile(bcc, 0.05) %>% round(3),
              max = max(bcc) %>% round(3),
              up_99 = quantile(bcc, 0.95) %>% round(3))
    return(mutate(bcc_tbl, 
                  bcc_scaled = scales::rescale(bcc, from = c(sum_stats$low_01, sum_stats$up_99))))
    }, .keep = T) %>%
  bind_rows()

ggplot(bcc_raw_vals) +
  geom_density(aes(x = bcc_scaled, colour = class)) +
  facet_wrap(~paste0(site, "_", year), scales = "free") +
  scale_x_continuous(limits = c(-0.5,1.5)) +
  theme_classic()


# Generate normalised bcc rasters
nrom_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("norm",.)]
pblapply(nrom_files, suffix = "", calc_bcc, cl = 31)

# Get list of normalised bcc rasters
bcc_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("bcc/",.)]
# Extract bcc for all files
bcc_norm_vals <- pblapply(bcc_files, extract_bcc_vals, cl = 12) %>% bind_rows()
# Check plot
bcc_norm_vals %>% filter(site == "cbh", year == "2017") %>%
  summarise(bbc_low05 = quantile(bcc, 0.5))
ggplot(bcc_norm_vals) +
  geom_density(aes(x = bcc, colour = class)) +
  facet_wrap(~paste0(site, "_", year), scales = "free") +
  geom_vline(xintercept = 0.343) +
  scale_x_continuous(limits = c(0,1)) +
  theme_classic()

# Helper function to calculate rcc rasters
calc_rcc <- function(rgb_file, suffix ="_raw"){
  # Get year and site
  year_interest <- gsub(".*/[a-z]{3}_([0-9]{4}.*)\\.tif", "\\1", rgb_file)
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", rgb_file)
  # Load rgb raster
  rgb_rast <- rast(rgb_file)
  # Assign band names
  names(rgb_rast) <- c("R", "G", "B", "alpha")
  # Calculate bcc
  bcc <- rgb_rast[["R"]] / (rgb_rast[["R"]] + rgb_rast[["G"]] + rgb_rast[["B"]])
  # Generate folder if needed
  dir.create(paste0("data/drone_time_series/", site_interest, "_timeseries/bcc_raw/"), showWarnings = FALSE)
  writeRaster(bcc,
              filename = paste0("data/drone_time_series/", site_interest, "_timeseries/rcc", suffix, "/", site_interest, "_", year_interest, ".tif"),
              overwrite = T)
  # Return NULL
  return(NULL)
}

# Generate normalised rcc rasters
nrom_files <- list.files("data/drone_time_series/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("norm",.)]
pblapply(nrom_files, suffix = "", calc_rcc, cl = 31)

