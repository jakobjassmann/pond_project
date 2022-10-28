# Dependencies
# install.packages(c("tidyverse", "terra", "sf", "exifr", "pbapply"))
library(tidyverse)
library(terra)
library(sf)
library(exifr)
library(pbapply)
library(parallel)
configure_exiftool(command ="C:/Scratch/pond_project/exiftool/exiftool.exe")

# Function to retrieve geotags
get_geotags <- function(folder_name){
    # Status
    cat("Processing folder", folder_name, "\n")
    # Get files in folder
    files_in_folder <- list.files(folder_name,
                                "JPG",
                                full.names = T,
                                recursive = F)
    # Prep cluster for parallel processing
    cl <- makeCluster(6)
    clusterEvalQ(cl, library(exifr))
    clusterEvalQ(cl, configure_exiftool(command ="C:/Scratch/pond_project/exiftool/exiftool.exe"))
    # Read in geotags 
    geotags <- pblapply(files_in_folder, 
                        read_exif,
                        tags = c("GPSLongitude", "GPSLatitude"),
                        cl = cl) %>%
      bind_rows()
    # Stop cluster
    stopCluster(cl)
    # Convert to an sf object
    geotags_sf <- geotags %>% 
      na.omit() %>%
      st_as_sf(coords =  c("GPSLongitude", "GPSLatitude"),
               crs = 4326) %>%
               st_sf()
    # Return sf object
    return(geotags_sf)
}

# Specify folders
folders <- c(
    "C:/Scratch/pond_project/cbh/cbh_2016/cbh_2016_rth/img",
    "C:/Scratch/pond_project/cbh/cbh_2016/cbh_2016_rw/img",
    "C:/Scratch/pond_project/cbh/cbh_2016/cbh_2016_tn/img")

# Get geotags for each folder
geotags <- map(folders, get_geotags)

