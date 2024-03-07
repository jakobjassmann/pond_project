# Quick script to calculate focal predictors for the thaw lake bed site
# Jakob J. Assmann jakob.assmann@uzh.ch 8 August 2022

# Dependencies
library(terra)

# Get list of raster files
raster_files <- list.files("data/drone_time_series/dtlb/dtlb_norm/",
                           full.names = T)

# Calculated focal mean
lapply(raster_files, function(raster_file){
  rast_obj <- rast(raster_file)
  focal(rast_obj, 
        w = 3, 
        fun = "mean",
        filename = gsub(".*/(.*_[0-9]{4}).*", 
                        "data/drone_time_series/dtlb/dtlb_focal_mean/\\1_focal_mean.tif", 
                        raster_file),
        overwrite = T)
  focal(rast_obj, 
        w = 9, 
        fun = "sd",
        filename = gsub(".*/(.*_[0-9]{4}).*", 
                        "data/drone_time_series/dtlb/dtlb_focal_sd/\\1_focal_sd.tif", 
                        raster_file),
        overwrite = T)
  return(NULL)
})
