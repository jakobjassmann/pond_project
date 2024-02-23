# This script normalises the colour profiles of the drone rasters across
# the three time-series
# Jakob J. Assmann 24 April 2023 jakob.assmann@uzh.ch

# Dependencies
# install.packages("CRImage")
# if (!require("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
# BiocManager::install("CRImage")
library(CRImage)
library(tidyverse)
library(parallel)
library(pbapply)
library(exifr)
library(terra)

correct_image <- function(source_image, target_image){
    cat(source_image, "\n")
    # read in the target image
    targetImage=readImage(target_image)
    # read in the image whose color values should be adapted
    imgToConvert=readImage(source_image)
    # calculate mean and standard deviation of target color channels
    mst=calculateMeanStdTarget(targetImage)
    # # create a white pixel mask
    # whitePixelMask=imgToConvert[,,1]>0.85 & imgToConvert[,,2]>0.85 & imgToConvert[,,3]>0.85
    # adapt color channels of image
    imgCorrected=colorCorrection(imgToConvert,mst)
    # plot the result
    #print(plot(imgCorrected))
    # write out
    writeImage(imgCorrected, 
               gsub("(.*)rgb(.*)", "\\1norm\\2", source_image))
    # return nothing
    return(NULL)
}

# Confirm datatypes of all rasters
data.frame(
    file = list.files("data/drone_time_series",
        pattern = "\\.tif$",
        recursive = T,
        full.names = T
    )
) %>%
    filter(grepl(".*/rgb/.*", file)) %>%
    filter(!grepl("native", file)) %>%
    filter(grepl("timeseries/", file)) %>%
        pull(file) %>%
        lapply(function(x){
            rast_x <- rast(x)
            data.frame(
                rast_name = gsub(".*/(.*)", "\\1", x),
                R = datatype(rast_x)[1],
                G = datatype(rast_x)[2],
                B = datatype(rast_x)[3],
                alpha = datatype(rast_x)[4]
            )
        }) %>% 
        bind_rows()

# Prepare cbh2014 raster which is INT16, convert to byte
cbh_2014 <- rast("data/drone_time_series/cbh_timeseries/rgb/cbh_2014.tif")
cbh_2014[[1]] <- round((cbh_2014[[1]] / 65535) * 255)
cbh_2014[[2]] <- round((cbh_2014[[2]] / 65535) * 255)
cbh_2014[[3]] <- round((cbh_2014[[3]] / 65535) * 255)
cbh_2014[[4]] <- round((cbh_2014[[4]] / 255) * 255)
writeRaster(
    cbh_2014,
    "data/drone_time_series/cbh_timeseries/rgb/cbh_2014_byte.tif",
    datatype = "INT1U",
    NAflag=NA,
    overwrite = TRUE
    )
# Re run the above for cbh again (and remove the first raster)
# Scan drone rasters to find that with the highest 95% refelctance values

# Load drone rasters
cbh_files <- list.files("data/drone_time_series/cbh_timeseries/rgb",
    ".tif$",
    full.names = T
) %>%
    .[-1] %>%
    lapply(rast)
tlb_files <- list.files("data/drone_time_series/tlb_timeseries/rgb",
    ".tif$",
    full.names = T
) %>% lapply(rast)
rdg_files <- list.files("data/drone_time_series/rdg_timeseries/rgb",
    ".tif$",
    full.names = T
) %>% lapply(rast)

# Helper function to set row names of a data frame (or alike object)
set_rownames <- function(x, y) {
    rownames(x) <- y
    return(x)
}

# Get NA values for each band
get_na <- function(x) {
    # cat(sources(x), "\n")
    rast_q95 <- global(x, function(x) sum(is.na(x)))
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get NA cells for all rasters
cbh_na <- pblapply(cbh_files, get_na) %>% bind_rows()
tlb_na <- pblapply(tlb_files, get_na) %>% bind_rows()
rdg_na <- pblapply(rdg_files, get_na) %>% bind_rows()

# arrange dataframes and caluclate max stats
cbh_na %>%
    bind_rows(tlb_na)  %>%
    bind_rows(rdg_na) 


# Get max values for each band
get_max <- function(x) {
    #cat(sources(x), "\n")
    rast_q95 <- global(x, max)
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get min values for each band
get_min <- function(x) {
    #cat(sources(x), "\n")
    rast_q95 <- global(x, min)
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get 95% quantiles for all rasters
cbh_max <- pblapply(cbh_files, get_max) %>% bind_rows()
tlb_max <- pblapply(tlb_files, get_max) %>% bind_rows()
rdg_max <- pblapply(rdg_files, get_max) %>% bind_rows()

# Get 95% quantiles for all rasters
cbh_min <- pblapply(cbh_files, get_min) %>% bind_rows()
tlb_min <- pblapply(tlb_files, get_min) %>% bind_rows()
rdg_min <- pblapply(rdg_files, get_min) %>% bind_rows()

# arrange dataframes and caluclate max stats
cbh_max %>%
    bind_rows(tlb_max)  %>%
    bind_rows(rdg_max) %>%
    mutate(sum = R + B + G) %>%
    arrange(sum)
cbh_min %>%
    bind_rows(rdg_min)  %>%
    bind_rows(tlb_min) 

# Helper function to get and parse upper 95%-tile of values in a raster
get_q95 <- function(x) {
    #cat(sources(x), "\n")
    rast_q95 <- global(x, function(x) quantile(x, .95))
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get 95% quantiles for all rasters
cbh_q95 <- pblapply(cbh_files, get_q95) %>% bind_rows()
tlb_q95 <- pblapply(tlb_files, get_q95) %>% bind_rows()
rdg_q95 <- pblapply(rdg_files, get_q95) %>% bind_rows()

# arrange dataframes and caluclate max stats
cbh_q95 %>%
    bind_rows(tlb_q95)  %>%
    bind_rows(rdg_q95) %>%
    mutate(sum = R + B + G) %>%
    arrange(sum)
# Seems that tlb_2019_a is the overall brightest one statistically speaking, also in the blue band

# Set target image for each time_series
target_image <- "data/drone_time_series/cbh_timeseries/rgb/cbh_2017.tif"
# cbh2017 Chosen on best seperation of water vs. no water in raw bcc


# Read in image file names
image_list <- data.frame(
    file = list.files("data/drone_time_series",
        pattern = "\\.tif$",
        recursive = T,
        full.names = T
    )
) %>%
    filter(grepl("timeseries/", file)) %>%
    filter(grepl(".*/rgb/.*", file)) %>%
    filter(!grepl("native", file)) %>%
    slice(-1)

# Add image number
image_list$image_no <- 1:nrow(image_list)

# create output directories
dir.create("data/drone_time_series/cbh_timeseries/norm")
dir.create("data/drone_time_series/tlb_timeseries/norm")
dir.create("data/drone_time_series/rdg_timeseries/norm")

# prep cluster
#cl <- makeCluster(1)
#clusterEvalQ(cl, library(CRImage))
# Correct images
pblapply(
    image_list$file,
    function(rast_file){
        correct_image(rast_file, target_image)
    }
    
     #,
    #cl = cl
    )
# Stop cluster
#stopCluster(cl)

# Now -> verify using file browser that converstion was satisfactory.

# Next we need to copy the exif tags (note this will also copy the thumbnails
# but the acual images are not affected)
copy_tags <- function(src){
    exiftool_call(
      c("-tagsfromfile",
     src,  
     gsub("(.*)rgb(.*)", "\\1norm\\2", src), 
     "-all:all"))
}
map(
    image_list %>%
        filter(!grepl(".*cbh_2014_byte.*", file)) %>% # The cbh 2014 file is somehow different
        pull(file),
    copy_tags
)
# Copy GPS tags only for cbh 2014
cbh_2014_file <- image_list %>%
    filter(grepl(".*cbh_2014_byte.*", file)) %>%
    pull(file)
chb_2014_target_file <- gsub("(.*)rgb(.*)", "\\1norm\\2", cbh_2014_file)
cbh_2014 <- rast(cbh_2014_file)
chb_2014_target <- rast(chb_2014_target_file)
ext(chb_2014_target) <- ext(cbh_2014)
crs(chb_2014_target) <- crs(cbh_2014)
# Write and re-write file
writeRaster(chb_2014_target,
    gsub("\\.tif", "_new\\.tiff", chb_2014_target_file),
    overwrite = T,
    NAflag = NA
)
rm(chb_2014_target)
file.remove(chb_2014_target_file)
file.copy(gsub("\\.tif", "_new\\.tiff", chb_2014_target_file), chb_2014_target_file)
file.remove(gsub("\\.tif", "_new\\.tiff", chb_2014_target_file))
# # Copy geotiff tags only
# exiftool_call(
#     c(
#         "-tagsfromfile",
#         cbh_2014_file,
#         gsub("(.*)rgb(.*)", "\\1norm\\2", cbh_2014_file),
#         "--geotiff"
#     )
# )

# Check everything worked well
norm_rasters <- list.files("data/drone_time_series", full.names = T, recursive = T) %>%
    .[grepl("cbh_timeseries", .) | grepl("tlb_timeseries", .) | grepl("rdg_timeseries", .) ] %>%
    .[grepl("norm", .)] %>%
    .[!grepl("original", .)] %>%
    lapply(rast)
pblapply(norm_rasters, get_na) %>% bind_rows()
pblapply(norm_rasters, get_min) %>% bind_rows()
pblapply(norm_rasters, get_max) %>% bind_rows()
# If yes: remove all original images
# I recommend only doing that with a back up in place
# map(image_list$file, function(x){
#     file.remove(gsub("(.*)(\\..*)", "\\1_corrected\\2_original", x))
# })

# ## Finally, the alpha NA values for the RDG rasters need to be reset
# # Get list of rdg raster files
# rdg_rasters <- list.files("data/drone_time_series/rdg_timeseries/norm/", "tif",
#                           full.names = T)

# # Helper function to set NA values
# set_NA <- function(rast_file){
#   cat("Processing", rast_file, "\n")
#   # Load raster
#   rast_to_set <- rast(rast_file)
#   # Set NA values based on alpha band
#   cat("Setting NA's on Band 4 (alpha)... \n")
#   rast_to_set[[4]] <- classify(rast_to_set[[4]], cbind(0, NA))
#   # Copy NA values to other bands
#   for(i in 1:3){
#     cat("Setting Na's on Band", i, "...\n")
#     rast_to_set[[i]][is.na(rast_to_set[[4]][])] <- NA
#   }
#   # Write out raster to temp file
#   cat("Writing raster...\n")
#   writeRaster(rast_to_set, 
#               gsub("(.*)(\\.tif)", "\\1_NAset\\2", rast_file),
#               overwrite = T)
#   # # Unload rast
#   # rm(rast_to_set)
#   # gc()
#   # # Remove old raster
#   # file.remove(rast_file)
#   # # Copy new raster
#   # file.copy(gsub("(.*)(\\.tif)", "\\1_NAset\\2", rast_file),
#   #           rast_file)
#   # # Delete temp raster
#   # file.remove(gsub("(.*)(\\.tif)", "\\1_NAset\\2", rast_file))
#   cat("Done.\n")
#   return(NULL)
# }

# # Apply to all files
# lapply(rdg_rasters, set_NA)
