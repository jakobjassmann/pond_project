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

# Helper function for correcting an image using the CRImage workflow
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

## Data preparation and screening

# Confirm datatypes of all rasters
data.frame(
    file = list.files("data/drone_data",
        pattern = "\\.tif$",
        recursive = T,
        full.names = T
    )
) %>%
    filter(grepl(".*/rgb/.*", file)) %>%
    filter(!grepl("native", file)) %>%
    pull(file) %>%
    lapply(function(x) {
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

# Prepare cbh2014 raster which is INT16 (INT2U), convert to byte INT1U
cbh_2014 <- rast("data/drone_data/cbh/rgb/cbh_2014.tif")
cbh_2014[[1]] <- round((cbh_2014[[1]] / 65535) * 255)
cbh_2014[[2]] <- round((cbh_2014[[2]] / 65535) * 255)
cbh_2014[[3]] <- round((cbh_2014[[3]] / 65535) * 255)
cbh_2014[[4]] <- round((cbh_2014[[4]] / 255) * 255)
writeRaster(
    cbh_2014,
    "data/drone_data/cbh/rgb/cbh_2014_byte.tif",
    datatype = "INT1U",
    NAflag=NA,
    overwrite = TRUE
    )

# Load drone rasters
cbh_files <- list.files("data/drone_data/cbh/rgb",
    ".tif$",
    full.names = T
) %>%
    .[-1] %>%
    lapply(rast)
tlb_files <- list.files("data/drone_data/tlb/rgb",
    ".tif$",
    full.names = T
) %>% lapply(rast)
rdg_files <- list.files("data/drone_daata/rgb",
    ".tif$",
    full.names = T
) %>% lapply(rast)

# Helper function to set row names of a data frame (or alike object)
set_rownames <- function(x, y) {
    rownames(x) <- y
    return(x)
}

# Helper function to get NA values for each image and band
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

# Arrange dataframes and caluclate na stats
cbh_na %>%
    bind_rows(tlb_na)  %>%
    bind_rows(rdg_na) 

# Helper function to get max values for each band
get_max <- function(x) {
    #cat(sources(x), "\n")
    rast_q95 <- global(x, max)
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get max values for all rasters
cbh_max <- pblapply(cbh_files, get_max) %>% bind_rows()
tlb_max <- pblapply(tlb_files, get_max) %>% bind_rows()
rdg_max <- pblapply(rdg_files, get_max) %>% bind_rows()

# arrange dataframes and caluclate cumulateive max
cbh_max %>%
    bind_rows(tlb_max)  %>%
    bind_rows(rdg_max) %>%
    mutate(sum = R + B + G) %>%
    arrange(sum)

# Helper function to get min values for each band
get_min <- function(x) {
    #cat(sources(x), "\n")
    rast_q95 <- global(x, min)
    t(rast_q95) %>%
        as.data.frame() %>%
        set_names(c("R", "G", "B", "alpha")) %>%
        set_rownames(gsub(".*/rgb/(.*)\\.tif", "\\1", sources(x)))
}

# Get min values for all rasters
cbh_min <- pblapply(cbh_files, get_min) %>% bind_rows()
tlb_min <- pblapply(tlb_files, get_min) %>% bind_rows()
rdg_min <- pblapply(rdg_files, get_min) %>% bind_rows()

# Arrange dataframes
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

# arrange dataframes and caluclate cumulative sum
cbh_q95 %>%
    bind_rows(tlb_q95)  %>%
    bind_rows(rdg_q95) %>%
    mutate(sum = R + B + G) %>%
    arrange(sum)

# Looking all good

### Normalise imagery

# Set target image for each time_series
# cbh_2017 Chosen as best seperation of water vs. no water in BCC
# See raw BCC vs. normalised BCC plots
target_image <- "data/drone_data/cbh/rgb/cbh_2017.tif"

# Read in image file names
image_list <- data.frame(
    file = list.files("data/drone_data",
        pattern = "\\.tif$",
        recursive = T,
        full.names = T
    )
) %>%
    filter(grepl(".*/rgb/.*", file)) %>% # Select only rgb rasters
    filter(!grepl("native", file)) %>%  # Throw out native res rasters
    filter(!grepl("cbh_2014\\.tif", file)) # Throw out wrongly coded cbh_2014 raster

# Add image number
image_list$image_no <- 1:nrow(image_list)

# create output directories
dir.create("data/drone_data/cbh/norm")
dir.create("data/drone_data/tlb/norm")
dir.create("data/drone_data/rdg/norm")

# Normalise all rasters using the helper function defined at top of script
pblapply(
    image_list$file,
    function(rast_file) {
        correct_image(rast_file, target_image)
    }
)
# Next -> confirm using file browser that converstion was satisfactory.

# Helper function to copy the exif tags (note thumbnails will not be copied)
copy_tags <- function(src){
    exiftool_call(
      c("-tagsfromfile",
     src,  
     gsub("(.*)rgb(.*)", "\\1norm\\2", src), 
     "-all:all"))
}

# Apply funciton to rasters
map(
    image_list %>%
        # filter(!grepl(".*cbh_2014_byte.*", file)) %>% # Previously this did not work, works now with newest version of exiftool / terra.
        pull(file),
    copy_tags
)

# # Copy GPS tags only for cbh 2014
# # Get file path
# cbh_2014_file <- image_list %>%
#     filter(grepl(".*cbh_2014_byte.*", file)) %>%
#     pull(file)
# # Define target files
# chb_2014_target_file <- gsub("(.*)rgb(.*)", "\\1norm\\2", cbh_2014_file)
# # Load raster
# cbh_2014 <- rast(cbh_2014_file)
# # Duplicate
# chb_2014_target <- rast(chb_2014_target_file)
# # Assign tags
# ext(chb_2014_target) <- ext(cbh_2014)
# crs(chb_2014_target) <- crs(cbh_2014)
# # Write and re-write file
# writeRaster(chb_2014_target,
#     gsub("\\.tif", "_new\\.tiff", chb_2014_target_file),
#     overwrite = T,
#     NAflag = NA
# )
# rm(chb_2014_target)
# file.remove(chb_2014_target_file)
# file.copy(gsub("\\.tif", "_new\\.tiff", chb_2014_target_file), chb_2014_target_file)
# file.remove(gsub("\\.tif", "_new\\.tiff", chb_2014_target_file))

# Remove back ups from tag copying
list.files("data/drone_data", full.names = T, recursive = T) %>%
    .[grepl("cbh/", .) | grepl("tlb/", .) | grepl("rdg/", .)] %>%
    .[grepl("norm", .)] %>%
    .[grepl("_original", .)] %>%
    file.remove()

# Check all worked
norm_rasters <- list.files("data/drone_data", full.names = T, recursive = T) %>%
    .[grepl("cbh/", .) | grepl("tlb/", .) | grepl("rdg/", .) ] %>%
    .[grepl("norm", .)] %>%
    lapply(rast)
pblapply(norm_rasters, get_na) %>% bind_rows()
pblapply(norm_rasters, get_min) %>% bind_rows()
pblapply(norm_rasters, get_max) %>% bind_rows()

# End of file