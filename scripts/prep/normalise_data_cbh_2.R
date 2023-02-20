# Quick script to correct the three images of different colour
# Jakob J. Assmann 28 October 2021 jakob.assmann@uzh.ch

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
    print(plot(imgCorrected))
    # write out
    writeImage(imgCorrected, 
               gsub("(.*)rgb(.*)", "\\1norm\\2", source_image))
    # return nothing
    return(NULL)
}

# Set target image
target_image <- "/scratch/jaassm/cbh_2021_rgb.tif"

# Read in image file names 
image_list <- data.frame(
    file = list.files("/scratch/jaassm/",
    full.names = T)[-7]
)

# Add image number
image_list$image_no <- 1:6



# prep cluster
cl <- makeCluster(6)
clusterEvalQ(cl, library(CRImage))
# Correct images
pblapply(
    image_list$file,
    target_image = target_image,
    FUN = correct_image,
    cl = 6)
# Stop cluster
stopCluster(cl)

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
map(image_list$file, copy_tags)
# If yes: remove all original images
# I recommend only doing that with a back up in place
# map(image_list$file, file.remove)
# map(image_list$file, function(x){
#     file.remove(gsub("(.*)(\\..*)", "\\1_corrected\\2_original", x))
# })
