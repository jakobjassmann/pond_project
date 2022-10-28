# Quick script to correct the three images of different colour
# Jakob J. Assmann 28 October 2021 jakob.assmann@uzh.ch

# Dependencies
#install.packages("CRImage")
#if (!require("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("CRImage")
library(CRImage)
library(tidyverse)
library(parallel)
library(pbapply)

correct_image <- function(source_image, target_image){
    cat(source_image, "\n")
    # read in the target image
    targetImage=readImage(target_image)
    # read in the image whose color values should be adapted
    imgToConvert=readImage(source_image)
    # calculate mean and standard deviation of target color channels
    mst=calculateMeanStdTarget(targetImage)
    # create a white pixel mask
    whitePixelMask=imgToConvert[,,1]>0.85 & imgToConvert[,,2]>0.85 & imgToConvert[,,3]>0.85
    # adapt color channels of image
    imgCorrected=colorCorrection(imgToConvert,mst,whitePixelMask)
    # plot the result
    print(plot(imgCorrected))
    # write out
    writeImage(imgCorrected, 
        gsub("(.*)(\\..*)", "\\1_corrected\\2", source_image))
    # return nothing
    return(NULL)
}

# Set target image
target_image <- "C:/Scratch/pond_project/cbh/cbh_2016/cbh_2016_merged/img/IMG_0700.JPG"

# Read in images 
image_list <- data.frame(
    file = list.files("C:/Scratch/pond_project/cbh/cbh_2016/cbh_2016_merged/img",
    full.names = T)
)

# Add image number
image_list$image_no <- as.numeric(gsub(".*.([0-9]{4})\\..*", "\\1", image_list$file))

# Subset 
image_list <- image_list %>% filter((image_no > 2000) | image_no %in% c(708:711))

# prep cluster
cl <- makeCluster(7)
clusterEvalQ(cl, library(CRImage))
# Correct images
pblapply(
    image_list$file,
    target_image = target_image,
    FUN = correct_image,
    cl = cl)
# Stop cluster
stopCluster(cl)

# Next -> verify using file browser that converstion was satisfactory.

# If yes: remove all original images
# I recommend only doing that with a back up in place
# map(image_list$file, file.delete)