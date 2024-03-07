# Dependencies
library(tidyverse)
library(terra)
library(ggplot2)
library(cowplot)
library(magick)


# Get list of files
cbh_rgb <- list.files("data/drone_time_series/cbh/cbh_norm/",
                        pattern = "tif",
                        full.names = T) 
cbh_preds <- list.files("data/drone_time_series/cbh/cbh_preds/",
                        pattern = "tif",
                        full.names = T) 

# Define function to plot water cover per year
plot_year <- function(year_interest, add_preds = TRUE){
  cat("Plotting year:", year_interest, "\n")
  rgb_norm <- rast(cbh_rgb[grepl(year_interest, cbh_rgb)])
  preds <- rast(cbh_preds[grepl(year_interest, cbh_preds)])
  preds[preds == 1] <- NA
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 1920, height = 1920)
    plotRGB(rgb_norm, scale = 1)
    if(add_preds == TRUE){
      plot(preds, col = "#ff09ff55", add = T)
    }
    # Add a progress bar
    prog <- (year_interest - 2014) / (2021-2014)
    # bar width
    rect(ext(rgb_norm)[1] + (ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.7,
         ext(rgb_norm)[3] + (ext(rgb_norm)[4] - ext(rgb_norm)[3]) * 0.925,
         ext(rgb_norm)[1] + (ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.85,
         ext(rgb_norm)[3] + (ext(rgb_norm)[4] - ext(rgb_norm)[3]) * 0.95,
         border = "white",
         lwd = 2)
    rect(ext(rgb_norm)[1] + (ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.7,
         ext(rgb_norm)[3] + (ext(rgb_norm)[4] - ext(rgb_norm)[3]) * 0.925,
         ext(rgb_norm)[1] + (ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.7 +
           (((ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.85) - ((ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.7)) * prog,
         ext(rgb_norm)[3] + (ext(rgb_norm)[4] - ext(rgb_norm)[3]) * 0.95,
         col = "white",
         border = "white")
    text(ext(rgb_norm)[1] + (ext(rgb_norm)[2] - ext(rgb_norm)[1]) * 0.925,
         ext(rgb_norm)[3] + (ext(rgb_norm)[4] - ext(rgb_norm)[3]) * (0.925 + ((0.95-0.925) * 0.5)),
         year_interest,
         col = "white",
         cex = 4)
  dev.off()
  return(temp_file)
}

# Specify years and generate plots
years <- c(2014,2016:2021)
list_of_images <- map(years, plot_year)

# Animate plots
img_list <- map(list_of_images, image_read)
img_joined <- image_join(img_list)

# Export as gif
image_write_gif(image = img_joined,
                path = "figures/cbh/time_series_cbh_animated.gif",
                delay = 2)

# Without water annotations:
list_of_images_no_water <- map(years, plot_year, add_preds = FALSE)
img_list_no_water <- map(list_of_images_no_water, image_read)
img_joined_nowater <- image_join(img_list_no_water)
image_write_gif(image = img_joined_nowater,
                path = "figures/cbh/time_series_cbh_animated_no_water.gif",
                delay = 2)

# Write function to simulate image sliding in
image_slide <- function(img_1, img_2, n_frames = 100,
                        hold_last_frame = 25){
  # Check images are of same dimension if not throw error
  if(!((image_info(img_1)$width == image_info(img_2)$width) &
       (image_info(img_1)$height == image_info(img_1)$height))){
    stop("Images of different dimensions")
  }
  # Calculate crop spacing for second image sliding in:
  slide_bins <- round(seq(0, image_info(img_1)$width, 
                          length = n_frames))
  # Generate frames
  frames <- map(1:n_frames, function(cutoff){
    cat("Frame", cutoff, "out of", n_frames, 
        "(", round((cutoff / n_frames) * 100), "%) \r")
    img_2_chop <- image_crop(img_2, paste0(slide_bins[cutoff],
                                           "x",
                                           image_info(img_1)$height))
    frame <- image_flatten(image_join(img_1, img_2_chop))
    return(frame)
  }) %>% image_join()
  
  # Hold last frame for an extra length if requested
  if(hold_last_frame > 0){
    last_frames <- rep(img_2, hold_last_frame) %>% image_join()
    frame <- image_join(frames, last_frames)
  }
  
  cat("\nDone\n")
  # Return frames
  return(frames)
}

# Generate slides for all image pairs
img_slide_series <- map(1:length(img_list), function(x){
  cat("Preparing slide_in No:", x, "\n")
  image_slide(img_list_no_water[[x]], img_list[[x]])
}) %>% image_join()

image_write_gif(image = img_slide_series,
                path = "figures/cbh/time_series_cbh_sliding.gif",
                delay = 0.04)

# # Try morphing them
# morphed_images <- map(1:length(years),
#                       function(x){
#                         if(x == length(years)){
#                           return(rep(list_of_images[[x]], 8) %>% map(image_read) %>% image_join())
#                         } else {
#                           image_rep <- rep(list_of_images[[x]], 8) %>% map(image_read) %>% image_join()
#                           morphed <- image_morph(
#                             image_join(
#                             image_read(list_of_images[[x]]),
#                             image_read(list_of_images[[x+1]])))
#                           return(image_join(image_rep, morphed))
#                         }
#                       })
# morphed_images %>% 
#   image_join() %>%
#   image_animate(optimize = TRUE) %>%
#   image_write("figures/cbh/time_series_cbh_animated_morph.gif")



