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
plot_year <- function(year_interest){
  cat("Plotting year:", year_interest, "\n")
  rgb_norm <- rast(cbh_rgb[grepl(year_interest, cbh_rgb)])
  preds <- rast(cbh_preds[grepl(year_interest, cbh_preds)])
  preds[preds == 1] <- NA
  temp_file <- tempfile(fileext = ".png")
  png(temp_file, width = 1920, height = 1920)
    plotRGB(rgb_norm, scale = 1)
    plot(preds, col = "#ff09ff55", add = T)
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

# Try morphing them
morphed_images <- map(1:length(years),
                      function(x){
                        if(x == length(years)){
                          return(rep(list_of_images[[x]], 8) %>% map(image_read) %>% image_join())
                        } else {
                          image_rep <- rep(list_of_images[[x]], 8) %>% map(image_read) %>% image_join()
                          morphed <- image_morph(
                            image_join(
                            image_read(list_of_images[[x]]),
                            image_read(list_of_images[[x+1]])))
                          return(image_join(image_rep, morphed))
                        }
                      })
morphed_images %>% 
  image_join() %>%
  image_animate(optimize = TRUE) %>%
  image_write("figures/cbh/time_series_cbh_animated_morph.gif")
