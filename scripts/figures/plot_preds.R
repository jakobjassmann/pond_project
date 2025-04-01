# Quick script to visualise preddictions for Quality control

# Dependencies
library(terra)
library(ggplot2)
library(tidyterra)
library(cowplot)
library(pbapply)

##  Prepare parallel environment
# Windows
# cl <- makeCluster(detectCores() - 1)
# clusterEvalQ(cl, {
#   library(terra)
#   library(ggplot2)
#   library(tidyterra)
#   library(cowplot)
# })
# Unix
cl <- detectCores() - 1

# get rasters
norm_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/norm/", .)]
preds_files <- list.files("data/drone_data/", pattern = ".tif$", recursive = T, full.names = T) %>%
  .[grepl("/preds_filtered/", .)]

# Filter by site
preds_cbh <- preds_files[grepl("cbh", preds_files)]
preds_tlb <- preds_files[grepl("tlb", preds_files)]
preds_rdg <- preds_files[grepl("rdg", preds_files)]

# # Helper function to load categorical preds raster
cat_rast <- function(rast_file) {
  rast_object <- rast(rast_file)
  levels(rast_object) <- data.frame(id = 1, class = "water")
  return(rast_object)
}
# Helper funciton to plot predictions and bcc
plot_preds <- function(preds_file) {
  # preds_file <- preds_cbh[1]
  cat(preds_file)
  year_interest <- gsub(".*/.*[a-z]{3}_([0-9]{4}.*)_preds.*\\.tif", "\\1", preds_file)
  site_interest <- gsub(".*(cbh|tlb|rdg).*", "\\1", preds_file)
  norm_file <- norm_files %>%
    .[grepl(site_interest, .)] %>%
    .[grepl(year_interest, .)]
  plot_grid(
    ggplot() +
      geom_spatraster_rgb(data = rast(norm_file), max_col_value = 65535) +
      geom_spatraster(data = cat_rast(preds_file)) +
      scale_fill_manual(values = "magenta", na.value = "transparent") +
      theme_nothing(),
    # ggplot() +
    #   geom_spatraster(data = cat_rast(preds_file)) +
    #   scale_fill_manual(values = c(NA, "magenta"), na.value = "transparent") +
    #   theme_nothing(),
    labels = c(paste0(site_interest, "_", year_interest), "")
  ) %>%
    return()
}

# Plot prediciton time-series
pblapply(preds_cbh, plot_preds, cl = cl) %>%
  plot_grid(plotlist = .) %>%
  save_plot(
    filename = "figures/cbh_preds.png",
    bg = "white",
    base_height = 10
  )
pblapply(preds_tlb, plot_preds, cl = cl) %>%
  plot_grid(plotlist = .) %>%
  save_plot(
    filename = "figures/tlb_preds.png",
    bg = "white",
    base_height = 10
  )
pblapply(preds_rdg, plot_preds, cl = cl) %>%
  plot_grid(plotlist = .) %>%
  save_plot(
    filename = "figures/rdg_preds.png",
    bg = "white",
    base_height = 10
  )

# Stop cluster on Windows
# stopCluster(cl)
