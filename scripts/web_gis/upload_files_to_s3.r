# Upload web-GIS data to AWS S3 for web hosting
# Jakob J. Assmann jakob.asmann@uzh.ch 10 May 2024

# Reminder: load AWS credentials into the system environment (incl. aws default region)
# See here: https://github.com/cloudyr/aws.s3

# Dependencies
# install.packages("aws.s3")
library(tidyverse)
library(aws.s3)
library(pbapply)
library(parallel)

# Create new bucked (if needed)
# put_bucket("pondproject")

# Upload pond time-series files
files_to_upload <- c(list.files("figures/cbh/individual_ponds", full.names = T),
  list.files("figures/tlb/individual_ponds", full.names = T))
cl <- makeCluster(4)
clusterEvalQ(cl, library(aws.s3))
pblapply(files_to_upload, function(x) {
    cat(x, "\n")
    put_object(x,
        bucket = "pondproject",
        object = gsub(".*/(.*\\.png)", "pond-time-series/\\1", x))
        return(NULL)
}, cl = cl)
stopCluster(cl)

# Upload geojson
put_object("data/web_data/pond_time_series.geojson",
  bucket = "pondproject")

# Upload map tiles using for parallel uploads
files_to_upload <- list.files("data/web_data/tiles", recursive = T)
files_to_upload <- files_to_upload[!grepl("googlemaps|leaflet|openlayers|stage", files_to_upload)]
# Temp modifier to filter certain years: files_to_upload <- files_to_upload[grepl("2016|2021", files_to_upload)]
cl <- makeCluster(4)
clusterEvalQ(cl, library(aws.s3))
pblapply(files_to_upload, function(x) {
    put_object(paste0("data/web_data/tiles/", x),
        bucket = "pondproject",
        object = x)
        return(NULL)
}, cl = cl)
stopCluster(cl)

# Upload leaflet
put_object("docs/index.html",
  bucket = "pondproject")

# Upload leaflet ajax dependency
# Dowanload from: https://github.com/calvinmetcalf/leaflet-ajax and store in docs folder
put_object("docs/leaflet.ajax.js",
  bucket = "pondproject")
