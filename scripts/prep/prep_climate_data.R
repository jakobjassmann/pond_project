# Climate data perparations for the Pond Project
# Jakob J. Assmann jakob.assmann@uzh.ch 27 October 2022

# Dependencies
reticulate::use_condaenv("rgee")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(rgee)
library(sf)

# Prep folder
dir.create("data/climate")

# Downlad climate data for Chokurdah from GHCN-D via KNMI
# Source URLS obtained from:
# Temp: https://climexp.knmi.nl/gdcntave.cgi?id=someone@somewhere&WMO=RSM00021946&STATION=CHOKURDAH&extraargs=
# Precip: https://climexp.knmi.nl/gdcnprcp.cgi?id=someone@somewhere&WMO=RSM00021946&STATION=CHOKURDAH&extraargs=
# Snow depth: https://climexp.knmi.nl/gdcnsnwd.cgi?id=someone@somewhere&WMO=RSM00021946&STATION=CHOKURDAH&extraargs=
download.file("https://climexp.knmi.nl/data/vgdcnRSM00021946.dat",
              "data/climate/vgdcnRSM00021946.dat")
download.file("https://climexp.knmi.nl/data/pgdcnRSM00021946.dat",
              "data/climate/pgdcnRSM00021946.dat")
download.file("https://climexp.knmi.nl/data/dgdcnRSM00021946.dat",
              "data/climate/dgdcnRSM00021946.dat")
# And also from ECA&D
# Temp: https://climexp.knmi.nl/ecatemp.cgi?id=someone@somewhere&WMO=3195&STATION=CHOKURDAH&extraargs=
# Precip: https://climexp.knmi.nl/ecaprcp.cgi?id=someone@somewhere&WMO=3195&STATION=CHOKURDAH&extraargs=
download.file("https://climexp.knmi.nl/data/teca3195.dat",
              "data/climate/teca3195.dat")
download.file("https://climexp.knmi.nl/data/peca3195.dat",
              "data/climate/peca3195.dat")

# Read and parse data
temp <- read_table("data/climate/vgdcnRSM00021946.dat",
                   skip = 21,
                   col_names = F) %>%
  set_names(c("year", "month", "day", "temp")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))
temp_ecad <- read_table("data/climate/teca3195.dat",
                   skip = 21,
                   col_names = F) %>%
  set_names(c("year", "month", "day", "temp")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))
precip <- read_table("data/climate/pgdcnRSM00021946.dat",
                   skip = 22,
                   col_names = F) %>%
  set_names(c("year", "month", "day", "precip")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))
precip_ecad <- read_table("data/climate/pgdcnRSM00021946.dat",
                     skip = 22,
                     col_names = F) %>%
  set_names(c("year", "month", "day", "precip")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))

snow_depth <- read_table("data/climate/dgdcnRSM00021946.dat",
                     skip = 21,
                     col_names = F) %>%
  set_names(c("year", "month", "day", "snow_depth")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))

# Merge into one
climate <- full_join(temp, precip) %>%
  full_join(snow_depth) %>%
  relocate(date) 
climate_ecad <- full_join(temp_ecad, precip_ecad)

# Quick graph to check validity
(ggplot(climate) + geom_line(aes(x = date, y = temp))) /
  (ggplot(climate) + geom_line(aes (x= date, y = precip))) /
    (ggplot(climate) + geom_line(aes (x= date, y = snow_depth)))
(ggplot(climate_ecad) + geom_line(aes(x = date, y = temp))) /
  (ggplot(climate_ecad) + geom_line(aes (x= date, y = precip))) 
  
# Check for NAs
sum(is.na(climate$temp))
sum(is.na(climate$precip))
sum(is.na(climate$snow_depth))

sum(is.na(climate_ecad$temp))
sum(is.na(climate_ecad$precip))

# Get min and max years
min(climate$year)
max(climate$year)
min(climate_ecad$year)
max(climate_ecad$year)

# How many NAs per year?
climate %>% group_by(year) %>% summarize(na_temp = sum(is.na(temp)),
                                         na_precip = sum(is.na(precip)),
                                         na_snow_depth = sum(is.na(snow_depth))) %>%
  view()
climate_ecad %>% group_by(year) %>% summarize(na_temp = sum(is.na(temp)),
                                         na_precip = sum(is.na(precip))) %>%
  view()

climate %>% filter(year >= 2019) %>% group_by(year, month) %>%
  summarize(na_temp = sum(is.na(temp)),
            na_precip = sum(is.na(precip)),
            na_snow_depth = sum(is.na(snow_depth))) %>%
  view()
# Conclusion: GHCN-D data is pretty patchy past 2019 
# EACD data only available till 2019, but more complete
# -> use ERA5 reanalysis data for gap-fill?

### Let's get the ERA5 data from the GEE

# Set site coordinates 
chokurdah <- data.frame(
  name = "Chokrudah",
  lat = 70.62,
  long = 147.88
) %>%
  st_as_sf(coords = c("long", "lat"), crs = 4326)

# Initialize EE
ee_Initialize()

# Get ERA5 image collection (filter 2000-2022)
era5land <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$
  select(list("temperature_2m", "total_precipitation"))

# send Chokurdah coordinates to EE
chokurdah_ee <- sf_as_ee(chokurdah)

# Extract time-series (using drive here to let it run in the background)
climate_era5land <- era5land$map(function(image){
  climate <- image$reduceRegion(ee$Reducer$first(), chokurdah_ee)
  climate <- climate$set("date_time", image$get("system:index"))
  ee$Feature(NULL, climate)
  }) 
climate_era5land_task <- ee_table_to_drive(climate_era5land,
                  description = "chokurdah_era5_export",
                  folder = "chokurdah_era5_export",
                  )
climate_era5land_task$start()                           
ee_check_task_status(task = "AES6MWH7AK2EGF6V4BDWQ4AW")
ee_drive_to_local(task = climate_era5land_task, 
                  dsn = "data/climate/ERA5_land_export_Chokurdah.csv")

# If interrupted copy over task outpur manuall from drive.
# Then read in ERA5 export and parse
climate_era5 <- t(as.matrix(read.csv("data/climate/ERA5_exports_Chokurdah.csv")))
climate_era5 <- as.data.frame(climate_era5)
climate_era5$date <- row.names(climate_era5)
row.names(climate_era5) <- NULL
climate_era5$var <- gsub("X[0-9]*_(.*)", "\\1", climate_era5$date)
climate_era5$date <- as.Date(gsub("X([0-9]*)_.*", "\\1", climate_era5$date), 
                             format = "%Y%m%d")
climate_era5 <- climate_era5[-1,]
climate_era5 <- climate_era5[c(-nrow(climate_era5),-nrow(climate_era5)-1),]
pivot_wider(climate_era5, names_from = var, values_from = V1)
