# Climate data perparations for the Pond Project
# Jakob J. Assmann jakob.assmann@uzh.ch 27 October 2022

# Dependencies
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)

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

# Read and parse data
temp <- read_table("data/climate/vgdcnRSM00021946.dat",
                   skip = 21,
                   col_names = F) %>%
  set_names(c("year", "month", "day", "temp")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))
temp2 <- read_table("data/climate/teca3195.dat",
                   skip = 21,
                   col_names = F) %>%
  set_names(c("year", "month", "day", "temp")) %>%
  mutate(date = as.Date(paste(year, month, day), format = "%Y %m %d"))
precip <- read_table("data/climate/pgdcnRSM00021946.dat",
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

# Quick graph to check validity
(ggplot(climate) + geom_line(aes(x = date, y = temp))) /
  (ggplot(climate) + geom_line(aes (x= date, y = precip))) /
    (ggplot(climate) + geom_line(aes (x= date, y = snow_depth)))

# Check for NAs
sum(is.na(climate$temp))
sum(is.na(climate$precip))
sum(is.na(climate$snow_depth))

# How many nas per year?
climate %>% group_by(year) %>% summarize(na_temp = sum(is.na(temp)),
                                         na_precip = sum(is.na(precip)),
                                         na_snow_depth = sum(is.na(snow_depth))) %>%
  view()

climate %>% filter(year >= 2019) %>% group_by(year, month) %>%
  summarize(na_temp = sum(is.na(temp)),
            na_precip = sum(is.na(precip)),
            na_snow_depth = sum(is.na(snow_depth))) %>%
  view()
# Conclusion: Data is pretty patchy past 2019 - use reanalysis data for gap-fill?

# Get min and max years
min(climate$year)
max(climate$year)
min(climate$year)
max(precip$year)
