# Climate data perparations for the Pond Project
# Jakob J. Assmann jakob.assmann@uzh.ch 27 October 2022

# Test line

# Dependencies
reticulate::use_condaenv("rgee")
library(tidyverse)
library(ggplot2)
library(patchwork)
library(cowplot)
library(rgee)
library(sf)
library(lubridate)
library(SPEI)

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

# ### Let's get the ERA5 data from the GEE
# 
# # Set site coordinates 
# chokurdah <- data.frame(
#   name = "Chokrudah",
#   lat = 70.62,
#   long = 147.88
# ) %>%
#   st_as_sf(coords = c("long", "lat"), crs = 4326)
# 
# # Initialize EE
# ee_Initialize()
# 
# # Get ERA5 image collection (filter 2000-2022)
# era5land <- ee$ImageCollection("ECMWF/ERA5_LAND/HOURLY")$
#   select(list("temperature_2m", "total_precipitation"))
# 
# # send Chokurdah coordinates to EE
# chokurdah_ee <- sf_as_ee(chokurdah)
# 
# # Extract time-series (using drive here to let it run in the background)
# climate_era5land <- era5land$map(function(image){
#   climate <- image$reduceRegion(ee$Reducer$first(), chokurdah_ee)
#   climate <- climate$set("date_time", image$get("system:index"))
#   ee$Feature(NULL, climate)
#   }) 
# climate_era5land_task <- ee_table_to_drive(climate_era5land,
#                   description = "chokurdah_era5_export",
#                   folder = "chokurdah_era5_export",
#                   )
# climate_era5land_task$start()                           
# climate_era5land_task <- ee_check_task_status(task = "AES6MWH7AK2EGF6V4BDWQ4AW")
# ee_drive_to_local(task = climate_era5land_task, 
#                   dsn = "data/climate/ERA5_land_export_Chokurdah.csv")
# 
# # Load and process data
# era5_data <- read_csv("data/climate/ERA5_land_export_Chokurdah.csv") %>%
#   select(-`system:index`, .geo) %>%
#   mutate(date = as.Date(format(date_time, "%Y-%m-%d"))) %>%
#     group_by(date) %>%
#     summarize(temp = mean(temperature_2m),
#               precip = sum(total_precipitation )) %>%
#   mutate(year = format(date, "%Y"),
#          month = format(date, "%m"),
#          doy = format(date, "%j")) 
# 
# era5_monthly <- era5_data %>%
#   group_by(year, month) %>%
#   summarize(mean_temp = mean(temp),
#             month_precip = sum(precip)) %>%
#   mutate(month_year = as.Date(paste0(year, "-", month, "-15")))%>%
#   filter(year >= 1965 & year < 2022)
# 
# era5_annual <- era5_data %>%
#   mutate(year = as.numeric(year)) %>%
#   group_by(year) %>%
#   summarize(annual_mean_temp = mean(temp) - 273.15,
#             annual_precip = sum(precip)) %>%
#   filter(year >= 1965 & year < 2022)
# 
# era5_annual <- era5_data %>%
#   mutate(year_precip = case_when(
#     as.numeric(month) >= 8 ~ as.numeric(year) - 1,
#     TRUE ~ as.numeric(year)
#   )) %>% 
#   group_by(year_precip) %>%
#   summarize(precip_season = sum(precip))  %>%
#   filter(year_precip >= 1965 & year_precip < 2022) %>%
#   mutate(year = year_precip) %>%
#   full_join(era5_annual)
#     
# era5_annual <- era5_data %>%
#   mutate(year_precip = case_when(
#     as.numeric(month) >= 9 ~ as.numeric(year) - 1,
#     TRUE ~ as.numeric(year)
#   )) %>% 
#   filter(month %in% c(10,11,12,1,2,3)) %>%
#   group_by(year_precip) %>%
#   summarize(precip_winter = sum(precip))  %>%
#   filter(year_precip >= 1965 & year_precip < 2022) %>%
#   mutate(year = year_precip) %>%
#   full_join(era5_annual)
# 
# ggplot(era5_annual) +
#   geom_point(aes(x = year, y = annual_mean_temp)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggplot(era5_annual) +
#   geom_point(aes(x = year, y = annual_precip)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggplot(era5_annual) +
#   geom_point(aes(x = as.character(year), y = precip_season)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# ggplot(era5_annual) +
#   geom_point(aes(x = as.character(year), y = precip_winter)) +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ggplot(era5_monthly %>% filter(year >= 2000)) +
#   geom_col(aes(x = month_year, y = month_precip)) +
#   scale_x_date(date_breaks = "year")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
# 
# ggplot(precip %>% group_by(year, month) %>% summarise(precip = sum(precip)) %>%
#          mutate(month_year = as.Date(paste0(year, "-", month, "-15")))
#        %>% filter(year >= 2000)) +
#   geom_col(aes(x = month_year, y = precip), fill = "red", colour = "red") +
#   geom_col(aes(x = month_year, y = month_precip * 10),
#            inherit.aes = F, 
#            data = era5_monthly %>% filter(year >= 2000),
#            fill = NA,
#            colour = "blue") + 
#   scale_x_date(date_breaks = "year")+
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
#   
# 
#   # If interrupted copy over task outpur manuall from drive.
# # Then read in ERA5 export and parse
# climate_era5 <- t(as.matrix(read.csv("data/climate/ERA5_exports_Chokurdah.csv")))
# climate_era5 <- as.data.frame(climate_era5)
# climate_era5$date <- row.names(climate_era5)
# row.names(climate_era5) <- NULL
# climate_era5$var <- gsub("X[0-9]*_(.*)", "\\1", climate_era5$date)
# climate_era5$date <- as.Date(gsub("X([0-9]*)_.*", "\\1", climate_era5$date), 
#                              format = "%Y%m%d")
# climate_era5 <- climate_era5[-1,]
# climate_era5 <- climate_era5[c(-nrow(climate_era5),-nrow(climate_era5)-1),]
# pivot_wider(climate_era5, names_from = var, values_from = V1)

# Webscrape climate data from http://www.pogodaiklimat.ru (recommended by Ramona)
# Imitate polite scraping session using bow
session <- bow("http://www.pogodaiklimat.ru/weather.php",
               force = T,
               delay =  5)

# Define year and month combinations to scrape (data available from 2011 - today) 
years_months <- expand.grid(2011:2021,1:12) %>%
  set_names("year", "month") %>%
  tibble() %>%
  arrange(year, month)

# Create output directory
dir.create("data/climate/pik_ru_monthly")

# Scrape over year and month combinations
chokurdakh_weather <- map2(years_months$year, 
                        years_months$month,
                        function(year, month){
                          cat("Obtaining record for Year:", year, "month:", month, "\n")
                          
                          # Check weather file exists
                          if(file.exists(paste0("data/climate/pik_ru_monthly/", year, "_", month, ".csv"))){
                            cat("Record exists, loading file.\n")
                            return(read_csv(paste0("data/climate/pik_ru_monthly/", year, "_", month, ".csv")))
                          }
                          # Request html table for year and month
                          results <- scrape(session,
                                            query = list(id = "21946", 
                                                         bday = "1", 
                                                         fday = as.character(days_in_month(ym(paste0(year, "-", month)))), 
                                                         amonth = as.character(month),
                                                         ayear= as.character(year), 
                                                         bot = "2"))
                          # Parse table and tidy up data
                          parsed_data <- bind_cols(
                            # Side table with data and time
                            results %>% 
                              html_nodes(".archive-table-left-column") %>% 
                              html_table(header = T),
                            # Table with weather station observations
                            results %>% 
                              html_nodes(".archive-table-wrap") %>% 
                              html_table(header = T)
                          ) %>%
                            # Add leading zeros to dates and hours where they are missing
                            # Also deal with date values for October being runcated
                            mutate(
                              `Время  (UTC),\tдата...2` = case_when(month == 10 ~ paste0(as.character(`Время  (UTC),\tдата...2`), "0"),
                                        TRUE ~ as.character(`Время  (UTC),\tдата...2`)),
                              `Время  (UTC),\tдата...2` = case_when(
                              nchar(`Время  (UTC),\tдата...2`) == 4 ~ paste0("0", `Время  (UTC),\tдата...2`),
                              TRUE ~ paste0(`Время  (UTC),\tдата...2`)),
                              `Время  (UTC),\tдата...1` = case_when(
                                nchar(`Время  (UTC),\tдата...1`) == 1 ~ paste0("0", `Время  (UTC),\tдата...1`),
                                TRUE ~ paste0(`Время  (UTC),\tдата...1`))
                            ) %>%
                            # Combine date and time, convert to POSIXct
                            mutate(date_time = 
                                     as.POSIXct(
                                       paste0(year, ".", 
                                              `Время  (UTC),\tдата...2`,
                                              ":", `Время  (UTC),\tдата...1`, ":00"),
                                       format = "%Y.%d.%m:%H:%M",
                                       tz = "UTC"
                                     )
                            ) %>%
                            # Select only date time, temp and precip
                            select(date_time, temp = `Т(С)`, precip = `R(мм)`)
                          
                          # Save parsed data
                          write_csv(parsed_data, paste0("data/climate/pik_ru_monthly/", year, "_", month, ".csv"))
                          
                          # Return parsed data
                          return(parsed_data)
                        }) %>% 
  # combine into one dataframe
  bind_rows()

# Save to CSV file
write_csv(chokurdakh_weather,
          "data/climate/pik_exports.csv")
# chokurdakh_weather <- read_csv("data/climate/pik_exports.csv")
# Add local time-zone variable
chokurdakh_weather$date_time_local <- with_tz(chokurdakh_weather$date_time, tzone = "Etc/GMT-11")

# Calculate daily values
chokurdakh_weather$date <- format(chokurdakh_weather$date_time_local,
                                  "%Y-%m-%d")

# Quick helper function for parsing precip values as these were
# only recorded once a day
parse_precip <- function(x){
  if(sum(is.na(x)) == length(x)) {
    return(NA)
  }
  else {
    return(sum(x, na.rm = T))
  }
}

chokurdakh_weather_daily <- chokurdakh_weather %>%
  group_by(date) %>%
  summarize(temp = mean(temp),
            precip = parse_precip(precip)) %>%
  mutate(date = as.Date(date))

# Check for NAs in temperature data (days with incomplete obs)
sum(is.na(chokurdakh_weather_daily$temp))
chokurdakh_weather_daily[which(is.na(chokurdakh_weather_daily$temp)),]
# the missing observations in temperature are few, I think we can ignore those

# Check for NAs in precip data (days with incomplete obs)
sum(is.na(chokurdakh_weather_daily$precip))
# 1718 is a lot, how many days are there where a precip of 0 was
# recorded?
sum(chokurdakh_weather_daily$precip == 0, na.rm = T)

# Okay, so it is unclear on whether the 1718 days without obs above
# are missing data or not available as not recorded. 
# For the purpose of moving on we will assume that NA means no
# precip as no data was recorded (this is a potentially flawed assumption)
chokurdakh_weather_daily$precip[is.na(chokurdakh_weather_daily$precip)] <- 0

# Check completeness of the years
chokurdakh_weather_daily %>% mutate(year = format(date, "%Y"),
                                    month = format(date, "%m")) %>%
  group_by(year, month) %>% tally() %>% view()

# Compare with other climate records
climate_ecad %>% filter(year >= 2011 & year <= 2021) %>%
  summarise(sum(is.na(precip)))
climate_ecad %>% filter(year >= 2011 & year <= 2021) %>%
  select(date, temp1 = temp, precip1 = precip) %>%
  full_join(chokurdakh_weather_daily) %>%
  filter(precip != precip1) %>% nrow()
# The values differ for 464 entries! Sigh! What are the differences?
climate_ecad %>% filter(year >= 2011 & year <= 2021) %>%
  select(date, temp1 = temp, precip1 = precip) %>%
  full_join(chokurdakh_weather_daily) %>%
  mutate(diff = precip - precip1) %>%
  filter(precip != precip1) %>%
  #filter(abs(diff) > 2) %>% 
  #view()
  summarize(mean(diff),
            min(diff),
            max(diff))
# There are three big old outliers, remove those by setting the precip to 0
chokurdakh_weather_daily[which(chokurdakh_weather_daily$precip > 50), ]
chokurdakh_weather_daily$precip[which(chokurdakh_weather_daily$precip > 50)] <- 0
# Check mean difference once more
climate_ecad %>% filter(year >= 2011 & year <= 2021) %>%
  select(date, temp1 = temp, precip1 = precip) %>%
  full_join(chokurdakh_weather_daily) %>%
  mutate(diff = precip - precip1) %>%
  filter(precip != precip1) %>%
  #filter(abs(diff) > 2) %>% 
  #view()
  summarize(mean(diff),
            min(diff),
            max(diff),
            n())

## Caluculate season summary statistics for each year - climate ecad
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  # annual temp mean and annual precip sums
  summarise(temp_annual_mean = mean(temp, na.rm = T),
            precip_annual_sum = sum(precip, na.rm = T))
# Sept-Aug precip
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month >= 9 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  # annual temp mean and annual precip sums
  summarise(precip_sept_aug_sum = sum(precip, na.rm = T)) %>%
  full_join(climate_ecad_by_year)
# Season sums JJA
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  filter(month %in% c(6,7,8)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_jja_mean = mean(temp, na.rm = T),
            precip_jja_sum = sum(precip, na.rm = T)) %>%
  full_join(climate_ecad_by_year)
# Season sums SON pervious year
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month >= 9 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  filter(month %in% c(9,10,11)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_son_mean = mean(temp, na.rm = T),
            precip_son_sum = sum(precip, na.rm = T)) %>%
  full_join(climate_ecad_by_year)
# Season sums djf
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month == 12 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  filter(month %in% c(12,1,2)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_djf_mean = mean(temp, na.rm = T),
            precip_dfj_sum = sum(precip, na.rm = T)) %>%
  full_join(climate_ecad_by_year)
# Season sums mam
climate_ecad_by_year <- climate_ecad %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  filter(month %in% c(3,4,5)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_mam_mean = mean(temp, na.rm = T),
            precip_mam_sum = sum(precip, na.rm = T)) %>%
  full_join(climate_ecad_by_year)
# Throw out first and last year
climate_ecad_by_year <- filter(climate_ecad_by_year, 
                               year != 2019,
                               year != 1944)


## Caluculate season summary statistics for each year - pik
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  # annual temp mean and annual precip sums
  summarise(temp_annual_mean = mean(temp, na.rm = T),
            precip_annual_sum = sum(precip, na.rm = T))
# Sept-Aug precip
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month >= 9 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  # annual temp mean and annual precip sums
  summarise(precip_sept_aug_sum = sum(precip, na.rm = T)) %>%
  full_join(pik_by_year)
# Season sums JJA
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  filter(month %in% c(6,7,8)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_jja_mean = mean(temp, na.rm = T),
            precip_jja_sum = sum(precip, na.rm = T)) %>%
  full_join(pik_by_year)
# Season sums SON
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month >= 9 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  filter(month %in% c(9,10,11)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_son_mean = mean(temp, na.rm = T),
            precip_son_sum = sum(precip, na.rm = T)) %>%
  full_join(pik_by_year)
# Season sums dfj
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  mutate(year = case_when(month == 12 ~ year + 1,
                          TRUE ~ year)) %>%
  group_by(year) %>%
  filter(month %in% c(12,1,2)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_djf_mean = mean(temp, na.rm = T),
            precip_dfj_sum = sum(precip, na.rm = T)) %>%
  full_join(pik_by_year)
# Season sums mam
pik_by_year <- chokurdakh_weather_daily %>%
  mutate(year = as.numeric(format(date, "%Y")),
         month = as.numeric(format(date, "%m"))) %>%
  group_by(year) %>%
  filter(month %in% c(3,4,5)) %>%
  # annual temp mean and annual precip sums
  summarise(temp_mam_mean = mean(temp, na.rm = T),
            precip_mam_sum = sum(precip, na.rm = T)) %>%
  full_join(pik_by_year)
# Throw out 2022
pik_by_year <- filter(pik_by_year, year != 2022)

## Check correlations for years in common
map(names(pik_by_year), function(x){
  years_common <- inner_join(select(climate_ecad_by_year, year),
                             select(filter(pik_by_year, year != 2011), year)) %>% pull(year)
  data.frame(
    variable = x,
    r = cor(filter(climate_ecad_by_year, year %in% years_common) %>% pull(!!x),
            filter(pik_by_year, year %in% years_common) %>% pull(!!x),
            use = "complete.obs"),
    mean_diff = mean(filter(climate_ecad_by_year, year %in% years_common) %>% pull(!!x) -
                           filter(pik_by_year, year %in% years_common) %>% pull(!!x),
                     na.rm = T)
  )
}) %>% bind_rows() %>%
  view()
# => Temperature is highly correlated and so is preciptation,
# but it is definitely in the latter where the error lies. 
# The ECAD data had lost of gaps in the precip record in 
# 2014, 2016 and 2017 and from 2019 onwards. Let's use the pik data to replace
# all those years
chok_annual_climate_final <- climate_ecad_by_year %>%
  filter(!(year %in% c(2014, 2016, 2017))) %>%
  bind_rows(filter(pik_by_year, year %in% c(2014, 2016, 2017, 2019:2021))) %>%
  arrange(year)
write_csv(chok_annual_climate_final,
          "data/climate/chok_annual_climate_final.csv")

### Caluclate the SPEI

# combine source datasets and calculate monthly values
spei <- bind_rows(climate_ecad %>%
                    mutate(year = as.numeric(format(date, "%Y")),
                           month = as.numeric(format(date, "%m"))) %>%
                    filter(!(year %in% c(2014, 2016, 2017, 2019))) %>%
                    group_by(year, month) %>%
                    summarize(temp = mean(temp, na.rm = T),
                              precip = sum(precip, na.rm = T)) %>%
                    mutate(water_bal = precip - as.numeric(thornthwaite(temp, 70.62))),
                  chokurdakh_weather_daily %>%
                    mutate(year = as.numeric(format(date, "%Y")),
                           month = as.numeric(format(date, "%m"))) %>%
                    filter(year %in%  c(2014, 2016, 2017, 2019:2021)) %>%
                    group_by(year, month) %>%
                    summarize(temp = mean(temp, na.rm = T),
                              precip = sum(precip, na.rm = T)) %>%
                    mutate(water_bal = precip - as.numeric(thornthwaite(temp, 70.62)))
                  ) %>%
  ungroup() %>%
  arrange(year, month) %>%
  mutate(spei_3_months = as.numeric(spei(water_bal, 3)$fitted),
         spei_6_months = as.numeric(spei(water_bal, 6)$fitted),
         spei_9_months = as.numeric(spei(water_bal, 9)$fitted),
         spei_12_months = as.numeric(spei(water_bal, 12)$fitted),
         spei_24_months = as.numeric(spei(water_bal, 24)$fitted)) %>%
  filter(year != 1944)

# Export monthly SPEI data (if required for Nils)
# write_csv(spei, "data/climate/spei_monthly.csv")

# Merge with final dataset and write out
chok_annual_climate_final <- chok_annual_climate_final %>% 
  full_join(select(spei, year, month, contains("spei"))%>% filter(month == 8) %>%
              select(-month))
write_csv(chok_annual_climate_final,
          "data/climate/chok_annual_climate_final.csv")
