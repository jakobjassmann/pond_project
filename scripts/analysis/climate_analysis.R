# Test climate predictors of water area
# Jakob J. Assmann 29 November 2022

# Dependencies
library(tidyverse)
library(ggplot2)
library(cowplot)

# Read climate and water data
water_area <- read_csv("data/drone_time_series/cbh/area_water_by_year.csv")
climate_data <- read_csv("data/climate/chok_annual_climate_final.csv")

# Inner join of the two datasets
water_climate <- inner_join(water_area,
                            climate_data)

# Test correlations
map(names(water_climate)[c(-1, -2, -3)],
    function(x){
      data.frame(
        var = x,
        r = cor(pull(water_climate, area) , pull(water_climate, !!x)))
    }) %>% bind_rows() %>%
  arrange(desc(abs(r)))
#           var           r
# 1        temp_jja_mean -0.70022859
# 2       precip_son_sum  0.66046811
# 3       spei_12_months  0.50930201
# 4       spei_24_months  0.47259753
# 5        spei_9_months  0.45581355
# 6       precip_dfj_sum  0.44006658
# 7       precip_jja_sum -0.43380374
# 8        temp_mam_mean  0.42690779
# 9  precip_sept_aug_sum  0.40133457
# 10       temp_son_mean  0.32145412
# 11      precip_mam_sum  0.30067822
# 12       spei_6_months  0.29293599
# 13       spei_3_months  0.15786447
# 14       temp_djf_mean  0.10306373
# 15   precip_annual_sum -0.09125071
# 16    temp_annual_mean  0.04370060

# Plot climate data for centrury
temp_cent <- ggplot(climate_data, aes(x = year, y = temp_jja_mean, group = 1)) +
  geom_line(colour = "red") +
  scale_y_continuous(limits = c(2.5,12.5), breaks = seq(2.5,12.5,2.5)) +
  scale_x_continuous(limits = c(1940, 2021)) +
  labs(x = "", y = "Summer mean temp. (°C)") +
  theme_cowplot()
save_plot("figures/temp_cent.png",
          temp_cent,
          bg = "white")

# Plot climate data for decade
temp_dec <- ggplot(climate_data %>%
         filter(year >= 2011), aes(x = year, y = temp_jja_mean, group = 1)) +
  geom_line(colour = "red") +
  geom_point() +
  scale_x_continuous(breaks = 2011:2021) +
  scale_y_continuous(limits = c(7,12), breaks = 7:12) +
  labs(x = "", y = "Summer mean temp. (°C)") +
  annotate("text", x = 2017, y = 10.5, 
           label = "2017", col = "darkblue", hjust = 0.1) +
  annotate("segment", x = 2017, xend = 2017, 
           y = climate_data[climate_data$year == 2017,]$temp_jja_mean, 
           yend = 10.3, col = "darkblue", linetype = "dashed", size = 0.1) +
  theme_cowplot()
save_plot("figures/temp_dec.png",
          temp_dec,
          bg = "white")

# Plot climate data for centrury
precip_cent <- ggplot(climate_data, 
                    aes(x = year, y = precip_son_sum)) +
  geom_col(colour = "blue",
           fill = "blue") +
  scale_y_continuous(limits = c(0, 150)) +
  scale_x_continuous(limits = c(1940, 2021)) +
  labs(x = "", y = "Sep-Nov precip prev. year (mm)") +
  theme_cowplot()
save_plot("figures/autumn_precip_cent.png",
          precip_cent,
          bg = "white")

# Plot climate data for decade
precip_dec <- ggplot(climate_data %>%
                     filter(year >= 2011), aes(x = year, y = precip_son_sum)) +
  geom_col(
    fill = "blue",
    colour = "blue") +
  scale_x_continuous(breaks = 2011:2021) +
  scale_y_continuous(limits = c(0, 150)) +
  labs(x = "", y = "Sep-Nov precip prev. year (mm)") +
  annotate("text", x = 2017, y = 150, 
           label = "2017", col = "red", hjust = 0.1) +
  annotate("segment", x = 2017, xend = 2017, 
           y = climate_data[climate_data$year == 2017,]$precip_son_sum, 
           yend = 145, col = "red", linetype = "dashed", size = 0.1) +
  theme_cowplot()
save_plot("figures/autumn_precip_dec.png",
          precip_dec,
          bg = "white")
