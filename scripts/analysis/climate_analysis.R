# Test climate predictors of water area
# Jakob J. Assmann 29 November 2022

# Dependencies
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggtext)

# Read climate and water data
water_area <- read_csv("tables/annual_water_prpop.csv") %>%
  group_by(site, calendar_year) %>% 
  summarise(prop = mean(prop)) %>%
  mutate(year = calendar_year) %>%
  select(-calendar_year)
climate_data <- read_csv("data/climate_data/chok_annual_climate_final.csv")

# Inner join of the two datasets
water_climate <- inner_join(water_area,
                            climate_data)

# Visualise histograms
water_climate %>% relocate(site, year) %>%
  pivot_longer(cols = 3:19) %>%
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~name, scales = "free") +
  theme_cowplot()

# Test correlations
correlations <- map(names(water_climate)[c(-1, -2, -3)],
    function(x){
      data.frame(
        var = x,
        r = cor(pull(water_climate, prop) , pull(water_climate, !!x), method = "pearson"),
        p = cor.test(pull(water_climate, prop) , pull(water_climate, !!x), method = "pearson")$p.value)
    }) %>% bind_rows() %>%
  arrange(desc(abs(r))) %>%
  mutate(r = round(r, 2), p = round(p, 3))
correlations
#                    var     r     p
# 1        temp_jja_mean -0.42 0.118
# 2       precip_son_sum  0.34 0.216
# 3        temp_son_mean  0.26 0.342
# 4       precip_jja_sum -0.26 0.351
# 5        temp_mam_mean  0.25 0.370
# 6       spei_12_months  0.22 0.428
# 7       precip_dfj_sum  0.22 0.438
# 8        spei_9_months  0.20 0.474
# 9       spei_24_months  0.20 0.479
# 10 precip_sept_aug_sum  0.17 0.555
# 11       spei_6_months  0.11 0.691
# 12   precip_annual_sum -0.09 0.748
# 13      precip_mam_sum  0.08 0.766
# 14       spei_3_months  0.08 0.773
# 15       temp_djf_mean  0.05 0.854
# 16    temp_annual_mean  0.03 0.902

# Save as table 
write_csv(correlations, "tables/climate_water_cor.csv")

# Get min and max values for summer temp and autum precipitation
climate_data %>% 
  select(temp_jja_mean, precip_son_sum) %>%
  summarise(min_temp = min(temp_jja_mean),
            max_temp = max(temp_jja_mean),
            min_precip = min(precip_son_sum),
            max_precip = max(precip_son_sum))

# Plot climate data for centrury
climate_cent <- ggplot(climate_data) +
    geom_col(aes(x = year, y = precip_son_sum / 10),
           colour = NA,
           fill = "#0028A5AA") +
           geom_line(aes(x = year, y = temp_jja_mean, group = 1),
            colour = "#FF0000",
            size = 1) +
  scale_y_continuous(name = "Temp. summer (°C)",
                    limits = c(0, 15),
                    sec.axis = sec_axis(~.*10, name = "Precip. autumn Y-1 (mm)")) +
  scale_x_continuous(limits = c(1940, 2025)) +
  labs(x = "") +
  annotate("segment", x = 2017, xend = 2017, y = 10, yend = 13) +
  annotate("text", x = 2017, y = 13, label = "2017", angle = 90, hjust =  -0.1,
    size = 5) +
  theme_cowplot(18) + 
  theme(axis.title.y.right = element_text(angle = 90))
save_plot("figures/climate_cent.png",
          climate_cent,
          bg = "white")

# Plot climate data for decade
climate_dec <- ggplot(climate_data %>%
         filter(year >= 2011), aes(x = year, y = temp_jja_mean, group = 1)) +
    geom_col(aes(x = year, y = precip_son_sum / 10),
           colour = NA,
           fill = "#0028A5AA") +
           geom_line(aes(x = year, y = temp_jja_mean, group = 1),
            colour = "#FF0000",
            size = 1) +
  scale_y_continuous(name = "Temp. summer (°C)",
                    limits = c(0, 15),
                    sec.axis = sec_axis(~.*10, name = "Precip. autumn Y-1 (mm)")) +
  scale_x_continuous(breaks = 2011:2021,
    labels = c(2011, "", 2013, "", 2015, "", 2017, "", 2019, "", 2021)) +
  labs(x = "") +
  annotate("segment", x = 2017, xend = 2017, y = 10, yend = 12.5) +
  annotate("text", x = 2017, y = 12.5, label = "2017", angle = 90, hjust =  -0.1,
    size = 5) +
  theme_cowplot(18) + 
  theme(axis.title.y.right = element_text(angle = 90),
        #axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5)
        )
save_plot("figures/climate_dec.png",
          climate_dec,
          bg = "white")

# Plot climate data for study time-frame
climate_study <- ggplot(climate_data %>%
         filter(year >= 2014), aes(x = year, y = temp_jja_mean, group = 1)) +
    geom_col(aes(x = year, y = precip_son_sum / 10),
           colour = NA,
           fill = "grey50",
           width = 0.6) +
           geom_line(aes(x = year, y = temp_jja_mean, group = 1),
            colour = "black",
            size = 2) +
  scale_y_continuous(name = "Summer Temperature (°C)",
                    limits = c(0, 15),
                    sec.axis = sec_axis(~.*10, name = "Autumn Preciptation (mm)")) +
  scale_x_continuous(breaks = 2014:2021,) +
  labs(x = "") +
  # annotate("segment", x = 2017, xend = 2017, y = 10, yend = 12.5) +
  # annotate("text", x = 2017, y = 12.5, label = "2017", angle = 90, hjust =  -0.1,
  #  size = 5) +
  theme_cowplot(16) + 
  theme(axis.ticks.y.left = element_line(colour = "black"),
        axis.ticks.y.right = element_line(colour = "grey50"),
        axis.line.y.left = element_line(colour = "black"),
        axis.line.y.right = element_line(colour = "grey50"),
        axis.text.y.left = element_text(colour = "black"),
        axis.text.y.right = element_text(colour = "grey50"),
        axis.title.y.left = element_text(angle = 90, colour = "black"),
        axis.title.y.right = element_text(angle = 90, colour = "grey50"),
        axis.title.x = element_blank())
save_plot("figures/climate_study.png",
          climate_study,
          base_asp = 3/2,
          bg = "white")
