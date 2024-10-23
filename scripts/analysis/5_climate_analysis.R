# Test climate predictors of water area
# Jakob J. Assmann 29 November 2022

# Dependencies
library(tidyverse)
library(ggplot2)
library(cowplot)
library(ggtext)

# Read climate and water data
water_area <- read_csv("tables/annual_water_prop.csv") %>%
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
      water_climate %>%
        split(.$site) %>%
        imap(function(y, z){
      data.frame(
        site = z,
        var = x,
        r = cor(pull(y, prop) , pull(y, !!x), method = "pearson"),
        p = cor.test(pull(y, prop) , pull(y, !!x), method = "pearson")$p.value)
        }) %>%
        bind_rows()
    }) %>% bind_rows() %>%
  arrange(site, desc(abs(r))) %>%
  mutate(r = round(r, 2), p = round(p, 3))
# site                 var     r     p
# 1   cbh       temp_jja_mean -0.71 0.074
# 2   cbh      precip_son_sum  0.56 0.192
# 3   cbh       temp_son_mean  0.46 0.296
# 4   cbh       temp_mam_mean  0.45 0.309
# 5   cbh      precip_jja_sum -0.39 0.382
# 6   cbh      spei_12_months  0.35 0.437
# 7   cbh      precip_dfj_sum  0.34 0.462
# 8   cbh       spei_9_months  0.33 0.466
# 9   cbh      spei_24_months  0.31 0.505
# 10  cbh precip_sept_aug_sum  0.28 0.548
# 11  cbh       spei_6_months  0.20 0.665
# 12  cbh       spei_3_months  0.16 0.731
# 13  cbh      precip_mam_sum  0.13 0.785
# 14  cbh   precip_annual_sum -0.12 0.800
# 15  cbh    temp_annual_mean  0.07 0.883
# 16  cbh       temp_djf_mean  0.06 0.898
# 
# 33  tlb       temp_mam_mean  0.56 0.191
# 34  tlb       temp_jja_mean -0.55 0.199
# 35  tlb      precip_jja_sum -0.44 0.322
# 36  tlb   precip_annual_sum -0.42 0.350
# 37  tlb      precip_son_sum  0.32 0.491
# 38  tlb       temp_son_mean  0.30 0.507
# 39  tlb      precip_dfj_sum  0.29 0.522
# 40  tlb      spei_24_months  0.26 0.577
# 41  tlb      spei_12_months  0.20 0.663
# 42  tlb    temp_annual_mean  0.19 0.675
# 43  tlb       spei_9_months  0.15 0.754
# 44  tlb       temp_djf_mean -0.05 0.913
# 45  tlb      precip_mam_sum  0.02 0.969
# 46  tlb precip_sept_aug_sum  0.02 0.973
# 47  tlb       spei_6_months  0.00 0.993
# 48  tlb       spei_3_months  0.00 0.995

# Save as table 
write_csv(correlations, "tables/climate_water_cor.csv")

# Get min, max and 95% quantiles for summer temp and autum precipitation
climate_extremes <- climate_data %>% 
  select(temp_jja_mean, precip_son_sum) %>%
  summarise(min_temp = min(temp_jja_mean),
            max_temp = max(temp_jja_mean),
            min_precip = min(precip_son_sum),
            max_precip = max(precip_son_sum),
            q0.05_temp = quantile(temp_jja_mean, 0.05),
            q0.95_temp = quantile(temp_jja_mean, 0.95),
            q0.05_precip = quantile(precip_son_sum, 0.05),
            q0.95_precip = quantile(precip_son_sum, 0.95))
# Do our time-series values exceed the extremes?
filter(climate_data, year >= 2014, year <= 2021) %>%
  pull(temp_jja_mean) %>%
  {. < climate_extremes$q0.05_temp}
filter(climate_data, year >= 2014, year <= 2021) %>%
  pull(temp_jja_mean) %>%
  {. > climate_extremes$q0.95_temp}
filter(climate_data, year >= 2014, year <= 2021) %>%
  pull(precip_son_sum) %>%
  {. < climate_extremes$q0.05_precip}
filter(climate_data, year >= 2014, year <= 2021) %>%
  pull(precip_son_sum) %>%
  {. > climate_extremes$q0.95_precip}

# Plot climate data for centrury
climate_cent <- ggplot(climate_data) +
    geom_col(aes(x = year, y = precip_son_sum / 10),
           colour = NA,
           fill = "#0028A5AA") +
           geom_line(aes(x = year, y = temp_jja_mean, group = 1),
            colour = "#FF0000",
            linewidth = 1) +
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
            linewidth = 1) +
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
           fill = "#0028A5AA",
           width = 0.6) +
           geom_line(aes(x = year, y = temp_jja_mean, group = 1),
            colour = "#FF0000",
            linewidth = 2) +
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
        axis.ticks.y.right = element_line(colour = "black"), #grey50
        axis.line.y.left = element_line(colour = "black"),
        axis.line.y.right = element_line(colour = "black"),
        axis.text.y.left = element_text(colour = "black"),
        axis.text.y.right = element_text(colour = "black"),
        axis.title.y.left = element_text(angle = 90, colour = "black"),
        axis.title.y.right = element_text(angle = 90, colour = "black"),
        axis.title.x = element_blank())
save_plot("figures/climate_study.png",
          climate_study,
          base_asp = 3/2,
          bg = "white")
