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
