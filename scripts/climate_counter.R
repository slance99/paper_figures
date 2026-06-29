#load libraries
library(dplyr)
library(tidyverse)
library(lubridate)
library(readr)
library(here)
library(ggplot2)
library(patchwork)

#loading climate csv
av_climate_og <- read.csv(here("data", "av_sites_climate.csv"))

#count the individual climate zones per site
av_climate_counts <- av_climate_og |>
  group_by(climate) |>
  summarise(count_climate = n()) |>
  ungroup() 
  
av_group_counts <- av_climate_og |>
  mutate(climate_group = substr(climate, 1, 1)) |>
  group_by(climate_group) |>
  summarise(count_group = n()) |>
  ungroup()

library(ggplot2)
library(patchwork)

# Define group base colors
group_palette <- c("A" = "#1d6fa4", "B" = "#cc5500", "C" = "#2e8b57", "D" = "#7b2d8b", "E" = "#8b6914")
group_light   <- c("A" = "#a8d4f5", "B" = "#f5c6a0", "C" = "#a8d4b8", "D" = "#d4a8e8", "E" = "#e8d4a8")

av_climate_colors <- av_climate_counts |>
  mutate(climate_group = substr(climate, 1, 1)) |>
  group_by(climate_group) |>
  mutate(color = colorRampPalette(c(group_light[climate_group[1]], group_palette[climate_group[1]]))(n())) |>
  ungroup()

p <- ggplot(av_climate_colors, aes(x = climate_group, y = count_climate, fill = climate)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = setNames(av_climate_colors$color, av_climate_colors$climate)) +
  labs(title = "Sites per Climate Group", x = "Climate Group", y = "Count") +
  theme_classic() +
  theme(legend.position = "right")

p
