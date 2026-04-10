###############################################################################

# Title: Counting Avulsion Events
# Purpose: Sending Qinghua Ding number of avulsion events globally, SA, Himalaya, PNG, and by avulsion type 
# Date: 4/8/26

###############################################################################

## Loading Libraries 
library(dplyr)
library(tidyverse)
library(readr)
library(here)
library(ggplot2)
library(stringr)
library(purrr)
library(viridis)
library(janitor)

#Loading Base Sites 
base_site <- 
  read_csv(here("data","vg_dis_z.csv")) |>
  inner_join(
    read_csv(here("data","base_sites.csv")) |>
      select(site_name, site_lat, site_lon),   # no av_type
    by = "site_name")

#Finding Count for the Amount of Avulsions for Each Year from 1984:2018
#Input zeros when no avulsion present
avulsion_count <- base_site |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0)) # Fill missing years with count = 0

#Finding Count for the Amount of Avulsions for Each Year from 1984:2018 by Avulsion Type

#Deltas
delta_count <- base_site |>
  filter(av_type == "delta") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0)) 

#Fans
fan_count <- base_site |>
  filter(av_type == "fan") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#Intra-Lobe
intra_lobe_count <- base_site |>
  filter(av_type == "intra-lobe") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#Retrogradational
retro <- base_site |>
  filter(av_type == "retrogradational") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#Find Count for Amount of Avulsions for Each Year from 1984:2018 by Region
#Himalaya
himalaya_count <- base_site |>
  filter(site_area == "Himalaya") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#South America
sa_count <- base_site |>
  filter(site_area == "South America") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#Pacific Islands 
pi_count <- base_site |>
  filter(site_area == "Pacific Islands") |>
  group_by(avulsion_year) |>
  summarise(count = n()) |>
  complete(avulsion_year= 1984:2018, 
           fill = list(count = 0))

#Binding
# Add a grouping label to each, then bind together
all_counts <- bind_rows(
  avulsion_count   |> mutate(group = "global"),
  delta_count      |> mutate(group = "delta"),
  fan_count        |> mutate(group = "fan"),
  intra_lobe_count |> mutate(group = "intra_lobe"),
  retro            |> mutate(group = "retrogradational"),
  himalaya_count   |> mutate(group = "himalaya"),
  sa_count         |> mutate(group = "south_america"),
  pi_count         |> mutate(group = "pacific_islands")
)

write_csv(all_counts, here("outputs", "avulsion_counts.csv"))