### Density plot ###

### Clean working space ###
rm(list = ls())

### Load in package ###

library(tidyverse)
library(sf)

### Read in data ###
total_merge <- read.csv("data/confidential/processed/fig3_merge_obs_and_trip_data.csv")

### distance from shore ###

ggplot(total_merge%>% filter(dist_km <= 20), aes(dist_km, fill = data_source)) +
  geom_density(alpha = 0.4)

### haul depth ###

ggplot(total_merge %>% filter(haul_depth_fa <= 50), aes(haul_depth_fa, fill = data_source)) +
  geom_density(alpha = 0.4)

### latitude location ###
ggplot(total_merge, aes(haul_lat_dd, fill = data_source)) +
  geom_density(alpha = 0.4)

### Julian day ###
ggplot(total_merge, aes(julian_day, fill = data_source)) +
  geom_density(alpha = 0.4)

### mesh size ###
ggplot(total_merge %>% filter(net_mesh_size_in <= 10), aes(net_mesh_size_in, fill = data_source)) +
  geom_density(alpha = 0.4)

### soak hour ###
ggplot(total_merge %>% filter(soak_hr <= 100), aes(soak_hr, fill = data_source)) +
  geom_density(alpha = 0.4)




