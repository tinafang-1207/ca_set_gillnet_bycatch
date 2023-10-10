
#### This script correct variables from the cleaned logbook data ####
# variables corrected:

# mesh size 
# soak hours
# haul depth

### clean working environment
rm(list = ls())

### read in package
library(tidyverse)

### read in data

# cleaned logbook data
data_orig <- readRDS("data/confidential/processed/processed_CDFW_logbook/CDFW_1980_2022_gillnet_logbook_new.Rds")

# block depth
block_max_median <- readRDS("data/confidential/processed/processed_gis/block_max_median_depth.Rds")


#### Assign mesh size

# function to find mode in mesh size
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# create mesh size key (based on mode of vessel id & target species)
data_mesh_size_key <- data_orig %>%
  #filter net type to set
  filter(net_type == "Set") %>%
  mutate(mesh_size_in = as.numeric(mesh_size_in)) %>%
  mutate(mesh_size_in = ifelse(mesh_size_in == 0|mesh_size_in >20, NA, mesh_size_in)) %>%
  group_by(vessel_id_use, target_spp) %>%
  summarize(mesh_size_mode = find_mode(mesh_size_in))

# create mesh size key (based only on target species)
data_mesh_size_target_key <- data_orig %>%
  filter(net_type == "Set") %>%
  mutate(mesh_size_in = as.numeric(mesh_size_in)) %>%
  mutate(mesh_size_in = ifelse(mesh_size_in == 0|mesh_size_in >20, NA, mesh_size_in)) %>%
  group_by(target_spp) %>%
  summarize(mesh_size_mode_target_spp = find_mode(mesh_size_in))

# Assign mesh size to logbook data
data_mesh_size_format <- data_orig %>%
  filter(net_type == "Set") %>%
  mutate(mesh_size_in = as.numeric(mesh_size_in)) %>%
  mutate(mesh_size_in = ifelse(mesh_size_in == 0|mesh_size_in >20, NA, mesh_size_in)) %>%
  # assign mesh size based on vessel level mesh size mode for each target species
  left_join(data_mesh_size_key, by = c("vessel_id_use", "target_spp")) %>%
  mutate(mesh_size_in = ifelse(is.na(mesh_size_in), mesh_size_mode, mesh_size_in)) %>%
  # assign mesh size only based on target species
  left_join(data_mesh_size_target_key, by = "target_spp") %>%
  mutate(mesh_size_in = ifelse(is.na(mesh_size_in), mesh_size_mode_target_spp, mesh_size_in)) %>%
  # remove last two columns
  select(-mesh_size_mode, -mesh_size_mode_target_spp)

### Assign soak hours
data_soak_hour_format <- data_mesh_size_format %>%
  # Format unusually large soak hours
  mutate(soak_hour = ifelse(soak_hour >= 96, 96, soak_hour)) %>%
  # Format unusually small soak hours
  mutate(soak_hour = ifelse(soak_hour == 0, median(soak_hour), soak_hour))

# Assign haul depth

# Join block lat & long to logbook data
block <- wcfish::blocks

block_format <- block %>%
  filter(block_state == "California") %>%
  select(block_id, block_long_dd, block_lat_dd) %>%
  sf::st_drop_geometry() 

data_block_join <- data_soak_hour_format %>%
  mutate(block_id = as.integer(block_id)) %>%
  left_join(block_format, by = "block_id")


data_haul_depth_format <- data_block_join %>%
  left_join(block_max_median, by = c("block_long_dd", "block_lat_dd") ) %>%
  mutate(block_depth_max = abs(block_depth_max), block_depth_median = abs(block_depth_median)) %>%
  # format unusually deep haul depth
  mutate(haul_depth_fa = ifelse(haul_depth_fa > block_depth_max, block_depth_median, haul_depth_fa)) %>%
  # format unusually small haul depth (0)
  mutate(haul_depth_fa = ifelse(haul_depth_fa == 0, block_depth_median, haul_depth_fa))

# Export data
# The exported data only contains set gillnet logbook data

saveRDS(data_haul_depth_format, file = file.path("data/confidential/processed/processed_CDFW_logbook/CDFW_1980_2022_gillnet_logbook_new_final.Rds"))

