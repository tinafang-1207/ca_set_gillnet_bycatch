
#### clean working environment ####
rm(list = ls())

#### load in package ####
library(tidyverse)


#### read in data ####

# Read in logbook data
data_orig <- readRDS("data/confidential/processed/logbook_new_pre_model.Rds")

data_orig_lb <- readRDS("data/confidential/processed/CDFW_1980_2022_gillnet_logbook_new.Rds")

# Read in depth data
bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")


### Assign mesh size data ###

# add target species to data_orig
data_orig_lb_target <- data_orig_lb %>%
  #filter net type to set
  filter(net_type == "Set") %>%
  #create set id
  mutate(set_id = paste(vessel_name, "-", vessel_id_use, "-", permit_num, "-", date, "-", block_id, "-", haul_depth_fa, "-", net_length_ft, "-", mesh_size_in, "-", soak_hour)) %>%
  #filter out duplicated identifier (only keep one set per trip)
  filter(!duplicated(set_id)) %>%
  # only select target_spp and set_id
  select(set_id, vessel_id_use, vessel_id_use_type, target_spp)

# function to find mode in mesh size
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

# create mesh size key based on vessel_id and target species
data_mesh_size_key <- data_orig %>%
  # join target_spp
  left_join(data_orig_lb_target, by = "set_id") %>%
  select(-date, -dist_m) %>%
  mutate(mesh_size_in = as.numeric(mesh_size_in)) %>%
  mutate(mesh_size_in = ifelse(mesh_size_in == 0|mesh_size_in >20, NA, mesh_size_in)) %>%
  group_by(vessel_id_use, target_spp) %>%
  summarize(mesh_size_mode = find_mode(mesh_size_in))



# Assign mesh size
data_mesh_size_format <- data_orig %>%
  # join target_spp
  left_join(data_orig_lb_target, by = "set_id") %>%
  select(-date, -dist_m) %>%
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


### Assign soak hour data ###
data_soak_hour_format <- data_mesh_size_format %>%
  # Format unusually large soak hours
  mutate(soak_hour = ifelse(soak_hour >= 96, 96, soak_hour)) %>%
  # Format unusually small soak hours
  mutate(soak_hour = ifelse(soak_hour == 0, median(soak_hour), soak_hour))

### Assign haul depth ###

# if the haul depth reported exceed maximum depth within that block, then assign median depth

bathy_200fa_wgs84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

block <- wcfish::blocks

block_format <- block %>%
  filter(block_state == "California") %>%
  select(block_id, block_long_dd, block_lat_dd)


block_format_sp <- block_format %>% 
  # convert to sf
  sf::st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs=raster::crs(bathy_200fa_wgs84)) %>%
  # convert to sp
  sf::as_Spatial()

block_depth_max <- raster::extract(x = bathy_200fa_wgs84, y = block_format_sp, fun = min)

block_depth_max_format <- block_depth_max %>%
  as.data.frame() %>%
  mutate(block_long_dd = block_format$block_long_dd, block_lat_dd = block_format$block_lat_dd) %>%
  rename(block_depth_max = "V1") %>%
  select(block_long_dd, block_lat_dd, block_depth_max)

block_depth_median <- raster::extract(x = bathy_200fa_wgs84, y = block_format_sp, fun = median)

block_depth_median_format <- block_depth_median %>%
  as.data.frame() %>%
  mutate(block_long_dd = block_format$block_long_dd, block_lat_dd = block_format$block_lat_dd) %>%
  rename(block_depth_median = "V1") %>%
  select(block_long_dd, block_lat_dd, block_depth_median)

# join depth data back

block_depth_join <- left_join(x = block_depth_max_format, y = block_depth_median_format, by = c("block_long_dd", "block_lat_dd"))

# save block_depth

saveRDS(block_depth_join, file = file.path("data/confidential/processed/block_max_median_depth.Rds"))

############### Assign haul depth ################

# read block_depth
block_depth_join <- readRDS("data/confidential/processed/block_max_median_depth.Rds")

# Format haul depth
data_haul_depth_format <- data_soak_hour_format %>%
  left_join(block_depth_join, by = c("block_long_dd", "block_lat_dd") ) %>%
  mutate(block_depth_max = abs(block_depth_max), block_depth_median = abs(block_depth_median)) %>%
  # format unusually deep haul depth
  mutate(haul_depth_fa = ifelse(haul_depth_fa > block_depth_max, block_depth_median, haul_depth_fa)) %>%
  # format unusually small haul depth (0)
  mutate(haul_depth_fa = ifelse(haul_depth_fa == 0, block_depth_median, haul_depth_fa))

# finalize cleaned data 
data_final_format <- data_haul_depth_format %>%
  select(set_id, haul_depth_fa, mesh_size_in, soak_hour, block_lat_dd, sst, julian_day, dist_km)
  
### check with histogram figure ###
data_final_check <- data_final_format %>%
  gather(key = "variable", value = "value", 2:ncol(.)) %>%
  mutate(value = as.numeric(value))

  
ggplot(data = data_final_check, aes(x = value)) +
  facet_wrap(.~variable, ncol = 4, scales = "free") +
  geom_histogram()

### save the final modeling data ###
saveRDS(data_final_format, file = file.path("data/confidential/processed/logbook_new_pre_model_final.Rds.Rds"))
  

  
