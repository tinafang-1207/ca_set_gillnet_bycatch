
#### clean working environment####
rm(list = ls())

### load in packages
library(tidyverse)

### read in data ###
####################################################
# original logbook data
data_orig <- readRDS("data/confidential/processed/CDFW_1980_2022_gillnet_logbook_new.Rds")

# block data
blocks <- wcfish::blocks

# sst data
sst_all <- readRDS("data/gis_data/data_sst_1980_2022.Rds")

# spatial data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa_ca <- usa %>% filter(name == "California")



### format logbook data
################################################

data_format <- data_orig %>%
  #filter net type to set
  filter(net_type == "Set") %>%
  #create set id
  mutate(set_id = paste(vessel_name, "-", vessel_id_use, "-", permit_num, "-", date, "-", block_id, "-", haul_depth_fa, "-", net_length_ft, "-", mesh_size_in, "-", soak_hour)) %>%
  #filter out duplicated identifier (only keep one set per trip)
  filter(!duplicated(set_id)) %>%
  #only select modeling variables
  select(set_id, date, block_id, haul_depth_fa, mesh_size_in, soak_hour)

### add block lat and long to format data
###############################################

block_xy <- blocks %>%
  select(block_id, block_long_dd, block_lat_dd) %>%
  mutate(block_id = as.character(block_id)) %>%
  sf::st_drop_geometry() 

data_format_block <- left_join(data_format, block_xy, by = "block_id")

### add sst
###############################################


# turn data into raster
sst_ras <- sst_all %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")

# Extract block coordinate from logbook data
data_block_xy <- data_format_block %>%
  select(block_lat_dd, block_long_dd) %>%
  unique() %>%
  filter(!is.na(block_lat_dd))

# Turn coordinates into spatial point (sp)
data_block_xy_sp <- data_block_xy %>% 
  # Convert to sf
  sf::st_as_sf(., coords=c("block_long_dd", "block_lat_dd"), crs="+proj=longlat +datum=WGS84") %>% 
  # Convert to spatial points
  sf::as_Spatial()

# Extract temperature data
data_sst_extract <- raster::extract(x=sst_ras, y=data_block_xy_sp)

# Format temperature data
data_sst_extract_df <- data_sst_extract %>%
  as.data.frame() %>%
  mutate(block_lat_dd = data_block_xy$block_lat_dd, block_long_dd = data_block_xy$block_long_dd) %>%
  select(block_lat_dd, block_long_dd, everything()) %>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date))

# left join temprature data with logbook data
data_format_sst <- left_join(data_format_block, data_sst_extract_df, by = c("block_long_dd", "block_lat_dd", "date"))

### add julian day
#############################################

data_format_jd <- data_format_sst %>%
  mutate(julian_day = lubridate::yday(date))

### add dist_km (from block centroid)
############################################

# set up projection to utm11
utm11 <- "+proj=utm +zone=11 +datum=NAD83"


# Convert fishing location data to sp
data_block_xy_dist_sp <- data_block_xy %>%
  #Convert to sf
  sf::st_as_sf(coords = c("block_long_dd", "block_lat_dd"), crs = sf::st_crs(usa_ca), remove = F) %>%
  #Convert to SP
  sf::as_Spatial()

data_block_xy_dist_sp_utm <- data_block_xy_dist_sp %>%
  sp::spTransform(utm11)

# Convert usa_ca to sp
usa_ca_sp <- usa_ca %>%
  sf::st_transform(crs = utm11) %>%
  sf::as_Spatial()

# Calculate distance of each point to ca shapefile
dist_mat <- rgeos::gDistance(data_block_xy_dist_sp_utm, usa_ca_sp, byid = T)

#format distance matrix
dist_df <- dist_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "dist_m", 1:ncol(.)) %>%
  mutate(block_long_dd = data_block_xy$block_long_dd, block_lat_dd = data_block_xy$block_lat_dd) %>%
  select(-row_id)


# Add distance matrix back to dataframe
data_format_dist <- left_join(data_format_jd, dist_df, by = c("block_long_dd", "block_lat_dd")) %>%
  mutate(dist_km = dist_m/1000)


data_format_dist_block <- data_format_dist %>%
  filter(is.na(block_lat_dd))


# Export table to model data
saveRDS(data_format_dist, file = file.path("data/confidential/processed/logbook_new_pre_model.Rds"))








