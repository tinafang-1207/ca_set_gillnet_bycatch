
# Clean working environment
rm(list = ls())

#load in packages
library(tidyverse)

# Read in data
#################################################################

#Bathymetry data
bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

#observer data
obs_data <- read.csv("data/confidential/processed/fig2_total_merge_no_bathy.csv")

# Format data
#################################################################

#Set crs for bathymetry data
bathy_200fa_wgs84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")


# Extract data
#################################################################
obs_sites_xy <- obs_data %>%
  select(set_id, haul_lat_dd, haul_long_dd) %>%
  filter(!is.na(haul_lat_dd)) %>%
  unique()

obs_sites_xy_sp <- obs_sites_xy %>% 
  # convert to sf
  sf::st_as_sf(coords=c("haul_long_dd", "haul_lat_dd"), crs=raster::crs(bathy_200fa_wgs84)) %>%
  # convert to sp
  sf::as_Spatial()

# Extract bathy data from raster
bathy_sites_orig <- raster::extract(x=bathy_200fa_wgs84, y=obs_sites_xy_sp)

# Format extracted data
bathy_sites <- bathy_sites_orig %>%
  as.data.frame() %>%
  mutate(set_id = obs_sites_xy$set_id, haul_long_dd = obs_sites_xy$haul_long_dd, haul_lat_dd = obs_sites_xy$haul_lat_dd) %>%
  rename(bathy_fa = ".") %>%
  select(set_id, haul_long_dd, haul_lat_dd, bathy_fa)

# Join and export data
#################################################################
  
# Join data
obs_data_bathy <- left_join(obs_data, bathy_sites, by = c("set_id", "haul_long_dd", "haul_lat_dd"))

# Export data
write.table(obs_data_bathy, file = "data/confidential/processed/fig2_total_merge_final.csv", row.names = F, sep = ",")


