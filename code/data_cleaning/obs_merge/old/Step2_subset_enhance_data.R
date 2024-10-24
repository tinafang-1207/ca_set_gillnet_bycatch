

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Export directory
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1983_2017_gillnet_observer_data.Rds"))

# Projections
wgs84 <- "+proj=longlat +datum=WGS84"
utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# Land
usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf", scale="large") 
usa_sp <- usa %>% 
  sf::st_transform(utm11) %>% 
  sf::as_Spatial()

# Calculate distance to shore
################################################################################

# Lat/long key
latlong_key <- data_orig %>% 
  select(lat_dd, long_dd) %>% 
  unique() %>% 
  na.omit()

# Convert to sp
latlong_key_sp <- latlong_key %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=wgs84) %>% 
  sf::st_transform(utm11) %>% 
  sf::as_Spatial()

# Calculate distance to shore
dist_mat <- rgeos::gDistance(latlong_key_sp, usa_sp, byid = T)

# Format distance matrix
dist_df <- dist_mat %>%
  # Convert to data frame
  as.data.frame() %>%
  # Add land polygon id
  mutate(land_id=1:nrow(.)) %>% 
  select(land_id, everything()) %>% 
  # Gather
  gather(key="latlong_id", value="dist_m", 2:ncol(.)) %>% 
  mutate(latlong_id=as.numeric(latlong_id)) %>% 
  # Find minimum distance
  arrange(latlong_id, dist_m) %>% 
  group_by(latlong_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Remove land id
  select(-land_id)

# Add distance to latlong key
latlong_key1 <- latlong_key %>% 
  mutate(shore_km=dist_df$dist_m/1000)

# Plot check
ggplot(latlong_key1, aes(x=long_dd, y=lat_dd, color=pmin(shore_km,10))) +
  geom_point() +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()

# Add shore distance
data <- data_orig %>% 
  # Add shore distance
  left_join(latlong_key1, by=c("long_dd", "lat_dd")) %>% 
  # Arrange
  select(dataset:long_dd, shore_km, everything())


# Subset to data of interest
################################################################################

# Net types
table(data$net_type)

#  Set nets
data_set <- data %>% 
  # Filter to set gillnets
  filter(net_type %in% c("set", "trammel 1 panel", "trammel 2 panel", "trammel 3 panel"))

# Inspect
table(data_set$target_spp)
table(data_set$mesh_size_in)

#  3.5" set nets
data_set_lg <- data_set %>% 
  # Filter to >3.5" mesh size
  filter(mesh_size_in >= 3.5)

# Inspect
table(data_set_lg$target_spp)

# Target species: California halibut, White seabass, Pacific angel shark
data_set_lg <- data_set %>% 
  # Filter to target species
  filter(target_spp %in% c("California halibut", "White seabass", "Pacific angel shark"))


# Export
################################################################################

# Export
saveRDS(data_set_lg, file=file.path(outdir, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))

