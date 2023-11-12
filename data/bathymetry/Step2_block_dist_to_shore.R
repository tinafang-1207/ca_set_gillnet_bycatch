


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/bathymetry/processed"

# Blocks
blocks <- wcfish::blocks

# Get land
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf", scale="large")

# Projections
utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# Read block depths
block_depths <- readRDS(file=file.path(datadir, "CA_block_depths.Rds"))


# Build data
################################################################################

# Format land
usa_utm <- usa %>% 
  # Reproject
  sf::st_transform(crs=utm11) %>% 
  # Convert to sp
  sf::as_Spatial()

# Format block centroids
block_centroids <- blocks %>% 
  # Simplify to CA/OR
  filter(block_state!="Washington") %>% 
  # Create point sf
  sf::st_drop_geometry() %>% 
  select(block_id, block_long_dd, block_lat_dd) %>% 
  sf::st_as_sf(coords=c("block_long_dd", "block_lat_dd"), crs=sf::st_crs(blocks)) %>% 
  # Reproject
  sf::st_transform(crs=utm11) %>% 
  # Convert to sp
  sf::as_Spatial()

# Calculate distance of each point to land shapefile
dist_mat <- rgeos::gDistance(block_centroids, usa_utm, byid = T)

# Format distance matrix
dist_df <- dist_mat %>%
  # Convert to data frame
  as.data.frame() %>%
  # Add block id
  setNames(block_centroids$block_id) %>% 
  # Add land polygon id
  mutate(land_id=1:nrow(.)) %>% 
  select(land_id, everything()) %>% 
  # Gather
  gather(key="block_id", value="dist_m", 2:ncol(.)) %>% 
  mutate(block_id=as.numeric(block_id)) %>% 
  # Find minimum distance
  arrange(block_id, dist_m) %>% 
  group_by(block_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  # Remove land id
  select(-land_id)

# Spatialize
blocks1 <- blocks %>% 
  left_join(dist_df)


# Plot data
################################################################################

ggplot() +
  # Plot blocks
  geom_sf(data=blocks1 %>% filter(block_type=="Inshore" & block_state!="Washington"), 
          mapping=aes(fill=dist_m/1000)) +
  # Plot land
  geom_sf(data=usa, fill="grey70", color="white", linewidth=0.2) +
  # Legend
  scale_fill_gradientn(name="Shore distance (km)", colors=RColorBrewer::brewer.pal(9, "Spectral")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  # coord_sf(xlim=c(-125, -117), ylim=c(32, 42)) +
  coord_sf(xlim=c(-120, -117), ylim=c(32, 35)) +
  # Theme
  theme_bw()


# Export data
################################################################################

# Export key
saveRDS(dist_df, file=file.path(datadir, "block_distance_to_shore.Rds"))



# Overall key
################################################################################

# Build key
block_key <- block_depths %>% 
  left_join(dist_df)

# Export
saveRDS(block_key , file=file.path(datadir, "block_key.Rds"))





