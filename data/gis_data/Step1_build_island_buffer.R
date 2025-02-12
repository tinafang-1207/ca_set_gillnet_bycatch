

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/gis_data"

# Read data
# https://earthworks.stanford.edu/catalog/stanford-nd958xt7750
data_orig <- sf::st_read(file.path(datadir, "california_islands/california_islands.shp"))

# Blocks
blocks <- wcfish::blocks

# Projection
wgs84 <- "+proj=longlat +datum=WGS84"

# Buffer
################################################################################

# Polygon
data_poly <- data_orig %>% 
  sf::st_cast("POLYGON")

# Buffer
buffer <- data_poly %>% 
  sf::st_buffer(dist=10*1000) %>% 
  sf::st_union() %>% 
  sf::st_transform(wgs84)

# Plot data
g <- ggplot() +
  geom_sf(data=blocks %>% filter(block_state=="California"), color="grey40", size=0.1, fill=NA) + 
  geom_sf(data=buffer, color="black", size=0.4) + 
  geom_sf(data=data_orig, color="black", fill="black") +
  coord_sf(xlim=c(-122, -116), ylim=c(32, 34.5)) +
  theme_bw()
g

# Export
saveRDS(buffer, file=file.path(datadir, "island_buffer_10km.Rds"))




