
# clean data
rm(list = ls())

# read in package
library(tidyverse)

# read in data
state_water <- readRDS("data/gis_data/CA_state_waters_polyline.Rds")
islands <- sf::st_read("data/gis_data/california_islands/california_islands.shp")
bathy <- raster::raster("data/gis_data/bd200m_v2i")
# bathy_spatial <- read.csv("data/gis_data/bathy_extracted.csv")

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

wgs84 <- "+proj=longlat +datum=WGS84"



## Directories
base.dir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
data.dir <- file.path(base.dir, "gis_data/raw")
out.dir <- file.path(base.dir, "gis_data/processed")

## Get depth data
## Source: https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/
depth_raw_fa <- stars::read_stars(file.path(data.dir, "200mEEZ_BathyGrids", "bd200fa_v2i", "w001001.adf")) # fathoms

# Convert from STARS to terra
depth_fa1 <- terra::rast(depth_raw_fa)

# Generate contour lines at the target depth
contour <- terra::as.contour(depth_fa1, levels = -70)

# Convert contour to sf object
contour_sf <- sf::st_as_sf(contour) %>%
  # Break
  sf::st_cast("LINESTRING") %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Remove mainland
  filter(id!=1) %>% 
  # Reproject
  sf::st_transform(crs = sf::st_crs("+proj=longlat +datum=WGS84")) %>% 
  # Break
  sf::st_cast('POLYGON')

# Export
sf::st_write(contour_sf, dsn="data/gis_data/70fathom_contour.shp")

ggplot() +
  geom_sf(data=contour_sf, mapping=aes(fill=as.character(id)))


##############################################################################

# create 1 nautical mile buffer around islands
islands_poly <- islands %>%
  sf::st_cast("POLYGON")

buffer <- islands_poly %>% 
  sf::st_buffer(dist=1852) %>%
  sf::st_union() %>%
  sf::st_transform(wgs84)

ggplot() +
  geom_sf(data=islands_poly %>% sf::st_transform(wgs84), fill="black") +
  geom_sf(data=contour_sf, fill=NA) +
  geom_sf(data=buffer, color="red", fill=NA) +
  coord_sf(ylim=c(32.7, 34.2), xlim=c(-120.5, -118))
