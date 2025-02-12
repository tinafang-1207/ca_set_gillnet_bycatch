
# clean data
rm(list = ls())

# read in package
library(tidyverse)

# read in data
state_water <- readRDS("data/gis_data/CA_state_waters_polyline.Rds")
islands <- sf::st_read("data/gis_data/california_islands/california_islands.shp")
# bathy <- raster::raster("data/gis_data/bd200m_v2i")
bathy_spatial <- read.csv("data/gis_data/bathy_extracted.csv")

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

wgs84 <- "+proj=longlat +datum=WGS84"

##############################################################################

# create 1 nautical mile buffer around islands
islands_poly <- islands %>%
  sf::st_cast("POLYGON")

buffer <- islands_poly %>% 
  sf::st_buffer(dist=1852) %>%
  sf::st_union() %>%
  sf::st_transform(wgs84)

#############################################################################

# extract the bathymetry data

#transfer the project
# bathy_wgs <- raster::projectRaster(bathy, crs = wgs84)
# 
# lats <- seq(32, 35, 0.02)
# longs <- seq(-121, -117, 0.02)
# spatial_xy_df <- expand.grid(lats = lats, longs = longs)
# spatial_xy <- spatial_xy_df %>%
#   # Convert to sf
#   sf::st_as_sf(., coords=c("longs", "lats"), crs="+proj=longlat +datum=WGS84") %>%
#   # Convert to sp
#   sf::as_Spatial()
# 
# # extract bathy depth
# bathy_spatial_orig <- raster::extract(x=bathy_wgs, y=spatial_xy)
# # Format the dataframe
# bathy_spatial <- bathy_spatial_orig %>%
#   as.data.frame() %>%
#   mutate(longs = spatial_xy_df$longs, lats = spatial_xy_df$lats) %>%
#   rename(bathy_m = ".") %>%
#   select(lats, longs, bathy_m)

#######################################################################
 
# filter the raster within 1 nautical mile boundary

# filter the original dataframe to 70 fathoms and transfer back to raster
bathy_filter_df <- bathy_spatial %>%
  filter(bathy_m >= -128)

bathy_filter_sf <- bathy_filter_df %>%
  sf::st_as_sf(coords = c("longs", "lats"), crs=wgs84)


bathy_extract <- sf::st_intersection(bathy_filter_sf, buffer)
coordinates_extract <- sf::st_coordinates(bathy_extract)
bathy_extract_df <- cbind(sf::st_drop_geometry(bathy_extract), coordinates_extract) %>%
  rename(longs = X,
         lats = Y)

bathy_extract_df_final <- bathy_extract_df %>%
  mutate(bathy_m = round(bathy_m, 3)) %>%
  mutate(bathy_m = ifelse(bathy_m == 0|bathy_m == 0.000, NA, bathy_m)) %>%
  drop_na()

bathy_extract_df_final_sf <- sf::st_as_sf(bathy_extract_df_final, coords = c("longs", "lats"), crs=wgs84) %>%
  mutate(bathy_m = 1)





# make the bathy_extract into a raster
r <- raster::raster(raster::extent(bathy_extract_df_final_sf), resolution = 0.02, crs = wgs84)
bathy_extract_raster <- raster::rasterize(bathy_extract_df_final_sf, r, field = "bathy_m")
bathy_extract_polygons <- raster::rasterToPolygons(bathy_extract_raster, 
                                    fun = NULL,          # Keep all values
                                    na.rm = TRUE,        # Remove NA values
                                    dissolve = TRUE)


bathy_extract_polygon_sf <- bathy_extract_polygons %>%
  sf::st_as_sf()


  






##################################
ggplot() + 
  geom_tile(data = bathy_extract_df_final, aes(x = longs, y = lats, fill = bathy_m)) +
  #geom_contour(data = bathy_extract_df_final, aes(x = longs, y = lats, z = bathy_m), breaks = contour_levels, color = "black", size = 0.75) +
  #geom_sf(data = buffer, color="black", fill = "transparent", size=0.4) +
  geom_sf(data = islands %>% sf::st_transform(wgs84)) +
  #geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  #geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -118), ylim = c(32, 35)) +
  theme_bw()



ggplot(data = bathy_extract_polygon_sf) +
  geom_sf()









#########################################################################
# state water boundary

ggplot() +
  geom_sf(data = state_water) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  theme_bw()

#######################################################################
# save the bathymetry data
write.csv(bathy_spatial, file = "data/gis_data/bathy_extracted.csv", row.names = FALSE)





