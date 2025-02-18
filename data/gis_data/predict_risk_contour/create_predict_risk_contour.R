
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)

### read in data ###
hotspot <- readRDS(file.path("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds"))
islands <- sf::st_read("data/gis_data/california_islands/california_islands.shp")

# wgs 84
wgs84 <- "+proj=longlat +datum=WGS84"
####################################################################
# California sea lion
hotspot_sl <- hotspot %>%
  filter(species == "California sea lion") %>%
  select(-species) %>%
  rename(long = Longitude, lat = Latitude)


# find the 90th percentile of California sealion
quantile(hotspot_sl$spatial_risk, 0.95)


# Convert to spatvect object that recognized by terra
hotspot_sl_vect <- terra::vect(hotspot_sl, geom = c("long", "lat"))
# set crs
terra::crs(hotspot_sl_vect) <- wgs84

# Define the extent of the raster
# For this example, use a bounding box covering the points.
bbox <- terra::ext(min(hotspot_sl$long), max(hotspot_sl$long), min(hotspot_sl$lat), max(hotspot_sl$lat))

# Create an empty raster with the same extent, and define resolution
r_sl <- terra::rast(bbox, resolution = 0.0201)  # Set resolution, adjust as needed

# Rasterize the spatial data (the risk values)
r_risk_sl <- terra::rasterize(hotspot_sl_vect, r_sl, field = "spatial_risk")

# Plot the raster
terra::plot(r_risk_sl)

# Make the contour of spatial risk>=0.35 for California sea lion
# Temporarily set levels = 0.35 - ask Chris to determine
contour_sl <- terra::as.contour(r_risk_sl, levels = 0.2782287)

contour_sl_sf <- sf::st_as_sf(contour_sl) %>%
  # Break
  sf::st_cast("LINESTRING") %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Remove the two small contours (ask Chris!)
  filter(id == 1) %>%
  # Reproject
  sf::st_transform(crs = sf::st_crs("+proj=longlat +datum=WGS84"))

# save the contour above
sf::st_write(contour_sl_sf, dsn = "data/gis_data/predict_risk_contour/sealion_predict_risk_contour.shp", append = FALSE)
########################################################################
# Harbor seal

hotspot_hs <- hotspot %>%
  filter(species == "Harbor seal") %>%
  select(-species) %>%
  rename(long = Longitude, lat = Latitude)

# find the 95% quantile
quantile(hotspot_hs$spatial_risk, 0.95)

# Convert to spatvect object that recognized by terra
hotspot_hs_vect <- terra::vect(hotspot_hs, geom = c("long", "lat"))
# set crs
terra::crs(hotspot_hs_vect) <- wgs84

# Define the extent of the raster (you may need to adjust it)
# For this example, use a bounding box covering the points.
bbox <- terra::ext(min(hotspot_hs$long), max(hotspot_hs$long), min(hotspot_hs$lat), max(hotspot_hs$lat))

# Create an empty raster with the same extent, and define resolution
r_hs <- terra::rast(bbox, resolution = 0.0201)  # Set resolution, adjust as needed

# Rasterize the spatial data (the risk values)
r_risk_hs <- terra::rasterize(hotspot_hs_vect, r_hs, field = "spatial_risk")

# Plot the raster
terra::plot(r_risk_hs)

# Make the contour of spatial risk>=0.15 for Harbor seal
# 0.15 is the 95% of harbor seal bycatch spatial risk
contour_hs <- terra::as.contour(r_risk_hs, levels = 0.1529044)

contour_hs_sf <- sf::st_as_sf(contour_hs) %>%
  # Break
  sf::st_cast("LINESTRING") %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Remove the two small contours (ask Chris!)
  filter(id %in%c(1,11,12)) %>%
  # Reproject
  sf::st_transform(crs = sf::st_crs(wgs84))

# save the contour above
sf::st_write(contour_hs_sf, dsn = "data/gis_data/predict_risk_contour/harbor_seal_predict_risk_contour.shp", append = FALSE)



######################################################################
# Common murre
hotspot_cm <- hotspot %>%
  filter(species == "Common murre") %>%
  select(-species) %>%
  rename(long = Longitude, lat = Latitude)

# check the 95% percentile
quantile(hotspot_cm$spatial_risk, 0.99)

# Convert to spatvect object that recognized by terra
hotspot_cm_vect <- terra::vect(hotspot_cm, geom = c("long", "lat"))
# set crs
terra::crs(hotspot_cm_vect) <- wgs84

# Define the extent of the raster (you may need to adjust it)
# For this example, use a bounding box covering the points.
bbox <- terra::ext(min(hotspot_cm$long), max(hotspot_cm$long), min(hotspot_cm$lat), max(hotspot_cm$lat))

# Create an empty raster with the same extent, and define resolution
r_cm <- terra::rast(bbox, resolution = 0.0201)  # Set resolution, adjust as needed

# Rasterize the spatial data (the risk values)
r_risk_cm <- terra::rasterize(hotspot_cm_vect, r_cm, field = "spatial_risk")

# Plot the raster
terra::plot(r_risk_cm)

# Make the contour of spatial risk>=0.15 for Harbor seal
# 0.15 is the 95% of harbor seal bycatch spatial risk
contour_cm <- terra::as.contour(r_risk_cm, levels = 0.1344438 )

contour_cm_sf <- sf::st_as_sf(contour_cm) %>%
  # Break
  sf::st_cast("LINESTRING") %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Remove the two small contours (ask Chris!)
  filter(id==2) %>%
  # Reproject
  sf::st_transform(crs = sf::st_crs(wgs84))

# save the contour above
sf::st_write(contour_cm_sf, dsn = "data/gis_data/predict_risk_contour/common_murre_predict_risk_contour.shp", append = FALSE)
#############################################################################

# Northern elephant seal
hotspot_ns <- hotspot %>%
  filter(species == "Northern elephant seal") %>%
  select(-species) %>%
  rename(long = Longitude, lat = Latitude)

# check the 95% percentile
quantile(hotspot_ns$spatial_risk, 0.99)

# Convert to spatvect object that recognized by terra
hotspot_ns_vect <- terra::vect(hotspot_ns, geom = c("long", "lat"))
# set crs
terra::crs(hotspot_ns_vect) <- wgs84

# Define the extent of the raster (you may need to adjust it)
# For this example, use a bounding box covering the points.
bbox <- terra::ext(min(hotspot_ns$long), max(hotspot_ns$long), min(hotspot_ns$lat), max(hotspot_ns$lat))

# Create an empty raster with the same extent, and define resolution
r_ns <- terra::rast(bbox, resolution = 0.0201)  # Set resolution, adjust as needed

# Rasterize the spatial data (the risk values)
r_risk_ns <- terra::rasterize(hotspot_ns_vect, r_ns, field = "spatial_risk")

# Plot the raster
terra::plot(r_risk_ns)

# Make the contour of spatial risk>=0.15 for Harbor seal
# 0.15 is the 95% of harbor seal bycatch spatial risk
contour_ns <- terra::as.contour(r_risk_ns, levels = 0.1257256)

contour_ns_sf <- sf::st_as_sf(contour_ns) %>%
  # Break
  sf::st_cast("LINESTRING") %>% 
  # Add id
  mutate(id=1:nrow(.)) %>% 
  # Remove the two small contours (ask Chris!)
  filter(id%in%c(1,3)) %>%
  # Reproject
  sf::st_transform(crs = sf::st_crs(wgs84))

# save the contour above
sf::st_write(contour_ns_sf, dsn = "data/gis_data/predict_risk_contour/ne_seal_predict_risk_contour.shp", append = FALSE)










#############################################

# Check result

ggplot() +
  geom_sf(data = contour_ns_sf, mapping=aes(color =as.character(id))) +
  geom_sf(data = islands %>% sf::st_transform(wgs84)) +
  coord_sf(xlim = c(-121, -118), ylim = c(32, 35)) +
  theme_bw()

# # Save the raster (optional)
# writeRaster(r_risk, "spatial_risk_raster.tif", overwrite = TRUE)
# 
# # Read the raster back (using terra)
# r_risk_terra <- rast("spatial_risk_raster.tif")
# 
# # Print raster info
# print(r_risk_terra)
###################################################




