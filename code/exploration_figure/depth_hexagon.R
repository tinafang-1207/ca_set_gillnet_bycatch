### reported depth vs. extracted depth heat map###

### clean environment
rm(list = ls())

### read in packages
library(tidyverse)
library(sf)

### read in data
trip_1980 <- readRDS("data/confidential/original/CDFW_1983_1989_gillnet_observer_set_info.Rds")

bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

bathy_200fa_WGS84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

### Extract depth from bathymetry data
trip_1980_pre_join_xy <- trip_1980 %>%
  select(long_dd, lat_dd)

trip_1980_depth_mat <- raster::extract(bathy_200fa_WGS84, trip_1980_pre_join_xy)

trip_1980_depth_df <- trip_1980_depth_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "extracted_depth_fa", 1:ncol(.)) %>%
  mutate(row_id = trip_1980$set_id) %>%
  rename(set_id = row_id)

trip_1980_pre_join <- merge(trip_1980, trip_1980_depth_df, by = "set_id") %>%
  mutate(extracted_depth_fa = abs(extracted_depth_fa)) %>%
  mutate(extracted_depth_fa = round(extracted_depth_fa, digits = 0)) %>%
  select(set_id, bottom_depth_fa, extracted_depth_fa)

ggplot(trip_1980_pre_join, aes(x = bottom_depth_fa, y = extracted_depth_fa)) +
  geom_hex(bins = 50) +
  geom_abline(intercept = 1, slope = 1, lty = 2, col = "darkgrey") +
  scale_fill_viridis_c(trans = "log10") +
  lims(y = c(0, 100), x = c(0, 100)) +
  labs(x = "reported depth (fa)", y = "extracted depth (fa)") +
  theme_bw()

ggplot(trip_1980_pre_join, aes(x = bottom_depth_fa, y = extracted_depth_fa)) +
  geom_point(alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, lty = 2, col = "darkgrey") +
  lims(y = c(0, 400), x = c(0, 400)) +
  labs(x = "reported depth (fa)", y = "extracted depth (fa)") +
  theme_bw()
