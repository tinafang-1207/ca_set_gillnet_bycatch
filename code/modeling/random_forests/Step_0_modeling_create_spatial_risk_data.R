
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###

# sst data
sst1 <- readRDS("data/gis_data/OISST_2000.Rds")
sst2 <- readRDS("data/gis_data/OISST_2018.Rds")

# bathymetry data (for haul depth)
bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

# coastal data (for distance to shore)
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa_ca <- usa %>% filter(name == "California")


### Format data ###

# latitude
lats <- seq(32, 35, 0.02)

# longitude
longs <- seq(-121, -117, 0.02)

# julian day
jdays<- seq(1, 365, 1)

# soak hour
soak_hr <- 24

# mesh size
mesh_size <- 8.5

# Combine into table
data_orig <- expand.grid(lats = lats,
                         longs = longs,
                         jdays = jdays,
                         soak_hr = soak_hr,
                         mesh_size = mesh_size)




# sst

sst_orig <- bind_rows(sst1, sst2) %>% 
  mutate(date=lubridate::ymd(date)) %>% 
  # filter to sst after 2010
  filter(date >= "2010-01-01") %>%
  na.omit()

# Transfer sst_orig into raster
sst_ras <- sst_orig %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")

# geographic location to extract sst

spatial_xy_df <- expand.grid(lats = lats, longs = longs)

spatial_xy <- spatial_xy_df %>%
  # Convert to sf
  sf::st_as_sf(., coords=c("longs", "lats"), crs="+proj=longlat +datum=WGS84") %>%
  # Convert to sp
  sf::as_Spatial()

# Extract sst

sst_ext <- raster::extract(x = sst_ras, y = spatial_xy)

sst_ext_df <- sst_ext %>%
  as.data.frame() %>%
  mutate(lats = spatial_xy_df$lats, longs = spatial_xy_df$longs) %>%
  select(lats, longs, everything())%>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(jdays = lubridate::yday(date))

sst_summarize <- sst_ext_df %>%
  group_by(jdays, lats, longs) %>%
  summarize(mean_sst = mean(sst))


# bathymetry

bathy_200fa_wgs84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

# extract bathymetry data
bathy_spatial_orig <- raster::extract(x=bathy_200fa_wgs84, y=spatial_xy)

bathy_spatial <- bathy_spatial_orig %>%
  as.data.frame() %>%
  mutate(longs = spatial_xy_df$longs, lats = spatial_xy_df$lats) %>%
  rename(bathy_fa = ".") %>%
  select(lats, longs, bathy_fa)

# distance to shore

utm11 <- "+proj=utm +zone=11 +datum=NAD83"

spatial_xy_utm <- spatial_xy %>%
  sp::spTransform(utm11)  

usa_ca_sp <- usa_ca %>%
  sf::st_transform(crs = utm11) %>%
  sf::as_Spatial()

dist_mat <- rgeos::gDistance(spatial_xy_utm, usa_ca_sp, byid = T)  

dist_df <- dist_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "dist_m", 1:ncol(.)) %>%
  mutate(longs = spatial_xy_df$longs, lats = spatial_xy_df$lats) %>%
  select(-row_id) %>%
  mutate(dist_km = dist_m/1000) %>%
  select(-dist_m)

  
# combine data together
data_sst <- left_join(data_orig, sst_summarize, by = c("lats", "longs", "jdays"))

data_bathy <- left_join(data_sst, bathy_spatial, by = c("lats", "longs"))

data_final <- left_join(data_bathy, dist_df, by = c("lats", "longs"))

# save to dropbox
saveRDS(data_final, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/create_spatial_risk_unpredict.Rds")


############ add island dummy below #################

# read in data

data_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/create_spatial_risk_unpredict.Rds")

# island buffer
buffer <- readRDS("data/gis_data/island_buffer_10km.Rds")

# select geographic coordinates
spatial_xy_df <- data_orig %>%
  select(lats, longs)

# turn it into a sf object
spatial_xy_sf <- spatial_xy_df %>%
  sf::st_as_sf(coords = c("longs", "lats"), crs="+proj=longlat +datum=WGS84")

out <- sf::st_within(spatial_xy_sf, buffer) %>% as.numeric() 
out_yn <- ifelse(is.na(out), 0, 1)

# join back
data_orig$island_yn <- out_yn

# save the dataframe

saveRDS(data_orig, file = "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/create_spatial_risk_unpredict.Rds")







