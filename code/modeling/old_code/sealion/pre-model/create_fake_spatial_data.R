
### clean working environment ###
rm(list = ls())

### read in packages ###
library(tidyverse)

### read in data ###

#sst data
sst_orig <- readRDS("data/gis_data/data_sst_1983_2020.RDS")

#depth data
bathy_200fa <- raster::raster("data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

#spatial data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

usa_ca <- usa %>% filter(name == "California")


### Create fake data ###

# make lats & longs in smaller spatial resolution (0.02)
lats <- seq(32, 35, 0.02)
longs <- seq(-121, -117, 0.02)
jdays<- seq(1, 365, 1)
soak_hr <- 24
mesh_size <- 8.5

fake_spatial_orig <- expand.grid(lats = lats,
                                 longs = longs,
                                 jdays = jdays,
                                 soak_hr = soak_hr,
                                 mesh_size = mesh_size)

### add haul depth ###

bathy_200fa_wgs84 <- raster::projectRaster(bathy_200fa, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

fake_spatial_xy <- expand.grid(lats = lats, longs = longs)

fake_spatial_xy_sp <- fake_spatial_xy %>% 
  # convert to sf
  sf::st_as_sf(coords=c("longs", "lats"), crs=raster::crs(bathy_200fa_wgs84)) %>%
  # convert to sp
  sf::as_Spatial()

bathy_spatial_orig <- raster::extract(x=bathy_200fa_wgs84, y=fake_spatial_xy_sp)

bathy_spatial <- bathy_spatial_orig %>%
  as.data.frame() %>%
  mutate(longs = fake_spatial_xy$longs, lats = fake_spatial_xy$lats) %>%
  rename(bathy_fa = ".") %>%
  select(lats, longs, bathy_fa)


### add mean sst ###

sst_2000 <- sst_orig %>%
  filter(date >= "2000-01-01")

sst_2000_ras <- sst_2000 %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")


fake_spatial_xy <- expand.grid(lats = lats, longs = longs)

fake_spatial_xy_sp <- fake_spatial_xy %>%
  # Convert to sf
  sf::st_as_sf(., coords=c("longs", "lats"), crs="+proj=longlat +datum=WGS84") %>%
  # Convert to sp
  sf::as_Spatial()

fake_sst_extract <- raster::extract(x=sst_2000_ras, y=fake_spatial_xy_sp)

fake_sst_extract_df <- fake_sst_extract %>%
  as.data.frame() %>%
  mutate(lats = fake_spatial_xy$lats, longs = fake_spatial_xy$longs) %>%
  select(lats, longs, everything())%>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date)) %>%
  mutate(jdays = lubridate::yday(date))


fake_sst_summarize <- fake_sst_extract_df %>%
  group_by(jdays, lats, longs) %>%
  summarize(mean_sst = mean(sst))


fake_spatial_sst <- left_join(fake_spatial_orig, fake_sst_summarize, by = c("lats", "longs", "jdays"))

fake_spatial_bathy <- left_join(fake_spatial_sst, bathy_spatial, by = c("lats", "longs"))

### add distance to shore ###

utm11 <- "+proj=utm +zone=11 +datum=NAD83"

fake_spatial_xy_sp_utm <- fake_spatial_xy_sp %>%
  sp::spTransform(utm11)  

usa_ca_sp <- usa_ca %>%
  sf::st_transform(crs = utm11) %>%
  sf::as_Spatial()

dist_mat <- rgeos::gDistance(fake_spatial_xy_sp_utm, usa_ca_sp, byid = T)  

dist_df <- dist_mat %>%
  as.data.frame() %>%
  gather(key = "row_id", value = "dist_m", 1:ncol(.)) %>%
  mutate(longs = fake_spatial_xy$longs, lats = fake_spatial_xy$lats) %>%
  select(-row_id)  

fake_spatial_final <-left_join(fake_spatial_bathy, dist_df, by = c("lats", "longs")) %>%
  mutate(dist_km = dist_m/1000)

### export table ###
saveRDS(fake_spatial_final, file = file.path("data/confidential/processed/fake_spatial_pre_model.Rds"))


  
  

