

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

# Read CDFW data
cdfwdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/processed"
cdfw_key_orig <- readRDS(file.path(cdfwdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))

# Read SWFSC data
swfscdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"
swfsc_key_orig <- readRDS(file.path(swfscdir, "SWFSC_1990_2017_set_net_observer_trips_merged.Rds"))

# Read Karin daata
karindir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state_karin/processed/"
karin_key_orig <- readRDS(file.path(karindir, "1987_1990_observer_metadata_from_karin.Rds"))

# Read logbook data
logs <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/CDFW_1981_2020_gillnet_logbook_data_imputed.Rds")

# Build logbook key
log_key <- logs %>% 
  select(vessel_id, date, block_id, net_type, mesh_in) %>% 
  unique() %>% 
  group_by(vessel_id, date, block_id, net_type) %>% 
  summarize(mesh_in_nvals=n_distinct(mesh_in),
            mesh_in=paste(sort(unique(mesh_in)), collapse=", ")) %>% 
  ungroup()


# Format keys
################################################################################

# Format CDFW key
cdfw_key <- cdfw_key_orig %>% 
  # Add dataset
  mutate(dataset="State") %>% 
  # Add trip id
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  # Reduce
  select(dataset, 
         trip_id, set_num,
         set_id, vessel_id, 
         date, 
         port_depart, port_landing, 
         net_type, 
         target_spp, 
         lat_dd, long_dd, 
         bottom_depth_fa, 
         net_length_fa, mesh_size1_in, soak_hr) %>% 
  # Rename
  rename(port_return=port_landing,
         depth_fa=bottom_depth_fa,
         mesh_size_in=mesh_size1_in)

# Format SWFSC key
swfsc_key <- swfsc_key_orig %>% 
  # Add dataset
  mutate(dataset="Federal") %>% 
  # Reduce
  select(dataset,
         trip_id, set_num, 
         set_id, vessel_plate, 
         date_haul, 
         port_depart, port_return, 
         net_type,
         target1_spp, 
         lat_dd_haul, long_dd_haul, 
         depth_fa_haul, 
         net_mesh_panel_length_fathoms, net_mesh_size_in, soak_hr) %>% 
  # Rename
  rename(vessel_id=vessel_plate, 
         date=date_haul, 
         target_spp=target1_spp,
         lat_dd=lat_dd_haul,
         long_dd=long_dd_haul,
         depth_fa=depth_fa_haul,
         net_length_fa=net_mesh_panel_length_fathoms,
         mesh_size_in=net_mesh_size_in) 

# Merge keys
set_key_orig <- bind_rows(cdfw_key, swfsc_key) %>% 
  # Add date things
  mutate(year=lubridate::year(date),
         month=lubridate::month(date),
         yday=lubridate::yday(date)) %>% 
  # Arrange
  select(dataset, year, month, yday, date, 
         vessel_id, trip_id, set_num, set_id, everything())

# Inspect
freeR::complete(set_key_orig)

# Inspect
g <- ggplot(set_key_orig, aes(x=long_dd, y=lat_dd, color=net_type)) +
  geom_point()
g

# Export key
saveRDS(set_key_orig, file.path(outdir, "1983_2017_gillnet_observer_metadata_unimputed.Rds"))


# Impute missing values
################################################################################

# 1. Fill missing coordinates and  identify closest block
# 3. Extract depth
# 4. Assign block depth to missing depths
# 5. Impute soak hours
# 6. Add mesh sizes


# 1a. Divide key into stes with and without GPS coords
################################################################################

# Divide set key
set_key_gps <- set_key_orig %>% 
  filter(!is.na(lat_dd) & !is.na(long_dd)) %>% 
  mutate(gps_type="Reported")
set_key_no_gps <- set_key_orig %>% 
  filter(!(!is.na(lat_dd) & !is.na(long_dd))) %>% 
  mutate(gps_type="Imputed")


# 1b. Find block for sets without coords and give block coords
################################################################################

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% 
  sf::st_drop_geometry() %>% 
  select(block_id, block_lat_dd, block_long_dd) %>% 
  rename(lat_dd=block_lat_dd, long_dd=block_long_dd)

# Format logs
logs_use <- logs %>% 
  filter(block_id %in% blocks$block_id) %>% 
  mutate(year=lubridate::year(date),
         month=lubridate::month(date))

# Most commonly visited block in month
block_key0 <- logs_use %>%
  # Summarize
  group_by(vessel_id, date, block_id) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Most common block
  arrange(vessel_id, date, desc(ntrips)) %>% 
  group_by(vessel_id, date) %>% 
  slice(1) %>% 
  ungroup()

# Most commonly visited block in month
block_key1 <- logs_use %>%
  # Summarize
  group_by(vessel_id, year, month, block_id) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Most common block
  arrange(vessel_id, year, month, desc(ntrips)) %>% 
  group_by(vessel_id, year, month) %>% 
  slice(1) %>% 
  ungroup()

# Most commonly visited block in year
block_key2 <- logs_use %>%
  # Summarize
  group_by(vessel_id, year, block_id) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Most common block
  arrange(vessel_id, year, desc(ntrips)) %>% 
  group_by(vessel_id, year) %>% 
  slice(1) %>% 
  ungroup()

# Most commonly visited block by vessel
block_key3 <- logs_use %>%
  # Summarize
  group_by(vessel_id, block_id) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Most common block
  arrange(vessel_id, desc(ntrips)) %>% 
  group_by(vessel_id) %>% 
  slice(1) %>% 
  ungroup()

# Most commonly visited block by vessel
block_key4 <- logs_use %>%
  # Summarize
  group_by(year, month, block_id) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Most common block
  arrange(year, month, desc(ntrips)) %>% 
  group_by(year, month) %>% 
  slice(1) %>% 
  ungroup()

# Add block id to 
set_key_no_gps1 <- set_key_no_gps %>%
  # Convert vessel id
  mutate(vessel_id=as.character(vessel_id)) %>% 
  # Add block candidates
  left_join(block_key0 %>% select(-ntrips), by=c("vessel_id", "date")) %>% 
  rename(block_id1=block_id) %>% 
  left_join(block_key1 %>% select(-ntrips), by=c("vessel_id", "year", "month")) %>% 
  rename(block_id2=block_id) %>% 
  left_join(block_key2 %>% select(-ntrips), by=c("vessel_id", "year")) %>% 
  rename(block_id3=block_id) %>% 
  left_join(block_key3 %>% select(-ntrips), by=c("vessel_id")) %>% 
  rename(block_id4=block_id) %>% 
  left_join(block_key4 %>% select(-ntrips), by=c("year", "month")) %>% 
  rename(block_id5=block_id) %>% 
  # Final block id
  mutate(block_id=ifelse(!is.na(block_id1), block_id1,
                         ifelse(!is.na(block_id2), block_id2,
                                ifelse(!is.na(block_id3), block_id3,
                                              ifelse(!is.na(block_id4), block_id4, block_id5))))) %>% 
  mutate(gps_type=ifelse(!is.na(block_id1), "Imputed-vessel day",
                         ifelse(!is.na(block_id2), "Imputed-vessel month",
                                ifelse(!is.na(block_id3), "Imputed-vessel year",
                                       ifelse(!is.na(block_id4), "Imputed-vessel alltime", "Imputed-year/month"))))) %>% 
  # Remove
  select(-c(block_id1:block_id5)) %>% 
  # Add coordinates
  select(-c(long_dd, lat_dd)) %>% 
  left_join(blocks_df) %>% 
  # Convert vessel id
  mutate(vessel_id=as.numeric(vessel_id))
  

# 1c. Find block for sets with coords
################################################################################

# Blocks
blocks <- wcfish::blocks

# Lat/long key
latlong_key <- set_key_gps %>% 
  select(long_dd, lat_dd) %>% 
  unique()

# Convert the GPS data to an sf object
latlong_key_sf <- sf::st_as_sf(latlong_key, coords = c("long_dd", "lat_dd"), crs = sf::st_crs(blocks))

# Use st_nearest_feature to find the nearest block for each GPS coordinate
nearest_block <- sf::st_nearest_feature(latlong_key_sf, blocks)

# Extract the block_id from the nearest block
nearest_block_id <- blocks$block_id[nearest_block]

# Add the block_id to the original GPS dataframe
latlong_key$block_id <- nearest_block_id

# Fill lat/long
set_key_gps1 <- set_key_gps %>% 
  # Add block id
  left_join(latlong_key, by=c("long_dd", "lat_dd")) %>% 
  # Move
  relocate(block_id, .after=long_dd)


# 1d. Find block for sets with coords
################################################################################

# Merge
set_key2 <- bind_rows(set_key_gps1, set_key_no_gps1) %>% 
  # Sort
  arrange(year, month, date, vessel_id) %>% 
  # Arrange
  select(dataset:target_spp, gps_type, everything())

# Inspect
freeR::complete(set_key2)


# 2. Impute soak hour
################################################################################

# Soak hour key
soak_key <- set_key2 %>% 
  group_by(vessel_id, net_type, target_spp, soak_hr) %>% 
  summarize(nsets=n()) %>% 
  ungroup() %>% 
  arrange(vessel_id, net_type, target_spp, desc(nsets)) %>% 
  group_by(vessel_id, net_type, target_spp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(soak_hr_mode=soak_hr)

# Impute soak time
set_key3 <- set_key2 %>% 
  # Add soak time
  left_join(soak_key %>% select(-nsets), by=c("vessel_id", "net_type", "target_spp")) %>% 
  # Impute
  mutate(soak_hr_type=ifelse(!is.na(soak_hr), "Reported", "Imputed"),
         soak_hr=ifelse(!is.na(soak_hr), soak_hr, soak_hr_mode))

# Plot
plot(soak_hr~soak_hr_mode, set_key3)

# Inspect
freeR::complete(set_key3)


# 3. Impute mesh size
################################################################################

# Mesh key - vessel, net type, target
mesh_key1 <- set_key2 %>% 
  filter(!is.na(mesh_size_in)) %>% 
  group_by(vessel_id, net_type, target_spp, mesh_size_in) %>% 
  summarize(nsets=n()) %>% 
  ungroup() %>% 
  arrange(vessel_id, net_type, target_spp, desc(nsets)) %>% 
  group_by(vessel_id, net_type, target_spp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(mesh_size_in_mode1=mesh_size_in)

# Mesh key 2 - net type, target
mesh_key2 <- set_key2 %>% 
  filter(!is.na(mesh_size_in)) %>% 
  group_by(net_type, target_spp, mesh_size_in) %>% 
  summarize(nsets=n()) %>% 
  ungroup() %>% 
  arrange(net_type, target_spp, desc(nsets)) %>% 
  group_by(net_type, target_spp) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(mesh_size_in_mode2=mesh_size_in)

# Mesh key 3 -net type
mesh_key3 <- set_key3 %>% 
  filter(!is.na(mesh_size_in)) %>% 
  group_by(net_type, mesh_size_in) %>% 
  summarize(nsets=n()) %>% 
  ungroup() %>% 
  arrange(net_type, desc(nsets)) %>% 
  group_by(net_type) %>% 
  slice(1) %>% 
  ungroup() %>% 
  rename(mesh_size_in_mode3=mesh_size_in)

# Build
set_key4 <- set_key3 %>% 
  # Simplify
  select(-soak_hr_mode) %>% 
  # Add mesh size
  left_join(mesh_key1 %>% select(-nsets), by=c("vessel_id", "net_type", "target_spp")) %>% 
  left_join(mesh_key2 %>% select(-nsets), by=c("net_type", "target_spp")) %>% 
  left_join(mesh_key3 %>% select(-nsets), by=c("net_type")) %>% 
  # Select mesh size
  mutate(mesh_size_in_type=ifelse(!is.na(mesh_size_in), "Reported",
                                  ifelse(!is.na(mesh_size_in_mode1), "Imputed-1",
                                         ifelse(!is.na(mesh_size_in_mode2), "Imputed-2", "Imputed-3")))) %>% 
  mutate(mesh_size_in=ifelse(!is.na(mesh_size_in), mesh_size_in,
                             ifelse(!is.na(mesh_size_in_mode1), mesh_size_in_mode1,
                                    ifelse(!is.na(mesh_size_in_mode2), mesh_size_in_mode2, mesh_size_in_mode3))))

# Inspect
freeR::complete(set_key4)

# Plot
plot(mesh_size_in~mesh_size_in_mode1, set_key4)
plot(mesh_size_in~mesh_size_in_mode2, set_key4)
plot(mesh_size_in~mesh_size_in_mode3, set_key4)

# Format
set_key5 <- set_key4 %>% 
  # Simplify
  select(-c(mesh_size_in_mode1, mesh_size_in_mode2, mesh_size_in_mode3)) %>% 
  relocate(mesh_size_in_type, .before=mesh_size_in)

# Inspect
freeR::complete(set_key5)


# 4. Compute distance from shore
################################################################################

# Projections
wgs84 <- "+proj=longlat +datum=WGS84"
utm11 <- "+proj=utm +zone=11 +datum=NAD83"

# Land
usa <- rnaturalearth::ne_countries(country = "United States of America", returnclass = "sf", scale="large") 
usa_sp <- usa %>% 
  sf::st_transform(utm11)# %>% 
# sf::as_Spatial()

# Lat/long key
latlong_key <- set_key5 %>% 
  select(lat_dd, long_dd) %>% 
  unique()

# Convert to sp
latlong_key_sp <- latlong_key %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=wgs84) %>% 
  sf::st_transform(utm11) #%>% 
#sf::as_Spatial()

# Calculate the distances between points and the polygon
distances_m <- sf::st_distance(latlong_key_sp, usa_sp)

# Calculate distance to shore
# rgeos is deprecated to I know longer user this code blcok
#dist_mat <- rgeos::gDistance(latlong_key_sp, usa_sp, byid = T)
# Format distance matrix
# dist_df <- dist_mat %>%
#   # Convert to data frame
#   as.data.frame() %>%
#   # Add land polygon id
#   mutate(land_id=1:nrow(.)) %>% 
#   select(land_id, everything()) %>% 
#   # Gather
#   gather(key="latlong_id", value="dist_m", 2:ncol(.)) %>% 
#   mutate(latlong_id=as.numeric(latlong_id)) %>% 
#   # Find minimum distance
#   arrange(latlong_id, dist_m) %>% 
#   group_by(latlong_id) %>% 
#   slice(1) %>% 
#   ungroup() %>% 
#   # Remove land id
#   select(-land_id)

# Add distance to latlong key
latlong_key1 <- latlong_key %>% 
  mutate(shore_km=as.numeric(distances_m)/1000)

# Plot check
ggplot(latlong_key1, aes(x=long_dd, y=lat_dd, color=pmin(shore_km,10))) +
  geom_point() +
  scale_color_gradientn(colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()

# Add shore distance
set_key6 <- set_key5 %>% 
  # Add shore distance
  left_join(latlong_key1, by=c("long_dd", "lat_dd"))

# Inspect
freeR::complete(set_key6)


# 6. Compute depth
################################################################################

# Read block depth values
block_depth_key <- readRDS("data/bathymetry/processed/CA_block_depths.Rds") %>% 
  select(block_id, depth_m_med) %>% 
  as.matrix() %>% 
  as.data.frame()

# Read bathymetry
bathy <- raster::raster("/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/bathymetry/processed/ca_bathymetry_200m_epsg3309.tif")

# Plot bathy
raster::plot(bathy)

# Create SP latlong key
latlong_key <- set_key6 %>% 
  select(block_id, long_dd, lat_dd) %>% 
  unique()
latlong_key_sp <- latlong_key %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs="+proj=longlat +datum=WGS84") %>% 
  sf::st_transform(crs=sf::st_crs(bathy)) %>% 
  sf::as_Spatial()
  
# Extract depth values
depth_values <- raster::extract(bathy, latlong_key_sp)

# MARMAP version
bathy_marmap <- marmap::getNOAA.bathy(lon1 = -116, lon2 = -125,
                                      lat1 = 32, lat2 = 42, resolution = 4)
plot(bathy_marmap)
depth_values_marmap <- marmap::get.depth(bathy_marmap, x=latlong_key %>% select(long_dd, lat_dd), locator=F)
plot(depth_values ~ depth_values_marmap$depth)
hist(measurements::conv_unit(set_key6$depth_fa, "fathom", "m"), breaks=seq(0,1000,10), xlim=c(0,200))

# Add to key
latlong_key_df <- latlong_key %>% 
  # Absolute value
  mutate(depth_m=depth_values*-1) %>% 
  # 0s are on land and therefore NA
  mutate(depth_m=ifelse(depth_m==0, NA, depth_m)) %>% 
  # Mark which are extracted vs imputed
  mutate(depth_m_type=ifelse(!is.na(depth_m), "Extracted", "Block median")) %>% 
  # Add and use imputed values
  left_join(block_depth_key) %>% 
  mutate(depth_m=ifelse(depth_m_type=="Block median", depth_m_med, depth_m)) %>% 
  select(-depth_m_med)

# Add to set
set_key7 <- set_key6 %>% 
  # Add extracted/computed depths
  left_join(latlong_key_df) %>% 
  mutate(depth_fa_imputed=measurements::conv_unit(depth_m, "m", "fathom")) %>% 
  # Impute depths
  mutate(depth_type=ifelse(!is.na(depth_fa), "Reported", depth_m_type),
         depth_fa=ifelse(!is.na(depth_fa), depth_fa, depth_fa_imputed)) %>% 
  select(-c(depth_m, depth_m_type)) %>% 
  # Arrange
  select(dataset:block_id, depth_type, depth_fa, depth_fa_imputed, everything())

# Inspect
freeR::complete(set_key7)
table(set_key7$depth_type)

# Inspect
plot(depth_fa_imputed ~ depth_fa, set_key7, xlim=c(0,200), ylim=c(0,200))


# 7. Mark islands
################################################################################

# Island buffer
buffer <- readRDS("data/gis_data/island_buffer_10km.Rds")

# Create an 'sf' object for the GPS coordinates
latlong_key <- set_key7 %>% 
  select(block_id, long_dd, lat_dd) %>% 
  unique()
latlong_key_sf <- latlong_key %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs="+proj=longlat +datum=WGS84")

# Check if each point falls within the buffer polygon
out <- sf::st_within(latlong_key_sf, buffer) %>% as.numeric() 
out_yn <- ifelse(is.na(out), 0, 1)
table(out_yn)

#
latlong_key$island_yn <- out_yn

# Plot check
g <- ggplot(latlong_key, aes(x=long_dd, y=lat_dd, color=island_yn)) +
  geom_sf(data=buffer, fill=NA, inherit.aes = F) +
  geom_point() +
  coord_sf(xlim=c(-121, -116), ylim=c(32, 35)) +
  theme_bw()
g

# Add island yes/no
set_key8 <- set_key7 %>% 
  left_join(latlong_key)


# Final inspection
################################################################################

# Inspect
freeR::complete(set_key8)

# Net type
table(set_key8$net_type)

# Net type
table(set_key8$target_spp)

# Ports
table(set_key8$port_depart)
table(set_key8$port_return)

# Impute type
table(set_key8$gps_type)
table(set_key8$mesh_size_in_type)
table(set_key8$soak_hr_type)
table(set_key8$depth_type)


# Export
################################################################################

# Export
saveRDS(set_key8, file.path(outdir, "1983_2017_gillnet_observer_metadata_all_no_karin.Rds"))

