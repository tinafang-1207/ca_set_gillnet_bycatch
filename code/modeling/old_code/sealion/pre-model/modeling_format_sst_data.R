
#### clean working environment ####
rm(list = ls())

#### load in packages ####
library(rerddap)
library(tidyverse)


# Read in observer data
obs_data <- read.csv("data/confidential/processed/fig2_total_merge_no_sst_bathy.csv") %>%
  mutate(date = lubridate::ymd(date))

datasets_grids <- rerddap::ed_datasets(which="griddap", url="https://upwell.pfeg.noaa.gov/erddap/")
datasets_tables <-rerddap::ed_datasets(which="tabledap", url="https://upwell.pfeg.noaa.gov/erddap/")

data_info <- rerddap::info("ncdcOisst21Agg_LonPM180")

# Download data
#################################################################

#### download data for 1981 - 1989 ####
OISST_dat_1980_orig <- rerddap::griddap(datasetx=data_info, 
                                   time = c("1983-04-23", "1989-12-31"),
                                   longitude = c(-123, -117), latitude = c(32, 38), 
                                   zlev = c(0, 0),
                                   fields="sst")

#### download data from 1990 - 1998 ####
OISST_dat_1990_orig <- rerddap::griddap(datasetx=data_info, 
                                        time = c("1990-01-01", "1998-12-31"),
                                        longitude = c(-123, -117), latitude = c(32, 38), 
                                        zlev = c(0, 0),
                                        fields="sst")

#### download data from 1999 - 2017 ####
OISST_dat_2000_orig <- rerddap::griddap(datasetx=data_info, 
                                        time = c("1999-01-01", "2017-12-31"),
                                        longitude = c(-123, -117), latitude = c(32, 38), 
                                        zlev = c(0, 0),
                                        fields="sst")


# Format data
#################################################################

# Format data in 1980
OISST_dat_1980_df <-OISST_dat_1980_orig$data

data_1980 <- OISST_dat_1980_df %>% 
  # Rename
  rename(date=time) %>% 
  select(longitude, latitude, date, sst) %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  # Format date
  mutate(date=gsub("T12:00:00Z", "", date))
  
# Inspect data
head(data_1980)

# save data
saveRDS(data_1980, file = file.path("data/gis_data/OISST_1980.RDS"))

# Format data in 1990
OISST_dat_1990_df <-OISST_dat_1990_orig$data

data_1990 <- OISST_dat_1990_df %>% 
  # Rename
  rename(date=time) %>% 
  select(longitude, latitude, date, sst) %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  # Format date
  mutate(date=gsub("T12:00:00Z", "", date))

# Inspect data
head(data_1990)

# save data
saveRDS(data_1990, file = file.path("data/gis_data/OISST_1990.RDS"))


# Format data in 2000
OISST_dat_2000_df <- OISST_dat_2000_orig$data

data_2000 <- OISST_dat_2000_df %>% 
  # Rename
  rename(date=time) %>% 
  select(longitude, latitude, date, sst) %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  # Format date
  mutate(date=gsub("T12:00:00Z", "", date))

# Inspect data
head(data_2000)

# save data
saveRDS(data_2000, file = file.path("data/gis_data/OISST_2000.RDS"))



# Extract data
#################################################################
# Turn data into raster
data_1980_ras <- data_1980 %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")

data_1990_ras <- data_1990 %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")

data_2000_ras <- data_2000 %>%
  spread(key="date", value="sst") %>%
  raster::rasterFromXYZ(., crs="+proj=longlat +datum=WGS84")

# Extract unique coordinates from original observer data
obs_data_1980_xy <- obs_data %>%
  filter(data_source == "CDFW(1983-1989)") %>%
  select(haul_lat_dd, haul_long_dd) %>%
  unique()

obs_data_1990_xy <- obs_data %>%
  filter(data_source == "SWFSC(1990-1994)") %>%
  select(haul_lat_dd, haul_long_dd) %>%
  filter(!is.na(haul_lat_dd)) %>%
  unique()

obs_data_2000_xy <- obs_data %>%
  filter(data_source == "SWFSC(1999-2017)") %>%
  select(haul_lat_dd, haul_long_dd) %>%
  filter(!is.na(haul_lat_dd)) %>%
  unique()

# Turn coordinates into spatial point (sp)
data_1980_xy_sp <- obs_data_1980_xy %>% 
  # Convert to sf
  sf::st_as_sf(., coords=c("haul_long_dd", "haul_lat_dd"), crs="+proj=longlat +datum=WGS84") %>% 
  # Convert to spatial points
  sf::as_Spatial()

data_1990_xy_sp <- obs_data_1990_xy %>% 
  # Convert to sf
  sf::st_as_sf(., coords=c("haul_long_dd", "haul_lat_dd"), crs="+proj=longlat +datum=WGS84") %>% 
  # Convert to spatial points
  sf::as_Spatial()

data_2000_xy_sp <- obs_data_2000_xy %>% 
  # Convert to sf
  sf::st_as_sf(., coords=c("haul_long_dd", "haul_lat_dd"), crs="+proj=longlat +datum=WGS84") %>% 
  # Convert to spatial points
  sf::as_Spatial()

# Extract temperature data from raster
data_sst_1980 <- raster::extract(x=data_1980_ras, y=data_1980_xy_sp)

data_sst_1990 <- raster::extract(x = data_1990_ras, y =data_1990_xy_sp)

data_sst_2000 <- raster::extract(x = data_2000_ras, y = data_2000_xy_sp)

# Format extracted data
data_sst_1980_df <- data_sst_1980 %>%
  as.data.frame() %>%
  mutate(haul_lat_dd = obs_data_1980_xy$haul_lat_dd, haul_long_dd = obs_data_1980_xy$haul_long_dd) %>%
  select(haul_long_dd, haul_lat_dd, everything()) %>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date))

data_sst_1990_df <- data_sst_1990 %>%
  as.data.frame() %>%
  mutate(haul_lat_dd = obs_data_1990_xy$haul_lat_dd, haul_long_dd = obs_data_1990_xy$haul_long_dd) %>%
  select(haul_long_dd, haul_lat_dd, everything()) %>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date))

data_sst_2000_df <- data_sst_2000 %>%
  as.data.frame() %>%
  mutate(haul_lat_dd = obs_data_2000_xy$haul_lat_dd, haul_long_dd = obs_data_2000_xy$haul_long_dd) %>%
  select(haul_long_dd, haul_lat_dd, everything()) %>%
  gather(key = "date", value = "sst", 3:ncol(.)) %>%
  mutate(date = gsub("X", "", date)) %>%
  mutate(date = lubridate::ymd(date))


# Join and merge data
#################################################################

# vertically join sst data
data_sst_all_years <- bind_rows(data_sst_1980_df, data_sst_1990_df, data_sst_2000_df)

# Inspect data
head(data_sst_all_years)

# Merge with original dataframe
obs_data_with_sst <- left_join(obs_data, data_sst_all_years, by = c("haul_long_dd", "haul_lat_dd", "date") )

# Export data with sst 
write.table(obs_data_with_sst, file = "data/confidential/processed/fig2_total_merge_no_bathy.csv", row.names = F, sep = ",")



