
#### clean working environment ####
rm(list = ls())

#### load in packages ####
library(rerddap)
library(tidyverse)

# Prepare to download data
##########################################################

datasets_grids <- rerddap::ed_datasets(which="griddap", url="https://upwell.pfeg.noaa.gov/erddap/")
datasets_tables <-rerddap::ed_datasets(which="tabledap", url="https://upwell.pfeg.noaa.gov/erddap/")

data_info <- rerddap::info("ncdcOisst21Agg_LonPM180")

# Download data
##########################################################

#### download data for 1981 - 1989 ####
OISST_dat_1980_orig <- rerddap::griddap(datasetx=data_info, 
                                        time = c("1981-09-01", "1989-12-31"),
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

#### download data from 2018 - 2022 ####
OISST_dat_2018_orig <- rerddap::griddap(datasetx=data_info, 
                                        time = c("2018-01-01", "2022-04-01"),
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

# Format data in 2018
OISST_dat_2018_df <- OISST_dat_2018_orig$data

data_2018 <- OISST_dat_2018_df %>% 
  # Rename
  rename(date=time) %>% 
  select(longitude, latitude, date, sst) %>%
  mutate(longitude = as.numeric(longitude), latitude = as.numeric(latitude)) %>%
  # Format date
  mutate(date=gsub("T12:00:00Z", "", date))

# Inspect data
head(data_2018)

# save data
saveRDS(data_2018, file = file.path("data/gis_data/OISST_2018.RDS"))

# Combine data
#################################################################

data_sst_all_years <- bind_rows(data_1980, data_1990, data_2000, data_2018)

saveRDS(data_sst_all_years, file = file.path("data/gis_data/data_sst_1980_2022.RDS"))








