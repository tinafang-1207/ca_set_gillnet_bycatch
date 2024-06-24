

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Export directory
keydir <- "data/keys"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge"
cdfwdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/processed"
swfscdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"


# Read data
data_orig <- readRDS(file=file.path(outdir, "1983_2017_gillnet_observer_data.Rds"))

# Read SST data
sst1 <- readRDS(file.path("data/gis_data/OISST_1980.Rds"))
sst2 <- readRDS(file.path("data/gis_data/OISST_1990.Rds"))
sst3 <- readRDS(file.path("data/gis_data/OISST_2000.Rds"))
sst4 <- readRDS(file.path("data/gis_data/OISST_2018.Rds"))

# Merge SST data
sst <- bind_rows(sst1, sst2, sst3, sst4) %>% 
  mutate(date=lubridate::ymd(date)) %>% 
  na.omit()




# Build date/location key
################################################################################

# Build key
data_coords <- data_orig %>% 
  select(date, long_dd, lat_dd) %>% 
  unique() %>% 
  na.omit()

# Function to find closest coordinates
coords <- data_coords[1,]
find_closest_coords <- function(coords) {
  
  # Params
  date_do <- coords$date
  xy <- coords[,c("long_dd", "lat_dd")] %>% as.numeric()
  
  # Eligible coordinates
  lookup_mat <- sst %>% 
    filter(date==date_do) %>% 
    select(longitude, latitude) %>% 
    as.matrix()
  
  # Compute distances
  distances <- geosphere::distm(xy, lookup_mat)
  
  # Select closest distance
  closest_index <- which.min(distances)
  
  # Extract closest coordinates
  coords_out <- lookup_mat[closest_index, ] %>% as.numeric()
  
  # Return
  return(coords_out)
  
}

# Add closest coordinates to sst_key
data_coords$closest_long <- NA
data_coords$closest_lat <- NA
for (i in 1:nrow(data_coords)) {
  closest_coords <- find_closest_coords(data_coords[i, ])
  data_coords$closest_long[i] <- closest_coords[1]
  data_coords$closest_lat[i] <- closest_coords[2]
}

# Build sst key
sst_key <- data_coords %>% 
  # Rename
  rename(long_dd_sst=closest_long,
         lat_dd_sst=closest_lat) %>% 
  # Add temperature on that day at the closest SST point
  left_join(sst, by=c("date", "long_dd_sst"="longitude", "lat_dd_sst"="latitude")) %>% 
  rename(sst_c=sst)

# Inspect
freeR::complete(sst_key)  

# Add SST to data
data <- data_orig %>% 
  left_join(sst_key %>% select(date, lat_dd, long_dd, sst_c))

# Inspect
freeR::complete(data)
table(data$dataset)

# Export
saveRDS(data, file=file.path(file=file.path(outdir, "1983_2017_gillnet_observer_data_with_sst.Rds")))


# # Old faster approach
# ################################################################################
# 
# # SST lat/longs
# sst_coords <- sst1 %>% 
#   select(longitude, latitude) %>% 
#   unique() %>% 
#   rename(long_dd_sst=longitude,
#          lat_dd_sst=latitude)
# 
# # Build key
# data_coords <- data_orig %>% 
#   select(long_dd, lat_dd) %>% 
#   unique() %>% 
#   na.omit()
# 
# # Function to find closest coordinates
# find_closest_coords <- function(key_row, coords_df) {
#   distances <- geosphere::distm(c(key_row["long_dd"], key_row["lat_dd"]) %>% as.numeric(), coords_df %>% as.matrix())
#   closest_index <- which.min(distances)
#   return(coords_df[closest_index, ])
# }
# 
# # Add closest coordinates to sst_key
# data_coords$closest_long <- NA
# data_coords$closest_lat <- NA
# for (i in 1:nrow(data_coords)) {
#   closest_coords <- find_closest_coords(data_coords[i, ], sst_coords)
#   data_coords[i, c("closest_long", "closest_lat")] <- closest_coords
# }
# 
# # Build sst key
# sst_key <- data_orig %>% 
#   # Unique GPS/date combos
#   select(date, long_dd, lat_dd) %>% 
#   unique() %>% 
#   na.omit() %>% 
#   # Add closest SST coordinates
#   left_join(data_coords) %>% 
#   rename(long_dd_sst=closest_long,
#          lat_dd_sst=closest_lat) %>% 
#   # Add temperature on that day at the closest SST point
#   left_join(sst, by=c("date", "long_dd_sst"="longitude", "lat_dd_sst"="latitude"))
# 
# # Inspect
# freeR::complete(sst_key)  

