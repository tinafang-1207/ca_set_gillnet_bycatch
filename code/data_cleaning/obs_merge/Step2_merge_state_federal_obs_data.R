

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

# Read meta-data
trip_key_orig <- readRDS(file.path(outdir, "1983_2017_gillnet_observer_metadata_all.Rds"))

# Read data
cdfw_orig <- readRDS(file.path(cdfwdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))
swfsc_orig <- readRDS(file.path(swfscdir, "SWFSC_set_net_observer_data_summed.Rds"))

# Read species key
spp_key <- read.csv(file.path(keydir, "species_key_final.csv"), as.is=T) %>% 
  rename(spp_type=type)


# Merge data 
################################################################################

# Prepare for merge
cdfw <- cdfw_orig %>% 
  # Add data
  mutate(dataset="State") %>% 
  # Remove some attributes
  select(-c(date, vessel_id, set_num,
            n_damaged, n_kept_sublegal, n_sold)) %>% 
  # Add column
  mutate(n_discarded_unknown=0) %>% 
  # Arrange
  select(dataset, set_id,
         spp_code_chr, comm_name, 
         n_caught, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_kept,
         everything())

# Inspect
freeR::complete(cdfw)


# Prepare for merge
swfsc <- swfsc_orig %>% 
  # Add data
  mutate(dataset="Federal") %>% 
  # Remove some
  select(-c(trip_id, sci_name, set_num, category)) %>% 
  # Rename some
  rename(spp_code_chr=spp_code,
         n_discarded_dead=n_returned_dead, 
         n_discarded_alive=n_returned_alive,
         n_discarded_unknown=n_returned_unknown) %>% 
  # Arrange
  select(dataset, set_id, 
         spp_code_chr, comm_name, 
         n_caught, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_kept,
         everything())

# Inspect
freeR::complete(swfsc)

# Merge data
data1 <- bind_rows(cdfw, swfsc)

# Inspect
freeR::complete(data1)


# Add meta-data
################################################################################

# Add meta-data
data2 <- data1 %>% 
  # Add meta-data
  left_join(trip_key_orig %>% select(-c(dataset))) %>% 
  # Add species category
  left_join(spp_key %>% select(comm_name, spp_type)) %>% 
  # Arrange
  select(dataset,
         year, month, yday, date, 
         vessel_id, trip_id, set_id, set_num, 
         net_type, target_spp, 
         port_depart, port_return, 
         gps_type, lat_dd, long_dd, block_id, shore_km, island_yn,
         depth_type, depth_fa, depth_fa_imputed,       
         net_length_fa, mesh_size_in_type, mesh_size_in, soak_hr, soak_hr_type,            
         spp_type, comm_name, spp_code_chr,
         n_caught:n_kept,
         everything())
 
# Inspect completeness
freeR::complete(data2)


# Export data
################################################################################

# Export data
saveRDS(data2, file=file.path(outdir, "1983_2017_gillnet_observer_data.Rds"))

