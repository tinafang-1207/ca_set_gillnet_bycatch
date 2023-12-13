

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

# Format key
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

# Format key
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
  # Arrange
  select(dataset, vessel_id, date, trip_id, set_num, set_id, everything())

# Inspect
freeR::complete(set_key_orig)

# Inspect
g <- ggplot(set_key_orig, aes(x=long_dd, y=lat_dd, color=net_type)) +
  geom_point()
g


# Impute data
################################################################################

# Fill lat/long
set_key1 <- set_key_orig %>% 
  group_by(vessel_id, port_depart, net_type) 

# Inspect
freeR::complete(set_key1)


