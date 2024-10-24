

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
cdfw_orig <- readRDS(file.path(cdfwdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))
cdfw_key_orig <- readRDS(file.path(cdfwdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))

# Read SWFSC data
swfscdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"
swfsc_orig <- readRDS(file.path(swfscdir, "SWFSC_set_net_observer_data.Rds"))
swfsc_key_orig <- readRDS(file.path(swfscdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))

# Read species key
keydir <- "data/keys"
spp_key <- read.csv(file.path(keydir, "species_key_final.csv"), as.is=T) %>% 
  rename(spp_type=type)

# Read logbook data
logs <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/CDFW_1981_2020_gillnet_logbook_data_imputed.Rds")
log_set_key <- logs %>% 
  select(trip_id, net_type, mesh_in) %>% 
  unique()


# Format keys
################################################################################

# Format key
cdfw_key <- cdfw_key_orig %>% 
  # Add dataset
  mutate(dataset="State") %>% 
  # Reduce
  select(dataset, set_id, vessel_id, date, port_depart, port_landing, net_type, target_spp, 
         lat_dd, long_dd, block_id, bottom_depth_fa, 
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
  select(dataset, set_id, vessel_plate, date_haul1, port_depart, port_return, target1_spp, soak_hr,
         haul_lat_dd, haul_long_dd, block_id, haul_depth_fa, net_type, net_mesh_panel_length_fathoms, net_mesh_size_in) %>% 
  # Rename
  rename(vessel_id=vessel_plate, 
         date=date_haul1, 
         target_spp=target1_spp,
         lat_dd=haul_lat_dd,
         long_dd=haul_long_dd,
         depth_fa=haul_depth_fa,
         net_length_fa=net_mesh_panel_length_fathoms,
         mesh_size_in=net_mesh_size_in)


# Merge keys
set_key_orig <- bind_rows(cdfw_key, swfsc_key) %>% 
  # Arrange
  select(dataset, vessel_id, date, set_id, everything())

# Inspect
freeR::complete(set_key_orig)







# Format SWFSC data
################################################################################

# Prepare for merge
cdfw <- cdfw_orig %>% 
  # Add data
  mutate(dataset="CDFW") %>% 
  # Remove some attribute
  select(-c(m_file_link, s_file_link,
            n_damaged, n_kept_sublegal, n_sold)) %>% 
  # Add set attributes
  left_join(cdfw_key, by="set_id") %>% 
  # Arrange
  select(dataset, set_id, date, vessel_id, set_num, 
         port_depart, port_return, target_spp, net_type, 
         block_id, lat_dd, long_dd,  depth_fa, 
         net_length_fa, mesh_size_in, soak_hr,
         spp_code_chr, comm_name, n_caught, n_discarded_dead, n_discarded_alive, n_kept,
         everything())
         
# Prepare for merge
swfsc <- swfsc_orig %>% 
  # Add data
  mutate(dataset="SWFSC") %>% 
  # Remove some
  select(-c(trip_id, comm_name_orig, sci_name)) %>% 
  # Rename some
  rename(spp_code_chr=spp_code,
         n_discarded_dead=n_returned_dead, 
         n_discarded_alive=n_returned_alive,
         n_discarded_unknown=n_returned_unknown) %>% 
  # Add set attributes
  left_join(swfsc_key, by="set_id") %>% 
  # Arrange
  select(dataset, set_id, date, vessel_id, set_num, 
         port_depart, port_return, target_spp, net_type, 
         block_id, lat_dd, long_dd, depth_fa, 
         net_length_fa, mesh_size_in, soak_hr, 
         spp_code_chr, comm_name, n_caught, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_kept,
         everything())



# Merge data
################################################################################

# Merge data
data <- bind_rows(cdfw, swfsc) %>% 
  # Add variables
  mutate(year=lubridate::year(date)) %>% 
  # Arrange
  select(dataset, set_id, year, date, everything()) %>% 
  # Format port
  mutate(port_depart=recode(port_depart,
                            "Santa Barbara Harbor"="Santa Barbara"),
         port_return=recode(port_return,
                            "Santa Barbara Harbor"="Santa Barbara")) %>%
  # Format target species
  mutate(target_spp=recode(target_spp,
                           "Unidentified rockfish"="Unspecified rockfish",
                           "Unidentified shark"="Unspecified shark")) %>% 
  # Add species type
  left_join(spp_key, by="comm_name") %>% 
  # Arrange
  arrange(year, date, vessel_id) %>% 
  select(dataset, set_id, year, date, vessel_id, set_num, 
         port_depart, port_return, target_spp, net_type, 
         block_id, lat_dd, long_dd, depth_fa, 
         net_length_fa, mesh_size_in, soak_hr,
         spp_code_chr, comm_name, spp_type, n_caught, n_discarded_dead, n_discarded_alive, n_discarded_unknown, n_kept,
         everything())

# Inspect completeness
freeR::complete(data)
str(data)

# Ports
table(data$port_depart)
table(data$port_return)

# Target species
table(data$target_spp)

# Net type
table(data$net_type)

# Inspect species key
spp_key_check <- data %>% 
  select(spp_code_chr, comm_name, spp_type) %>% 
  unique()
freeR::which_duplicated(spp_key_check$comm_name) # MUST BE ZERO

# Vessel id
vessel_key <- data %>% 
  select(vessel_id) %>% 
  unique()


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "1983_2017_gillnet_observer_data.Rds"))

