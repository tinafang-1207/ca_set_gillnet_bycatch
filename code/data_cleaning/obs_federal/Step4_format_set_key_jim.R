

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"
plotdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/figures"

# Load Jim data
load(file.path(indir, "carretta_metadata/TripSetData.to.UCSB.RData"))
key_jim_orig <- TripSetData.to.UCSB
rm(TripSetData.to.UCSB)

# Read port key
port_key <- readxl::read_excel(file.path(indir, "SWFSC_observer_program_port_codes.xlsx"))

# Read species key
spp_key <- readRDS(file.path(outdir, "SWFSC_observer_program_spp_key.Rds")) %>% 
  filter(!is.na(spp_code))


#  Format Jim data
################################################################################

# Format Jim data
key_jim <- key_jim_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(set_id=trip_set,
         trip_id=trip_number, 
         port_depart_code=port_dep,
         port_return_code=port_rtn,
         target1_spp_code=tgt1,
         target2_spp_code=tgt2,
         lat_dd=lat,
         long_dd=lon, 
         beaufort_set=beau_set,
         beaufort_haul=beau_pul,
         sst_f_set=temp_set,
         sst_f_haul=temp_pul,
         hour_set=hh_set,
         hour_haul=hh_pul,
         soak_hr=soak_hrs) %>% 
  # Add ports
  left_join(port_key, by=c('port_depart_code'="port_code")) %>% 
  rename(port_depart=port) %>% 
  left_join(port_key, by=c('port_return_code'="port_code")) %>% 
  rename(port_return=port) %>% 
  # Target species 1
  mutate(target1_spp_code=as.character(target1_spp_code)) %>% 
  left_join(spp_key %>% select(spp_code, comm_name), by=c("target1_spp_code"="spp_code")) %>% 
  rename(target1_spp=comm_name) %>% 
  # Target species 2
  mutate(target2_spp_code=as.character(target2_spp_code)) %>% 
  left_join(spp_key %>% select(spp_code, comm_name), by=c("target2_spp_code"="spp_code")) %>% 
  rename(target2_spp=comm_name) %>% 
  # Spiny dogfish shark	is missing from key, add manually
  mutate(target2_spp=ifelse(target2_spp_code=="152", "Spiny dogfish shark", target2_spp)) %>% 
  # Arrange
  select(set_id, trip_id, set, date_haul,
         port_depart_code, port_depart,
         port_return_code, port_return,
         target1_spp_code, target1_spp,
         target2_spp_code, target2_spp,
         lat_dd, long_dd, depth_m, 
         sst_f_set, sst_f_haul,
         beaufort_set, beaufort_haul,
         everything())

# Inspect data
str(key_jim)
freeR::complete(key_jim)

# Date
range(key_jim$date_haul)

# Target species
targ_spp1 <- key_jim %>% 
  select(target1_spp_code, target1_spp) %>% unique()
targ_spp2 <- key_jim %>% 
  select(target2_spp_code, target2_spp) %>% unique()


# Ports
port1 <- key_jim %>% 
  select(port_depart_code, port_depart) %>% unique()
port2 <- key_jim %>% 
  select(port_return_code, port_return) %>% unique()


# Plot data
g <- ggplot(key_jim, aes(x=long_dd, y=lat_dd)) +
  geom_point()
g

# Export
saveRDS(key_jim, file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips_jim.Rds"))

