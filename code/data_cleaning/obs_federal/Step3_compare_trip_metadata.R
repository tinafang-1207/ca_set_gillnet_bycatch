

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

# Read data
data_orig1 <- readRDS(file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))
data_orig2 <- readRDS(file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips_jim.Rds"))


# Compare quantitative values
################################################################################

# Format original key for merge
data1q <- data_orig1 %>% 
  select(trip_id, set_num, lat_dd_haul, long_dd_haul, depth_fa_haul, sst_f_haul, beaufort_haul, soak_hr) %>% 
  rename(set=set_num) %>% 
  mutate(depth_m_haul=measurements::conv_unit(depth_fa_haul, "fathom", "m")) %>% 
  select(-depth_fa_haul) %>% 
  gather(key="variable", value="value", 3:ncol(.))

# Format Jim key for merge
data2q <- data_orig2 %>% 
  mutate(depth_m=abs(depth_m)) %>% 
  rename(depth_m_haul=depth_m,
         lat_dd_haul=lat_dd, 
         long_dd_haul=long_dd) %>% 
  select(trip_id, set, lat_dd_haul, long_dd_haul, depth_m_haul, sst_f_haul, beaufort_haul, soak_hr) %>% 
  gather(key="variable", value="value", 3:ncol(.))

# Merge
data_q <- data1q %>% 
  left_join(data2q, by=c("trip_id", "set", "variable"))


# Plot correlations
ggplot(data_q, aes(x=value.x, y=value.y)) +
  facet_wrap(~variable, scales='free') +
  geom_abline(slope=1) +
  geom_point() +
  # Labels
  labs(x="Our data", y="Jim's data") +
  # Theme
  theme_bw()



# Compare categorical values
################################################################################

# Format original key for merge
data1c <- data_orig1 %>% 
  select(trip_id, set_num, date_haul, port_depart, port_return, target1_spp, target2_spp) %>% 
  rename(set=set_num) %>% 
  mutate(date_haul=as.character(date_haul)) %>% 
  gather(key="variable", value="value", 3:ncol(.))

# Format for merge
data2c <- data_orig2 %>% 
  select(trip_id, set, date_haul, port_depart, port_return, target1_spp, target2_spp) %>% 
  mutate(date_haul=as.character(date_haul)) %>% 
  gather(key="variable", value="value", 3:ncol(.))

# Merge
data_c <- data1c %>% 
  # Merge
  left_join(data2c, by=c("trip_id", "set", "variable")) %>% 
  # Check
  mutate(check=value.x==value.y)
sum(data_c$check==F, na.rm=T)




# Merge trip key
################################################################################

# Inspect
freeR::complete(data_orig1)
freeR::complete(data_orig2)

# Format
data2_add <- data_orig2 %>% 
  select(trip_id, set, 
         target1_spp_code, target1_spp, 
         target2_spp_code, target2_spp, 
         lat_dd, long_dd, 
         sst_f_set, sst_f_haul,
         beaufort_set, beaufort_haul, 
         hour_set, hour_haul, soak_hr) %>% 
  rename(target1_spp_code_jim=target1_spp_code, 
         target1_spp_jim=target1_spp,
         target2_spp_code_jim=target2_spp_code, 
         target2_spp_jim= target2_spp, 
         lat_dd_haul_jim=lat_dd, 
         long_dd_haul_jim=long_dd, 
         sst_f_set_jim=sst_f_set, 
         sst_f_haul_jim=sst_f_haul,
         beaufort_set_jim=beaufort_set, 
         beaufort_haul_jim=beaufort_haul, 
         hour_set_jim=hour_set, 
         hour_haul_jim=hour_haul, 
         soak_hr_jim=soak_hr)
  

# Add data
data <- data_orig1 %>% 
  # Add Jim data
  left_join(data2_add, by=c("trip_id", "set_num"="set")) %>% 
  # Arrange
  select(season, trip_id, set_num, set_id, 
         vessel,vessel_plate, vessel_permit, 
         port_depart, port_return, 
         date_haul,
         hour_set_jim, hour_haul_jim, 
         soak_hr, soak_hr_est, soak_hr_jim,
         obs_perc, 
         target1_spp_code, target1_spp, target1_spp_code_jim, target1_spp_jim,
         target2_spp_code, target2_spp, target2_spp_code_jim, target2_spp_jim, 
         pos_code_haul, lat_dd_haul, long_dd_haul, lat_dd_haul_jim, long_dd_haul_jim,
         depth_fa_haul, 
         temp_device_haul, sst_f_set_jim, sst_f_haul, sst_f_haul_jim, 
         beaufort_set_jim, beaufort_haul, beaufort_haul_jim,
         sn_vals_n, fn_vals_n, net_type, perc_obs,
         net_hang_length_in, net_mesh_size_in, net_suspender_length_in, net_extender_length_in, net_perc_slack,
         net_n_meshes_hang, net_material_strength_lbs, net_mesh_panel_length_fathoms, net_depth_in_mesh_n, net_color_code,
         net_hang_line_material_code, net_material_code, net_material_strength_code, 
         everything()) %>% 
  # Keep columns from Jim that are unique to Jim
  rename(hour_set=hour_set_jim, 
         hour_haul=hour_haul_jim,
         sst_f_set=sst_f_set_jim,
         beaufort_set=beaufort_set_jim) %>% 
  # Fill potential data gaps
  mutate(soak_hr=ifelse(!is.na(soak_hr), soak_hr, soak_hr_jim),
         target1_spp_code=ifelse(!is.na(target1_spp_code), target1_spp_code, target1_spp_code_jim),
         target1_spp=ifelse(!is.na(target1_spp), target1_spp, target1_spp_jim),
         target2_spp_code=ifelse(!is.na(target2_spp_code), target2_spp_code, target2_spp_code_jim),
         target2_spp=ifelse(!is.na(target2_spp), target2_spp, target2_spp_jim),
         sst_f_haul=ifelse(!is.na(sst_f_haul), sst_f_haul, sst_f_haul_jim),
         lat_dd_haul=ifelse(!is.na(lat_dd_haul), lat_dd_haul, lat_dd_haul_jim),
         long_dd_haul=ifelse(!is.na(long_dd_haul), long_dd_haul, long_dd_haul_jim),
         beaufort_haul=ifelse(!is.na(beaufort_haul), beaufort_haul, beaufort_haul_jim))
        
# Inspect
freeR::complete(data)

# Remove duplicate columns
data_out <- data %>% 
  select(-c(target1_spp_code_jim, target1_spp_jim,
            target2_spp_code_jim, target2_spp_jim,
            lat_dd_haul_jim, long_dd_haul_jim, soak_hr_jim,
            sst_f_haul_jim, beaufort_haul_jim))

# Inspect
freeR::complete(data_out)

# Export
saveRDS(data_out, file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips_merged.Rds"))
