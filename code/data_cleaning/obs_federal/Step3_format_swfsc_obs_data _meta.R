

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
list.files(indir)
data_orig3 <- read.csv(file.path(indir, "SN_trip_SET_1990_2017.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS(file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))


# Format set metadata (data 3)
################################################################################

# MAJOR ASSUMPTION
# We assume that sets without a set type (no net metadata) that target known set gillnet species primarily or secondarily are set nets

# Known set net target species
set_target_spp <- c("California halibut", "White seabass", "Pacific angel shark", "Spider crab", "White croaker", "Soupfin shark")

# Format data 3
data3 <- data_orig3 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         vessel=vessel_names,
         vessel_plate=vessel_plates,
         vessel_permit=vessel_permits,
         port_depart=departure_port_name,
         port_return=return_port_name,
         date_haul1=haul_date,
         set_num=set_number,
         soak_hr=soak_time_hrs,
         soak_hr_est=est_soak_time,
         obs_perc=percent_observed,
         target1_spp_code=primary_target_species_code,
         target1_spp=primary_target_species_name,
         target2_spp_code=secondary_target_species_code,
         target2_spp=secondary_target_species_name,
         date_haul2=begin_haul_date_time,
         temp_device_haul=begin_haul_temp_device,
         pos_code_haul=begin_haul_position_code,
         lat_dd_haul=begin_haul_latitude,
         long_dd_haul=begin_haul_longitude,
         depth_fa_haul=begin_haul_depth,
         sst_f_haul=begin_haul_surface_temp,
         beaufort_haul=begin_haul_beaufort_number) %>%
  # Format ports
  mutate(port_depart=stringr::str_to_title(port_depart),
         port_return=stringr::str_to_title(port_return)) %>%
  # Format dates
  mutate(date_haul1=lubridate::dmy(date_haul1),
         date_haul2=lubridate::dmy(date_haul2)) %>%
  # Format target species
  mutate(target1_spp=wcfish::reverse_names(target1_spp),
         target2_spp=wcfish::reverse_names(target2_spp)) %>% 
  # Format temperature
  
  # Label net type
  rowwise() %>%
  mutate(sn_vals_n=sum(!is.na(set_net_percent_described),
                       !is.na(set_net_hanging_length_inches),
                       !is.na(set_net_mesh_size_inches),
                       !is.na(set_net_suspender_length_inches),
                       !is.na(set_net_extender_length_feet),
                       !is.na(set_net_percent_slack),
                       !is.na(set_net_number_of_meshes_hanging),
                       !is.na(set_net_material_strength_lbs),
                       !is.na(set_net_mesh_panel_length_fathoms),
                       !is.na(set_net_net_depth_in_mesh_number),
                       !is.na(set_net_net_color_code),
                       !is.na(set_net_net_hanging_line_mat_code),
                       !is.na(set_net_net_material_code),
                       !is.na(set_net_net_mat_strength_unit_code)),
         fn_vals_n=sum(!is.na(float_net_percent_described),
                       !is.na(float_net_hanging_length_inches),
                       !is.na(float_net_mesh_size_inches),
                       !is.na(float_net_suspender_length_inches),
                       !is.na(float_net_extender_length_feet),
                       !is.na(float_net_percent_slack),
                       !is.na(float_net_number_of_meshes_hanging),
                       !is.na(float_net_material_strength_lbs),
                       !is.na(float_net_mesh_panel_length_fathoms),
                       !is.na(float_net_net_depth_in_mesh_number),
                       !is.na(float_net_net_color_code),
                       !is.na(float_net_net_hanging_line_mat_code),
                       !is.na(float_net_net_material_code),
                       !is.na(float_net_net_mat_strength_unit_code))) %>%
  ungroup() %>%
  mutate(net_type=ifelse(fn_vals_n>0 & sn_vals_n>0, "both",
                         ifelse(fn_vals_n==0 & sn_vals_n>0, "set",
                                ifelse(sn_vals_n==0 & fn_vals_n>0, "drift",
                                       ifelse(sn_vals_n==0 & fn_vals_n==0, "unknown", "other"))))) %>%
  # Classify unknown nets targeting California halibut as set nets
  mutate(net_type=ifelse(net_type=="unknown" & ( (!is.na(target1_spp) & target1_spp %in% set_target_spp) | (!is.na(target2_spp) & target2_spp %in% set_target_spp) ), "set", net_type)) %>% 
  # Recode net stuff
  mutate(perc_obs=ifelse(net_type=="set", set_net_percent_described, float_net_percent_described),
         net_hang_length_in=ifelse(net_type=="set", set_net_hanging_length_inches, float_net_hanging_length_inches),
         net_mesh_size_in=ifelse(net_type=="set", set_net_mesh_size_inches, float_net_mesh_size_inches),
         net_suspender_length_in=ifelse(net_type=="set", set_net_suspender_length_inches, float_net_suspender_length_inches),
         net_extender_length_ft=ifelse(net_type=="set", set_net_extender_length_feet, float_net_extender_length_feet),
         net_perc_slack=ifelse(net_type=="set", set_net_percent_slack, float_net_percent_slack),
         net_n_meshes_hang=ifelse(net_type=="set", set_net_number_of_meshes_hanging, float_net_number_of_meshes_hanging),
         net_material_strength_lbs=ifelse(net_type=="set", set_net_material_strength_lbs, float_net_material_strength_lbs),
         net_mesh_panel_length_fathoms=ifelse(net_type=="set", set_net_mesh_panel_length_fathoms, float_net_mesh_panel_length_fathoms),
         net_depth_in_mesh_n=ifelse(net_type=="set", set_net_net_depth_in_mesh_number, float_net_net_depth_in_mesh_number),
         net_color_code=ifelse(net_type=="set", set_net_net_color_code, float_net_net_color_code),
         net_hang_line_material_code=ifelse(net_type=="set", set_net_net_hanging_line_mat_code, float_net_net_hanging_line_mat_code),
         net_material_code=ifelse(net_type=="set", set_net_net_material_code, float_net_net_material_code),
         net_material_strength_code=ifelse(net_type=="set", set_net_net_mat_strength_unit_code, float_net_net_mat_strength_unit_code)) %>%
  # Remove useless net columns
  select(-c(set_net_percent_described:float_net_net_mat_strength_unit_code)) %>%
  # Replace 0 values with NAs
  mutate(net_mesh_size_in=ifelse(net_mesh_size_in==0, NA, net_mesh_size_in)) %>% 
  # Add set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Add year
  mutate(year=lubridate::year(date_haul1)) %>% 
  # Convert 0s to NAs
  mutate(depth_fa_haul=ifelse(depth_fa_haul==0, NA, depth_fa_haul),
         sst_f_haul=ifelse(sst_f_haul==0, NA, sst_f_haul),
         net_hang_length_in=ifelse(net_hang_length_in==0, NA, net_hang_length_in),
         net_extender_length_ft=ifelse(net_extender_length_ft==0, NA, net_extender_length_ft),
         net_suspender_length_in=ifelse(net_suspender_length_in==0, NA, net_suspender_length_in),
         net_n_meshes_hang=ifelse(net_n_meshes_hang==0, NA, net_n_meshes_hang),
         net_material_strength_lbs=ifelse(net_material_strength_lbs==0, NA, net_material_strength_lbs),
         net_mesh_panel_length_fathoms=ifelse(net_mesh_panel_length_fathoms==0, NA, net_mesh_panel_length_fathoms),
         net_depth_in_mesh_n=ifelse(net_depth_in_mesh_n==0, NA, net_depth_in_mesh_n)) %>% 
  # Recode codes
  mutate(net_material=recode(net_material_code, 
                             "MLT"="multifilament",
                             "MON"="monofilament",
                             "TWM"="twisted monofilament")) %>% 
  mutate(net_hang_line_material_code=recode(net_hang_line_material_code, 
                                           "NAT"="natural",
                                           "SYN"="synthetic")) %>% 
  # Arrange
  select(year, season, trip_id, set_num, set_id, 
         vessel, vessel_plate, vessel_permit,
         port_depart, port_return,
         date_haul1, date_haul2, 
         soak_hr, soak_hr_est, 
         target1_spp_code, target1_spp, 
         target2_spp_code, target2_spp, 
         pos_code_haul, lat_dd_haul, long_dd_haul, depth_fa_haul,
         temp_device_haul, sst_f_haul, beaufort_haul, 
         everything()) %>%
  arrange(year, season, trip_id, set_num, set_id)

# Inspect
str(data3)
freeR::complete(data3)

# Dates
range(data3$date_haul1)
range(data3$date_haul2)
sum(data3$date_haul1!=data3$date_haul2) # haul dates are identical so you can remove

# Are season and year identical?
sum(data3$season!=data3$year) # no, remove season since you could derive later?

# Ports
table(data3$port_depart)
table(data3$port_return)

# SST/GPS devices
table(data3$temp_device_haul) # OTR-Other, SPL-spirit?, VSL-Vessel temperature gauge
table(data3$pos_code_haul) # 1-LOR = Loran, 2-DED = Dead reckoning, 3-SAT = GPS, 4-VER = Verbal

# Target species
table(data3$target1_spp)
table(data3$target2_spp)

# Net type
table(data3$net_type)
range(data3$net_mesh_size_in, na.rm=T)

# Coordinates
range(data3$lat_dd_haul, na.rm=T)
range(data3$long_dd_haul, na.rm=T)

# Map coordinates
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
g <- ggplot(data3, aes(x=long_dd_haul, y=lat_dd_haul)) +
  geom_sf(data=usa, fill="grey90", color="white", inherit.aes = F) +
  geom_point() +
  coord_sf(xlim=c(-122, -117), ylim=c(32, 36)) +
  theme_bw()
g

# Net characteristics
table(data3$net_type)
table(data3$net_color_code) # green (1), red (2), blue (3), brown (4), other (5)
table(data3$net_material_code) # monofilament (1) / multifilament (2) / combination (3) / twisted monofilament (4)
table(data3$net_hang_line_material_code)  # synthetic (1) / natural (2)
table(data3$net_material_strength_code) # pounds test (1) / twine size (2)

# Quantitative net characteristics
boxplot(data3$net_suspender_length_in)


# Net / target species key
net_targ_key <- data3 %>% 
  count(target1_spp, net_type)

# Vessel key
vessel_key <- data3 %>% 
  select(vessel_plate, vessel) %>% 
  unique()
freeR::which_duplicated(vessel_key$vessel)
freeR::which_duplicated(vessel_key$vessel_plate) # unique identifier
saveRDS(vessel_key, file=file.path(outdir, "SWFSC_observer_vessel_key.Rds"))


# Export data
################################################################################

# Slighlty simplify
data3_out <- data3 %>% 
  select(-c(year, date_haul2)) %>% 
  rename(date_haul=date_haul1)

# Export data
saveRDS(data3_out, file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))


