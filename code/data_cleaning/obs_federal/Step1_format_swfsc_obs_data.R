

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
data_orig1 <- read.csv(file.path(indir, "obs_setnet_catch.csv"), as.is=T, na.strings="")
data_orig2 <- read.csv(file.path(indir, "obs_setnet_measurement.csv"), as.is=T, na.strings="")
data_orig3 <- read.csv(file.path(indir, "SN_trip_SET_1990_2017.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS(file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))


# Format data 1
################################################################################

# Format data 1
data1 <- data_orig1 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_num=set_number,
         spp_code=catch_species_code,
         comm_name_orig=species_common_name,
         n_caught=total_catch_count,
         n_kept=total_kept_count,
         n_returned_alive=returned_alive_count,
         n_returned_dead=returned_dead_count,
         n_returned_unknown=returned_unknown_count,
         n_damaged_mammals=damage_by_marine_mammals_count,
         n_damaged=damage_total_count,
         tag_yn=was_tag_present,
         mammal_damage_yn=was_damaged_by_marine_mammals,
         condition=condition_description) %>%
  # Format species
  mutate(spp_code=ifelse(is.na(spp_code), "0", spp_code),
         spp_code=recode(spp_code,
                         "000"="0",
                         "001"="1",
                         "003"="3",
                         "019"="19",
                         "040"="40",
                         "050"="50",
                         "051"="51",
                         "055"="55",
                         "079"="79",
                         "080"="80",
                         "096"="96")) %>%
  # Add species info
  left_join(spp_key %>% select(spp_code, comm_name, sci_name), by="spp_code") %>%
  # Fill in missing species
  mutate(comm_name_orig=ifelse(spp_code=="152", "Shark, Spiny Dogfish", comm_name_orig),
         comm_name=ifelse(spp_code=="152", "Spiny dogfish shark", comm_name),
         sci_name=ifelse(spp_code=="152", "Squalus suckleyi", sci_name)) %>%
  # Format sex
  mutate(sex=ifelse(sex=="", "Unknown", sex),
         sex=recode(sex,
                    "U"="Unknown",
                    "M"="Male",
                    "F"="Female")) %>%
  # Format condition
  mutate(condition=ifelse(condition=="", "Unknown", condition)) %>%
  # Format tag
  mutate(tag_yn=recode(tag_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Format marine mammal damage?
  mutate(mammal_damage_yn=recode(mammal_damage_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Check totals
  mutate(n_caught_calc=n_kept+n_returned_alive+n_returned_dead+n_returned_unknown,
         n_caught_diff=n_caught-n_caught_calc) %>%
  # Remove b/c pass check
  select(-c(n_caught_calc, n_caught_diff)) %>%
  # Build set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Arrange
  select(trip_id, set_num, set_id,
         spp_code, comm_name_orig, comm_name, sci_name,
         everything()) %>%
  arrange(trip_id, set_num, set_id, spp_code)

# Inspect
str(data1)
freeR::complete(data1)

# Inspect
sort(unique(data1$tag_yn))
sort(unique(data1$mammal_damage_yn))
sort(unique(data1$condition_code))
sort(unique(data1$condition))
sort(unique(data1$sex))

# Species key
spp_key_check <- data1 %>%
  select(spp_code, comm_name) %>%
  unique() %>%
  arrange(spp_code)
freeR::which_duplicated(spp_key$spp_code)
freeR::which_duplicated(spp_key$comm_name)

# Export data
saveRDS(data1, file=file.path(outdir, "SWFSC_set_net_observer_data.Rds"))


# Format data 2
################################################################################

# Format data 2
data2 <- data_orig2 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_num=set_number,
         spp_code=catch_species_code,
         comm_name=species_common_name,
         length_cm=measurement) %>%
  # Add set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Remove useless columns
  select(-c(measurement_units, condition)) %>%
  # Arrange
  select(trip_id, set_num, set_id, everything()) %>%
  arrange(trip_id, set_num, set_id, spp_code)

# Inspect
str(data2)
freeR::complete(data2)

# Inspect columns
# sort(unique(data2$condition)) ## ALWAYS EMPTY
# sort(unique(data2$measurement_units)) ## EMPTY on "cm"
sort(unique(data2$disposition))

# Inspect species key
spp_key <- data2 %>%
  select(spp_code, comm_name) %>%
  unique()

# Plot lengths
# g <- ggplot(data2, aes(x=length_cm)) +
#   facet_wrap(~comm_name, ncol=8, scales = "free") +
#   geom_density() +
#   # Labels
#   labs(x="Length (cm)", y="Density") +
#   # Theme
#   theme_bw()
# g

# Export data
saveRDS(data2, file=file.path(outdir, "SWFSC_set_net_observer_length_comps.Rds"))


# Format data 3
################################################################################

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
         haul_temp_device=begin_haul_temp_device,
         haul_pos_code=begin_haul_position_code,
         haul_lat_dd=begin_haul_latitude,
         haul_long_dd=begin_haul_longitude,
         haul_depth_fa=begin_haul_depth,
         haul_sst_f=begin_haul_surface_temp,
         haul_beauf=begin_haul_beaufort_number) %>%
  # Format ports
  mutate(port_depart=stringr::str_to_title(port_depart),
         port_return=stringr::str_to_title(port_return)) %>%
  # Format dates
  mutate(date_haul1=lubridate::dmy(date_haul1),
         date_haul2=lubridate::dmy(date_haul2)) %>%
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
                         ifelse(fn_vals_n==0 & sn_vals_n>0, "set net",
                                ifelse(sn_vals_n==0 & fn_vals_n>0, "float net",
                                       ifelse(sn_vals_n==0 & fn_vals_n==0, "unknown", "other"))))) %>%
  # Recode net stuff
  mutate(perc_obs=ifelse(net_type=="set net", set_net_percent_described, float_net_percent_described),
         net_hang_length_in=ifelse(net_type=="set net", set_net_hanging_length_inches, float_net_hanging_length_inches),
         net_mesh_size_in=ifelse(net_type=="set net", set_net_mesh_size_inches, float_net_mesh_size_inches),
         net_suspender_length_in=ifelse(net_type=="set net", set_net_suspender_length_inches, float_net_suspender_length_inches),
         net_extender_length_in=ifelse(net_type=="set net", set_net_extender_length_feet, float_net_extender_length_feet),
         net_perc_slack=ifelse(net_type=="set net", set_net_percent_slack, float_net_percent_slack),
         net_n_meshes_hang=ifelse(net_type=="set net", set_net_number_of_meshes_hanging, float_net_number_of_meshes_hanging),
         net_material_strength_lbs=ifelse(net_type=="set net", set_net_material_strength_lbs, float_net_material_strength_lbs),
         net_mesh_panel_length_fathoms=ifelse(net_type=="set net", set_net_mesh_panel_length_fathoms, float_net_mesh_panel_length_fathoms),
         net_depth_in_mesh_n=ifelse(net_type=="set net", set_net_net_depth_in_mesh_number, float_net_net_depth_in_mesh_number),
         net_color_code=ifelse(net_type=="set net", set_net_net_color_code, float_net_net_color_code),
         net_hang_line_material_code=ifelse(net_type=="set net", set_net_net_hanging_line_mat_code, float_net_net_hanging_line_mat_code),
         net_material_code=ifelse(net_type=="set net", set_net_net_material_code, float_net_net_material_code),
         net_material_strength_code=ifelse(net_type=="set net", set_net_net_mat_strength_unit_code, float_net_net_mat_strength_unit_code)) %>%
  # Remove useless net columns
  select(-c(set_net_percent_described:float_net_net_mat_strength_unit_code)) %>%
  # Add set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Arrange
  select(season, trip_id, set_num, set_id, everything()) %>%
  arrange(season, trip_id, set_num, set_id)

# Inspect
str(data3)
freeR::complete(data3)

# Inspect
range(data3$date_haul1)
table(data3$port_depart)
table(data3$port_return)
table(data3$haul_temp_device)
table(data3$haul_pos_code)

# Coordinates
range(data3$haul_lat_dd, na.rm=T)
range(data3$haul_long_dd, na.rm=T)

# Map coordinates
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
g <- ggplot(data3, aes(x=haul_long_dd, y=haul_lat_dd)) +
  geom_sf(data=usa, fill="grey90", color="white", inherit.aes = F) +
  geom_point() +
  coord_sf(xlim=c(-122, -117), ylim=c(32, 36)) +
  theme_bw()
g

# Net characteristics
table(data3$net_type)
table(data3$net_color_code) # always empty
table(data3$net_material_code)
table(data3$net_hang_line_material_code)
table(data3$net_material_strength_code)

# Export data
saveRDS(data3, file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))



