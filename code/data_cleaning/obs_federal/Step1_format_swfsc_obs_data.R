

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


# Format counts (data 1)
################################################################################

# Categories recorded differently (protect spp)
protected_catg <- c("Marine mammals", "Sea turtles", "Seabirds")

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
  left_join(spp_key %>% select(spp_code, comm_name, sci_name, category), by="spp_code") %>%
  # Fill in missing species
  mutate(comm_name_orig=ifelse(spp_code=="152", "Shark, Spiny Dogfish", comm_name_orig),
         comm_name=ifelse(spp_code=="152", "Spiny dogfish shark", comm_name),
         sci_name=ifelse(spp_code=="152", "Squalus suckleyi", sci_name),
         category=ifelse(spp_code=="152", "Fish", category)) %>%
  # Format sex
  mutate(sex=ifelse(sex=="", "Unknown", sex),
         sex=recode(sex,
                    "U"="Unknown",
                    "M"="Male",
                    "F"="Female")) %>%
  # Format condition
  mutate(condition=ifelse(category %in% protected_catg & (condition=="" | is.na(condition)), "Unknown", condition)) %>%
  # Format tag
  mutate(tag_yn=recode(tag_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Format marine mammal damage?
  mutate(mammal_damage_yn=recode(mammal_damage_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Record marine mammals in n caught column
  mutate(n_caught=ifelse(category %in% protected_catg & is.na(n_caught), 1, n_caught),
         n_returned_dead=ifelse(!is.na(condition) & condition=="Dead" & is.na(n_returned_dead), 1, n_returned_dead),
         n_returned_alive=ifelse(!is.na(condition) & condition=="Alive" & is.na(n_returned_alive), 1, n_returned_alive)) %>% 
  # Check totals
  mutate(n_caught_calc=n_kept+n_returned_alive+n_returned_dead+n_returned_unknown,
         n_caught_diff=n_caught-n_caught_calc) %>%
  # Remove b/c pass check
  # select(-c(n_caught_calc, n_caught_diff)) %>%
  # Build set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Arrange
  select(trip_id, set_num, set_id,
         category, spp_code, comm_name_orig, comm_name, sci_name,
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


# Format length compositions (data 2)
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
  select(trip_id, set_num, set_id, spp_code, everything()) %>%
  arrange(trip_id, set_num, set_id, spp_code)

# Inspect
str(data2)
freeR::complete(data2)

# Inspect columns
# sort(unique(data2$condition)) ## ALWAYS EMPTY
# sort(unique(data2$measurement_units)) ## EMPTY on "cm"
sort(unique(data2$disposition))

# Inspect species key
spp_key_check2 <- data2 %>%
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


# Format set metadata (data 3)
################################################################################

# MAJOR ASSUMPTION
# We assume that sets without a set type (no net metadata) that target known set gillnet species primarily or secondarily are set nets

# Known set net target species
set_target_spp <- c("California halibut", "White seabass", "Pacific angel shark")

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
  # Format target species
  mutate(target1_spp=wcfish::reverse_names(target1_spp),
         target2_spp=wcfish::reverse_names(target2_spp)) %>% 
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
         net_extender_length_in=ifelse(net_type=="set", set_net_extender_length_feet, float_net_extender_length_feet),
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
  # Add set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Arrange
  select(season, trip_id, set_num, set_id, everything()) %>%
  arrange(season, trip_id, set_num, set_id)

# Inspect
str(data3)
freeR::complete(data3)

# Dates
range(data3$date_haul1)

# Ports
table(data3$port_depart)
table(data3$port_return)

# SST/GPS devices
table(data3$haul_temp_device) # don't know what these codes mean
table(data3$haul_pos_code) # don't know what these codes mean

# Target species
table(data3$target1_spp)
table(data3$target2_spp)

# Net type
table(data3$net_type)

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
table(data3$net_material_code) # don't know what these codes mean
table(data3$net_hang_line_material_code)  # natural / synthetic???
table(data3$net_material_strength_code) # pounds / size??

# Vessel key
vessel_key <- data3 %>% 
  select(vessel_plate, vessel) %>% 
  unique()
freeR::which_duplicated(vessel_key$vessel)
freeR::which_duplicated(vessel_key$vessel_plate) # unique identifier
saveRDS(vessel_key, file=file.path(outdir, "SWFSC_observer_vessel_key.Rds"))


# Export data
saveRDS(data3, file=file.path(outdir, "SWFSC_1990_2017_set_net_observer_trips.Rds"))



