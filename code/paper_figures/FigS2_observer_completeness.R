

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir1 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/processed"

# Read data
data_orig1 <- readRDS(file.path(datadir2, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))
data_orig2 <- readRDS(file.path(datadir1, "SWFSC_1990_2017_set_net_observer_trips_merged.Rds"))


# Format and merge data
################################################################################

# Format state
colnames(data_orig1)
data1 <- data_orig1 %>% 
  # Simplify
  select(set_id, target_spp, 
         lat_dd, long_dd, 
         bottom_depth_fa,
         date,
         soak_hr, 
         mesh_size1_in,
         net_length_fa, 
         net_depth_nmeshes, hanging_length_in, 
         suspender_length_ft, extender_length_ft,
         net_orientation, net_material, hratio) %>% 
  # Rename
  rename(mesh_size_in=mesh_size1_in, 
         net_extender_length_ft=extender_length_ft,
         depth_fa=bottom_depth_fa,
         suspender_length=suspender_length_ft,
         hanging_ratio=hratio)

# Format federal
colnames(data_orig2)
data2 <- data_orig2 %>% 
  # Simplify
  select(set_id, target1_spp, 
         lat_dd_haul, long_dd_haul, 
         depth_fa_haul,
         date_haul,
         soak_hr, 
         sst_f_haul, beaufort_haul,
         sst_f_set, beaufort_set,
         net_mesh_size_in,     
         net_mesh_panel_length_fathoms, 
         net_depth_in_mesh_n, net_hang_length_in,
         net_suspender_length_in, net_extender_length_ft,
         net_material, net_material_strength_lbs, net_color_code) %>% 
  # Rename
  rename(target_spp=target1_spp,
         lat_dd=lat_dd_haul,
         long_dd=long_dd_haul,
         depth_fa=depth_fa_haul,
         date=date_haul,
         net_length_fa=net_mesh_panel_length_fathoms,
         net_depth_nmeshes=net_depth_in_mesh_n,
         mesh_size_in=net_mesh_size_in,
         hanging_length_in=net_hang_length_in,
         suspender_length=net_suspender_length_in)

# Merge data
data <- bind_rows(data1, data2)
colnames(data)


# Plot data
################################################################################

# Build data
completeness <- freeR::complete(data) / nrow(data)
stats <- tibble(variable=names(completeness),
                perc=completeness) %>% 
  arrange(desc(perc)) %>% 
  # Recode
  mutate(variable=recode(variable,
                         "set_id"="Set id",
                         "target_spp"="Target species",                   
                         "lat_dd"="Latitude (°N)",                       
                         "long_dd"="Longitude (°W)",  
                         "depth_fa"="Depth (fathoms)",
                         "date"="Date",                          
                         "soak_hr"="Soak time (hr)",                
                         "net_orientation"="Net orientation",             
                         "net_material"="Net material",   
                         "net_length_fa"="Net length (fathoms)",                  
                         "net_depth_nmeshes"="Net depth (# of meshes)",              
                         "sst_f_haul"="Temperature (°F) - haul",                   
                         "beaufort_haul"="Beaufort scale - haul", 
                         "sst_f_set"="Temperature (°F) - set",                   
                         "beaufort_set"="Beaufort scale - set",  
                         "mesh_size_in"="Mesh size (in)",
                         "hanging_length_in"="Hanging length (in)",
                         "suspender_length"="Suspender length", 
                         "hanging_ratio"="Hanging ratio",
                         "net_material_strength_lbs"="Net material strength (lbs)",
                         "net_color_code"="Net color",
                         "net_extender_length_ft"="Extender length (ft)")) %>% # "net_hang_length_in"
  # Remove
  filter(!variable %in% c("Set id", "Date", "Target species")) %>% 
  # Factor
  mutate(variable=factor(variable, levels=variable))


# Plot data
################################################################################

# Plot data
g <- ggplot(stats, aes(y=variable, x=perc)) +
  geom_point() +
  # Labels
  labs(x="Percent incomplete", y="") +
  scale_x_continuous(labels = scales::percent, trans="log10", lim=c(0.001, 1)) +
  # Theme
  theme_bw()
g

