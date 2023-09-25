

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/ca_gillnet_bycatch_chris/figures"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/swfsc_obs/processed/SWFSC_1990_2017_set_net_observer_trips.Rds")

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Build data
################################################################################

# Build completeness data
n_obs <- nrow(data_orig)
n_missing <- freeR::complete(data_orig)
n_present <- n_obs - n_missing
data <- tibble(variable=names(n_missing),
               n=n_present) %>% 
  mutate(variable=recode(variable,
                         "season" = "Season",                       
                         "trip_id" = "Trip id",                        
                         "set_num" = "Set number",                          
                         "set_id" = "Set id",                            
                         "vessel" = "Vessel name",                            
                         "vessel_plate" = "Vessel plate",                    
                         "vessel_permit" = "Vessel permit",                     
                         "port_depart" = "Port of departure",                       
                         "port_return" = "Port of return",                      
                         "date_haul1" = "Haul date",                        
                         "soak_hr" = "Soak hours",                           
                         "soak_hr_est" = "Soak hours (estimated)",                      
                         "obs_perc" = "Percent observed",                          
                         "target1_spp_code" = "Primary target species code",                  
                         "target1_spp" = "Primary target species",                      
                         "target2_spp_code" = "Secondary target species code",                  
                         "target2_spp" = "Secondary target species",                       
                         "date_haul2" = "Haul date (repeated)",                       
                         "haul_temp_device" = "Temperature device",                  
                         "haul_pos_code" = "Position measurement type",                     
                         "haul_lat_dd" = "Latitude (°N)",                      
                         "haul_long_dd" = "Longitude (°W)",                      
                         "haul_depth_fa" = "Depth (fa)",                     
                         "haul_sst_f" = "Temperature (°F)",                       
                         "haul_beauf" = "Beaufort scale",                        
                         "sn_vals_n" = "Number of set net values",
                         "fn_vals_n" = "Number of drift net values",
                         "net_type" = "Net type",                          
                         "perc_obs" = "Percent observed (repeated)",                          
                         "net_hang_length_in" = "Net hang length (in)",               
                         "net_mesh_size_in" = "Net mesh size (in)",                  
                         "net_suspender_length_in" = "Net suspender length (in)",           
                         "net_extender_length_in" = "Net extender length (in)",           
                         "net_perc_slack" = "Net percent slack",                    
                         "net_n_meshes_hang" = "Net number of hanging meshes",                 
                         "net_material_strength_lbs" = "Net material strength (lbs)",        
                         "net_mesh_panel_length_fathoms" = "Net mesh panel length (fa)",     
                         "net_depth_in_mesh_n" = "Net depth in mesh number",               
                         "net_color_code" = "Net color code",                  
                         "net_hang_line_material_code" = "Net hangline material code",       
                         "net_material_code" = "Net material code",                 
                         "net_material_strength_code" = "Net material strength code" ))
  
data_yr <- data_orig %>% 
  count(season)

table(data_orig$net_type)
data_orig %>% 
  filter(season<=1995 & net_type!="float net") %>% 
  nrow()

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.subtitle = element_text(size=8),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g1 <- ggplot(data, aes(y=reorder(variable, desc(n)), x=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(y="", x="Number of observations with data", tag="A",
       subtitle="10,216 sets observed") +
  # Theme
  theme_bw() + my_theme
g1

# Plot
g2 <- ggplot(data_yr, aes(x=season, y=n)) +
  geom_bar(stat="identity") +
  # Labels
  labs(y="Number of observed sets", x="Year", tag="B") +
  scale_x_continuous(breaks=seq(1990, 2020, 5)) +
  scale_y_continuous(breaks=seq(0, 2500, 500)) +
  # Theme
  theme_bw() + my_theme
g2


# Plot
g3 <- ggplot() +
  # Plot land
  geom_sf(data=foreign, fill="grey90", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey90", color="white", lwd=0.3) +
  # Labels
  labs(tag="C")
  # Points
  geom_point(data=data_orig, mapping=aes(y=haul_lat_dd, x=haul_long_dd), pch=21, alpha=0.5) +
  # Crop
  coord_sf(xlim = c(-121.5, -116.5), ylim = c(36, 32)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title = element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=matrix(data=c(1,2, 
                                                                     1,3), nrow=2, byrow=T))
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_federal_obs_data.png"), 
       width=6.5, height=5.5, units="in", dpi=600) 
