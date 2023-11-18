

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir1 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris

# Read observer data
obs_orig <- readRDS(file=file.path(datadir1, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))

# Read logbook data
logs_orig <- readRDS(file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))


# Build data
################################################################################

# Build observer data
obs <- obs_orig %>% 
  # Add new columns
  mutate(dataset="Observer data",
         yday=lubridate::yday(date)) %>% 
  # Simplify
  select(dataset, set_id, target_spp, yday, mesh_size_in,  
         depth_fa, lat_dd, soak_hr, shore_km) %>% 
  unique() 

  # Build logbooks data
logs <- logs_orig %>% 
  # Add new columns
  mutate(dataset="Logbooks",
         yday=lubridate::yday(date)) %>% 
  # Rename
  rename(mesh_size_in=mesh_in,
         lat_dd=block_lat_dd) %>% 
  # Simplify
  select(dataset, set_id, target_spp, yday, mesh_size_in, 
         depth_fa, lat_dd, soak_hr, shore_km)  %>% 
  unique()


# Merge data
data <- bind_rows(obs, logs) %>%
  # TEMP CAP
  mutate(depth_fa=pmin(depth_fa, 100)) %>% 
  mutate(mesh_size_in=pmin(mesh_size_in, 20)) %>% 
  mutate(soak_hr=pmin(soak_hr, 24*4)) %>% 
  mutate(shore_km=pmin(shore_km, 50)) %>% 
  # Gather
  gather(key="variable", value="value", 4:ncol(.)) %>% 
  # Recode
  mutate(variable=recode(variable, 
                         "depth_fa"="Depth (fathoms)",
                         "lat_dd"="Latitude (°N)",
                         "mesh_size_in"="Mesh size (inches)",
                         "yday"="Julian day",
                         "soak_hr"="Soak time (hours)",
                         "shore_km"="Shore distance (km)"))
  

# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text = element_text(size=5),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.9, 0.9),
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data,  aes(x=value, fill=dataset)) +
  facet_wrap(~variable, scales="free", ncol=3) +
  geom_density(alpha=0.5, linewidth=0.4) +
  # Labels
  labs(x="Variable value", y="Density") +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_logbook_observer_traits.png"), 
       width=6.5, height=4.5, units="in", dpi=600)






