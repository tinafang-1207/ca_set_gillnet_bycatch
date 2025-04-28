

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
datadir3 <-  "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge"


# Completeness
################################################################################

# Read data
data_orig1 <- readRDS(file.path(datadir2, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))
data_orig2 <- readRDS(file.path(datadir1, "SWFSC_1990_2017_set_net_observer_trips_merged.Rds"))

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
  mutate(variable=factor(variable, levels=variable)) %>% 
  # Add perc label
  mutate(label=round(perc*100, 1) %>% paste0(., "%"))


# Imputation methods
################################################################################

# Read data
data_raw_orig <- readRDS(file.path(datadir3, "1983_2017_gillnet_observer_metadata_unimputed.Rds"))
data_imputed_orig <- readRDS(file.path(datadir3, "1983_2017_gillnet_observer_metadata_all.Rds"))


# Format imputed data
table(data_imputed_orig$net_type)
data_imputed <- data_imputed_orig %>% 
  # Set nets
  filter(!net_type %in% c("drift", "float")) 

# Format raw data
table(data$net_type)
data_raw <- data_raw_orig %>% 
  # Set nets
  filter(!net_type %in% c("drift", "float")) %>% 
  mutate(soak_day=soak_hr/24)

# Mesh size by target stats
mesh_stats <- data_raw %>% 
  group_by(target_spp) %>% 
  summarise(mesh_in=median(mesh_size_in, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(mesh_in))

# Order data by mesh size
data_raw1 <- data_raw %>% 
  mutate(target_spp=factor(target_spp, levels=mesh_stats$target_spp))

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=6),
                    plot.tag=element_text(size=7),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))


# Completeness
g1 <- ggplot(stats, aes(y=variable, x=perc)) +
  geom_point(size=0.8) +
  geom_text(mapping=aes(label=label), size=1.8, hjust=1.3) +
  # Labels
  labs(x="Percent incomplete", y="Data attribue", tag="A") +
  scale_x_continuous(trans="log10", 
                     lim=c(0.001, 1), 
                     breaks=c(0.001, 0.01, 0.1, 1),
                     labels=c("0.1%", "1%", "10%", "100%")) +
  # Theme
  theme_bw() +
  theme(panel.grid.major.x = element_blank(), 
        panel.grid.minor.x = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text=element_text(size=5),
        axis.title=element_text(size=7),
        legend.text=element_text(size=5),
        legend.title=element_text(size=6),
        plot.tag=element_text(size=7),
        plot.title = element_blank())
g1

# Depths
g2 <- ggplot(data_raw, aes(x=depth_fa)) +
  geom_histogram(breaks=seq(0,200,5)) +
  # Labels
  labs(x="Depth (fathoms)", y="Number of observed sets", tag="B") +
  # scale_x_continuous(breaks=seq(0, 10, 1), lim=c(0,10.2)) +
  # Theme
  theme_bw() + base_theme 
g2

# Soak time
max_hrs <- data_raw$soak_hr %>% max(., na.rm=T)
g3 <- ggplot(data_raw, aes(x=soak_day)) +
  geom_histogram(breaks=seq(0, 10, 0.25)) +
  # Labels
  labs(x="Soak time (days)", y="Number of observed sets", tag="C") +
  scale_x_continuous(breaks=seq(0, 10, 1), lim=c(0,10.2)) +
  # Max
  annotate(geom="text", x=max_hrs/24, y=0, label=paste0(max_hrs, " hr\nmax"), vjust=-0.1, size=1.8) +
  # Theme
  theme_bw() + base_theme 
g3

#  Mesh sizes
max_in <- data_raw$mesh_size_in %>% max(., na.rm=T)
g4 <- ggplot(data_raw, aes(x=mesh_size_in)) +
  geom_histogram(breaks=seq(0, 30, 0.5)) +
  # Labels
  labs(x="Mesh size (in)", y="Number of observed sets", tag="D") +
  scale_x_continuous(breaks=c(0, 3.5, 8.5, 15, 20, 25, 30), lim=c(0,26)) + 
  # Max
  annotate(geom="text", x=max_in, y=0, label=paste0(max_in, '"\nmax'), vjust=-0.1, size=1.8) +
  # Theme
  theme_bw() + base_theme 
g4

# Mesh size by species
g5 <- ggplot(data_raw1, aes(y=target_spp, x=mesh_size_in)) +
  geom_boxplot(outlier.shape=21, linewidth=0.2, outlier.size = 0.3) +
  # Labels
  labs(x="Mesh size (in)", y="Target species", tag="E") +
  scale_x_continuous(breaks=c(0, 3.5, 6.5, 8.5, seq(10,30, 5)), lim=c(0, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
g5

# Merge
layout_matrix <- matrix(data=c(1,2,3,
                               4,5,5), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, 
                             layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_observer_data_imputation.png"), 
       width=6.5, height=4.5, units="in", dpi=600)





