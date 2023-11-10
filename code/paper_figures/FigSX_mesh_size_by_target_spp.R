

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
logs_orig <- readRDS(file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data.Rds"))


# Build data
################################################################################

# Build observer data
obs <- obs_orig %>% 
  # Simplify
  select(set_id, target_spp, mesh_size_in) %>% 
  unique() %>% 
  # Add
  mutate(dataset="Observer data")
  
# Build logbooks data
logs <- logs_orig %>% 
  # Reduce
  filter(net_type=="Set") %>%
  # Build set
  mutate(set_id=paste(date, vessel_id_use, block_id, depth_fa, 
                      net_length_fa, mesh_size_in, buoy_line_depth_ft,
                      soak_hr, target_spp, sep="-")) %>% 
  # Simplify
  select(set_id, target_spp, mesh_size_in_num)  %>% 
  unique() %>% 
  # Rename
  rename(mesh_size_in=mesh_size_in_num) %>% 
  # Reduce
  filter(target_spp %in% c("White Seabass", "Halibut", "Angel Shark")) %>% 
  mutate(target_spp=recode(target_spp,
                           "White Seabass"="White seabass", 
                           "Halibut"="California halibut",
                           "Angel Shark"="Pacific angel shark")) %>% 
  # Add
  mutate(dataset="Logbooks")

# Merge data
data <- bind_rows(obs, logs)


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
                   legend.position = "top",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data,  aes(y=target_spp, x=mesh_size_in, fill=dataset)) +
  geom_boxplot(outlier.shape=21, linewidth=0.5) +
  # Labels
  labs(x="Mesh size (inches)", y="") +
  scale_x_continuous(breaks=c(seq(0,30,5), 3.5, 8.5), lim=c(0, 30)) +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigSX_mesh_size_by_target.png"), 
       width=4.5, height=2.5, units="in", dpi=600)






