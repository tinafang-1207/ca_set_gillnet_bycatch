


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/bathymetry/processed"

# Read data
data_orig <- raster::raster("/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/bathymetry/processed/ca_bathymetry_200m_epsg3309.tif")


# Read data
################################################################################

# Project
blocks_wgs84 <- wcfish::blocks %>% 
  filter(block_state=="California")
blocks_utm <- blocks_wgs84 %>% 
  sf::st_transform(crs=raster::crs(data_orig))

# Calculate summary statistics
depth_max <- raster::extract(x=data_orig, y=blocks_utm, fun=max, na.rm=T)
depth_min <- raster::extract(x=data_orig, y=blocks_utm, fun=min, na.rm=T)
depth_med <- raster::extract(x=data_orig, y=blocks_utm, fun=median, na.rm=T)
depth_avg <- raster::extract(x=data_orig, y=blocks_utm, fun=mean, na.rm=T)

# Merge
data <- blocks_utm %>% 
  sf::st_drop_geometry() %>% 
  select(block_id) %>% 
  mutate(depth_m_min=abs(depth_max),
         depth_m_max=abs(depth_min),
         depth_m_med=abs(depth_med),
         depth_m_avg=abs(depth_avg))

# Export data
saveRDS(data, file=file.path(datadir, "CA_block_depths.Rds"))


# Plot data
################################################################################

# Spatialize
data_sf <- data %>% 
  gather(key="metric", value="depth_m", 2:ncol(.)) %>% 
  mutate(metric=recode_factor(metric, 
                             "depth_m_min"="Minimum depth",
                             "depth_m_max"="Maximum depth",
                             "depth_m_med"="Median depth",
                             "depth_m_avg"="Average depth")) %>% 
  left_join(blocks_wgs84 %>% select(block_id, block_type)) %>% 
  sf::st_as_sf() %>% 
  filter(block_type=="Inshore") %>% 
  filter(metric!="Average depth")

# Land
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=7),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_sf) +
  # Plot data
  facet_wrap(~metric, nrow=1) +
  geom_sf(mapping=aes(fill=depth_m), linewidth=0.2, color="grey30") +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Legend
  scale_fill_gradientn(name='Depth (m)', colors=cmocean::cmocean("deep")(10)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Crop
  coord_sf(xlim = c(-125, -117), ylim = c(32, 42)) +
  # Theme
  theme_bw() + base_theme
g

# Export
ggsave(g, filename=file.path(datadir, "FigX_depth_by_block.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


