
# clean working space
################################################################################
rm(list = ls())

# read in packages
################################################################################
library(tidyverse)

# read in data
################################################################################
# prediction data
data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds") # Yutian
data <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds") # Chris

# geographical data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Read observer data

obs_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")
obs_orig <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds") # Chris


# Format data
################################################################################

# Format observer data
obs <- obs_orig %>% 
  filter(comm_name %in% c("California sea lion", "Common murre", "Harbor seal", "Soupfin shark")) %>% 
  rename(species=comm_name)

# Spatialize
obs_sp <- obs %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_sp) <- c("long_dd", "lat_dd")

# Sea lion
obs_slion <- obs %>% 
  filter(species=="California sea lion") %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_slion) <- c("long_dd", "lat_dd")

# Create a raster template covering the extent of the points with cell size 0.1 degrees
raster_template <- raster(extent(obs_sp), res = 0.05)

# Convert points to raster (count points within each cell)
count_tot <- rasterize(obs_sp, raster_template, fun = "count")
count_slion <- rasterize(obs_slion, raster_template, fun = "count")
slion_rate <- count_slion / count_tot
slion_rate_df <- slion_rate %>% 
  as.data.frame(xy=T)

# Plot the raster
plot(count_tot)
plot(count_slion )
plot(slion_rate)

g <- ggplot() +
  geom_tile(data = slion_rate_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()
g

# Plot data
################################################################################

# theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


# One legend plot

g <- ggplot() +
  geom_tile(data = data, aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  # geom_point(data=obs, mapping=aes(x=long_dd, y=lat_dd), shape=1) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g

# Multiple legend plot


g1 <- ggplot() +
  geom_tile(data = data %>% filter(species == "California sea lion"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.2, 0.4)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g1

g2 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Harbor seal"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.1, 0.3)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colourbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())


g2

g3 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Soupfin shark"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.2, 0.4)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g3

g4 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Common murre"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.1, 0.2)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g4


g_total <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol = 2)

g_total


# save figures
################################################################################

plotdir <- "figures"

# one legend plot
ggsave(g, filename=file.path(plotdir, "Fig7_spatial_prediction_one_legend.png"), 
       width=5.5, height=4.5, units="in", dpi=600)

# multiple legend plot
ggsave(g_total, filename=file.path(plotdir, "Fig7_spatial_prediction_multi_legend.png"), 
       width=4, height=4.5, units="in", dpi=600)

















