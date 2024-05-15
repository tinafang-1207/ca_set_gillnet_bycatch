
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

# landmark
landmark <- read_csv("data/gis_data/figure6_landmark.csv")

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
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g1

landmark_hs <- landmark %>% filter(location%in%c("Santa Barbara",
                                                "Pismo beach", 
                                                "Santa Cruz Island", 
                                                "Santa Catalina Island", 
                                                "San Clemente Island", 
                                                "Point Dume", 
                                                "US-Mexico border"))

g2 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Harbor seal"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  # plot land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  # plot landmark
  geom_point(data = landmark_hs, mappin  = aes(x = long_dd, y = lat_dd)) +
  geom_text(data = landmark_hs, mapping = aes(x = long_dd, y = lat_dd, label = location)) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.1, 0.3)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
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
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
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
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
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
ggsave(g, filename=file.path(plotdir, "Fig6_spatial_prediction_one_legend.png"), 
       width=5.5, height=4.5, units="in", dpi=600)

# multiple legend plot
ggsave(g_total, filename=file.path(plotdir, "Fig6_spatial_prediction_multi_legend.png"), 
       width=4, height=4.5, units="in", dpi=600)

















