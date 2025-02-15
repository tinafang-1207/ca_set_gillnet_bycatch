
# clean working space
################################################################################
rm(list = ls())

# read in packages
################################################################################
library(tidyverse)

# read in data
################################################################################
# prediction data

# Yutian
data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds") 

# Chris
data <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds")

# geographical data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# landmark
landmark <- read_csv("data/gis_data/landmark_key.csv")


# read in statewater
state_water <- readRDS("data/gis_data/CA_state_waters_polyline.Rds")

# read in island exclusion area
island_exclusion <- sf::st_read("data/gis_data/island_gillnet_exclusion_areas.shp")

# read in california marine protected area
ca_mpa <- wcfish::mpas_ca

# format data
##################################################

wgs84 <- "+proj=longlat +datum=WGS84"

state_water_mainland <- state_water %>%
  mutate(id = row_number()) %>%
  mutate(id = as.factor(id)) %>%
  filter(id == "8")

island_exclusion_crs <- island_exclusion %>%
  sf::st_transform(crs = wgs84)

ca_mpa_clean <- ca_mpa %>%
  filter(region == "SCSR") %>%
  filter(!(name %in% c("Campus Point SMCA (No-Take)",
                    "Point Conception SMR",
                    "Kashtayit SMCA",
                    "Naples SMCA",
                    "Goleta Slough SMCA (No-Take)",
                    "Point Dume SMCA",
                    "Point Dume SMR",
                    "Point Vicente SMCA (No-Take)",
                    "Abalone Cove SMCA",
                    "Bolsa Bay SMCA",
                    "Bolsa Chica Basin SMCA (No-Take)",
                    "Upper Newport Bay SMCA",
                    "Crystal Cove SMCA",
                    "Laguna Beach SMCA (No-Take)",
                    "Laguna Beach SMR",
                    "Dana Point SMCA",
                    "Batiquitos Lagoon SMCA (No-Take)",
                    "Swami's SMCA",
                    "San Diego-Scripps Coastal SMCA",
                    "San Dieguito Lagoon SMCA",
                    "San Elijo Lagoon SMCA (No-Take)",
                    "South La Jolla SMR",
                    "South La Jolla SMCA",
                    "Matlahuayl SMR",
                    "Famosa Slough SMCA (No-Take)",
                    "Cabrillo SMR",
                    "Tijuana River Mouth SMCA")))

# There were two invalid geometries in the MPA data

ca_mpa_valid <- sf::st_make_valid(ca_mpa_clean)

ca_mpa_dissolve <- ca_mpa_valid %>%
  sf::st_buffer(dist = 50) %>%
  sf::st_union()



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
  geom_sf(data = state_water_mainland, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F ) +
  geom_sf(data = island_exclusion_crs, fill = NA, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F) +
  geom_sf(data = ca_mpa_dissolve, fill = NA, color = "gray40", linewidth = 0.2) +
  # plot land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  # plot landmark
  geom_point(data = landmark %>% filter(species == "California sea lion"), mapping  = aes(x = long_dd, y = lat_dd), size = 1, pch = 1) +
  geom_text(data = landmark %>% filter(species == "California sea lion"), mapping = aes(x = long_dd, y = lat_dd, label = landmark, hjust = hjust, vjust = vjust), size = 2, color = "black") +
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



g2 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Harbor seal"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = state_water_mainland, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F ) +
  geom_sf(data = island_exclusion_crs, fill = NA, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F) +
  geom_sf(data = ca_mpa_dissolve, fill = NA, color = "gray40", linewidth = 0.2) +
  # plot land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  # plot landmark
  geom_point(data = landmark %>% filter(species == "Harbor seal"), mapping  = aes(x = long_dd, y = lat_dd), size = 1, pch = 1) +
  geom_text(data = landmark %>% filter(species == "Harbor seal"), mapping = aes(x = long_dd, y = lat_dd, label = landmark, hjust = hjust, vjust = vjust), size = 2) +
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


g2



g3 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Common murre"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = state_water_mainland, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F ) +
  geom_sf(data = island_exclusion_crs, fill = NA, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F) +
  geom_sf(data = ca_mpa_dissolve, fill = NA, color = "gray40", linewidth = 0.2) +
  # plot land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  # plot landmark
  geom_point(data = landmark %>% filter(species == "Common murre"), mapping  = aes(x = long_dd, y = lat_dd), size = 1, pch = 1) +
  geom_text(data = landmark %>% filter(species == "Common murre"), mapping = aes(x = long_dd, y = lat_dd, label = landmark, hjust = hjust, vjust = vjust), size = 2) +
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

g3



g4 <- ggplot() +
  geom_tile(data = data %>% filter(species == "Northern elephant seal"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = state_water_mainland, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F ) +
  geom_sf(data = island_exclusion_crs, fill = NA, color = "#fd5602", linewidth = 0.2, linetype = 1, inherit.aes = F) +
  geom_sf(data = ca_mpa_dissolve, fill = NA, color = "gray40", linewidth = 0.2) +
  # plot land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  # plot landmark
  geom_point(data = landmark %>% filter(species == "Northern elephant seal"), mapping  = aes(x = long_dd, y = lat_dd), size = 1, pch = 1) +
  geom_text(data = landmark %>% filter(species == "Northern elephant seal"), mapping = aes(x = long_dd, y = lat_dd, label = landmark, hjust = hjust, vjust = vjust), size = 2) +
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

# multiple legend plot
ggsave(g_total, filename=file.path(plotdir, "Fig6_spatial_prediction_multi_legend.png"), 
       width=5, height=4.5, units="in", dpi=600)

















