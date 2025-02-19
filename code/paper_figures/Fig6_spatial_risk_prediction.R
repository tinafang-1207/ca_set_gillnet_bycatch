
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
# data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds") 

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
island_exclusion <- sf::st_read("data/gis_data/exclusion_zone/island_gillnet_exclusion_areas.shp")

# read in california marine protected area
ca_mpa <- wcfish::mpas_ca

####################################################################
# Fishing efforts data and risk contour

datadir <- "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/" # Yutian
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

block_key <- readRDS("data/strata/block_strata_key.Rds")

# risk contour
sl_contour <- sf::st_read("data/gis_data/predict_risk_contour/sealion_predict_risk_contour.shp")
hs_contour <- sf::st_read("data/gis_data/predict_risk_contour/harbor_seal_predict_risk_contour.shp")
cm_contour <- sf::st_read("data/gis_data/predict_risk_contour/common_murre_predict_risk_contour.shp")
ns_contour <- sf::st_read("data/gis_data/predict_risk_contour/ne_seal_predict_risk_contour.shp")


sl_contour <- sl_contour %>%
  mutate(species = "California sea lion")

hs_contour <- hs_contour %>%
  mutate(species = "Harbor seal")

cm_contour <- cm_contour %>%
  mutate(species = "Common murre")

ns_contour <- ns_contour %>%
  mutate(species = "Northern elephant seal")



##################################################
# format data (spatial risk prediction)

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

#############################################################################
# format data (fishing efforts)
data_fe <- data_orig %>%
  # Filter years after 2002
  filter(year >= 2002 & year <= 2021) %>%
  # add hotspot seasons and normal seasons
  mutate(fishing_season = case_when(yday>=91 & yday <=166~"Closure period (4/1-6/15)",
                                    yday<91|yday>166~"Open period (rest of the year)") )%>% 
  filter(!is.na(fishing_season)) %>%
  # Add strata
  left_join(block_key)


# build block data
stats_blocks <- data_fe %>%
  # Summarize by fishing season and block
  group_by(fishing_season, block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nvesseldays=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Calculate proportion by period
  group_by(fishing_season) %>% 
  mutate(prop=nvesseldays/sum(nvesseldays)) %>% 
  ungroup() %>% 
  # Add block props
  mutate(block_id=as.numeric(block_id)) %>% 
  left_join(blocks_df %>% select(block_id, block_type, block_lat_dd, block_long_dd), by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  # Remove confidential data
  filter(nvessels>=3)

stats_blocks_sf <- stats_blocks %>% 
  left_join(blocks %>% select(block_id)) %>% 
  sf::st_as_sf()

################################################################################
# Plot data (spatial risk prediction)


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
  # Label
  labs(tag="A") +
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
  # Label
  labs(tag="") +
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
  # Label
  labs(tag="") +
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
  # Label
  labs(tag="") +
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


#######################################################################
# Plot data (fishing efforts)

species <- c("California sea lion" = "#1B9E77",
             "Harbor seal" = "#7570B3",
             "Common murre" = "#D95F02",
             "Northern elephant seal" = "#66A61E")

g5 <- ggplot() +
  geom_sf(data = stats_blocks_sf %>% filter(fishing_season == "Closure period (4/1-6/15)"), mapping = aes(fill = prop), alpha = 0.7) +
  facet_wrap(~fishing_season, nrow = 2) +
  # Label
  labs(tag="B") +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = sl_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = hs_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = cm_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = ns_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  # Axes
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       breaks=c(1, 10)/100,
                       labels = c("1%", "10%")) +
  guides(fill = "none",
         color = guide_legend(override.aes = list(linetype = 1, size = 1))) +
  scale_color_manual(name = "Species", values = species) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text=element_text(size=6),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.key = element_rect(color = NA),
        legend.position = c(0.4, 0.25),
        plot.subtitle=element_text(size=4, face="italic"))

g5

g6 <- ggplot() +
  geom_sf(data = stats_blocks_sf %>% filter(fishing_season == "Open period (rest of the year)"), mapping = aes(fill = prop), alpha = 0.7) +
  facet_wrap(~fishing_season, nrow = 2) +
  # Label
  labs(tag="") +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.01, inherit.aes = F) +
  geom_sf(data = sl_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = hs_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = cm_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = ns_contour, aes(color = species), linewidth = 0.4, linetype = 1, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  # Axes
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       breaks=c(c(0.1, 1, 10)/100),
                       labels = c("0.1%", "1%", "10%")) +
  scale_color_manual(name = "Species", values = species) +
  guides(fill = guide_colorbar(ticks.colour = "black", 
                               frame.colour = "black", 
                               frame.linewidth = 0.2),
         color = "none") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text=element_text(size=6),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.15, "cm"),
        legend.position = c(0.2, 0.25),
        plot.subtitle=element_text(size=4, face="italic"))

g6
######################################################
# tag_A <- grid::textGrob("A", x = unit(0.1, "npc"), y = unit(0.75, "npc"), gp = grid::gpar(fontsize = 12, fontface = "bold"))
# tag_B <-  grid::textGrob("B", x = unit(0.1, "npc"), y = unit(0.75, "npc"), gp = grid::gpar(fontsize = 12, fontface = "bold"))
# 
# 
# g_total <-
#   # Arrange the plots with tags A and B
#   gridExtra::grid.arrange(
#     # First group of plots with tag "A"
#     gridExtra::arrangeGrob(g1, g2, g3, g4, ncol = 2, top = tag_A),
#     
#     # Second group of plots with tag "B"
#     gridExtra::arrangeGrob(g5, g6, ncol = 1, top = tag_B),
#     
#     # Combine the two groups side by side
#     ncol = 2  # Place the two groups side by side
#   )
# g_total

g_total <- gridExtra::grid.arrange(g1, g2, g5, g3, g4, g6, ncol = 3)

g_total

# save figures
################################################################################

plotdir <- "figures"

# multiple legend plot
ggsave(g_total, filename=file.path(plotdir, "Fig6_spatial_prediction_multi_legend.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

















