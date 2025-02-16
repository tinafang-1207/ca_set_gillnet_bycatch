
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)

### read in data ###
datadir <- "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# world
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

block_key <- readRDS("data/strata/block_strata_key.Rds")

# State waters
state_waters <- readRDS(file.path("data/gis_data/CA_state_waters_polyline.Rds"))

# risk contour
sl_contour <- sf::st_read("data/gis_data/predict_risk_contour/sealion_predict_risk_contour.shp")
hs_contour <- sf::st_read("data/gis_data/predict_risk_contour/harbor_seal_predict_risk_contour.shp")


# Format data
###############################################


data <- data_orig %>%
  # Filter years after 2002
  filter(year >= 2002 & year <= 2021) %>%
  # add hotspot seasons and normal seasons
  mutate(fishing_season = case_when(yday>=91 & yday <=166~"Suggested closure period (4/1-6/15)",
                                    yday<91|yday>166~"Suggested opening period (rest of the year)") )%>% 
  # Add strata
  left_join(block_key)


# build block data
stats_blocks <- data %>%
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

# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_blank(),
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
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Strata lines
lats <- c(33.84, 34.5, 35.66, 37.33, 38.84)
xends <- c(-121.5, -122, -122.5, -124, -125)
lat_df <- tibble(x=-117,
                 xend=xends,
                 y=lats,
                 yend=lats)

# Plot map
g1 <- ggplot() +
  geom_sf(data = stats_blocks_sf, mapping = aes(fill = prop)) +
  facet_wrap(~fishing_season, nrow = 1) +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = sl_contour, color = "#dc143c", linewidth = 0.4, linetype = 1, inherit.aes = F) +
  geom_sf(data = hs_contour, color = "#0073e6", linewidth = 0.4, linetype = 1, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-121.5, -117), ylim = c(32, 36)) +
  # Axes
  scale_x_continuous(breaks=seq(-121, -117, 2)) +
  scale_y_continuous(breaks=seq(32, 36, 2)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       breaks=c(0.01, 0.1, 1, 10)/100,
                       labels = c("0.01%", "0.1%", "1%", "10%")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text=element_text(size=6),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.6, 0.25),
        plot.subtitle=element_text(size=4, face="italic"))
g1
#######################################################################
# Save the plot
plotdir <- "figures"

ggsave(g1, filename=file.path(plotdir, "FigSX_fishing_efforts_risk_contour.png"), 
       width=5, height=4, units="in", dpi=600)






