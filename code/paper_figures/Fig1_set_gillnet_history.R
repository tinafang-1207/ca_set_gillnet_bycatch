

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
gisdatadir <- "data/gis_data"

# Read data
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

# State waters
state_waters <- readRDS(file.path(gisdatadir, "CA_state_waters_polyline.Rds"))


# Format data
################################################################################

# Inspect
freeR::complete(data_orig) # sets are actually sets pre-2017ish but are logooks after, vessel id is good

# Build data
data <- data_orig %>%
  # Set gillnet
  filter(net_type=="Set") %>%
  # Build vessel-day
  mutate(vessel_day=paste(vessel_id_use, date, sep="-")) %>% 
  # Build set
  mutate(set_id=paste(date, vessel_id_use, block_id, depth_fa, 
                      net_length_fa, mesh_size_in, buoy_line_depth_ft,
                      soak_hr, target_spp, sep="-")) %>% 
  # Add period
  mutate(period=cut(year, breaks=c(1980, reg_years, 2023), 
                    labels=c("1981-1986", "1987-1993", "1994-2001", "2002-present"), right=F))

# Inspect period key
period_key <- data %>% 
  count(period, year)

# Build annual time series
stats_yr <- data %>%
  # Summarize
  group_by(period, year) %>%
  summarize(nsets=n_distinct(set_id),
            nvesseldays=n_distinct(vessel_day),
            nvessels=n_distinct(vessel_id_use))  %>%
  ungroup()

# Build block data
stats_blocks <- data %>%
  # Summarize by period and block
  group_by(period, block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id_use),
            nvesseldays=n_distinct(vessel_day)) %>% 
  ungroup() %>% 
  # Calculate proportion by period
  group_by(period) %>% 
  mutate(prop=nvesseldays/sum(nvesseldays)) %>% 
  ungroup() %>% 
  # Add block props
  mutate(block_id=as.numeric(block_id)) %>% 
  left_join(blocks_df %>% select(block_id, block_type, block_lat_dd, block_long_dd), by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  # Adjust longitude to stagger
  mutate(long_adj=recode(period,
                         "1981-1986"=3,
                         "1987-1993"=2,
                         "1994-2001"=1,
                         "2002-present"=0,
                         ) %>% as.numeric()) %>% 
  mutate(block_long_dd_adj = block_long_dd- long_adj) %>% 
  # Remove confidential data
  filter(nvessels>=3)

# Spatializae
stats_blocks_sf <- stats_blocks %>% 
  left_join(blocks %>% select(block_id)) %>% 
  sf::st_as_sf()

# Inspect year key
count(data_blocks, period, year)

# Calulcate sets per trip
stats_trip <- data %>% 
  group_by(vessel_day) %>% 
  summarize(nsets=n_distinct(set_id)) %>% 
  ungroup()

g <- ggplot(stats_trip, aes(x=nsets)) +
  geom_histogram() +
  theme_bw()
g

# Plot data
################################################################################

# Reg dates
reg_years <- c(1987, 1994, 2002)
reg_data <- tibble(year=reg_years,
                   yval=c(380, 280, 180),
                   label=c("1987\n40 fathom depth restrition",
                           "1994\nMainland state waters exclusion",
                           "2002\n60 fathom depth\nrestriction"))

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=7),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot map
g1 <- ggplot(data=stats_blocks_sf, mapping=aes(fill=prop)) +
  facet_wrap(~period, nrow=1) +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # State waters
  geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
  # Plot Point Arguello
  geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Labels
  labs(x="", y=" ", tag="A") +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -118, 2)) +
  scale_y_continuous(breaks=seq(32, 38, 2)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       labels = scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.81, 0.25))
g1

# Number of vessels
g2 <- ggplot(stats_yr, aes(x=year, y=nvessels)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", size=0.25) +
  geom_text(data=reg_data, mapping=aes(x=year, y=yval, label=label), 
            color="grey50", vjust=1, hjust=0, size=1.8, nudge_x=1) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels", tag="B") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g2

# Number of vessel daya
g3 <- ggplot(stats_yr, aes(x=year, y=nvesseldays)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", size=0.25) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessel days", tag="C") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g3

# Number of sets
g4 <- ggplot(stats_yr, aes(x=year, y=nsets)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", size=0.25) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Number of sets", tag="D") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g4


# Merge plots
layout_matrix <- matrix(c(1,1,1,
                          2,3,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, 
                             layout_matrix=layout_matrix, heights=c(0.57, 0.43))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_set_gillnet_history.png"),
       width=6.5, height=4, units="in", dpi=600)




# # Merge plots
# layout_matrix <- matrix(c(1,3,
#                           1,4,
#                           2,5), ncol=2, byrow=T)
# g <- gridExtra::grid.arrange(g1, g2, g3, g4, #g5,
#                              layout_matrix=layout_matrix)
# g

# # Plot map
# g1 <- ggplot() +
#   # Blocks
#   # geom_sf(data=blocks, fill=NA, color="grey70", size=0.1) +
#   # Plot data
#   geom_point(data=stats_blocks, 
#              mapping=aes(x=block_long_dd_adj, y=block_lat_dd, color=period, size=prop)) +
#   # State waters
#   geom_sf(data=state_waters, color="grey40", size=0.15) +
#   # Plot Point Arguello
#   geom_hline(yintercept = 34.577201, lwd=0.2) +
#   # Land
#   geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
#   geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
#   # Labels
#   labs(x="", y="", tag="A") +
#   # Crop
#   # coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) + # southern CA
#   coord_sf(xlim = c(-126.5, -117), ylim = c(32.3, 38.5)) +
#   # Legend
#   scale_color_discrete(name="Period") +
#   scale_size_continuous(name="% of trips", labels = scales::percent) +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title=element_blank(),
#         legend.key.size = unit(0.3, "cm"),
#         legend.position = c(0.8, 0.8))
# g1


# # Amount of landings
# g5 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
#   geom_line() +
#   # Labels
#   labs(x="Year", y="Landings (millions of lbs)", tag="D") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title.x=element_blank())
# g5
# 
# # Amount of revenues
# g6 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
#   geom_line() +
#   # Labels
#   labs(x="Year", y="Revenues (USD millions)", tag="E") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title.x=element_blank())
# g6