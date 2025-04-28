

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris
tabledir <- "tables"

# Read obersever data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


# Build data
################################################################################

# Strata lines
lats <- c(33.84, 34.5, 35.66, 37.33, 38.84)
xends <- c(-121.5, -122, -122.5, -124, -125)
lat_df <- tibble(x=-117,
                 xend=xends,
                 y=lats,
                 yend=lats)

# Bins
lat_breaks <- seq(31.5, 37.5, 0.3)
long_breaks <- seq(-124, -116.5, 0.3)
lat_bins <- zoo::rollmean(lat_breaks, k=2)
long_bins <- zoo::rollmean(long_breaks, k=2)


# Buid data
data <- data_orig %>% 
  # Add period
  mutate(period=cut(year, breaks=c(-Inf, 1995, 2001, Inf), 
                    labels=c("1983-1995\n(all regions)", "1999-2000\n(Monterey Bay only)", "2006-07, 2010-13, 2017\n(south of Point Conception)"))) %>% 
  # Add lat/long bings
  mutate(lat_dd_bin=cut(lat_dd, breaks=lat_breaks, labels=lat_bins) %>% as.character() %>% as.numeric(),
         long_dd_bin=cut(long_dd, breaks=long_breaks, labels=long_bins) %>% as.character() %>% as.numeric()) %>% 
  # Add category
  group_by(period, lat_dd_bin, long_dd_bin) %>% 
  summarise(nvessels=n_distinct(vessel_id),
            ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  filter(nvessels>=3)
  

# Plot data
################################################################################

# Base theme
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
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))
# Plot
# g <- ggplot(data, aes(x=long_dd, y=lat_dd)) +
g <- ggplot(data, aes(x=long_dd_bin, y=lat_dd_bin, fill=ntrips)) +
  facet_wrap(~period, nrow=1) +
  # Plot regions
  geom_segment(data=lat_df, mapping=aes(y=y, yend=yend, x=x, xend=xend), inherit.aes = F, linewidth=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  geom_tile() +
  # geom_point() +
  # Legend
  scale_fill_gradientn(name="# of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-123, -117), ylim = c(32.3, 38.5)) +
  # Theme
  theme_bw() + base_theme
g

