

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
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/gillnet_logbooks/processed/CDFW_2000_2020_gillnet_logbook_data.Rds")

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks <- wcfish::blocks

# State waters
state_waters <- readRDS(file.path(gisdatadir, "CA_state_waters_polyline.Rds"))


# Format data
################################################################################

# Inspect
freeR::complete(data_orig) # sets are actually sets pre-2017ish but are logooks after, vessel id is good

# Build annual time series
data <- data_orig %>%
  # Set gillnet
  filter(net_type=="Set") %>%
  # Summarize
  group_by(year) %>%
  summarize(nsets=n_distinct(set_id),
            nvessels=n_distinct(vessel_id),
            landings_lb=sum(catch_lb[status=="Kept"], na.rm=T)) %>% # this is temp shortcut
  ungroup()


# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    plot.tag =element_text(size=8),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


# Plot map
g1 <- ggplot() +
  # Blocks
  geom_sf(data=blocks, fill=NA, color="grey70", size=0.1) +
  # State waters
  geom_sf(data=state_waters, color="grey40", size=0.15) +
  # Plot Point Arguello
  geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Labels
  labs(x="", y="", tag="A") +
  # Crop
  coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank())

# Number of vessels
g2 <- ggplot(data, aes(x=year, y=nvessels)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels", tag="B") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g2

# Number of sets
g3 <- ggplot(data, aes(x=year, y=nsets)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of sets", tag="C") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g3

# Amount of landings
g4 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Landings (millions of lbs)", tag="D") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g4

# Amount of revenues
g5 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", tag="E") +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g5

# Merge plots
layout_matrix <- matrix(c(1,3,
                          1,4,
                          2,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5,
                             layout_matrix=layout_matrix)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_set_gillnet_history.png"),
       width=6.5, height=5.5, units="in", dpi=600)




