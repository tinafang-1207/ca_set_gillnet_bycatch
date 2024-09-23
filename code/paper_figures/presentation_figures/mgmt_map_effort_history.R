

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
# testing push 
library(tidyverse)

# Directories
plotdir <- "figures"
gisdatadir <- "data/gis_data"

# Read data
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/gillnet_logbooks/processed/CDFW_2000_2020_gillnet_logbook_data.Rds")

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# State waters
state_waters <- readRDS(file.path(gisdatadir, "CA_state_waters_polyline.Rds"))




# Plot data
################################################################################

# Base theme
base_theme <- theme(axis.text=element_text(size=8),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_blank(),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"))


# Plot map
g <- ggplot() +
  # State waters
  geom_sf(data=state_waters, color="grey40", size=0.15, linetype="dotted") +
  # Plot Point Arguello
  annotate(geom="text", y=34.577201, x=-122, label="Point Arguello", size=2.4, hjust=0, vjust=-0.5) +
  geom_hline(yintercept = 34.577201, lwd=0.3) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title=element_blank())
g

# Export plot
ggsave(g, filename=file.path(plotdir, "mgmt_map.png"),
       width=4.5, height=4.5, units="in", dpi=600)




