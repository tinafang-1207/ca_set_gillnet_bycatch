
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks_orig <- wcfish::blocks

# Read block key
block_key <- readRDS(file="data/strata/block_strata_key.Rds")

# Read island buffer
buffer <- readRDS(file="data/gis_data/island_buffer_10km.Rds")


# Build data
################################################################################

# Reference lines
lats <- c(34, 36, 37)

# Build data
blocks <-  blocks_orig %>% 
  # California
  filter(block_state=="California") %>% 
  # Remove large blocks
  filter(block_type!="Offshore") %>% 
  # Add strata
  left_join(block_key, by="block_id") %>% 
  # Order strata
  mutate(strata=factor(strata, levels=c("Northern California", 
                                        "San Francisco", 
                                        "Monterey Bay", 
                                        "Morro Bay", 
                                        "Ventura",
                                        "Channel Islands", 
                                        "Southern California")))


# Plot data
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                  axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_blank(),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=7),
                  plot.tag =element_text(size=8),
                  plot.title=element_blank(),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = c(0.2, 0.25),
                  legend.key.size = unit(0.5, "cm"),
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Reference lines
lats <- c(33.84, 34.5, 35.66, 37.33, 38.84)

# Plot data
g <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(fill=strata), color="grey85") +
  # Plot Point Arguello
  # geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Plot reference lines
  geom_segment(mapping=aes(y=lats, yend=lats, xend=rep(-117, 5), x=c(-122, -122.2, -123.2, -124, -125.2)),
               color="black", linetype="solid", linewidth=0.5) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot island buffer
  geom_sf(data=buffer, fill=NA, color="black", linetype="solid", linewidth=0.3, inherit.aes = F) +
  # Crop
  coord_sf(xlim = c(-125.2, -117), ylim = c(32.3, 39.5)) + # truncated
  # coord_sf(xlim = c(-127, -117), ylim = c(32.3, 42)) + # whole state
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_manual(name="Strata", values=RColorBrewer::brewer.pal(nlevels(blocks$strata), "Set1")) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS8_strata_map.png"), 
       width=4.25, height=4.5, units="in", dpi=600)

