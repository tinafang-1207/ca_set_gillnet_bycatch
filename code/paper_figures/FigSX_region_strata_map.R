
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
gisdatadir <- "data/gis_data"

# Read data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks_orig <- wcfish::blocks


# Build data
################################################################################

# Observer coverage
range(data_orig$lat_dd, na.rm = T)
range(data_orig$long_dd, na.rm = T)

# Block ids
ci_blocks <- c(684:690, 707:713, 813:814, 760:762, 806:807, 829, 850, 849, 867, 765)
ventura_blocks <- c(651:663, 664:677, 697, 776, 691:696, 714:717, 701:706, 678:683) 

# Label blocks
blocks <- blocks_orig %>% 
  # Remove large blocks
  filter(block_type!="Offshore") %>% 
  # Label strata
  mutate(strata=case_when(block_id %in% ci_blocks ~ "Channel Islands",
                          block_id %in% ventura_blocks ~ "Ventura",
                          block_id <= 650 ~ "Central California",
                          T ~ "Southern California")) %>% 
  # Order strata
  mutate(strata=factor(strata, levels=c("Central California", "Ventura", 
                                        "Channel Islands", "Southern California")))


# Add strata to 
data <- data_orig %>% 
  # Label strata
  mutate(strata=case_when(block_id %in% ci_blocks ~ "Channel Islands",
                          block_id %in% ventura_blocks ~ "Ventura",
                          block_id <= 650 ~ "Central California",
                          T ~ "Southern California")) %>% 
  # Order strata
  mutate(strata=factor(strata, levels=c("Central California", "Ventura", 
                                        "Channel Islands", "Southern California")))



# Confirm that gillnet blocks are in strata
gn_blocks <- sort(unique(data$block_id))
gn_blocks[!gn_blocks %in% blocks$block_id]


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
                    legend.position = c(0.8, 0.8),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(fill=strata), color="grey85", alpha=0.6) +
  # Plot Point Arguello
  geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot data
  geom_point(data=data, mapping=aes(x=long_dd, y=lat_dd, color=strata)) +
  # Crop
  coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_ordinal(name="Strata") +
  scale_color_ordinal(name="Strata") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_strata_map.png"), 
       width=4.25, height=4.5, units="in", dpi=600)



