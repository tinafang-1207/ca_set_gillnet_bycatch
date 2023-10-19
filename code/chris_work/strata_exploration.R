
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
data_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/cdfw_obs/processed/CDFW_1983_1989_gillnet_observer_set_info.Rds")

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks_orig <- wcfish::blocks


# Map strata
################################################################################

# Observer coverage
range(data_orig$lat_dd)
range(data_orig$long_dd)

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
  geom_sf(data=blocks, mapping=aes(fill=strata), color="grey85") +
  # Plot Point Arguello
  geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  # Plot data
  geom_point(data=data_orig, mapping=aes(x=long_dd, y=lat_dd)) +
  # Crop
  coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_ordinal(name="Strata") +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(plotdir, "strata_map.png"), 
       width=4.25, height=4.5, units="in", dpi=600)


# Add strata to data
################################################################################

# Convert to spatial
blocks_sp <- blocks %>% 
  sf::as_Spatial()
data_sp <- data_orig %>% 
  # Convert to sf
  sf::st_as_sf(., coords=c("long_dd", "lat_dd"), crs=sf::st_crs(blocks)) %>% 
  # Convert to sp
  sf::as_Spatial()

# Extract block id
block_intersecting <- sp::over(data_sp, blocks_sp)
block_ids <- block_intersecting$block_id

# Add block id
data <- data_orig %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Add month
  mutate(month=lubridate::month(date)) %>% 
  # Add season
  mutate(season=case_when(month %in% c(12,1,2) ~ "Winter",
                          month %in% c(3,4,5) ~ "Spring",
                          month %in% c(6,7,8) ~ "Summer",
                          month %in% c(9,10,11) ~ "Fall"),
         season=factor(season, levels=c("Winter", "Spring", "Fall", "Summer"))) %>% 
  # Add block id
  mutate(block_id=block_ids) %>% 
  # Add strata
  left_join(blocks %>% select(block_id, strata) %>% sf::st_drop_geometry(), by="block_id")

# Confirm that gillnet blocks are in strata
gn_blocks <- sort(unique(data$block_id))
gn_blocks[!gn_blocks %in% blocks$block_id]


# Visualize strata sample sizes
################################################################################

# Compute stats
stats <- data %>% 
  group_by(year, strata, season) %>% 
  summarize(nsets=n_distinct(set_id)) %>% 
  ungroup()

# Plot data
g <- ggplot(stats, aes(x=year, y=season, fill=nsets)) +
  facet_wrap(~strata, ncol=1) +
  geom_tile() +
  geom_text(mapping=aes(label=nsets)) +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=1980:1990) +
  # Legend
  scale_fill_gradientn(name="# of sets", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Theme 
  theme_bw()
g
