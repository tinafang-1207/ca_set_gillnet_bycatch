
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/species_distributions/processed"

# Read data
haulouts <- readRDS(file.path(datadir, "2001_2003_harbor_seal_haulouts.Rds"))
colonies <- readRDS(file.path(datadir,  "2010_seabird_colony_database.Rds" ))
ranges <- readRDS(file.path(datadir, "cwhr_ranges_simplified.Rds"))
hporp_ras <- raster::raster(file.path(datadir, "harbor_porpoise_range.tiff"))
eseal_ts <- readxl::read_excel("data/species_distributions/raw/elephant_seal/Lowry_2014_Table2.xlsx", sheet=1)
eseal_xy <- readxl::read_excel("data/species_distributions/raw/elephant_seal/Lowry_2014_Table2.xlsx", sheet=2)
slion_ts <- readxl::read_excel("data/species_distributions/raw/california_sea_lion/Lowry_etal_2021_Table4.xlsx", sheet=1)
slion_xy <- readxl::read_excel("data/species_distributions/raw/california_sea_lion/Lowry_etal_2021_Table4.xlsx", sheet=2)


# Format data
################################################################################

# Convert harbor porpoise raster
hporp_ras_df <- hporp_ras %>% 
  raster::as.data.frame(xy=T) %>% 
  filter(layer==1)

# Build elephant seal data
eseal <- eseal_ts %>% 
  # Reduce to most recent year
  filter(Year==2010) %>% 
  # Gather
  gather(key="rookery", value="nbirths", 2:ncol(.)) %>% 
  select(-Year) %>% 
  # Eliminate totals
  filter(!grepl("total", tolower(rookery))) %>% 
  # Add metadata
  full_join(eseal_xy) %>% 
  # Arrange
  select(region, rookery, long_dd, lat_dd, nbirths)

# Build sea lion data
slion <- slion_ts %>% 
  # Mean
  group_by(species, haulout, area) %>% 
  summarise(nlive=mean(live_total)) %>% 
  ungroup() %>% 
  # Reduce 
  filter(species=="California sea lion") %>% 
  # Add xy
  left_join(slion_xy)


# Plot data
################################################################################

# Reference lines
lats <- c(33.84, 34.5, 35.66, 37.33, 38.84)

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

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

# Sea lion
g1 <- ggplot(ranges %>% filter(comm_name=="California sea lion")) +
  facet_wrap(~comm_name) +
  geom_sf(fill="lightblue", color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Rookeries
  geom_point(data=slion, mapping=aes(x=long_dd, y=lat_dd, size=nlive)) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_size_continuous(name="Haulout size\n(# of sea lions)", range=c(0.2, 3)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g1

# Elephant seal
g2 <- ggplot(ranges %>% filter(comm_name=="Northern elephant seal")) +
  facet_wrap(~comm_name) +
  geom_sf(fill="lightblue", color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Rookeries
  geom_point(data=eseal, mapping=aes(x=long_dd, y=lat_dd, size=nbirths)) +
  geom_point(data=eseal %>% filter(is.na(nbirths)), mapping=aes(x=long_dd, y=lat_dd), shape="x", size=2.5) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_size_continuous(name="Rookery size\n(# of births)", range=c(0.2, 3)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g2

# Harbor seal
g3 <- ggplot(ranges %>% filter(comm_name=="Harbor seal")) +
  facet_wrap(~comm_name) +
  geom_sf(fill="lightblue", color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Haulouts
  geom_sf(data=haulouts %>% filter(count_photo>0), mapping=aes(size=count_photo)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_size_continuous(name="Haulout size\n(# of seals)", range=c(0.2, 3)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g3

# Common murre
g4 <- ggplot(ranges %>% filter(comm_name=="Common murre"), aes(fill=season)) +
  facet_wrap(~comm_name) +
  geom_sf(color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Plot Point Sur
  geom_segment(mapping=aes(y=36.3, yend=36.3, xend=-117, x=-123.7), linewidth=0.2) +
  annotate(geom="text", x=-123.7, y=36.3, hjust=0, vjust=-0.3, label="Point Sur", fontface="italic", size=2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Colonies
  geom_sf(colonies %>% filter(comu>0), mapping=aes(size=comu), inherit.aes = F) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_fill_manual(name="Season", values=c("yellow", "lightblue")) +
  scale_size_continuous(name="Colony size\n(max count)", range=c(0.2, 3)) +
  guides(fill = guide_legend(order = 1), size = guide_legend(order = 2)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.3),
        legend.margin=margin(-3,0,-3,0))
g4

# Brandt's cormorant
g5 <- ggplot(ranges %>% filter(comm_name=="Brandt's cormorant")) +
  facet_wrap(~comm_name) +
  geom_sf(fill="lightblue", color=NA) +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Colonies
  geom_sf(colonies %>% filter(brco>0), mapping=aes(size=brco)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  scale_size_continuous(name="Colony size\n(max count)", range=c(0.2, 3)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g5

# Stock lats
hporp_lats <- tibble(comm_name="Harbor porpoise",
                     lat_dd=c(34.5, 36.1, 37.1, 39.1))

# Harbor porpoise
g6 <- ggplot(hporp_lats) +
  facet_wrap(~comm_name) +
  # Plot raster
  geom_tile(data=hporp_ras_df, mapping=aes(x=x, y=y), fill="lightblue") +
  # Plot reference lines
  # geom_hline(yintercept=lats,
  #            linetype="dotted", linewidth=0.3, color="grey50") +
  # Stock lines
  geom_hline(mapping=aes(yintercept=lat_dd), linetype="dashed", linewidth=0.3, color="grey30") +
  annotate(geom="text", 
           x=-124.1, 
           y=c(35.3, 36.6, 37.8), 
           hjust=0,
           label=c("Morro\nBay stock", "Monterey\nBay stock", "SFRR\nstock"),
           color="grey30", size=1.8, fontface="italic") +
  # geom_sf(fill="lightblue", color=NA) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  scale_y_continuous(breaks=seq(32, 42, 2)) +
  # Legend
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g6

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6, ncol=3)

# Export
ggsave(g, filename=file.path(plotdir, "FigS15_species_ranges.png"), 
       width=6.5, height=5, units="in", dpi=600)

