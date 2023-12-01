
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/strata"


# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks_orig <- wcfish::blocks


# Build data
################################################################################

# Block ids
ci_blocks <- c(684:690, 707:713, 813:814, 760:762, 806:807, 829, 850, 849, 867, 765)
ventura_blocks <- c(651:663, 664:677, 697, 776, 691:696, 714:717, 701:706, 678:683) 
sf_blocks <- c(407:477, 484:486, 301, 488, 489)
monterey_blocks <- c(478:483, 487, 501:568)
morro_blocks <- c(601:650)
south_blocks <- c(718:904, 916)
north_blocks <- c(101:281, 401:406)

# Label blocks
blocks <- blocks_orig %>% 
  # California
  filter(block_state=="California") %>% 
  # Remove large blocks
  filter(block_type!="Offshore") %>% 
  # Label strata
  mutate(strata=case_when(block_id %in% ci_blocks ~ "Channel Islands",
                          block_id %in% ventura_blocks ~ "Ventura",
                          block_id %in% morro_blocks ~ "Morro Bay",
                          block_id %in% monterey_blocks ~ "Monterey Bay",
                          block_id %in% sf_blocks ~ "San Francisco", # 
                          block_id %in% north_blocks  ~ "Northern California",
                          block_id %in% south_blocks ~ "Southern California",
                          T ~ "xxx")) %>% 
  # Order strata
  mutate(strata=factor(strata, levels=c("Northern California", 
                                        "San Francisco", 
                                        "Monterey Bay", 
                                        "Morro Bay", 
                                        "Ventura",
                                        "Channel Islands", 
                                        "Southern California")))


# SImplify
blocks_df <- blocks %>% 
  sf::st_drop_geometry() %>% 
  select(block_id, strata) %>% 
  mutate(strata=as.character(strata))
write.csv(blocks_df, file=file.path(datadir, "block_strata_key.csv"), row.names = F)
saveRDS(blocks_df, file=file.path(datadir, "block_strata_key.Rds"))


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

# Plot data
g <- ggplot() +
  # Blocks
  geom_sf(data=blocks, mapping=aes(fill=strata), color="grey85") +
  # Plot Point Arguello
  # geom_hline(yintercept = 34.577201, lwd=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
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
ggsave(g, filename=file.path(datadir, "strata_map.png"), 
       width=4.25, height=4.5, units="in", dpi=600)



