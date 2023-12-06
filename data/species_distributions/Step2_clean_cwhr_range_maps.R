

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/public/CWHR_SpeciesRanges_GIS_20200420/"
birddir <- file.path(basedir, "Birds")
mammaldir <- file.path(basedir, "Mammals")
outdir <- "data/species_distributions/processed"

# Read range key
range_key_orig <- readxl::read_excel(file.path(basedir, "lookup.xlsx"))

# Species
species <- c("Harbor Seal", "Northern Elephant Seal", "Harbor Porpoise", "Common Murre", "Brandt'S Cormorant", "California Sea-Lion")

# Range key
range_key <- range_key_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Filter
  filter(common_name %in% species)
range_key

# Read data
slion <- sf::st_read(file.path(mammaldir, "m170.shp"))
hseal <- sf::st_read(file.path(mammaldir, "m171.shp"))
eseal <- sf::st_read(file.path(mammaldir, "m173.shp"))
brco <- sf::st_read(file.path(birddir, "b046.shp"))
comu <- sf::st_read(file.path(birddir, "b237.shp"))

# Merge data
data_orig <- bind_rows(slion, hseal, eseal, brco, comu)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(comm_name=c_name,
         sci_name=s_name,
         code=shape_name) %>% 
  # Format season
  mutate(season=ifelse(!is.na(season), season, season_2),
         season=recode(season,
                       "Y"="Year-round",
                       "W"="Winter")) %>% 
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name),
         comm_name=recode(comm_name,
                          "California sea-lion"="California sea lion")) %>% 
  # Arrange
  select(code, comm_name, sci_name, season, geometry)

# Inspect
head(data)

# Export
saveRDS(data, file=file.path(outdir, "cwhr_ranges.Rds"))


# Simplify data
################################################################################

wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")

data_simple <- data %>% 
  sf::st_simplify(dTolerance=1000) %>% # 1000 m
  sf::st_transform(wgs84)
  

# Plot data
################################################################################

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

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
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_simple, aes(fill=season)) +
  facet_wrap(~comm_name, ncol=3) +
  geom_sf(color=NA) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Legend
  scale_fill_discrete(name="Season") +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.6),
        legend.key.size = unit(0.3, "cm"))
g

# Export
ggsave(g, filename=file.path(outdir, "FigSX_species_range_maps.png"), 
       width=6.5, height=5.25, units="in", dpi=600)



