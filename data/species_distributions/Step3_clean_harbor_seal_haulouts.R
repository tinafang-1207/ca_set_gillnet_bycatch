

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/species_distributions/raw/harbor_seal_haulouts"
outdir <- "data/species_distributions/processed"

# Read data
data_orig <- sf::st_read(file.path(indir, "ds106.shp"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Format data
################################################################################

# Meta-data: https://map.dfg.ca.gov/metadata/ds0106.html
# aerial_est: Number of individuals estimated while flying.
# photocount: Number of individuals counted in the photograph.
# grnd_count: Number of individuals counted on the ground.

# Format data
data <- data_orig %>% 
  # Project
  sf::st_transform(wgs84) %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(long_dd=londd,
         lat_dd=latdd,
         flight_num=flight_no,
         count_air=aerial_est,
         count_ground=grnd_count,
         count_photo=photocount) %>% 
  # Arrange
  select(county, location, long_dd, lat_dd, substrate, 
         year, date, time, flight_num, everything())

# Inspect
head(data)
freeR::complete(data)
sort(unique(data$county))
sort(unique(data$location))
sort(unique(data$substrate))
sort(unique(data$year))


# Export
saveRDS(data, file=file.path(outdir, "2001_2003_harbor_seal_haulouts.Rds"))


  

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
g <- ggplot(data %>% filter(count_photo>0), aes(size=count_photo, fill=count_photo)) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Data
  geom_sf(shape=21) +
  # Legend
  scale_size_continuous(name="# of seals") +
  scale_fill_gradientn(name="# of seals", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -116, 2)) +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8,0.6),
        legend.key.size = unit(0.3, "cm"))
g



