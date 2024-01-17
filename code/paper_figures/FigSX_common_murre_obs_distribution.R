
# clean working environment
rm(list = ls())

# read in library
library(tidyverse)

# read in data
data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds") 

# Reference lines
lats <- c(32, 33, 34, 35, 36, 37)

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


# process data
data_cm <- data %>%
  mutate(has_cm = ifelse(comm_name == "Common murre", "yes", "no"))

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
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


ggplot() +
  geom_point(data = data_cm, mapping = aes(x = long_dd, y = lat_dd, color = has_cm)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("yes", "no"), values = c("red", "black"), name = "Has common murre") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  theme_bw() + base_theme








