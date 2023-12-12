
### clean working environment ###
rm(list = ls())

### read in library ###
library(tidyverse)

### read in data ###

# Jim's trip data (90s)
load("data/confidential/processed/TripSetData.to.UCSB.Rdata")

# original trip data (90s)
obs_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed/SWFSC_1990_2017_set_net_observer_trips.Rds")

# original bycatch data(90s)
bycatch_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed/SWFSC_set_net_observer_data.Rds")

# make a plot to compare 90's data location before and after the new data

obs_orig_location <- obs_orig %>%
  select(trip_id, set_id, lat_dd_haul, long_dd_haul) %>%
  mutate(source = "original")

obs_new_location <- TripSetData.to.UCSB %>%
  select(TripSet, TripNumber, Lat, Lon) %>%
  rename(set_id = TripSet,
         trip_id = TripNumber,
         lat_dd_haul = Lat,
         long_dd_haul = Lon) %>%
  mutate(set_id = str_replace(set_id, "_", "-")) %>%
  mutate(source = "new")

obs_all_loc <- rbind(obs_orig_location, obs_new_location)

# read in us and mexico

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# base theme
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

ggplot() +
  geom_point(data = obs_all_loc, mapping = aes(x = long_dd_haul, y = lat_dd_haul)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  facet_wrap(.~source) +
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme

# now take a look at species

# merge the bycatch data to observer trip data

obs_orig_bycatch = left_join(bycatch_orig, obs_orig_location, by = c("trip_id", "set_id"))

obs_new_bycatch = left_join(bycatch_orig, obs_new_location, by = c("trip_id", "set_id"))

# combine the two dataframe

obs_all_loc_bycatch <- rbind(obs_orig_bycatch, obs_new_bycatch)


# plot couple of species bycatch location

g1 <- ggplot() +
  geom_point(data = obs_all_loc_bycatch %>% filter(comm_name == "California sea lion"), mapping = aes(x = long_dd_haul, y = lat_dd_haul)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  facet_wrap(.~source) +
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme

g2 <- ggplot() +
  geom_point(data = obs_all_loc_bycatch %>% filter(comm_name == "Common murre"), mapping = aes(x = long_dd_haul, y = lat_dd_haul)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  facet_wrap(.~source) +
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme

g3 <- ggplot() +
  geom_point(data = obs_all_loc_bycatch %>% filter(comm_name == "Harbor porpoise"), mapping = aes(x = long_dd_haul, y = lat_dd_haul)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  facet_wrap(.~source) +
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme


g4 <- ggplot() +
  geom_point(data = obs_all_loc_bycatch %>% filter(comm_name == "Harbor seal"), mapping = aes(x = long_dd_haul, y = lat_dd_haul)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  facet_wrap(.~source) +
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  theme_bw() + base_theme





