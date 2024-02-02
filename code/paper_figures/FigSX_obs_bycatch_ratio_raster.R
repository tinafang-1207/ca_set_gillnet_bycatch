
# clean working space
################################################################################
rm(list = ls())

# read in packages
################################################################################
library(tidyverse)
library(raster)

# read in data
################################################################################

# geographical data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Read observer data
obs_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")
obs_orig <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds") # Chris


# Format data
################################################################################

# Format observer data
obs <- obs_orig %>% 
  filter(comm_name %in% c("California sea lion", "Common murre", "Harbor seal", "Soupfin shark")) %>% 
  rename(species=comm_name)

obs_sp <- obs_orig %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>%
  unique() %>% 
  dplyr::select(-set_id)
coordinates(obs_sp) <- c("long_dd", "lat_dd")

# Create a raster template covering the extent of the points with cell size 0.1 degrees
raster_template <- raster(extent(obs_sp), res = 0.05)

# Sea lion
obs_slion <- obs %>% 
  filter(species=="California sea lion") %>%
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_slion) <- c("long_dd", "lat_dd")

count_tot <- rasterize(obs_sp, raster_template, fun = "count")
count_slion <- rasterize(obs_slion, raster_template, fun = "count")
slion_rate <- count_slion / count_tot
slion_rate_df <- slion_rate %>% 
  as.data.frame(xy=T) %>%
  mutate(species = "California sea lion")

# Harbor seal
obs_seal <- obs %>%
  filter(species=="Harbor seal") %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_seal) <- c("long_dd", "lat_dd")

count_tot <- rasterize(obs_sp, raster_template, fun = "count")
count_seal <- rasterize(obs_seal, raster_template, fun = "count")
seal_rate <- count_seal / count_tot
seal_rate_df <- seal_rate %>% 
  as.data.frame(xy=T) %>%
  mutate(species = "Harbor seal")

# Soupfin shark
obs_ss <- obs %>%
  filter(species=="Soupfin shark") %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_ss) <- c("long_dd", "lat_dd")

count_tot <- rasterize(obs_sp, raster_template, fun = "count")
count_ss <- rasterize(obs_ss, raster_template, fun = "count")
ss_rate <- count_ss / count_tot
ss_rate_df <- ss_rate %>% 
  as.data.frame(xy=T) %>%
  mutate(species = "Soupfin shark")

# Common murre
obs_cm <- obs %>%
  filter(species=="Common murre") %>% 
  dplyr::select(set_id, long_dd, lat_dd) %>% 
  unique() %>% 
  dplyr::select(-set_id) 
coordinates(obs_cm) <- c("long_dd", "lat_dd")

count_tot <- rasterize(obs_sp, raster_template, fun = "count")
count_cm <- rasterize(obs_cm, raster_template, fun = "count")
cm_rate <- count_cm / count_tot
cm_rate_df <- cm_rate %>% 
  as.data.frame(xy=T) %>%
  mutate(species = "Common murre")


df_all <- bind_rows(slion_rate_df, seal_rate_df, ss_rate_df, cm_rate_df)

#############################################################

# theme
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



# make plot below

g_all <- ggplot() +
  geom_tile(data = df_all, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  scale_fill_gradientn(name = "Observed bycatch risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  facet_wrap(.~species, ncol=2) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(axis.title.x = element_blank(),
                                  axis.title.y = element_blank())

g_all

plotdir <- "figures"

ggsave(g_all, filename=file.path(plotdir, "FigSX_obs_bycatch_ratio_raster.png"), 
       width=5.5, height=4.5, units="in", dpi=600)
