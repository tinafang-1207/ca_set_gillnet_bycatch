
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

obs_sp <- obs %>% 
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

count_ss <- rasterize(obs_ss, raster_template, fun = "count")
ss_rate <- count_ss / count_tot
ss_rate_df <- ss_rate %>% 
  as.data.frame(xy=T) %>%
  mutate(species = "Soupfin shark")







g_sealion <- ggplot() +
  geom_tile(data = slion_rate_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_fill_gradientn(name = "Observed bycatch risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()

g_sealion

g_seal <- ggplot() +
  geom_tile(data = seal_rate_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_fill_gradientn(name = "Observed bycatch risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()

g_seal


g_ss <- ggplot() +
  geom_tile(data = ss_rate_df, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_fill_gradientn(name = "Observed bycatch risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  theme_bw()

g_ss

df_all <- bind_rows(slion_rate_df, seal_rate_df, ss_rate_df)

g_all <- ggplot() +
  geom_tile(data = df_all, aes(x = x, y = y, fill = layer)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_fill_gradientn(name = "Observed bycatch risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  facet_wrap(.~species) +
  theme_bw()

g_all

plotdir <- "figures"

ggsave(g_all, filename=file.path(plotdir, "FigSX_obs_bycatch_ratio_raster.png"), 
       width=8, height=4.5, units="in", dpi=600)
