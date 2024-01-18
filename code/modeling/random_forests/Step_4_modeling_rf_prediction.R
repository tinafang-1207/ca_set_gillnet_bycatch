
# clean working space
rm(list = ls())

# read in packages 
library(tidyverse)

library(tidymodels)
library(ranger)
library(workflows)
library(randomForest)


# read in predict data

# notice: this is the fake data to show the spatial pattern

data_predict <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/create_spatial_risk_unpredict.Rds")

# read in the logbook data
logbook <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/CDFW_1981_2020_gillnet_logbook_data_use.Rds") 


# read in best model results

# sealion - SMOTE
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/california_sea_lion_model_balanced_rf.Rds")

# harbor seal-weight 75
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark - Upsample
output_ss_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/soupfin_shark_model_balanced_rf.Rds")

# common murre - weight 25
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/common_murre_model_weighted_rf.Rds")


### Extract the model best fit (to training data)

# sealion - SMOTE
sl_best_fit <- output_sl_balanced[["final_fit"]][["model_smote_final_fit"]]

# harbor seal - weight - 75
hs_best_fit <- output_hs_weighted[["final_fit"]][[3]]

# soupfin shark - Upsample
ss_best_fit <- output_ss_balanced[["final_fit"]][["model_up_final_fit"]]

# common murre - weight - 25
cm_best_fit <- output_cm_weighted[["final_fit"]][[1]]

# source the spatial risk predict function
source("code/modeling/random_forests/helper_functions/predict_rf.R")

###########################################################
################### model prediction ######################
###########################################################


# Format the predict data
predict_data_format <- data_predict %>%
  rename(lat_dd = lats,
         long_dd = longs,
         yday = jdays,
         depth_fa = bathy_fa, 
         mesh_size_in = mesh_size,
         sst_c = mean_sst,
         shore_km = dist_km) %>%
  mutate(depth_fa = as.integer(depth_fa), 
         soak_hr = as.double(soak_hr),
         mesh_size_in = as.double(mesh_size_in),
         island_yn = as.factor(island_yn)) %>%
  # filter only to the fishing region
  filter(shore_km <= 10) %>%
  drop_na()


# make prediction

sealion_spatial_risk <- predict_spatial_risk(best_model_fit = sl_best_fit, predict_data = predict_data_format, spp = "California sea lion")
seal_spatial_risk <- predict_spatial_risk(best_model_fit = hs_best_fit, predict_data = predict_data_format, spp = "Harbor seal")
soupfin_spatial_risk <- predict_spatial_risk(best_model_fit = ss_best_fit, predict_data = predict_data_format, spp = "Soupfin shark")
murre_spatial_risk <- predict_spatial_risk(best_model_fit = cm_best_fit, predict_data = predict_data_format, spp = "Common murre")


# combine the dataframe
spatial_risk_final <- rbind(sealion_spatial_risk,
                            seal_spatial_risk,
                            soupfin_spatial_risk,
                            murre_spatial_risk) %>%
  rename(Latitude = lat_dd, Longitude = long_dd)

# save the combined dataframe
saveRDS(spatial_risk_final, file.path("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/spatial_risk_predict_final.Rds"))


###########################################################
################### Figure making ######################
###########################################################


# read in geographical reference data

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
  geom_tile(data = spatial_risk_final, aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "RdYlGn") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g1 <- ggplot() +
  geom_tile(data = spatial_risk_final %>% filter(species == "California sea lion"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g1

g2 <- ggplot() +
  geom_tile(data = spatial_risk_final %>% filter(species == "Harbor seal"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g2

g3 <- ggplot() +
  geom_tile(data = spatial_risk_final %>% filter(species == "Soupfin shark"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g3

g4 <- ggplot() +
  geom_tile(data = spatial_risk_final %>% filter(species == "Common murre"), aes(x = Longitude, y = Latitude, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g4


g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2, ncol = 2)

g

plotdir <- "figures"

ggsave(g, filename=file.path(plotdir, "Fig7_spatial_prediction.png"), 
       width=7, height=6.5, units="in", dpi=600)


