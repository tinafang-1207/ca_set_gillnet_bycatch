
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

# The SST needs to fix in the future (we want to have average SST from 2010 rather than 2000)
data_predict <- readRDS("data/confidential/processed/pre_model/sealion/fake_spatial_pre_model.Rds")

# read in the logbook data
logbook <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/CDFW_1981_2020_gillnet_logbook_data_use.Rds") 


# read in model results

# sealion
output_sl_balanced <- readRDS("model_result/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("model_result/weighted_rf/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("model_result/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("model_result/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("model_result/balanced_rf/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("model_result/weighted_rf/soupfin_shark_model_weighted_rf.Rds")

### Extract the model best fit (to training data)

# sealion - weight - 75
sl_best_fit <- output_sl_weighted[["final_fit"]][[3]]

# harbor seal - SMOTE
hs_best_fit <- output_hs_balanced[["final_fit"]][["model_smote_final_fit"]]

# soupfin shark - SMOTE
ss_best_fit <- output_ss_balanced[["final_fit"]][["model_smote_final_fit"]]

# source the spatial risk predict function
source("code/function/predict_rf.R")

###########################################################
################### model prediction ######################
###########################################################


# Format the predict data
predict_data_format <- data_predict %>%
  rename(haul_lat_dd = lats,
         haul_long_dd = longs,
         julian_day = jdays,
         haul_depth_fa = bathy_fa, 
         net_mesh_size_in = mesh_size,
         sst = mean_sst) %>%
  mutate(haul_depth_fa = as.integer(haul_depth_fa), 
         soak_hr = as.double(soak_hr),
         net_mesh_size_in = as.double(net_mesh_size_in)) %>%
  drop_na()


# make prediction

sealion_spatial_risk <- predict_spatial_risk(best_model_fit = sl_best_fit, predict_data = predict_data_format, spp = "California sea lion")
seal_spatial_risk <- predict_spatial_risk(best_model_fit = hs_best_fit, predict_data = predict_data_format, spp = "Harbor seal")
soupfin_spatial_risk <- predict_spatial_risk(best_model_fit = ss_best_fit, predict_data = predict_data_format, spp = "Soupfin shark")

# combine the dataframe
spatial_risk_final <- rbind(sealion_spatial_risk,
                            seal_spatial_risk,
                            soupfin_spatial_risk) %>%
  rename(Latitude = haul_lat_dd, Longitude = haul_long_dd)

# save the combined dataframe
saveRDS(spatial_risk_final, file.path("model_result/rf_prediction/spatial_risk_final.Rds"))


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

g <- gridExtra::grid.arrange(g1, g2, g3, ncol = 3)

g

plotdir <- "figures"

ggsave(g, filename=file.path(plotdir, "Fig7_spatial_prediction.png"), 
       width=7, height=6.5, units="in", dpi=600)


