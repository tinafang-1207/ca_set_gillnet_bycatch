
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
  # filter only to the fishing region (Chris said to 20km)
  filter(shore_km <= 20) %>%
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

############################################################################

#Experiment with long below

sl_best_fit <- output_sl_weighted[["final_fit"]][[2]]


# geographical data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

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


obs_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

obs <- obs_orig %>% 
  filter(comm_name == "California sea lion") %>% 
  rename(species=comm_name)



g1 <- ggplot() +
  geom_tile(data = sealion_spatial_risk, aes(x = long_dd, y = lat_dd, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = mexico, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  #geom_point(data=obs, mapping=aes(x=long_dd, y=lat_dd), shape=1) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.2, 0.4)) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  scale_x_continuous(breaks=seq(-122, -118, 1)) +
  scale_y_continuous(breaks=seq(32, 35, 1)) +
  facet_wrap(.~species) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme + theme(legend.position= c(0.5, 0.1),
                                  legend.key.size=unit(0.25, "cm"),
                                  legend.direction = "horizontal",
                                  axis.title.x = element_blank(),
                                  axis.title.y = element_blank())



g1








