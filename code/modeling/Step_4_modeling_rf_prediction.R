
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


# function to make spatial risk prediction
predict_spatial_risk <- function(best_model_fit, predict_data, spp){
  
  preds_fac <- predict(best_model_fit, predict_data)
  preds_prob <- predict(best_model_fit, predict_data, type = "prob")
  
  
  # combine columns to have final prediction dataframe
  predict_final <- predict_data %>%
    bind_cols(preds_fac) %>%
    bind_cols(preds_prob) %>%
    mutate(species = spp) %>%
    select(haul_lat_dd, haul_long_dd, julian_day, .pred_class, .pred_0, .pred_1, species)
    
  # calculate weight by julian day
  
  # weight will be vessel-day (will be the trip-id, use n_distinct(trip_id))
  logbook_weight <- logbook %>%
    # create julian day
    mutate(julian_day = lubridate::yday(date)) %>%
    # filter
    filter(year >=2010) %>%
    filter(net_type == "Set") %>%
    # count
    group_by(julian_day) %>%
    summarize(vessel_days_weight = n_distinct(trip_id))
    
  #combine the dataframe with weight and calculate the spatial risk
  predict_spatial_risk <- predict_final %>%
    left_join(logbook_weight, by = "julian_day") %>%
    group_by(haul_lat_dd, haul_long_dd) %>%
    summarize(spatial_risk = weighted.mean(.pred_1, vessel_days_weight)) %>%
    mutate(species = spp)
  
  return(predict_spatial_risk)

}

# make prediction

sealion_spatial_risk <- predict_spatial_risk(best_model_fit = sl_best_fit, predict_data = predict_data_format, spp = "California sea lion")
seal_spatial_risk <- predict_spatial_risk(best_model_fit = hs_best_fit, predict_data = predict_data_format, spp = "Harbor seal")
soupfin_spatial_risk <- predict_spatial_risk(best_model_fit = ss_best_fit, predict_data = predict_data_format, spp = "Soupfin shark")

# combine the dataframe
spatial_risk_final <- rbind(predict_spatial_risk,
                            seal_spatial_risk,
                            soupfin_spatial_risk)

# save the combined dataframe
saveRDS(spatial_risk_final, file.path("model_result/rf_prediction/spatial_risk_final.Rds"))


###########################################################
################### Figure making ######################
###########################################################


# read in geographical reference data

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

ggplot() +
  geom_tile(data = soupfin_spatial_risk, aes(x = haul_long_dd, y = haul_lat_dd, fill = spatial_risk)) +
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  scale_fill_gradientn(name = "Spatial risk", colors = RColorBrewer::brewer.pal(9, "YlOrRd")) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  theme_bw()









