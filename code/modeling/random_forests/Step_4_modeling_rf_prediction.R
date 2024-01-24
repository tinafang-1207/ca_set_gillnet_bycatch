
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

