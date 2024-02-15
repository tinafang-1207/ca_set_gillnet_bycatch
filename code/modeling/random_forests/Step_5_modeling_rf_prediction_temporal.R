
# clean working space
rm(list = ls())

# read in packages 
library(tidyverse)

library(tidymodels)
library(ranger)
library(workflows)
library(randomForest)


# read in data

data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/CDFW_1981_2020_gillnet_logbook_data_use.Rds")

# read in block_id
block_key <- readRDS("data/strata/block_strata_key.Rds")

# read in model results

# read in best model results

# sealion - weight 50
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")

# harbor seal-weight 150
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

# soupfin shark - weight 25
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/soupfin_shark_model_weighted_rf.Rds")

# common murre - Upsample
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/common_murre_model_balanced_rf.Rds")


### Extract the model best fit (to training data)

# sealion - weight 50
sl_best_fit <- output_sl_weighted[["final_fit"]][[2]]

# harbor seal - weight - 150
hs_best_fit <- output_hs_weighted[["final_fit"]][[6]]

# soupfin shark - weight 25
ss_best_fit <- output_ss_weighted[["final_fit"]][[1]]

# common murre - Upsample
cm_best_fit <- output_cm_balanced[["final_fit"]][["model_up_final_fit"]]


# source function
source("code/modeling/random_forests/helper_functions/predict_rf_temporal.R")

####################################################################

# Format predict data

predict_data <- data %>%
  left_join(block_key, by = "block_id") %>%
  select(set_id, year, yday, block_lat_dd, block_long_dd, strata, shore_km, island_yn, depth_fa, mesh_in, soak_hr, sst_c) %>%
  rename(lat_dd = block_lat_dd,
         long_dd = block_long_dd, 
         mesh_size_in = mesh_in) %>%
  mutate(island_yn = ifelse(island_yn == "FALSE", 0, 1)) %>%
  mutate(depth_fa = as.integer(depth_fa), 
         soak_hr = as.double(soak_hr),
         mesh_size_in = as.double(mesh_size_in),
         island_yn = as.factor(island_yn)) %>%
  filter(year!=1981) %>%
  filter(year!=2022) %>%
  filter(!duplicated(set_id)) %>%
  # drop na from the dataframe
  drop_na()

################################################################

# Predict for each species

sealion_bycatch_temporal <- predict_temporal(best_model_fit = sl_best_fit, predict_data = predict_data, spp = "California sea lion")

seal_bycatch_temporal <- predict_temporal(best_model_fit = hs_best_fit, predict_data = predict_data, spp = "Harbor seal")

soupfin_bycatch_temporal <- predict_temporal(best_model_fit = ss_best_fit, predict_data = predict_data, spp = "Soupfin shark")

murre_bycatch_temporal <- predict_temporal(best_model_fit = cm_best_fit, predict_data = predict_data, spp = "Common murre")



#

sealion_final <- sealion_bycatch_temporal %>%
  mutate(species = "California sea lion")

seal_final <- seal_bycatch_temporal %>%
  mutate(species = "Harbor seal")

shark_final <- soupfin_bycatch_temporal %>%
  mutate(species = "Soupfin shark")

murre_final <- murre_bycatch_temporal %>%
  mutate(species = "Common murre")

temporal_final <- bind_rows(sealion_final, seal_final, shark_final, murre_final)

# save temporal trend without strata
write.csv(temporal_final, file = "model_result/temporal_prediction.csv", row.names = FALSE)

# save temporal trend with strata
write.csv(temporal_final, file = "model_result/temporal_prediction_strata.csv", row.names = FALSE)




















































