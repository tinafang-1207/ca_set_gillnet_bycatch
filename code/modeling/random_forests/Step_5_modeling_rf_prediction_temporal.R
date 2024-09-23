
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

# sealion
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

# Harbor porpoise
output_hp_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_porpoise_model_weighted_rf.Rds")

# Common murre
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/common_murre_model_weighted_rf.Rds")

# Northern elephant seal
output_ns_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/northern_elephant_seal_model_weighted_rf.Rds")



### Extract the model best fit (to training data)

# sealion - weight 25
sl_best_fit <- output_sl_weighted[["final_fit"]][[1]]

# harbor seal  - weight 75
hs_best_fit <- output_hs_weighted[["final_fit"]][[3]]

# harbor porpoise - weight 50
hp_best_fit <- output_hp_weighted[["final_fit"]][[2]]

# common murre - weight 25
cm_best_fit <- output_cm_weighted[["final_fit"]][[1]]

# northern elephant seal - weight 25
ns_best_fit <- output_ns_weighted[["final_fit"]][[1]]


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

elephant_seal_bycatch_temporal <- predict_temporal(best_model_fit = ns_best_fit, predict_data = predict_data, spp = "Northern elephant seal")

murre_bycatch_temporal <- predict_temporal(best_model_fit = cm_best_fit, predict_data = predict_data, spp = "Common murre")



#

sealion_final <- sealion_bycatch_temporal %>%
  mutate(species = "California sea lion")

seal_final <- seal_bycatch_temporal %>%
  mutate(species = "Harbor seal")

elephant_seal_final <- elephant_seal_bycatch_temporal %>%
  mutate(species = "Northern elephant seal")

murre_final <- murre_bycatch_temporal %>%
  mutate(species = "Common murre")

temporal_strata_final <- bind_rows(sealion_final, seal_final, elephant_seal_final, murre_final)

temporal_total_final <- bind_rows(sealion_final, seal_final, elephant_seal_final, murre_final) %>%
  group_by(year, species) %>%
  summarize(total_bycatch = sum(total_bycatch),
            bycatch_sets = sum(bycatch_sets))



# save temporal trend without strata
write.csv(temporal_total_final, file = "model_result/temporal_prediction.csv", row.names = FALSE)

# save temporal trend with strata
write.csv(temporal_strata_final, file = "model_result/temporal_prediction_strata.csv", row.names = FALSE)




















































