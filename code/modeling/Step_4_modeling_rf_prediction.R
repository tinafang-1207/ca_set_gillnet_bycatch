
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
sl <- output_sl_weighted[["final_fit"]][[3]]

# harbor seal - SMOTE
hs <- output_hs_balanced[["final_fit"]][["model_smote_final_fit"]]

# soupfin shark - SMOTE
ss <- output_ss_balanced[["final_fit"]][["model_smote_final_fit"]]

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


#### list of the best model fit ####

best_model_fit <- list(sl, hs, ss)


best_model_fit = sl

best_model_fit_run = sl

predict_data = predict_data_format
  
# function to predict model
predict_best_model <- function(best_model_fit, predict_data){
  
  preds_fac <- predict(best_model_fit, predict_data)
  preds_prob <- predict(best_model_fit, predict_data, type = "prob")
  
  # combine columns
  predict_final <- predict_data %>%
    bind_cols(preds_fac) %>%
    bind_cols(preds_prob) %>%
    rename(!!(paste0(gsub(best_model_fit), "_bycatch_class" )) := .pred_class, 
           !!(paste0(gsub(best_model_fit), "_bycatch_prob_no")) := .pred_0,
           !!(paste0(gsub(best_model_fit), "_bycatch_prob_yes")) := .pred_1)
  
  return(predict_final)

}


# this does not work now (need to change the column names!)

# so I will just predict one by one (this will change later!)

# sealion

preds_fac_sl <- predict(sl, predict_data_format)
preds_prob_sl <- predict(sl, predict_data_format, type = "prob")
preds_sl_final <-  predict_data_format %>%
  bind_cols(preds_fac_sl) %>%
  bind_cols(preds_prob_sl) %>%
  rename(sl_bycatch_class = .pred_class, sl_bycatch_prob_no = .pred_0, sl_bycatch_prob_yes = .pred_1)


# seal
preds_fac_hs <- predict(hs, predict_data_format)
preds_prob_hs <- predict(hs, predict_data_format, type = "prob")
preds_hs_final <- predict_data_format %>%
  bind_cols(preds_fac_hs) %>%
  bind_cols(preds_prob_hs) %>%
  rename(hs_bycatch_class = .pred_class, hs_bycatch_prob_no = .pred_0, hs_bycatch_prob_yes = .pred_1)

# soupfin shark
preds_fac_ss <- predict(ss, predict_data_format)
preds_prob_ss <- predict(ss, predict_data_format, type = "prob")
preds_ss_final <- predict_data_format %>%
  bind_cols(preds_fac_ss) %>%
  bind_cols(preds_prob_ss) %>%
  rename(ss_bycatch_class = .pred_class, ss_bycatch_prob_no = .pred_0, ss_bycatch_prob_yes = .pred_1)

# save the result

# sealion
saveRDS(preds_sl_final, file.path("model_result/rf_prediction/rf_sl_spatial_pred.Rds"))

# seal
saveRDS(preds_hs_final, file.path("model_result/rf_prediction/rf_hs_spatial_pred.Rds"))

# soupfin shark
saveRDS(preds_ss_final, file.path("model_result/rf_prediction/rf_ss_spatial_pred.Rds") )

###########################################################
################### Figure making ######################
###########################################################

# Step 1: we want to have the set numbers after 2010

logbook <- readRDS("data/confidential/processed/processed_CDFW_logbook/CDFW_1980_2022_gillnet_logbook_new_final.Rds")

# wait should we weighted by sets? as we agree we can't identify sets from the logbook (but I'll keep going for now)

# Step 2: calculate the weighted mean

# Step 3: make figure









