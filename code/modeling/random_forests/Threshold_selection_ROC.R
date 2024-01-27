
# clean working environment
rm(list = ls())

# read in packages
library(tidyverse)

library(tidymodels)
library(vip)
library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

# read in data
model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")


# Calculate ROC for test data
#####################################################################

# Read in model result

# sealion
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/soupfin_shark_model_weighted_rf.Rds")

# common murre
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/common_murre_model_balanced_rf.Rds")
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/common_murre_model_weighted_rf.Rds")

# Extract best model fit

# sealion - SMOTE
sl_best_fit <-  output_sl_balanced[["final_fit"]][["model_smote_final_fit"]]

# harbor seal - weighted - 75
hs_best_fit <- output_hs_weighted[["final_fit"]][[3]]

# soupfin shark - Upsample
ss_best_fit <- output_ss_balanced[["final_fit"]][["model_up_final_fit"]]

# common murre - weighted -25
cm_best_fit <- output_cm_weighted[["final_fit"]][[1]]

# Extract test data

# California sea lion
model_test_balanced_sl <- output_sl_balanced[["data_test"]]

# Harbor seal
model_test_weighted_hs <- output_hs_weighted[["data_test"]]

# Soupfin shark
model_test_balanced_ss <- output_ss_balanced[["data_test"]]

# Common murre
model_test_weighted_cm <- output_cm_weighted[["data_test"]]

# Create roc_curve

# sl roc
sl_roc <- predict(sl_best_fit, new_data = model_test_balanced_sl, type = "prob") %>%
  bind_cols(model_test_balanced_sl) %>%
  roc_curve(response, .pred_1, event_level = "second") %>%
  mutate(species = "California sea lion")

# sl autoplot
autoplot(sl_roc)

# hs roc
hs_roc <- predict(hs_best_fit, new_data = model_test_weighted_hs, type = "prob") %>%
  bind_cols(model_test_weighted_hs) %>%
  roc_curve(response, .pred_1, event_level = "second") %>%
  mutate(species = "Harbor seal")

# hs autoplot
autoplot(hs_roc)

# ss roc
ss_roc <- predict(ss_best_fit, new_data = model_test_balanced_ss, type = "prob") %>%
  bind_cols(model_test_balanced_ss) %>%
  roc_curve(response, .pred_1, event_level = "second") %>%
  mutate(species = "Soupfin shark")

# ss autoplot
autoplot(ss_roc)

# cm roc
cm_roc <- predict(cm_best_fit, new_data = model_test_weighted_cm, type = "prob") %>%
  bind_cols(model_test_weighted_cm) %>%
  roc_curve(response, .pred_1, event_level = "second") %>%
  mutate(species = "Common murre")

# cm autoplot
autoplot(cm_roc)

# bind columns 

roc_all <- bind_rows(sl_roc, hs_roc, ss_roc, cm_roc)

# Explort the table
write.csv(roc_all, file = "model_result/roc_curve.csv", row.names = FALSE)



# calculate ROC for training data
###########################################################################
# California sea lion

# Format data (predictor and response)

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

# predictors
# lat_dd = latitude
# depth_fa = haul depth (ignore depth_fa_imputed)
# soak_hr = soak hours
# mesh_size_in = mesh size
# shore_km = distance to shore (in km)
# yday = julian day
# sst_c = sea surface temperature
# island_yn = island dummy
# long_dd = longitude


predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(!duplicated(set_id))

#Join model data

# Balanced rf
model_data_balance <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

# Split model data

# Balanced rf
set.seed(1207)
model_split_balance <- initial_split(model_data_balance, strata = response, prop = 4/5)

model_train_balance <- training(model_split_balance) %>%
  select(-set_id) %>%
  drop_na()

model_test_balance <- testing (model_split_balance) %>%
  select(-set_id) %>%
  drop_na()


# Set up model recipe

# Balanced rf-smote
model_rec_smote <- recipe(response~., data = model_train_balance) %>%
  step_dummy(island_yn) %>%
  step_normalize(all_predictors()) %>%
  step_smote(response)


# Set up mode and engine for rf model

# Balanced rf
rf_spec_balanced <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")


# Set up grid search
set.seed(1207)

model_fold <- vfold_cv(model_train_balance, strata = response)

param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 


# Set up workflow

# Balanced rf
set.seed(1207)

rf_workflow_smote <- workflow() %>%
  add_model(rf_spec_balanced) %>%
  add_recipe(model_rec_smote)


# Set up grid tuning
rf_res_smote <- 
  rf_workflow_smote %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )


rf_best <- 
  rf_res_smote %>% 
  select_best(metric = "kap")

sl_roc <- rf_res_smote %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(response,.pred_1, event_level = "second") %>%
  mutate(species = "California sea lion")

autoplot(sl_roc)

sl_roc_optimum <- sl_roc %>%
  mutate(difference = sensitivity - (1-specificity)) %>%
  filter(difference == max(difference))


######################################################################

# Harbor seal


response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Harbor seal", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

# predictors
# lat_dd = latitude
# depth_fa = haul depth (ignore depth_fa_imputed)
# soak_hr = soak hours
# mesh_size_in = mesh size
# shore_km = distance to shore (in km)
# yday = julian day
# sst_c = sea surface temperature
# island_yn = island dummy


predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
  mutate(case_wts = ifelse(response == "1", 75, 1),
         case_wts = importance_weights(case_wts))

# split between training and testing data
set.seed(1207)

model_split_weighted <- initial_split(model_data_weighted, prop = 4/5, strata = response)

model_train_weighted <- training(model_split_weighted) %>%
  select(-set_id) %>%
  drop_na()

model_test_weighted <- testing (model_split_weighted) %>%
  select(-set_id) %>%
  drop_na()

# set up model recipe
model_rec_weighted <- recipe(response~., data = model_train_weighted) %>%
  step_dummy(island_yn) %>%
  step_normalize(all_predictors())

# set up model engine
rf_spec_weighted <- rand_forest(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 

# set up model workflow
set.seed(1207)

rf_workflow_weighted <- workflow() %>%
  add_recipe(model_rec_weighted) %>%
  add_model(rf_spec_weighted) %>%
  add_case_weights(case_wts)


# Tuning hyperparameters
rf_res_weighted <- 
  rf_workflow_weighted %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_weighted %>% 
  select_best(metric = "kap")

hs_roc <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(response,.pred_1, event_level = "second") %>%
  mutate(species = "Harbor seal")

autoplot(hs_roc)

######################################################################

# soupfin shark

# Format data (predictor and response)

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Soupfin shark", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

# predictors
# lat_dd = latitude
# depth_fa = haul depth (ignore depth_fa_imputed)
# soak_hr = soak hours
# mesh_size_in = mesh size
# shore_km = distance to shore (in km)
# yday = julian day
# sst_c = sea surface temperature
# island_yn = island dummy
# long_dd = longitude


predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(!duplicated(set_id))

#Join model data

# Balanced rf
model_data_balance <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

# Split model data

# Balanced rf
set.seed(1207)
model_split_balance <- initial_split(model_data_balance, strata = response, prop = 4/5)

model_train_balance <- training(model_split_balance) %>%
  select(-set_id) %>%
  drop_na()

model_test_balance <- testing (model_split_balance) %>%
  select(-set_id) %>%
  drop_na()


# Set up model recipe

# Balanced rf-smote
model_rec_up <- recipe(response~., data = model_train_balance) %>%
  step_dummy(island_yn) %>%
  step_normalize(all_predictors()) %>%
  step_upsample(response)


# Set up mode and engine for rf model

# Balanced rf
rf_spec_balanced <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")


# Set up grid search
set.seed(1207)

model_fold <- vfold_cv(model_train_balance, strata = response)

param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 


# Set up workflow

# Balanced rf
set.seed(1207)

rf_workflow_up <- workflow() %>%
  add_model(rf_spec_balanced) %>%
  add_recipe(model_rec_up)


# Set up grid tuning
rf_res_up <- 
  rf_workflow_up %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_up %>% 
  select_best(metric = "kap")

ss_roc <- rf_res_up %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(response,.pred_1, event_level = "second") %>%
  mutate(species = "Soupfin shark")

autoplot(ss_roc)

######################################################################

# Common murre

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Common murre", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

# predictors
# lat_dd = latitude
# depth_fa = haul depth (ignore depth_fa_imputed)
# soak_hr = soak hours
# mesh_size_in = mesh size
# shore_km = distance to shore (in km)
# yday = julian day
# sst_c = sea surface temperature
# island_yn = island dummy


predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
  mutate(case_wts = ifelse(response == "1", 25, 1),
         case_wts = importance_weights(case_wts))

# split between training and testing data
set.seed(1207)

model_split_weighted <- initial_split(model_data_weighted, prop = 4/5, strata = response)

model_train_weighted <- training(model_split_weighted) %>%
  select(-set_id) %>%
  drop_na()

model_test_weighted <- testing (model_split_weighted) %>%
  select(-set_id) %>%
  drop_na()

# set up model recipe
model_rec_weighted <- recipe(response~., data = model_train_weighted) %>%
  step_dummy(island_yn) %>%
  step_normalize(all_predictors())

# set up model engine
rf_spec_weighted <- rand_forest(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 

# set up model workflow
set.seed(1207)

rf_workflow_weighted <- workflow() %>%
  add_recipe(model_rec_weighted) %>%
  add_model(rf_spec_weighted) %>%
  add_case_weights(case_wts)


# Tuning hyperparameters
rf_res_weighted <- 
  rf_workflow_weighted %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_weighted %>% 
  select_best(metric = "kap")


cm_roc <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  roc_curve(response,.pred_1, event_level = "second") %>%
  mutate(species = "Common murre")

autoplot(cm_roc)

# combine all rows

roc_all <- bind_rows(sl_roc, hs_roc, ss_roc, cm_roc)

# save data

write.csv(roc_all, file = "model_result/roc_curve_training.csv", row.names = FALSE)













