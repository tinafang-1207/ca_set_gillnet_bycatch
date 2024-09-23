
# clean working environment
rm(list = ls())

# read in packages
library(tidyverse)
library(tidymodels)
library(ranger)
library(workflows)
library(probably)

# threshold_perf() generates performance metrics across probability thresholds

########### Kappa for test data #################

# read in the best model

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


# extract the best model fit

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

# extract the test data

# sealion
model_test_weighted_sl <- output_sl_weighted[["data_test"]] %>%
  select(-case_wts)

# harbor seal
model_test_weighted_hs <- output_hs_weighted[["data_test"]] %>%
  select(-case_wts)

# harbor porpoise
model_test_weighted_hp <- output_hp_weighted[["data_test"]] %>%
  select(-case_wts)

# common murre
model_test_weighted_cm <- output_cm_weighted[["data_test"]] %>%
  select(-case_wts)

# northern elephant seal
model_test_weighted_ns <- output_ns_weighted[["data_test"]] %>%
  select(-case_wts)


# make prediction

# sealion
sl_test_prob <- predict(sl_best_fit, model_test_weighted_sl, type = "prob")

sl_test_final <- model_test_weighted_sl %>%
  bind_cols(sl_test_prob) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "California sea lion")

# Harbor seal
hs_test_prob <- predict(hs_best_fit, model_test_weighted_hs, type = "prob")

hs_test_final <-  model_test_weighted_hs %>%
  bind_cols(hs_test_prob) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Harbor seal")

# Harbor porpoise
hp_test_prob <- predict(hp_best_fit, model_test_weighted_hp, type = "prob")

hp_test_final <-  model_test_weighted_hp %>%
  bind_cols(hp_test_prob) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Harbor porpoise")

# Common murre
cm_test_prob <- predict(cm_best_fit, model_test_weighted_cm, type = "prob")

cm_test_final <-  model_test_weighted_cm %>%
  bind_cols(cm_test_prob) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Common murre")

# Northern elephant seal
ns_test_prob <- predict(ns_best_fit, model_test_weighted_ns, type = "prob")

ns_test_final <-  model_test_weighted_ns %>%
  bind_cols(ns_test_prob) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Northern elephant seal")

# Calculate probability threshold for kappa

# California sea lion
sl_kap_test <- threshold_perf(sl_test_final, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "California sea lion",
         Dataset = "Test data")

# Harbor seal
hs_kap_test <- threshold_perf(hs_test_final, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Harbor seal",
         Dataset = "Test data")

# Harbor porpoise
hp_kap_test <- threshold_perf(hp_test_final, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Harbor porpoise",
         Dataset = "Test data")

# Common murre
cm_kap_test <- threshold_perf(cm_test_final, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Common murre",
         Dataset = "Test data")

# Northern elephant seal
ns_kap_test <- threshold_perf(ns_test_final, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Northern elephant seal",
         Dataset = "Test data")

# Combine kappa result for test data

kap_test_final <- bind_rows(sl_kap_test, hs_kap_test, hp_kap_test, cm_kap_test, ns_kap_test)

# save the data

write.csv(kap_test_final, file = "model_result/threshold_kappa_test_final.csv", row.names = FALSE)

########### Kappa for training data #################


# read in model data

model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

######################################################################

# California sea lion


response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
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

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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

sl_train_prob <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "California sea lion")


sl_kap_train <- threshold_perf(sl_train_prob, 
                              truth = response, 
                              estimate = .pred_1,
                              event_level = "second",
                              thresholds = seq(0.1, 0.9, by = 0.01),
                              metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "California sea lion",
         Dataset = "Train data")

# Harbor seal
######################################################################

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Harbor seal", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
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

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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



hs_train_prob <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Harbor seal")


hs_kap_train <- threshold_perf(hs_train_prob, 
                               truth = response, 
                               estimate = .pred_1,
                               event_level = "second",
                               thresholds = seq(0.1, 0.9, by = 0.01),
                               metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Harbor seal",
         Dataset = "Train data")




# Harbor porpoise
######################################################################

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Harbor porpoise", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
  mutate(case_wts = ifelse(response == "1", 50, 1),
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

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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


hp_train_prob <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Harbor porpoise")


hp_kap_train <- threshold_perf(hp_train_prob, 
                               truth = response, 
                               estimate = .pred_1,
                               event_level = "second",
                               thresholds = seq(0.1, 0.9, by = 0.01),
                               metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Harbor porpoise",
         Dataset = "Train data")



# Common murre
######################################################################

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Common murre", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
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

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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



cm_train_prob <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Common murre")


cm_kap_train <- threshold_perf(cm_train_prob, 
                               truth = response, 
                               estimate = .pred_1,
                               event_level = "second",
                               thresholds = seq(0.1, 0.9, by = 0.01),
                               metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Common murre",
         Dataset = "Train data")

# Northern elephant seal
######################################################################

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Northern elephant seal", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(response = sum(response)) %>%
  mutate(response = ifelse(response >= 1, 1, response))

predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
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

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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


ns_train_prob <- rf_res_weighted %>%
  collect_predictions(parameters = rf_best) %>%
  select(response, .pred_0, .pred_1) %>%
  mutate(species = "Northern elephant seal")


ns_kap_train <- threshold_perf(ns_train_prob, 
                               truth = response, 
                               estimate = .pred_1,
                               event_level = "second",
                               thresholds = seq(0.1, 0.9, by = 0.01),
                               metric_set(kap)) %>%
  select(.threshold, .estimate) %>%
  rename(threshold = .threshold,
         estimate_kappa = .estimate) %>%
  mutate(Species = "Northern elephant seal",
         Dataset = "Train data")



# Combine kappa result for train data

kap_train_final <- bind_rows(sl_kap_train, hs_kap_train, hp_kap_train, cm_kap_train, ns_kap_train)

# save the data

write.csv(kap_train_final, file = "model_result/threshold_kappa_train_final.csv", row.names = FALSE)





