
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

library(probably)

# read in data
model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

# Get prediction result from training data
###########################################################################

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

thresholds <- seq(0.1, 0.9, by = 0.01)
  
hs_kap <- rf_res_weighted %>%
  collect_predictions(parameter = rf_best) %>%
  probably::threshold_perf(truth = response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap, sens,spec), event_level = "second")






