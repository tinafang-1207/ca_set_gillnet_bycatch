
#### clean working environment ####

rm(list = ls())

### read in packages ###

library(tidyverse)

library(tidymodels)
library(vip)
library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

### read in data ###

model_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv")

### set up the species ###

spp <- "California sea lion"

###

fit_rf_model <- function(spp, model_orig) {
  
  # Format data (predictor and response)
  
  response_pre_join <- model_orig %>%
    # don't want data from 1990 - 1994
    filter(data_source != "SWFSC(1990-1994)") %>%
    mutate(response = ifelse(comm_name == spp, 1, 0)) %>%
    group_by(set_id) %>%
    summarize(response = sum(response)) %>%
    mutate(response = ifelse(response >= 1, 1, response))
  
  predictor_pre_join <- model_orig %>%
    filter(data_source != "SWFSC(1990-1994)") %>%
    separate("date", c("year", "month", "day"), sep = "-") %>%
    select(set_id, haul_long_dd, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
    filter(!duplicated(set_id))
  
  #Join model data
  
  # Balanced rf
  model_data_balance <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
    filter(!duplicated(set_id)) %>%
    mutate(response = as.factor(response))
    
  # weighted rf
  
  
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
  
  # weighted rf
  
  
  # Set up model recipe
  
  # Balanced rf - downsampling
  model_rec_down <- recipe(response~., data = model_train_balance) %>%
    step_downsample(response)
  
  # Balanced rf- upsampling
  model_rec_up <- recipe(response~., data = model_train_balance) %>%
    step_upsample(response)
  
  # Balanced rf-smote
  model_rec_smote <- recipe(response~., data = model_train_balance) %>%
    step_smote(response)
  
  
  # Set up mode and engine for rf model
  
  # Balanced rf
  rf_spec_balanced <- rand_forest(mtry = tune()) %>%
    set_engine("randomForest", importance = TRUE) %>%
    set_mode("classification")
  
  # Weighted rf
  
  # Set up grid search
  set.seed(1207)
  
  model_fold <- vfold_cv(model_train_balance)
  
  param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 
  
  
  # Set up workflow
  
  # Balanced rf
  set.seed(1207)
  
  rf_workflow_down <- workflow() %>%
    add_model(rf_spec_balanced) %>%
    add_recipe(model_rec_down)
  
  rf_workflow_up <- workflow()%>%
    add_model(rf_spec_balanced) %>%
    add_recipe(model_rec_up)
  
  rf_workflow_smote <- workflow() %>%
    add_model(rf_spec_balanced) %>%
    add_recipe(model_rec_smote)


  # Set up grid tuning
  
  rf_res_down <- 
    rf_workflow_down %>%
    tune_grid(
      resamples = model_fold,
      grid = param_grid, 
      metrics = metric_set(roc_auc, kap)
    )
  
  
  rf_res_up <- 
    rf_workflow_up %>%
    tune_grid(
      resamples = model_fold,
      grid = param_grid, 
      metrics = metric_set(roc_auc, kap)
    )
  
  
  rf_res_smote <- 
    rf_workflow_smote %>%
    tune_grid(
      resamples = model_fold,
      grid = param_grid, 
      metrics = metric_set(roc_auc, kap)
    )
  
  # Return evaluation results for all fits
  
  rf_down_df <- rf_res_down %>%
    collect_metrics() %>%
    as.data.frame() %>%
    mutate(balanced_type = "downsample")
  
  rf_up_df <- rf_res_up %>%
    collect_metrics() %>%
    as.data.frame() %>%
    mutate(balanced_type = "upsample")
  
  rf_smote_df <- rf_res_smote %>%
    collect_metrics() %>%
    as.data.frame() %>%
    mutate(balanced_type = "smote")
  
  rf_all_df <- rbind(rf_down_df, rf_up_df, rf_smote_df)
  
  return(rf_all_df)
  
  
  
}











fit_rf_model(spp = "California sea lion", model_orig)





















