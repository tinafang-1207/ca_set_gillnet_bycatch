
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

model_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv") # yutian

model_orig <- read.csv("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/processed/fig2_total_merge_final.csv") # chris

### set up the species ###

spp <- "California sea lion"

#############################################################################
######################### Balanced random forest ############################
#############################################################################

# Notice: This takes about 20 minutes to run for 1 species

fit_balanced_rf_model <- function(spp, model_orig) {
  
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
  
  # Extract best tunes
  model_down_best <- select_best(rf_res_down, metric = "kap")
  model_up_best <- select_best(rf_res_up, metric = "kap")
  model_smote_best <- select_best(rf_res_smote, metric = "kap")
  
  # Merge best
  best_models <- list(model_up_best=model_up_best, 
                      model_down_best=model_down_best,
                      model_smote_best=model_smote_best)
  
  # Merge outputs
  output <- list(rf_all_df=rf_all_df, # tuning results - a dataframe
                 best_models = best_models, # best models - list of model objects
                 data_train=model_train_balance, # training data
                 data_test=model_test_balance # test data
                 )
  
  return(output)
  
  
  
}


# Run example

output_sl_balanced <- fit_balanced_rf_model(spp = "California sea lion", model_orig)

# Extract info

best_models <- output_sl[["best_models"]]

rf_all_df <- output_sl[["rf_all_df"]]


#############################################################################
######################### Weighted random forest ############################
#############################################################################

# Notice: This takes about 20 - 30 minutes to run for 1 species


fit_weighted_rf_model <- function(spp, model_orig) {
  
  
  # Format model data
  
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
  
  # Join model data
  
  model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
    filter(!duplicated(set_id)) %>%
    mutate(response = as.factor(response))
  
  # For loop to find the best weight
  
  # Test weight from 25 to 200 increasing by 25
  wt_vec <- seq(25, 200, 25)
  
  i <- 1
  
  # create the best model list
  best_model_list <- list()
  
  for(i in 1:length(wt_vec)){
    
    #set up wt_do
    wt_do <- wt_vec[i]
    
    #assign weight to model data
    model_data_weighted <- model_data_weighted %>%
      mutate(case_wts = ifelse(response == "1", wt_do, 1),
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
    model_rec_weighted <- recipe(response~., data = model_train_weighted)
    
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
        metrics = metric_set(roc_auc, kap)
      )
    
    # set up extracted dataframe
    rf_res_weighted_df <- rf_res_weighted %>%
      collect_metrics() %>%
      as.data.frame() %>%
      filter(.metric == "kap") %>%
      filter(mean == max(mean)) %>%
      select(mtry, mean)
    
    # Extract the best model via cross-validation from each weight
    model_weighted_best <- select_best(rf_res_weighted, metric = "kap")
    
    
    if(i == 1) {
      df_out_weighted <- rf_res_weighted_df
      best_model_list <- list(model_weighted_best)
      }else{
      df_out_weighted <- rbind(df_out_weighted, rf_res_weighted_df)
      best_model_list <- append(best_model_list, list(model_weighted_best))
      }
    
  }
  
  # Extract the best weight from the weighted for loop result
  df_out_weighted_final <-wt_vec %>%
    as.data.frame() %>%
    mutate(mtry = df_out_weighted$mtry, mean_kappa = df_out_weighted$mean) %>%
    rename(case_wts = ".")
  
  # Extract the best model list
  best_model_list <- best_model_list
  
  # format the output to return
  output_weighted <- list(rf_weighted_final = df_out_weighted_final, # tuning results - a dataframe
                          best_model = best_model_list, # best models - list of model objects
                          data_train=model_train_weighted, # training data
                          data_test=model_test_weighted # test data
                          )
  
   # return the output
  return(output_weighted)
  
  
}

# run example

output_sl_weighted <- fit_weighted_rf_model(spp = "California sea lion", model_orig)

# Extract data

rf_weighted_final <- output_sl_weighted[["rf_weighted_final"]]


############ Compare the output and select the best model #########

stats <- rf_all_df %>% 
  filter(.metric=="kap") %>% 
  arrange(balanced_type, desc(mean)) %>% 
  group_by(balanced_type) %>% 
  slice(1) %>% 
  mutate(balanced_type=factor(balanced_type, levels=c("upsample", "downsample", "smote"))) %>% 
  arrange(balanced_type)

which.max(stats$mean)








