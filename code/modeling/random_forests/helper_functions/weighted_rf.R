
# Weighted random forest

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
  
  # create the final model fit list
  model_fit_list <- list()
  
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
      mutate(weight = wt_do, balanced_type = "weighted")
    
    # Extract the best model via cross-validation from each weight
    model_weighted_best <- select_best(rf_res_weighted, metric = "kap")
    
    # finalize the best model workflow for each weight
    model_weighted_final_workflow <- finalize_workflow(rf_workflow_weighted, model_weighted_best)
    
    # finalize the best fit for each weight
    model_weighted_final_fit <- fit(model_weighted_final_workflow, data = model_train_weighted)
    
    
    if(i == 1) {
      df_out_weighted <- rf_res_weighted_df
      best_model_list <- list(model_weighted_best)
      model_fit_list <- list(model_weighted_final_fit)
    }else{
      df_out_weighted <- rbind(df_out_weighted, rf_res_weighted_df)
      best_model_list <- append(best_model_list, list(model_weighted_best))
      model_fit_list <- append(model_fit_list, list(model_weighted_final_fit))
    }
    
  }
  
  # Extract the best weight from the weighted for loop result
  df_out_weighted_final <- df_out_weighted
  
  # Extract the best model list
  best_model_list <- best_model_list
  
  # Extract the final model fit for each weight
  model_fit_list <- model_fit_list
  
  # format the output to return
  output_weighted <- list(rf_weighted_final = df_out_weighted_final, # tuning results - a dataframe
                          best_models = best_model_list, # best models - list of model objects
                          final_fit = model_fit_list, # model fit for each model under best weight
                          data_train=model_train_weighted, # training data
                          data_test=model_test_weighted # test data
  )
  
  # return the output
  return(output_weighted)
  
  
}