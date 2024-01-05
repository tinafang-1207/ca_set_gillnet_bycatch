
# Balance random forest Function

#############################################################################
######################### Balanced random forest ############################
#############################################################################


# Notice: This takes about 20 minutes to run for 1 species

fit_balanced_rf_model <- function(spp, model_orig) {
  
  # Format data (predictor and response)
  
  response_pre_join <- model_orig %>%
    mutate(response = ifelse(comm_name == spp, 1, 0)) %>%
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
  
  model_fold <- vfold_cv(model_train_balance, strata = response)
  
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
  
  #Finalize workflow
  model_down_final_workflow <- finalize_workflow(rf_workflow_down, model_down_best)
  model_up_final_workflow <- finalize_workflow(rf_workflow_up, model_up_best)
  model_smote_final_workflow <- finalize_workflow(rf_workflow_smote,model_smote_best)
  
  # fit training data in the best model
  model_down_final_fit <- fit(model_down_final_workflow, data = model_train_balance)
  model_up_final_fit <- fit(model_up_final_workflow, data = model_train_balance)
  model_smote_final_fit <- fit(model_smote_final_workflow, data = model_train_balance)
  
  # Merge final fit result
  # we need the final fit object to make variable importance plot and marginal effects plot
  final_fit <- list(model_down_final_fit = model_down_final_fit,
                    model_up_final_fit = model_up_final_fit,
                    model_smote_final_fit = model_smote_final_fit)
  
  
  # Merge outputs
  output <- list(rf_all_df=rf_all_df, # tuning results - a dataframe
                 best_models = best_models, # best models - list of model objects
                 final_fit = final_fit, # final fit of the best model - list of model objects
                 data_train=model_train_balance, # training data
                 data_test=model_test_balance # test data
  )
  
  return(output)
  
  
  
}
