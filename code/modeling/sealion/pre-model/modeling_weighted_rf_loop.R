
#### clean working environment####
rm(list = ls())

#### load in packages####
library(tidyverse)

library(tidymodels)
library(vip)
#library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)



#### read in data
######################################################################

model_data_orig <- read.csv("data/confidential/processed/fig2_total_merge_final_block.csv")


#### Format model data: California Sealion
######################################################################

response_pre_join <- model_data_orig %>%
  # don't want data from 1990 - 1994
  filter(data_source != "SWFSC(1990-1994)") %>%
  mutate(sl_response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(sl_response = sum(sl_response)) %>%
  mutate(sl_response = ifelse(sl_response >= 1, 1, sl_response))

predictor_pre_join <- model_data_orig %>%
  filter(data_source != "SWFSC(1990-1994)") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  select(set_id, block_long_dd, block_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(sl_response = as.factor(sl_response))

#### start for loop
######################################################################

wt_vec <- seq(25, 200, 25)

i<-1

for(i in 1:length(wt_vec)){
  
  #set up wt_do
  wt_do <- wt_vec[i]
  
  #assign weight to model data
  model_data <- model_data %>%
    mutate(case_wts = ifelse(sl_response == "1", wt_do, 1),
           case_wts = importance_weights(case_wts))
  
  # split between training and testing data
  set.seed(1207)
  
  model_split <- initial_split(model_data, strata = sl_response)
  
  model_train <- training(model_split) %>%
    select(-set_id) %>%
    drop_na()
  
  model_test <- testing (model_split) %>%
    select(-set_id) %>%
    drop_na()
  
  # set up model recipe
  model_rec <- recipe(sl_response~., data = model_train)
  
  # set up model engine
  rf_spec <- rand_forest(mtry = tune()) %>%
    set_engine("ranger", importance = "impurity") %>%
    set_mode("classification")
  
  # hyperparameter tuning
  set.seed(1207)
  
  model_fold <- vfold_cv(model_train)
  
  param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 
  
  # set up model workflow
  set.seed(1207)
  
  rf_workflow <- workflow() %>%
    add_recipe(model_rec) %>%
    add_model(rf_spec) %>%
    add_case_weights(case_wts)
  
  
  # Tuning hyperparameters
  rf_res <- 
    rf_workflow %>%
    tune_grid(
      resamples = model_fold,
      grid = param_grid, 
      metrics = metric_set(roc_auc, kap)
    )
  
  
  # set up extracted dataframe
  rf_res_df <- rf_res %>%
    collect_metrics() %>%
    as.data.frame() %>%
    filter(.metric == "kap") %>%
    filter(mean == max(mean)) %>%
    select(mtry, mean)
  
  if(i == 1) {df_out <- rf_res_df}else{df_out <- rbind(df_out, rf_res_df)}

 }

# Format tuning table
wt_tuning_df <- wt_vec %>%
  as.data.frame() %>%
  mutate(mtry = df_out$mtry, mean_kappa = df_out$mean) %>%
  rename(case_wts = ".")

# Make figures
ggplot(data = wt_tuning_df, aes(x = case_wts, y = mean_kappa)) +
  geom_point(size = 2)

# Export tuning result
write.table(wt_tuning_df, file = "model_result/weighted_rf_tuning.csv", row.names = F, sep = ",")




