
### clean working environment ###
rm(list = ls())

### load in library ###
library(tidyverse)

library(tidymodels)
library(vip)
library(janitor)
library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

### read in data ###

# sealion
output_sl_balanced <- readRDS("model_result/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("model_result/weighted_rf/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("model_result/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("model_result/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("model_result/balanced_rf/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("model_result/weighted_rf/soupfin_shark_model_weighted_rf.Rds")


### Extract the best model object ###

# sealion - weight - 75
sl_best_model <- output_sl_weighted[["best_model"]][[3]]

# harbor seal - SMOTE
hs_best_model <- output_hs_balanced[["best_models"]][["model_smote_best"]]

# soupfin shark - SMOTE
ss_best_model <- output_ss_balanced[["best_models"]][["model_smote_best"]]


##### this will change later ##### 

#### do a model fit for only the selected model below ####

# read in model data
model_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv")

# sealion

response_pre_join <- model_orig %>%
  # don't want data from 1990 - 1994
  filter(data_source != "SWFSC(1990-1994)") %>%
  mutate(sl_response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(sl_response = sum(sl_response)) %>%
  mutate(sl_response = ifelse(sl_response >= 1, 1, sl_response))

predictor_pre_join <- model_orig %>%
  filter(data_source != "SWFSC(1990-1994)") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  select(set_id, haul_long_dd, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(sl_response = as.factor(sl_response)) %>%
  # assign case weight
  mutate(case_wts = ifelse(sl_response == "1", 75, 1),
         case_wts = importance_weights(case_wts))

# model below

set.seed(1207)
model_split <- initial_split(model_data, prop = 4/5, strata = sl_response)

model_train <- training(model_split) %>%
  select(-set_id) %>%
  drop_na()

model_test <- testing (model_split) %>%
  select(-set_id) %>%
  drop_na()

model_rec <- recipe(sl_response~., data = model_train)

rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")


rf_workflow <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_spec) %>%
  add_case_weights(case_wts)


rf_final_sl <- finalize_workflow(rf_workflow, sl_best_model)

rf_final_fit_sl <- fit(rf_final_sl, data = model_train)

# variable importance

sl_vi_df <- rf_final_fit_sl %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "California sea lion") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# marginal effect

explainer_rf_sl <- explain_tidymodels(
  rf_final_fit_sl,
  data = dplyr::select(model_train, -sl_response),
  y = as.integer(model_train$sl_response),
  verbose = FALSE
)

pdp_rf_sl <- model_profile(explainer_rf_sl, 
                        N = NULL, 
                        variables = c("net_mesh_size_in",
                                      "sst",
                                      "julian_day",
                                      "haul_long_dd",
                                      "haul_lat_dd",
                                      "soak_hr",
                                      "dist_km",
                                      "haul_depth_fa"), 
                        groups = NULL)


pdp_rf_df_sl <- as.data.frame(pdp_rf_sl$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "haul_long_dd"="Longitude (°W)",     
                                "julian_day"="Julian day",      
                                "sst"="Temperature (°C)",             
                                "haul_lat_dd"="Latitude (°N)",
                                "net_mesh_size_in"="Mesh size (cm)",
                                "dist_km"="Shore distance (km)",
                                "haul_depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "California sea lion")



  



# harbor seal

response_pre_join <- model_orig %>%
  # don't want data from 1990 - 1994
  filter(data_source != "SWFSC(1990-1994)") %>%
  mutate(hs_response = ifelse(comm_name == "Harbor seal", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(hs_response = sum(hs_response)) %>%
  mutate(hs_response = ifelse(hs_response >= 1, 1, hs_response))

predictor_pre_join <- model_orig %>%
  filter(data_source != "SWFSC(1990-1994)") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  select(set_id, haul_long_dd, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(hs_response = as.factor(hs_response))

# model below

set.seed(1207)
model_split <- initial_split(model_data, prop = 4/5, strata = hs_response)

model_train <- training(model_split) %>%
  select(-set_id) %>%
  drop_na()

model_test <- testing (model_split) %>%
  select(-set_id) %>%
  drop_na()

model_rec <- recipe(hs_response~., data = model_train) %>%
  step_smote(hs_response)

rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

set.seed(1207)

rf_workflow <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_spec)


rf_final_hs <- finalize_workflow(rf_workflow, hs_best_model)

rf_final_fit_hs <- fit(rf_final_hs, data = model_train)

# variable importance

hs_vi_df <- rf_final_fit_hs %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Harbor seal") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# marginal effect

explainer_rf_hs <- explain_tidymodels(
  rf_final_fit_hs,
  data = dplyr::select(model_train, -hs_response),
  y = as.integer(model_train$hs_response),
  verbose = FALSE
)

pdp_rf_hs <- model_profile(explainer_rf_hs, 
                           N = NULL, 
                           variables = c("net_mesh_size_in",
                                         "sst",
                                         "julian_day",
                                         "haul_long_dd",
                                         "haul_lat_dd",
                                         "soak_hr",
                                         "dist_km",
                                         "haul_depth_fa"), 
                           groups = NULL)


pdp_rf_df_hs <- as.data.frame(pdp_rf_hs$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "haul_long_dd"="Longitude (°W)",     
                                "julian_day"="Julian day",      
                                "sst"="Temperature (°C)",             
                                "haul_lat_dd"="Latitude (°N)",
                                "net_mesh_size_in"="Mesh size (cm)",
                                "dist_km"="Shore distance (km)",
                                "haul_depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "Harbor seal")






# soupfin shark

response_pre_join <- model_orig %>%
  # don't want data from 1990 - 1994
  filter(data_source != "SWFSC(1990-1994)") %>%
  mutate(ss_response = ifelse(comm_name == "Soupfin shark", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(ss_response = sum(ss_response)) %>%
  mutate(ss_response = ifelse(ss_response >= 1, 1, ss_response))

predictor_pre_join <- model_orig %>%
  filter(data_source != "SWFSC(1990-1994)") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  select(set_id, haul_long_dd, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(ss_response = as.factor(ss_response))

# model below

set.seed(1207)
model_split <- initial_split(model_data, prop = 4/5, strata = ss_response)

model_train <- training(model_split) %>%
  select(-set_id) %>%
  drop_na()

model_test <- testing (model_split) %>%
  select(-set_id) %>%
  drop_na()

model_rec <- recipe(ss_response~., data = model_train) %>%
  step_smote(ss_response)

rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

set.seed(1207)

rf_workflow <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_spec)


rf_final_ss <- finalize_workflow(rf_workflow, ss_best_model)

rf_final_fit_ss <- fit(rf_final_ss, data = model_train)

# variable importance

ss_vi_df <- rf_final_fit_ss %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Soupfin shark") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# marginal effect

explainer_rf_ss <- explain_tidymodels(
  rf_final_fit_ss,
  data = dplyr::select(model_train, -ss_response),
  y = as.integer(model_train$ss_response),
  verbose = FALSE
)

pdp_rf_ss <- model_profile(explainer_rf_ss, 
                           N = NULL, 
                           variables = c("net_mesh_size_in",
                                         "sst",
                                         "julian_day",
                                         "haul_long_dd",
                                         "haul_lat_dd",
                                         "soak_hr",
                                         "dist_km",
                                         "haul_depth_fa"), 
                           groups = NULL)


pdp_rf_df_ss <- as.data.frame(pdp_rf_ss$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "haul_long_dd"="Longitude (°W)",     
                                "julian_day"="Julian day",      
                                "sst"="Temperature (°C)",             
                                "haul_lat_dd"="Latitude (°N)",
                                "net_mesh_size_in"="Mesh size (cm)",
                                "dist_km"="Shore distance (km)",
                                "haul_depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "Soupfin shark")






################# Final table ####################

# variable importance

vi_df_final <- rbind(sl_vi_df, hs_vi_df, ss_vi_df)

me_df_final <- rbind(pdp_rf_df_sl, pdp_rf_df_hs, pdp_rf_df_ss)

# marginal effect

write.csv(vi_df_final, file = "model_result/variable_importance.csv")

write.csv(me_df_final, file = "model_result/marginal_effects.csv")





