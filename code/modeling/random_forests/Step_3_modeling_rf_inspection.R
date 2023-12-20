
### clean working environment ###
rm(list = ls())

### load in library ###
library(tidyverse)

### read in model result ###

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

### Extract the model best fit (to training data)

# sealion - weight - 75
sl_best_fit <- output_sl_weighted[["final_fit"]][[3]]

# harbor seal - SMOTE
hs_best_fit <- output_hs_balanced[["final_fit"]][["model_smote_final_fit"]]

# soupfin shark - SMOTE
ss_best_fit <- output_ss_balanced[["final_fit"]][["model_smote_final_fit"]]


######## Variable Importance ########

# sealion

sl_vi_df <- sl_best_fit %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "California sea lion") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# harbor seal

hs_vi_df <- hs_best_fit %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Harbor seal") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# soupfin shark

ss_vi_df <- ss_best_fit %>%
  extract_fit_parsnip %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Soupfin shark") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)



######## Marginal Effects ########


# sealion

# training data

model_train_weighted_sl <- output_sl_weighted[["data_train"]]

# marginal effect

explainer_rf_sl <- explain_tidymodels(
  sl_best_fit,
  data = dplyr::select(model_train_weighted_sl, -response),
  y = as.integer(model_train_weighted_sl$response),
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


# training data

model_train_balanced_hs <- output_hs_balanced[["data_train"]]

# marginal effect

explainer_rf_hs <- explain_tidymodels(
  hs_best_fit,
  data = dplyr::select(model_train_balanced_hs, -response),
  y = as.integer(model_train_balanced_hs$response),
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


# training data

model_train_balanced_ss <- output_ss_balanced[["data_train"]]


# marginal effect

explainer_rf_ss <- explain_tidymodels(
  ss_best_fit,
  data = dplyr::select(model_train_balanced_ss, -response),
  y = as.integer(model_train_balanced_ss$response),
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

# variable importance (once have more species, we'll add them)

vi_df_final <- rbind(sl_vi_df, hs_vi_df, ss_vi_df)

# marginal effects (once have more species, we'll add them)

me_df_final <- rbind(pdp_rf_df_sl, pdp_rf_df_hs, pdp_rf_df_ss)

# save tables

write.csv(vi_df_final, file = "model_result/variable_importance.csv")

write.csv(me_df_final, file = "model_result/marginal_effects.csv")





