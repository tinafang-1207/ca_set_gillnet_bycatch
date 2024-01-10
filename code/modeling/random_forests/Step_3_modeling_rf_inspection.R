
### clean working environment ###
rm(list = ls())

### load in library ###
library(tidyverse)
library(tidymodels)
library(vip)
library(DALEXtra)

### read in model result ###

### move forward with four species ONLY

# California sea lion
# Harbor seal
# Soupfin shark
# Common murre

# sealion
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/california_sea_lion_model_weighted_rf.Rds")
# harbor seal
output_hs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/soupfin_shark_model_weighted_rf.Rds")

# Common murre
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/common_murre_model_balanced_rf.Rds")
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/common_murre_model_weighted_rf.Rds")


### Extract the best model object ###

# sealion - SMOTE
sl_best_model <- output_sl_balanced[["best_models"]][["model_smote_best"]]

# harbor seal - weighted - 75
hs_best_model <- output_hs_weighted[["best_models"]][[3]]

# soupfin shark - SMOTE
ss_best_model <- output_ss_balanced[["best_models"]][["model_smote_best"]]

# common murre - weighted-25
cm_best_model <- output_cm_weighted[["best_models"]][[1]]

### Extract the model best fit (to training data)

# sealion - SMOTE
sl_best_fit <-  output_sl_balanced[["final_fit"]][["model_smote_final_fit"]]

# harbor seal - weighted - 75
hs_best_fit <- output_hs_weighted[["final_fit"]][[3]]

# soupfin shark - SMOTE
ss_best_fit <- output_ss_balanced[["final_fit"]][["model_smote_final_fit"]]

# common murre - weighted -25
cm_best_fit <- output_cm_weighted[["final_fit"]][[1]]


######## Variable Importance ########

# sealion

sl_vi_df <- sl_best_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "California sea lion") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# harbor seal

hs_vi_df <- hs_best_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Harbor seal") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# soupfin shark

ss_vi_df <- ss_best_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Soupfin shark") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

# common murre
cm_vi_df <- cm_best_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Common murre") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)



######## Marginal Effects ########


# sealion

# training data

model_train_weighted_sl <- output_sl_balanced[["data_train"]]

# marginal effect

explainer_rf_sl <- explain_tidymodels(
  sl_best_fit,
  data = dplyr::select(model_train_weighted_sl, -response),
  y = as.integer(model_train_weighted_sl$response),
  verbose = FALSE
)

pdp_rf_sl <- model_profile(explainer_rf_sl, 
                        N = NULL, 
                        variables = c("mesh_size_in",
                                      "sst_c",
                                      "yday",
                                      "lat_dd",
                                      "soak_hr",
                                      "shore_km",
                                      "depth_fa",
                                      "island_yn"), 
                        groups = NULL)


pdp_rf_df_sl <- as.data.frame(pdp_rf_sl$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)",
                                "island_yn" = "Island location (yes or no)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "California sea lion")



# harbor seal


# training data

model_train_balanced_hs <- output_hs_weighted[["data_train"]]

# marginal effect

explainer_rf_hs <- explain_tidymodels(
  hs_best_fit,
  data = dplyr::select(model_train_balanced_hs, -response),
  y = as.integer(model_train_balanced_hs$response),
  verbose = FALSE
)

pdp_rf_hs <- model_profile(explainer_rf_hs, 
                           N = NULL, 
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa",
                                         "island_yn"), 
                           groups = NULL)


pdp_rf_df_hs <- as.data.frame(pdp_rf_hs$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)",
                                "island_yn" = "Island location (yes or no)")) %>% 
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
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa",
                                         "island_yn"), 
                           groups = NULL)


pdp_rf_df_ss <- as.data.frame(pdp_rf_ss$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)",
                                "island_yn" = "Island location (yes or no)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "Soupfin shark")





# common murre


# training data

model_train_weighted_cm <- output_cm_weighted[["data_train"]]


# marginal effect

explainer_rf_cm <- explain_tidymodels(
  cm_best_fit,
  data = dplyr::select(model_train_weighted_cm, -response),
  y = as.integer(model_train_weighted_cm$response),
  verbose = FALSE
)

pdp_rf_cm <- model_profile(explainer_rf_cm, 
                           N = NULL, 
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa",
                                         "island_yn"), 
                           groups = NULL)


pdp_rf_df_cm <- as.data.frame(pdp_rf_cm$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)",
                                "island_yn" = "Island location (yes or no)")) %>% 
  # Remove outlier values (BUT WE SHOULD DO THIS RIGHT SOMEWHERE)
  filter(!(variable=="Depth (fathoms)" & value>100)) %>% 
  filter(!(variable=="Shore distance (km)" & value>20)) %>% 
  filter(!(variable=="Soak time (hr)" & value>96)) %>%
  # add species column
  mutate(species = "Common murre")





################# Final table ####################

# variable importance

vi_df_final <- rbind(sl_vi_df, hs_vi_df, ss_vi_df, cm_vi_df)

# marginal effects (once have more species, we'll add them)

me_df_final <- rbind(pdp_rf_df_sl, pdp_rf_df_hs, pdp_rf_df_ss, pdp_rf_df_cm)

# save tables

write.csv(vi_df_final, file = "model_result/variable_importance.csv")

write.csv(me_df_final, file = "model_result/marginal_effects.csv")





