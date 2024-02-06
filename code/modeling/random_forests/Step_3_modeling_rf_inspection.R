
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
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")
# harbor seal
output_hs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/soupfin_shark_model_weighted_rf.Rds")

# Common murre
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/common_murre_model_balanced_rf.Rds")
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/common_murre_model_weighted_rf.Rds")


### Extract the best model object ###

# sealion - weight 50
sl_best_model <- output_sl_weighted[["best_models"]][[2]]

# harbor seal - weight - 150
hs_best_model <- output_hs_weighted[["best_models"]][[6]]

# soupfin shark - weight - 25
ss_best_model <- output_ss_weighted[["best_models"]][[1]]

# common murre - balance - upsample
cm_best_model <- output_cm_balanced[["best_models"]][["model_up_best"]]

### Extract the model best fit (to training data)

# sealion - weight 50
sl_best_fit <- output_sl_weighted[["final_fit"]][[2]]

# harbor seal  - weight - 150
hs_best_fit <- output_hs_weighted[["final_fit"]][[6]]

# soupfin shark - weight - 25
ss_best_fit <- output_ss_weighted[["final_fit"]][[1]]

# common murre - balance - upsample
cm_best_fit <- output_cm_balanced[["final_fit"]][["model_up_final_fit"]]


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

model_train_weighted_sl <- output_sl_weighted[["data_train"]]

# marginal effect

explainer_rf_sl <- explain_tidymodels(
  sl_best_fit,
  data = dplyr::select(model_train_weighted_sl, -response),
  y = as.integer(model_train_weighted_sl$response),
  verbose = FALSE
)

# for continuous variables

pdp_rf_sl <- model_profile(explainer_rf_sl, 
                        N = NULL, 
                        variables = c("mesh_size_in",
                                      "sst_c",
                                      "yday",
                                      "lat_dd",
                                      "long_dd",
                                      "soak_hr",
                                      "shore_km",
                                      "depth_fa"), 
                        groups = NULL)

pdp_rf_df_sl <- as.data.frame(pdp_rf_sl$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "long_dd" ="Longitude (°W)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>%
  # add species column
  mutate(species = "California sea lion")

# for categorical variable - island_yn 

pdp_rf_sl_cat <- model_profile(explainer_rf_sl, N = NULL, variables = "island_yn", variable_type = "categorical", groups = NULL, type = "partial")

pdp_rf_sl_cat_df <- as.data.frame(pdp_rf_sl_cat$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  mutate(variable = recode_factor(variable, "island_yn" = "Island area?")) %>%
  # add species column
  mutate(species = "California sea lion")





# harbor seal


# training data

model_train_weighted_hs <- output_hs_weighted[["data_train"]]

# marginal effect

explainer_rf_hs <- explain_tidymodels(
  hs_best_fit,
  data = dplyr::select(model_train_weighted_hs, -response),
  y = as.integer(model_train_weighted_hs$response),
  verbose = FALSE
)

pdp_rf_hs <- model_profile(explainer_rf_hs, 
                           N = NULL, 
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "long_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa"), 
                           groups = NULL)


pdp_rf_df_hs <- as.data.frame(pdp_rf_hs$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "long_dd" ="Longitude (°W)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>%
  # add species column
  mutate(species = "Harbor seal")


# for categorical variable - island_yn 

pdp_rf_hs_cat <- model_profile(explainer_rf_hs, N = NULL, variables = "island_yn", variable_type = "categorical", groups = NULL, type = "partial")

pdp_rf_hs_cat_df <- as.data.frame(pdp_rf_hs_cat$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  mutate(variable = recode_factor(variable, "island_yn" = "Island area?")) %>%
  # add species
  mutate(species = "Harbor seal")


# soupfin shark


# training data

model_train_weighted_ss <- output_ss_weighted[["data_train"]]


# marginal effect

explainer_rf_ss <- explain_tidymodels(
  ss_best_fit,
  data = dplyr::select(model_train_weighted_ss, -response),
  y = as.integer(model_train_weighted_ss$response),
  verbose = FALSE
)

pdp_rf_ss <- model_profile(explainer_rf_ss, 
                           N = NULL, 
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "long_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa"), 
                           groups = NULL)


pdp_rf_df_ss <- as.data.frame(pdp_rf_ss$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "long_dd" ="Longitude (°W)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>% 
  # add species column
  mutate(species = "Soupfin shark")

# for categorical variable - island_yn

pdp_rf_ss_cat <- model_profile(explainer_rf_ss, N = NULL, variables = "island_yn", variable_type = "categorical", groups = NULL, type = "partial")

pdp_rf_ss_cat_df <- as.data.frame(pdp_rf_ss_cat$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  mutate(variable = recode_factor(variable, "island_yn" = "Island area?")) %>%
  # add species
  mutate(species = "Soupfin shark")





# common murre


# training data

model_train_balanced_cm <- output_cm_balanced[["data_train"]]


# marginal effect

explainer_rf_cm <- explain_tidymodels(
  cm_best_fit,
  data = dplyr::select(model_train_balanced_cm, -response),
  y = as.integer(model_train_balanced_cm$response),
  verbose = FALSE
)

pdp_rf_cm <- model_profile(explainer_rf_cm, 
                           N = NULL, 
                           variables = c("mesh_size_in",
                                         "sst_c",
                                         "yday",
                                         "lat_dd",
                                         "long_dd",
                                         "soak_hr",
                                         "shore_km",
                                         "depth_fa"), 
                           groups = NULL)


pdp_rf_df_cm <- as.data.frame(pdp_rf_cm$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  # Format variables
  mutate(variable=recode_factor(variable, 
                                "yday"="Julian day",      
                                "sst_c"="Temperature (°C)",             
                                "lat_dd"="Latitude (°N)",
                                "long_dd" ="Longitude (°W)",
                                "mesh_size_in"="Mesh size (cm)",
                                "shore_km"="Shore distance (km)",
                                "depth_fa"="Depth (fathoms)",    
                                "soak_hr"="Soak time (hr)")) %>%
  # add species column
  mutate(species = "Common murre")

# for categorical variable - island_yn

pdp_rf_cm_cat <- model_profile(explainer_rf_cm, N = NULL, variables = "island_yn", variable_type = "categorical", groups = NULL, type = "partial")

pdp_rf_cm_cat_df <- as.data.frame(pdp_rf_cm_cat$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable = "_vname_", value = "_x_", prob = "_yhat_") %>%
  mutate(variable = recode_factor(variable, "island_yn" = "Island area?")) %>%
  # add species
  mutate(species = "Common murre")





################# Final table ####################

# variable importance

vi_df_final <- rbind(sl_vi_df, hs_vi_df, ss_vi_df, cm_vi_df)

# marginal effects (once have more species, we'll add them)

me_df_final <- rbind(pdp_rf_df_sl, pdp_rf_df_hs, pdp_rf_df_ss, pdp_rf_df_cm)

me_df_cat_final <- rbind(pdp_rf_sl_cat_df, pdp_rf_hs_cat_df, pdp_rf_ss_cat_df, pdp_rf_cm_cat_df) %>%
  mutate(value = case_when(value == "0"~"No",
                           value == "1"~"Yes"))

# save tables

write.csv(vi_df_final, file = "model_result/variable_importance.csv", row.names = FALSE)

write.csv(me_df_final, file = "model_result/marginal_effects.csv", row.names = FALSE)

write.csv(me_df_cat_final, file = "model_result/categorical_marginal_effects.csv", row.names = FALSE)





