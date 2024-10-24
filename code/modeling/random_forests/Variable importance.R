
rm(list = ls())

library(tidyverse)

library(tidymodels)
library(vip)
library(ranger)
library(DALEXtra)
library(workflows)

# read in data

model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds") 


#####################################################################################

# California sealion

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
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
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(shore_km<=20) %>%
  filter(soak_hr<=96) %>%
  filter(depth_fa <=100) %>%
  filter(!duplicated(set_id))


#Join model data

# Weighted rf
model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

#assign weight to model data
model_data_weighted <- model_data_weighted %>%
  mutate(case_wts = ifelse(response == "1", 25, 1),
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
  set_engine("ranger", importance = "impurity_corrected") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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
  mutate(weight = 25, balanced_type = "weighted")

# Extract the best model via cross-validation from each weight
model_weighted_best <- select_best(rf_res_weighted, metric = "kap")

# finalize the best model workflow for each weight
model_weighted_final_workflow <- finalize_workflow(rf_workflow_weighted, model_weighted_best)

# finalize the best fit for each weight
model_weighted_final_fit <- fit(model_weighted_final_workflow, data = model_train_weighted)


sl_vi_df <- model_weighted_final_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "California sea lion") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

write.csv(sl_vi_df, "model_result/sl_vi_ranger_weighted_impurity_corrected.csv", row.names = FALSE)

#####################################################################################

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
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(shore_km<=20) %>%
  filter(soak_hr<=96) %>%
  filter(depth_fa <=100) %>%
  filter(!duplicated(set_id))


#Join model data

# Weighted rf
model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

#assign weight to model data
model_data_weighted <- model_data_weighted %>%
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
  set_engine("ranger", importance = "impurity_corrected") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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
  mutate(weight = 75, balanced_type = "weighted")

# Extract the best model via cross-validation from each weight
model_weighted_best <- select_best(rf_res_weighted, metric = "kap")

# finalize the best model workflow for each weight
model_weighted_final_workflow <- finalize_workflow(rf_workflow_weighted, model_weighted_best)

# finalize the best fit for each weight
model_weighted_final_fit <- fit(model_weighted_final_workflow, data = model_train_weighted)


hs_vi_df <- model_weighted_final_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Harbor seal") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

write.csv(hs_vi_df, "model_result/hs_vi_ranger_weighted_impurity_corrected.csv", row.names = FALSE)

#####################################################################################

# Common murre

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Common murre", 1, 0)) %>%
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
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(shore_km<=20) %>%
  filter(soak_hr<=96) %>%
  filter(depth_fa <=100) %>%
  filter(!duplicated(set_id))


#Join model data

# Weighted rf
model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

#assign weight to model data
model_data_weighted <- model_data_weighted %>%
  mutate(case_wts = ifelse(response == "1", 25, 1),
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
  set_engine("ranger", importance = "impurity_corrected") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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
  mutate(weight = 25, balanced_type = "weighted")

# Extract the best model via cross-validation from each weight
model_weighted_best <- select_best(rf_res_weighted, metric = "kap")

# finalize the best model workflow for each weight
model_weighted_final_workflow <- finalize_workflow(rf_workflow_weighted, model_weighted_best)

# finalize the best fit for each weight
model_weighted_final_fit <- fit(model_weighted_final_workflow, data = model_train_weighted)


cm_vi_df <- model_weighted_final_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Common murre") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

write.csv(cm_vi_df, "model_result/cm_vi_ranger_weighted_impurity_corrected.csv", row.names = FALSE)


#####################################################################################

# Northern elephant seal

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Northern elephant seal", 1, 0)) %>%
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
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
  filter(shore_km<=20) %>%
  filter(soak_hr<=96) %>%
  filter(depth_fa <=100) %>%
  filter(!duplicated(set_id))


#Join model data

# Weighted rf
model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response))

#assign weight to model data
model_data_weighted <- model_data_weighted %>%
  mutate(case_wts = ifelse(response == "1", 25, 1),
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
  set_engine("ranger", importance = "impurity_corrected") %>%
  set_mode("classification")

# hyperparameter tuning
set.seed(1207)

model_fold <- vfold_cv(model_train_weighted)

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 

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
  mutate(weight = 25, balanced_type = "weighted")

# Extract the best model via cross-validation from each weight
model_weighted_best <- select_best(rf_res_weighted, metric = "kap")

# finalize the best model workflow for each weight
model_weighted_final_workflow <- finalize_workflow(rf_workflow_weighted, model_weighted_best)

# finalize the best fit for each weight
model_weighted_final_fit <- fit(model_weighted_final_workflow, data = model_train_weighted)


ns_vi_df <- model_weighted_final_fit %>%
  extract_fit_engine() %>%
  vi() %>%
  as.data.frame() %>%
  mutate(species = "Northern elephant seal") %>%
  rename(variable = Variable, importance = Importance) %>%
  select(species, variable, importance)

write.csv(ns_vi_df, "model_result/ns_vi_ranger_weighted_impurity_corrected.csv", row.names = FALSE)























































####################################################################
######### Make figure below

library(tidyverse)

vi_ranger_impurity <- read.csv("model_result/cm_ranger_impurity_trail.csv") %>%
  mutate(package = "ranger",
         metric = "impurity")

vi_ranger_permutation <- read.csv("model_result/cm_ranger_permutation_trail.csv") %>%
  mutate(package = "ranger",
         metric = "permutation")

vi_randomforest_impurity <- read.csv("model_result/cm_randomforest_impurity_trail.csv") %>%
  rename(importance = MeanDecreaseGini ) %>%
  mutate(package = "randomforest",
         metric = "impurity")

vi_randomforest_permutation <- read.csv("model_result/cm_randomforest_permutation_trail.csv") %>%
  rename(importance = MeanDecreaseAccuracy) %>%
  mutate(package = "randomforest",
         metric = "permutation")

vi_all <- bind_rows(vi_ranger_impurity, vi_ranger_permutation, vi_randomforest_impurity, vi_randomforest_permutation)

data <- vi_all %>%
  mutate(variable = recode_factor(variable, 
                                  "yday"="Julian day",      
                                  "sst_c"="Temperature",             
                                  "lat_dd"="Latitude",
                                  "long_dd" = "Longitude",
                                  "mesh_size_in"="Mesh size",
                                  "shore_km"="Shore distance",
                                  "depth_fa"="Depth",    
                                  "soak_hr"="Soak time",
                                  "island_yn_X1" = "Island area?")) %>%
  mutate(category = case_when(variable %in% c("Mesh size", "Soak time")~"Fishing-related",
                              variable %in% c("Latitude", "Longitude","Depth", "Shore distance", "Island area?")~"Spatial",
                              variable %in% c("Julian day")~"Temporal",
                              variable%in% c("Temperature")~"Environmental"))



base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

# set category color
category_color <- c("Spatial" = "#B83945", "Environmental" = "#4F845C", "Temporal" = "#E3E457", "Fishing-related" = "#377483")

# Plot
g_impurity <- ggplot(data %>% filter(metric == "impurity"), aes(x=importance, 
                                                                y= tidytext::reorder_within(variable, importance, package),
                                                                fill = category)) +
  facet_wrap(~package, scales="free_y") +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Category", values = category_color) +
  # Labels
  labs(x="Variable importance", y="Variables") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())

g_impurity


g_permutation <- ggplot(data %>% filter(metric == "permutation"), aes(x=importance, 
                                                                      y=tidytext::reorder_within(variable, importance, package),
                                                                      fill = category)) +
  facet_wrap(~package, scales="free") +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Category", values = category_color) +
  # Labels
  labs(x="Variable importance", y="Variables") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())

g_permutation


plotdir <- "figures"


# Export
ggsave(g_impurity, filename=file.path(plotdir, "FigSX_variable_importance_trail_impurity.png"), 
       width=5.5, height=4, units="in", dpi=600)

ggsave(g_permutation, filename=file.path(plotdir, "FigSX_variable_importance_trail_permutation.png"), 
       width=5.5, height=4, units="in", dpi=600)

write.csv(vi_all, file = "model_result/cm_vi_trail.csv", row.names = FALSE)


##################################################################

vi_ranger_permutation_weighted <- read.csv("model_result/hs_vi_permutation_scaled.csv") %>%
  mutate(package = "ranger",
         metric = "permutation",
         type = "weighted random forest")



vi_ranger_permutation_nonweighted <- read.csv("model_result/hs_vi_ranger_permutation_trial.csv") %>%
  mutate(package = "ranger",
         metric = "permutation",
         type = "regular random forest") %>%
  select(-X)

vi_all <- bind_rows(vi_ranger_permutation_weighted, vi_ranger_permutation_nonweighted)

data <- vi_all %>%
  mutate(variable = recode_factor(variable, 
                                  "yday"="Julian day",      
                                  "sst_c"="Temperature",             
                                  "lat_dd"="Latitude",
                                  "long_dd" = "Longitude",
                                  "mesh_size_in"="Mesh size",
                                  "shore_km"="Shore distance",
                                  "depth_fa"="Depth",    
                                  "soak_hr"="Soak time",
                                  "island_yn_X1" = "Island area?")) %>%
  mutate(category = case_when(variable %in% c("Mesh size", "Soak time")~"Fishing-related",
                              variable %in% c("Latitude", "Longitude","Depth", "Shore distance", "Island area?")~"Spatial",
                              variable %in% c("Julian day")~"Temporal",
                              variable%in% c("Temperature")~"Environmental"))

# set category color
category_color <- c("Spatial" = "#B83945", "Environmental" = "#4F845C", "Temporal" = "#E3E457", "Fishing-related" = "#377483")



g_permutation <- ggplot(data, aes(x=importance, 
                                  y=tidytext::reorder_within(variable, importance, type),
                                  fill = category)) +
  facet_wrap(~type, scales="free") +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Category", values = category_color) +
  # Labels
  labs(x="Variable importance", y="Variables") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())

g_permutation






