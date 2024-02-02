
# clean working environment
rm(list = ls())

# read in packages
library(tidyverse)

library(tidymodels)
library(vip)
library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

library(probably)

# read in data
model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

# Get prediction result from training data
###########################################################################
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
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
  mutate(case_wts = ifelse(response == "1", 150, 1),
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
  set_engine("ranger", importance = "impurity") %>%
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
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_weighted %>% 
  select_best(metric = "kap")

thresholds <- seq(0.1, 0.9, by = 0.01)
  
hs_kap <- rf_res_weighted %>%
  collect_predictions(parameter = rf_best) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Harbor seal")



ggplot(data = hs_kap, aes(x = .threshold, y = .estimate)) +
  geom_line() +
  facet_wrap(.~.metric) +
  theme_bw()

###########################################################################
# California sea lion

# Format data (predictor and response)

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
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
  mutate(case_wts = ifelse(response == "1", 50, 1),
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
  set_engine("ranger", importance = "impurity") %>%
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
rf_res_weighted_sl <- 
  rf_workflow_weighted %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_weighted_sl %>% 
  select_best(metric = "kap")


###########################################################################
# Soupfin shark

# Format data (predictor and response)

response_pre_join <- model_orig %>%
  mutate(response = ifelse(comm_name == "Soupfin shark", 1, 0)) %>%
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
  filter(!duplicated(set_id))

model_data_weighted <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(response = as.factor(response)) %>%
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
  set_engine("ranger", importance = "impurity") %>%
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
rf_res_weighted_ss <- 
  rf_workflow_weighted %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )

rf_best <- 
  rf_res_weighted_ss %>% 
  select_best(metric = "kap")

###########################################################################
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
# long_dd = longitude


predictor_pre_join <- model_orig %>%
  select(set_id, lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  mutate(island_yn = as.factor(island_yn)) %>%
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

# Balanced rf- upsampling
model_rec_up <- recipe(response~., data = model_train_balance) %>%
  step_dummy(island_yn) %>%
  step_normalize(all_predictors()) %>%
  step_upsample(response)


# Set up mode and engine for rf model

# Balanced rf
rf_spec_balanced <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")


# Set up grid search
set.seed(1207)

model_fold <- vfold_cv(model_train_balance, strata = response)

param_grid <- grid_regular(mtry(range = c(1, 9)), levels = 9) 


# Set up workflow

# Balanced rf
set.seed(1207)

rf_workflow_up <- workflow()%>%
  add_model(rf_spec_balanced) %>%
  add_recipe(model_rec_up)

# Set up grid tuning

rf_res_up <- 
  rf_workflow_up %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    control = control_grid(save_pred = TRUE),
    metrics = metric_set(roc_auc, kap)
  )


rf_best <- 
  rf_res_up %>% 
  select_best(metric = "kap")


############################################################
# select kappa threshold

thresholds <- seq(0.1, 0.9, by = 0.01)

hs_kap <- rf_res_weighted %>%
  collect_predictions(parameter = rf_best) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Harbor seal") %>%
  mutate(type = "training data")

sl_kap <- rf_res_weighted_sl %>%
  collect_predictions(parameter = rf_best) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "California sea lion") %>%
  mutate(type = "training data")

ss_kap <- rf_res_weighted_ss %>%
  collect_predictions(parameter = rf_best) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Soupfin shark") %>%
  mutate(type = "training data")

cm_kap <- rf_res_up %>%
  collect_predictions(parameter = rf_best) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Common murre") %>%
  mutate(type = "training data")

################################################################################

# test data

# best model

# sealion - SMOTE
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")

# harbor seal-weight 75
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

# soupfin shark - Upsample
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/soupfin_shark_model_weighted_rf.Rds")

# common murre - weight 25
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/common_murre_model_balanced_rf.Rds")


### Extract the model best fit (to training data)

# sealion - weight - 50
sl_best_fit <- output_sl_weighted[["final_fit"]][[2]]

# harbor seal - weight - 150
hs_best_fit <- output_hs_weighted[["final_fit"]][[6]]

# soupfin shark - Upsample
ss_best_fit <- output_ss_weighted[["final_fit"]][[1]]

# common murre - weight - 25
cm_best_fit <- output_cm_balanced[["final_fit"]][["model_up_final_fit"]]

### Extract test data

# California sea lion
model_test_weighted_sl <- output_sl_weighted[["data_test"]]

# Harbor seal
model_test_weighted_hs <- output_hs_weighted[["data_test"]]

# Soupfin shark
model_test_weighted_ss <- output_ss_weighted[["data_test"]]

# Common murre
model_test_balanced_cm <- output_cm_balanced[["data_test"]]


sl_test_kap <- predict(sl_best_fit, new_data = model_test_weighted_sl, type = "prob") %>%
  bind_cols(model_test_weighted_sl) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "California sea lion") %>%
  mutate(type = "test data")


hs_test_kap <- predict(hs_best_fit, new_data = model_test_weighted_hs, type = "prob") %>%
  bind_cols(model_test_weighted_hs) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Harbor seal") %>%
  mutate(type = "test data")


ss_test_kap <- predict(ss_best_fit, new_data = model_test_weighted_ss, type = "prob") %>%
  bind_cols(model_test_weighted_ss) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Soupfin shark") %>%
  mutate(type = "test data")

cm_test_kap <- predict(cm_best_fit, new_data = model_test_balanced_cm, type = "prob") %>%
  bind_cols(model_test_balanced_cm) %>%
  threshold_perf(response, estimate = .pred_1, thresholds = thresholds, metrics = metric_set(kap), event_level = "second") %>%
  mutate(species = "Common murre") %>%
  mutate(type = "test data")



# save the table

eva_all <- bind_rows(sl_kap, hs_kap, ss_kap, cm_kap, sl_test_kap, hs_test_kap, ss_test_kap, cm_test_kap)

write.csv(eva_all, file = "model_result/threshold_kappa.csv", row.names = FALSE)


# Format eva_all
eva_all_format <- eva_all %>%
  rename(Threshold = .threshold,
         Kappa = .estimate,
         Species = species,
         Type = type)

# select max kappa value

eva_all_kappa <- eva_all_format %>%
  filter(Type == "training data") %>%
  group_by(Species) %>%
  filter(Kappa == max(Kappa))

eva_all_test_kappa <- eva_all_format %>%
  filter(Type == "test data") %>%
  filter(Species == "California sea lion"&Threshold == 0.51|Species == "Harbor seal"&Threshold == 0.50|Species == "Soupfin shark"&Threshold == 0.58|Species == "Common murre"&Threshold == 0.38)

eva_all_kappa_final <- bind_rows(eva_all_kappa, eva_all_test_kappa)



# Base theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=7),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))





# make figure

g <- ggplot(data = eva_all_format, aes(x = Threshold, y = Kappa, color = Species)) +
  geom_line(aes(linetype = Type)) +
  geom_point(data = eva_all_kappa_final, aes(x = Threshold, y = Kappa, color = Species)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey") +
  geom_text(data = eva_all_kappa_final %>% filter(Type == "training data"), 
            aes(x = Threshold, y = Kappa, color = Species, label = Threshold), 
            vjust = -0.2) +
  facet_wrap(.~.metric) +
  theme_bw() + base_theme

g

plotdir <- "figures"

ggsave(g, filename=file.path(plotdir, "FigSX_rf_threshold_selection_kappa.png"),
       width=5.5, height=4, units="in", dpi=600)




