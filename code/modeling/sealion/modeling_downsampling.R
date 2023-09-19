

#### clean working environment####
rm(list = ls())

#### load in packages####
library(tidyverse)

library(tidymodels)
library(vip)
library(janitor)
library(randomForest)

#### read in data ####
model_data_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv")


#### Format data: California Sealion
#######################################

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
  select(set_id, haul_lat_dd, haul_long_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst)


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  filter(!duplicated(set_id)) %>%
  mutate(sl_response = as.factor(sl_response))

####Balanced random forest
######################################################################

####RF-downsampling
#####################################################################


# split training and testing data
set.seed(1207)

sampleid_yes <- model_data$set_id[model_data$sl_response == 1]
sampleid_no <- model_data$set_id[model_data$sl_response == 0]

# split is 0.8 training data, 0.2 testing data
# downsampling so there are same amounts of "yes" and "no" in training dataset
sample_yes_train <- sample(sampleid_yes, 0.8*length(sampleid_yes))
sample_no_train <- sample(sampleid_no, length(sample_yes_train))

model_data <- model_data %>%
  mutate(dataset = ifelse(set_id %in% c(sample_yes_train, sample_no_train), "train", "test")) 

model_train <- model_data %>%
  filter(dataset == "train") %>%
  select(-set_id, -dataset) %>%
  drop_na()

model_test <- model_data %>%
  filter(dataset == "test") %>%
  select(-set_id, -dataset) %>%
  drop_na()

# response: sl_response - 0(no)/1(yes)
# predictors: 
  # year(factor)
  # haul_lat_dd(haul latitude)
  # haul_depth_fa(haul depth in fathoms)
  # soak_hr(soak hours)
  # net_mesh_size_in(net mesh size inches)
  # dist_km (distance to shore in km)
  # Julian day
  # sst
  # bathy_fa

#engine: random forest
#mode: classification


####RF-downsampling - themis package(downsampling)
#####################################################################

# split training and testing data

set.seed(1207)
model_split <- initial_split(model_data, strata = sl_response)

model_train <- training(model_split) %>%
  select(-set_id) %>%
  drop_na()

model_test <- testing (model_split) %>%
  select(-set_id) %>%
  drop_na()

model_rec <- recipe(sl_response~., data = model_train)
  # step_smote from themis package use smote algorithm to deal with imbalanced data
  #step_downsample(sl_response)






rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("randomForest", importance = TRUE) %>%
  set_mode("classification")

# bootstrap
set.seed(1207)

model_fold <- vfold_cv(model_train) # default is 10 folds 

# tune hyperparameter
# increase from 1 predictor to total 7 predictors
param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 


#######################################
#### add workflow to training data ####
#######################################

set.seed(1207)

rf_workflow <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(model_rec)

# metric argument just pick up which performance metric should be measured
# if not specified, for classification, accuracy and ROC will be picked
# what is the difference between kap and cohen's kap

rf_res <- 
  rf_workflow %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    metrics = metric_set(roc_auc, kap)
  )

rf_res %>%
  collect_metrics()

rf_res %>%
  collect_metrics() %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(mtry, mean)) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2)

###################################################################################
### select best model and fit to training data ###
###################################################################################

# how to add workflow here?

best_model <- select_best(rf_res, metric = "kap")

rf_final <- finalize_workflow(rf_workflow, best_model)

#last fit
rf_final_fit <- fit(rf_final, data = model_train)
  


# check variable importance plot for the best final fit
rf_final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 8)


# partial dependence plot (pdp)

explainer_rf <- explain_tidymodels(
  rf_final_fit,
  data = dplyr::select(model_train, -sl_response),
  y = as.integer(model_train$sl_response),
  verbose = FALSE
)

pdp_rf <- model_profile(explainer_rf, 
                        N = NULL, 
                        variables = c("haul_long_dd", 
                                      "net_mesh_size_in",
                                      "sst",
                                      "julian_day",
                                      "haul_lat_dd",
                                      "soak_hr",
                                      "dist_km",
                                      "haul_depth_fa"), 
                        groups = NULL)

pdp_rf_df <- as.data.frame(pdp_rf$agr_profiles) %>%
  select(-"_label_",-"_ids_") %>%
  rename(variable_name = "_vname_", predictor = "_x_", prob_prediction = "_yhat_")

ggplot(data = pdp_rf_df, aes(x = predictor, y = prob_prediction)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(.~variable_name, scales = "free_x")



###################################
### fit best model to test data ###
##################################

augment(rf_final_fit, new_data = model_test) %>%
  conf_mat(truth = sl_response, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

augment(rf_final_fit, new_data = model_test) %>%
  accuracy(truth = sl_response, estimate = .pred_class)

augment(rf_final_fit, new_data = model_test) %>%
  kap(truth = sl_response, estimate = .pred_class)

### save model result

saveRDS(rf_res, file.path("model_result/rf_downsample_themis_ratio_1.Rds"))












