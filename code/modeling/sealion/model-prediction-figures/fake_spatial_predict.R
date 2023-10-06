
### clean working environment###
########################################
rm(list = ls())

### load in packages ###
########################################
library(tidyverse)

# model packages
library(tidymodels)
library(vip)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

### read in data ###
##########################################

# build model
obs_data <- read.csv("data/confidential/processed/fig2_total_merge_final.csv")

# predict model
predict_data <- readRDS("data/confidential/processed/fake_spatial_pre_model.Rds")


### build model ###
########################################
response_pre_join <- obs_data %>%
  # don't want data from 1990 - 1994
  filter(data_source != "SWFSC(1990-1994)") %>%
  mutate(sl_response = ifelse(comm_name == "California sea lion", 1, 0)) %>%
  group_by(set_id) %>%
  summarize(sl_response = sum(sl_response)) %>%
  mutate(sl_response = ifelse(sl_response >= 1, 1, sl_response))

predictor_pre_join <- obs_data %>%
  filter(data_source != "SWFSC(1990-1994)") %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  # remove longitude
  select(set_id, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(sl_response = as.factor(sl_response)) %>%
  # assign case weight
  mutate(case_wts = ifelse(sl_response == "1", 75, 1),
         case_wts = importance_weights(case_wts))

#predictors:

# haul_lat_dd
# haul_depth_fa
# soak_hr
# net_mesh_size_in
# dist_km
# julian_day
# sst




####Weighted random forest - ranger
######################################################################

set.seed(1207)
model_split <- initial_split(model_data, strata = sl_response)

model_train <- training(model_split) %>%
  select(-set_id) %>%
  drop_na()

model_test <- testing (model_split) %>%
  select(-set_id) %>%
  drop_na()

model_rec <- recipe(sl_response~., data = model_train)

#engine: ranger
#mode: classification

rf_spec <- rand_forest(mtry = tune()) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# bootstrap

set.seed(1207)

model_fold <- vfold_cv(model_train)

# tune hyperparameter
# increase from 1 predictor to total 8 predictors
param_grid <- grid_regular(mtry(range = c(1, 7)), levels = 7) 

# add workflow to training data 

set.seed(1207)

rf_workflow <- workflow() %>%
  add_recipe(model_rec) %>%
  add_model(rf_spec) %>%
  add_case_weights(case_wts)

# Hyperparameter tuning

rf_res <- 
  rf_workflow %>%
  tune_grid(
    resamples = model_fold,
    grid = param_grid, 
    metrics = metric_set(roc_auc, kap)
  )


# see metric result
rf_res %>%
  collect_metrics()


# see tuning result in figures
rf_res %>%
  collect_metrics() %>%
  mutate(mtry = factor(mtry)) %>%
  ggplot(aes(mtry, mean)) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2)

# select best model and fit to training data 

best_model <- select_best(rf_res, metric = "kap")


rf_final <- finalize_workflow(rf_workflow, best_model)

#last fit
rf_final_fit <- fit(rf_final, data = model_train)

augment(rf_final_fit, new_data = model_test) %>%
  conf_mat(truth = sl_response, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

augment(rf_final_fit, new_data = model_test) %>%
  kap(truth = sl_response, estimate = .pred_class)

#variable importance plot
rf_final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 7)

#marginal plot

explainer_rf <- explain_tidymodels(
  rf_final_fit,
  data = dplyr::select(model_train, -sl_response),
  y = as.integer(model_train$sl_response),
  verbose = FALSE
)

pdp_rf <- model_profile(explainer_rf, 
                        N = NULL, 
                        variables = c("net_mesh_size_in",
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


### make prediction on manufactured data
#############################################

predict_data_format <- predict_data %>%
  rename(haul_lat_dd = lats,
         haul_long_dd = longs,
         julian_day = jdays,
         haul_depth_fa = bathy_fa, 
         net_mesh_size_in = mesh_size,
         sst = mean_sst) %>%
  mutate(haul_depth_fa = as.integer(haul_depth_fa), 
         soak_hr = as.double(soak_hr),
         net_mesh_size_in = as.double(net_mesh_size_in)) %>%
  drop_na()

preds_fac <- predict(rf_final_fit, predict_data_format)
preds_prob <- predict(rf_final_fit, predict_data_format, type = "prob")

predict_data_final <- predict_data_format %>%
  bind_cols(preds_fac) %>%
  bind_cols(preds_prob) %>%
  rename(sl_bycatch_class = .pred_class, sl_bycatch_prob_no = .pred_0, sl_bycatch_prob_yes = .pred_1)


saveRDS(predict_data_final, file.path("data/confidential/processed/fake_spatial_rf_predictions.Rds"))


### making figures ###
######################################


# read in data
predict_data_final <- readRDS("data/confidential/processed/fake_spatial_rf_predictions.Rds")

predict_logbook_final <- readRDS("data/confidential/processed/lb_2000_pre_model.Rds")

# count how many sets in each julian day after year 2010 from logbook
predict_logbook_final_set <- predict_logbook_final %>%
  separate("date", c("year", "month", "day"), sep = "-") %>%
  filter(year >= "2010") %>%
  group_by(julian_day) %>%
  summarize(set_counts = n())


predict_data_join <- left_join(x = predict_data_final, y = predict_logbook_final_set, by = "julian_day")

# weight probability of bycatch by sets numbers from logbook 
predict_data_spatial_summarize <- predict_data_join %>%
  group_by(haul_lat_dd, haul_long_dd) %>%
  summarize(prob_average = weighted.mean(sl_bycatch_prob_yes, set_counts))


# make figure

# read in mexico and usa data
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


ggplot() +
  geom_tile(data = predict_data_spatial_summarize, aes(x = haul_long_dd, y = haul_lat_dd, fill = prob_average)) +
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  scale_fill_gradientn(name = "Bycatch Numbers", colors = RColorBrewer::brewer.pal(9, "YlOrRd")) +
  coord_sf(xlim = c(-121, -117), ylim = c(32, 35)) +
  theme_bw()





#### add month and season to predict data ####
##############################################

# format data
#predict_data_spatial <- predict_data_final %>%
#mutate(season = case_when(julian_day %in% (335:365)~"winter",
#julian_day %in%(1:59)~"winter",
#julian_day %in%(60:151)~"spring",
#julian_day %in%(152:243)~"summer",
#julian_day %in%(224:334)~"fall")) %>%
#mutate(month = case_when(julian_day %in% (1:31)~"Jan",
#julian_day%in%(32:59)~"Feb",
#julian_day%in%(60:90)~"Mar",
#julian_day%in%(91:120)~"Apr",
#julian_day%in%(121:151)~"May",
#julian_day%in%(152:181)~"Jun",
#julian_day%in%(182:212)~"Jul",
#julian_day%in%(213:243)~"Aug",
#julian_day%in%(244:273)~"Sep",
#julian_day%in%(274:304)~"Oct",
#julian_day%in%(305:334)~"Nov",
#julian_day%in%(335:365)~"Dec")) %>%
#mutate(have_sl_bycatch = ifelse(sl_bycatch_class == "1", "yes", "no")) %>%
#sf::st_as_sf(coords = c("haul_long_dd", "haul_lat_dd"), remove = FALSE)










    
    

