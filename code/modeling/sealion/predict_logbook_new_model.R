
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
obs_data <- read.csv("data/confidential/processed/fig2_total_merge_final_block.csv")

# predict model
predict_data <- readRDS("data/confidential/processed/logbook_new_pre_model_final.Rds")


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
  select(set_id, block_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(sl_response = as.factor(sl_response)) %>%
  # assign case weight
  mutate(case_wts = ifelse(sl_response == "1", 75, 1),
         case_wts = importance_weights(case_wts))


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
# increase from 1 predictor to total 7 predictors
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




### make prediction on logbook data 
#############################################


predict_data_format <- predict_data %>%
  rename(net_mesh_size_in = mesh_size_in, soak_hr = soak_hour) %>%
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

# Any way to evaluate the model accuracy result?

# Export result

saveRDS(predict_data_final, file.path("model_result/logbook_new_prediction.Rds"))







# read in data

predict_data_final <- readRDS("model_result/logbook_new_prediction.Rds")




### Start to think about making figures
#################################################

#Create base theme of figure

base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))


# Time series plot (by year and season)

predict_time <- predict_data_final %>%
  separate(set_id, into = c("vessel_name", "vessel_id_use", "permit_num", "year", "month", "day"), sep = "-") %>%
  mutate(season = case_when(month%in% c("12", "01", "02")~"winter",
                            month%in% c("03", "04", "05")~"spring",
                            month%in% c("06", "07", "08")~"summer",
                            month%in% c("09", "10", "11")~"fall")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(have_sl_bycatch = ifelse(sl_bycatch_class== "1", "yes", "no")) %>%
  group_by(year, season, have_sl_bycatch) %>%
  mutate(sl_bycatch_class = as.numeric(sl_bycatch_class)) %>%
  summarize(set_number = sum(sl_bycatch_class))



ggplot(data = predict_time %>% filter(have_sl_bycatch == "yes")) +
  geom_bar(aes(x = year, y = set_number, fill = season), stat = "identity") +
  scale_x_continuous(breaks = seq(1981, 2022, 5)) +
  scale_fill_discrete(name = "Season") +
  theme_bw() + base_theme



ggplot(data = predict_time %>% filter(have_sl_bycatch == "yes"), aes(x = year, y = set_number)) +
  geom_line() +
  scale_x_continuous() +
  theme_bw() + base_theme

