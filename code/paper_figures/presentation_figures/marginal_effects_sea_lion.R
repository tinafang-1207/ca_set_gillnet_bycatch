

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(tidymodels)
library(vip)
#library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

# Directories
plotdir <- "figures"

# Read data
ref_rs <- readRDS("model_result/weighted_rf_weight75.Rds")


# Build data
################################################################################


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

# check variable importance plot for the best final fit
rf_final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 8)

augment(rf_final_fit, new_data = model_test) %>%
  conf_mat(truth = sl_response, estimate = .pred_class) %>%
  autoplot(type = "heatmap")


augment(rf_final_fit, new_data = model_test) %>%
  accuracy(truth = sl_response, estimate = .pred_class)

augment(rf_final_fit, new_data = model_test) %>%
  kap(truth = sl_response, estimate = .pred_class)

# marginal effect plot
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
                                      "haul_long_dd",
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

# Plot data
################################################################################

