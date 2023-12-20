

#### clean working environment####
rm(list = ls())

#### load in packages####
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
confdatadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential" # chris

#### read in data
######################################################################

model_data_orig <- read_csv(file.path(confdatadir, "processed/fig2_total_merge_final.csv"))

#### Format model data: California Sealion
######################################################################

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
  select(set_id, haul_long_dd, haul_lat_dd, haul_depth_fa, soak_hr, net_mesh_size_in, dist_km, julian_day, sst) %>%
  filter(!duplicated(set_id))


model_data <- left_join(response_pre_join, predictor_pre_join, by = "set_id") %>%
  mutate(sl_response = as.factor(sl_response)) %>%
  # assign case weight
  mutate(case_wts = ifelse(sl_response == "1", 75, 1),
         case_wts = importance_weights(case_wts))

# response: 

# sl_response - 0(no)/1(yes)

# predictors: 

# haul_latitude
# haul_longitude
# haul_depth_fa(haul depth in fathoms)
# soak_hr(soak hours)
# net_mesh_size_in(net mesh size inches)
# dist_km (distance to shore in km)
# Julian day
# sst

####Weighted random forest - ranger
######################################################################

set.seed(1207)
model_split <- initial_split(model_data, prop = 4/5, strata = sl_response)

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
param_grid <- grid_regular(mtry(range = c(1, 8)), levels = 8) 

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

# save model result
saveRDS(rf_res, file.path("model_result/weighted_rf_weight75.Rds"))
save(rf_final_fit,
     model_train,
     model_test,
     pdp_rf, 
     pdp_rf_df,
     file=file.path(file.path("model_result/weighted_rf_weight75.Rdata")))


# Chris' work on marginal effects figure
######################################################################

# Compute and quickly plot variable importance 
var_imp <- rf_final_fit %>%
  extract_fit_parsnip() %>%
  vip()

# Extract and format importance values
var_imp_df <- var_imp$data %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  # Format variables
  mutate(variable=recode(variable, 
                         "haul_long_dd"="Longitude (°W)",     
                         "julian_day"="Julian day",      
                         "sst"="Temperature (°C)",             
                         "haul_lat_dd"="Latitude (°N)",
                         "net_mesh_size_in"="Mesh size (cm)",
                         "dist_km"="Shore distance (km)",
                         "haul_depth_fa"="Depth (fathoms)",    
                         "soak_hr"="Soak time (hr)"))

# Format marginal effects
marg_effect <- pdp_rf_df %>% 
  # Rename
  rename(variable=variable_name,
         value=predictor,
         prob=prob_prediction) %>% 
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
  filter(!(variable=="Soak time (hr)" & value>96))

  

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot variable importance
g1 <- ggplot(var_imp_df, aes(x=importance, y=reorder(variable, importance))) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Variable importance", y="", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Plot marginal effects
g2 <- ggplot(marg_effect, aes(x=value, y=prob)) +
  facet_wrap(~variable, nrow=2, scales="free_x") +
  geom_line() +
  # Labels
  labs(x="Value", y="Bycatch risk", tag="B") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.25, 0.75))
g

# Export
ggsave(g, filename=file.path(plotdir, "sea_lion_marg_effects.png"), 
       width=8.5, height=3.5, units="in", dpi=600)


