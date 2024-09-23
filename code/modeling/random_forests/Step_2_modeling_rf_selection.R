

#### clean working space ####
rm(list = ls())

#### read in library ####
library(tidyverse)
library(tidymodels)

### output directory ###
plotdir <- "figures"

#### read in model result ####

# sealion
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

# harbor porpoise
output_hp_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/harbor_porpoise_model_balanced_rf.Rds")
output_hp_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_porpoise_model_weighted_rf.Rds")

# common murre
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/common_murre_model_balanced_rf.Rds")
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/common_murre_model_weighted_rf.Rds")

# norther elephant seal
output_ns_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/northern_elephant_seal_model_balanced_rf.Rds")
output_ns_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/northern_elephant_seal_model_weighted_rf.Rds")

# brandt's cormorant 
output_bc_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/brandt's_cormorant_model_balanced_rf.Rds")
output_bc_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/brandt's_cormorant_model_weighted_rf.Rds")

#### Extract output from model result ####

# sealion
sl_balanced_df <- output_sl_balanced[["rf_all_df"]] %>%
  mutate(species = "California sea lion", weight = NA)

sl_weighted_df <- output_sl_weighted[["rf_weighted_final"]] %>%
  mutate(species = "California sea lion")

# harbor seal
hs_balanced_df <- output_hs_balanced[["rf_all_df"]] %>%
  mutate(species = "Harbor seal", weight = NA)

hs_weighted_df <- output_hs_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Harbor seal")

#harbor porpoise
hp_balanced_df <- output_hp_balanced[["rf_all_df"]] %>%
  mutate(species = "Harbor porpoise", weight = NA)

hp_weighted_df <- output_hp_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Harbor porpoise")

#northern elephant seal
ns_balanced_df <- output_ns_balanced[["rf_all_df"]] %>%
  mutate(species = "Northern elephant seal", weight = NA)

ns_weighted_df <- output_ns_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Northern elephant seal")

#common murre
cm_balanced_df <- output_cm_balanced[["rf_all_df"]] %>%
  mutate(species = "Common murre", weight = NA)

cm_weighted_df <- output_cm_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Common murre")

# Brandt's cormorant
bc_balanced_df <-  output_bc_balanced[["rf_all_df"]] %>%
  mutate(species = "Brandt's cormorant", weight = NA)

bc_weighted_df <- output_bc_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Brandt's cormorant")

###################################################################

# Get roc_auc and kappa for test datasets

### Extract the model best fit (to training data)

# sealion - weight 25
sl_best_fit <- output_sl_weighted[["final_fit"]][[1]]

# harbor seal  - weight 75
hs_best_fit <- output_hs_weighted[["final_fit"]][[3]]

# harbor porpoise - weight 50
hp_best_fit <- output_hp_weighted[["final_fit"]][[2]]

# common murre - weight 25
cm_best_fit <- output_cm_weighted[["final_fit"]][[1]]

# northern elephant seal - weight 25
ns_best_fit <- output_ns_weighted[["final_fit"]][[1]]

# Brandt's cormorant - weight 25
bc_best_fit <- output_bc_weighted[["final_fit"]][[1]]

### Extract test dataset

sl_test <- output_sl_weighted[["data_test"]]

hs_test <- output_hs_weighted[["data_test"]]

cm_test <- output_cm_weighted[["data_test"]]

ns_test <- output_ns_weighted[["data_test"]]

hp_test <- output_hp_weighted[["data_test"]]

bc_test <- output_bc_weighted[["data_test"]]


bycatch_metrics <- metric_set(roc_auc, kap)

augment(sl_best_fit, new_data = sl_test) %>%
 kap(truth = response, estimate = .pred_class)

augment(hs_best_fit, new_data = hs_test) %>%
  kap(truth = response, estimate = .pred_class)

augment(ns_best_fit, new_data = ns_test) %>%
  kap(truth = response, estimate = .pred_class)

augment(hp_best_fit, new_data = hp_test) %>%
  kap(truth = response, estimate = .pred_class)

augment(cm_best_fit, new_data = cm_test) %>%
  kap(truth = response, estimate = .pred_class)

augment(bc_best_fit, new_data = bc_test) %>%
  kap(truth = response, estimate = .pred_class)

# roc_auc

augment(sl_best_fit, new_data = sl_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

augment(hs_best_fit, new_data = hs_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

augment(cm_best_fit, new_data = cm_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

augment(hp_best_fit, new_data = hp_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

augment(bc_best_fit, new_data = bc_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

augment(ns_best_fit, new_data = ns_test) %>%
  roc_auc(response,.pred_1, event_level = "second")

#### Make plots to show the result ####

# make the final table

# add other dfs later

model_df_final <- rbind(sl_balanced_df, 
                        sl_weighted_df, 
                        hs_balanced_df, 
                        hs_weighted_df, 
                        hp_balanced_df, 
                        hp_weighted_df,
                        cm_balanced_df,
                        cm_weighted_df,
                        ns_balanced_df,
                        ns_weighted_df,
                        bc_balanced_df,
                        bc_weighted_df
                        ) %>%
  rename(metric = .metric) %>%
  mutate(metric = recode_factor(metric, 
                                "kap" = "Cohen's kappa", 
                                "roc_auc" = "Area under the ROC curve"),
         balanced_type = recode_factor(balanced_type, 
                                "downsample" = "Downsample", 
                                "upsample" = "Upsample",
                                "smote" = "SMOTE",
                                "weighted" = "Weighted")) %>%
  mutate(model_id = paste(metric, .config, balanced_type, species, mtry, weight, sep = "-")) %>%
  mutate(species = species %>% fct_reorder(mean))





# make the plot

#theme

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

kappa_df <- tibble(metric = "Cohen's kappa", 
                   value = c(0.2, 0.4, 0.7))

auc_df <- tibble(metric = "Area under the ROC curve", 
                  value = c(0.7, 0.8, 0.9))

ref_df <- rbind(kappa_df, auc_df) %>%
  mutate(metric = factor(metric, levels = c("Cohen's kappa", "Area under the ROC curve")))

# create best model result from balanced rf

g_balanced <-ggplot(data = model_df_final %>% filter(balanced_type != "Weighted"), 
                    mapping = aes(x = mtry, y = mean)) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  geom_line(aes(color = balanced_type)) +
  labs(x = "Number of variables (mtry)", y = "Value" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_discrete(name = "Balance type") +
  theme_bw()+base_theme

g_balanced

# create best model result from weighted rf

g_weighted <- ggplot(data = model_df_final %>% filter(balanced_type == "Weighted"), 
                     mapping = aes(x = mtry, y = mean, color = weight, group = weight)) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  geom_line() +
  labs(x = "Number of variables (mtry)", y = "Value" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_gradientn(name = "Weight", 
                    colors = RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() +base_theme

g_weighted

# color palette 
# RColorBrewer::display.brewer.all(9)


#### select the best model ####

# based on kappa

best_model_kappa <- model_df_final %>% 
  filter(metric=="Cohen's kappa") %>% 
  arrange(balanced_type, desc(mean)) %>% 
  group_by(balanced_type, species) %>% 
  slice(1) %>% 
  mutate(balanced_type=factor(balanced_type, levels=c("Upsample", "Downsample", "SMOTE", "Weighted"))) %>% 
  arrange(balanced_type)

select_model_kappa <- best_model_kappa %>%
  group_by(species) %>%
  filter(mean == max(mean)) %>%
  ungroup()


select_model_roc <- model_df_final %>%
  filter(metric == "Area under the ROC curve") %>%
  filter(model_id == "Area under the ROC curve-Preprocessor1_Model3-Weighted-California sea lion-3-25"|model_id == "Area under the ROC curve-Preprocessor1_Model1-Weighted-Northern elephant seal-1-25"|model_id == "Area under the ROC curve-Preprocessor1_Model6-Weighted-Common murre-6-25"|model_id == "Area under the ROC curve-Preprocessor1_Model2-Weighted-Harbor seal-2-75"|model_id == "Area under the ROC curve-Preprocessor1_Model2-Weighted-Harbor porpoise-2-50"|model_id == "Area under the ROC curve-Preprocessor1_Model8-Weighted-Brandt's cormorant-8-25")

select_model_final <- rbind(select_model_kappa, select_model_roc) %>%
  mutate(metric = factor(metric, levels = c("Cohen's kappa", "Area under the ROC curve")))
  
##### Experiment in making Chris figure below #####

# All best models
model_best_all <- model_df_final %>%
  filter(balanced_type%in%c("Upsample", "Downsample", "SMOTE")|(species == "California sea lion" & weight == "25")|(species == "Harbor seal" & weight == "75")|(species == "Harbor porpoise"& weight == "50")|(species == "Common murre"& weight == "25")|(species == "Brandt's cormorant"& weight == "25")|(species == "Northern elephant seal"& weight == "25")) %>%
  mutate(metric = factor(metric, levels = c("Cohen's kappa", "Area under the ROC curve"))) %>%
  select(-weight)

g_best <- ggplot(data = model_best_all, mapping = aes(x = mtry, y = mean)) +
  geom_line(aes(color = balanced_type)) +
  geom_point(data = select_model_final, aes(x = mtry, y = mean, color = balanced_type), shape = 20, size = 3) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  labs(x = "Number of variables (mtry)", y = "Value",tag = "A" ) +
  scale_x_continuous(breaks = seq(2,8,2)) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_discrete(name = "Balance type") +
  theme_bw()+base_theme

g_best


#best weight model

model_best_weight_kappa <- model_df_final %>%
  filter(balanced_type == "Weighted") %>%
  filter(metric == "Cohen's kappa") %>%
  group_by(species) %>%
  filter(mean == max(mean)) %>%
  ungroup()

model_best_weight_roc <- model_df_final %>%
  filter(balanced_type == "Weighted") %>%
  filter(metric == "Area under the ROC curve") %>%
  filter(model_id == "Area under the ROC curve-Preprocessor1_Model3-Weighted-California sea lion-3-25"|model_id == "Area under the ROC curve-Preprocessor1_Model1-Weighted-Northern elephant seal-1-25"|model_id == "Area under the ROC curve-Preprocessor1_Model6-Weighted-Common murre-6-25"|model_id == "Area under the ROC curve-Preprocessor1_Model2-Weighted-Harbor seal-2-75"|model_id == "Area under the ROC curve-Preprocessor1_Model2-Weighted-Harbor porpoise-2-50"|model_id == "Area under the ROC curve-Preprocessor1_Model8-Weighted-Brandt's cormorant-8-25")

model_best_weight_final<-rbind(model_best_weight_kappa, model_best_weight_roc)

g_best_weight <- ggplot(data = model_df_final %>% filter(balanced_type == "Weighted"), 
                        mapping = aes(x = mtry, y = mean, color = weight, group = weight)) +
  geom_line() +
  geom_point(data = model_best_weight_final, aes(x = mtry, y = mean, color = weight), shape = 20, size = 3) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  labs(x = "Number of variables (mtry)", y = "Value", tag = "B" ) +
  scale_x_continuous(breaks = seq(2,8,2)) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_gradientn(name = "Weight                ", 
                        colors = RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() +base_theme  

g_best_weight  


g <- gridExtra::grid.arrange(g_best, g_best_weight, ncol = 1)

g


ggsave(g, filename=file.path(plotdir, "FigS13_rf_model_selection.png"),
       width=8.5, height=7, units="in", dpi=600)


