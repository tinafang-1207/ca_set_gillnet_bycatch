

#### clean working space ####
rm(list = ls())

#### read in library ####
library(tidyverse)

### output directory ###
plotdir <- "figures"

#### read in model result ####

# sealion
output_sl_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/harbor_seal_model_weighted_rf.Rds")

# soupfin shark
output_ss_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/soupfin_shark_model_balanced_rf.Rds")
output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/soupfin_shark_model_weighted_rf.Rds")

# giant seabass
output_gs_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/giant_sea_bass_model_balanced_rf.Rds")
output_gs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/giant_sea_bass_model_weighted_rf.Rds")

# common murre
output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/common_murre_model_balanced_rf.Rds")
output_cm_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/common_murre_model_weighted_rf.Rds")

# brandt's cormorant 
output_bc_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf/brandt's_cormorant_model_balanced_rf.Rds")
output_bc_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf/brandt's_cormorant_model_weighted_rf.Rds")

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

#soupfin shark
ss_balanced_df <- output_ss_balanced[["rf_all_df"]] %>%
  mutate(species = "Soupfin shark", weight = NA)

ss_weighted_df <- output_ss_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Soupfin shark")

#giant seabass
gs_balanced_df <- output_gs_balanced[["rf_all_df"]] %>%
  mutate(species = "Giant sea bass", weight = NA)

gs_weighted_df <- output_gs_weighted[["rf_weighted_final"]] %>%
  mutate(species = "Giant sea bass")

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

  
#### Make plots to show the result ####

# make the final table

model_df_final <- rbind(sl_balanced_df, 
                        sl_weighted_df, 
                        hs_balanced_df, 
                        hs_weighted_df, 
                        ss_balanced_df, 
                        ss_weighted_df,
                        gs_balanced_df,
                        gs_weighted_df,
                        cm_balanced_df,
                        cm_weighted_df,
                        bc_balanced_df,
                        bc_weighted_df) %>%
  rename(metric = .metric) %>%
  mutate(metric = recode_factor(metric, 
                                "kap" = "Cohen's kappa", 
                                "roc_auc" = "Area under the ROC curve"),
         balanced_type = recode_factor(balanced_type, 
                                "downsample" = "Downsample", 
                                "upsample" = "Upsample",
                                "smote" = "SMOTE",
                                "weighted" = "Weighted")) %>%
  mutate(model_id = paste(metric, .config, balanced_type, species, mtry, weight, sep = "-"))


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

g_balanced <-ggplot(data = model_df_final %>% filter(balanced_type != "Weighted"), 
                    mapping = aes(x = mtry, y = mean)) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  geom_line(aes(color = balanced_type)) +
  labs(x = "Number of variables (mtry)", y = "Value" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_discrete(name = "Balance type") +
  theme_bw()+base_theme

g_balanced

g_weighted <- ggplot(data = model_df_final %>% filter(balanced_type == "Weighted"), 
                     mapping = aes(x = mtry, y = mean, color = weight, group = weight)) +
  geom_line() +
  labs(x = "Number of variables (mtry)", y = "Value" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_gradientn(name = "Weight", 
                    colors = RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() +base_theme

g_weighted

RColorBrewer::display.brewer.all(9)


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
  filter(model_id == "Area under the ROC curve-Preprocessor1_Model3-SMOTE-Harbor seal-3-NA"|model_id == "Area under the ROC curve-Preprocessor1_Model1-SMOTE-Soupfin shark-1-NA"|model_id == "Area under the ROC curve-Preprocessor1_Model1-Weighted-California sea lion-1-75")

select_model_final <- rbind(select_model_kappa, select_model_roc) %>%
  mutate(metric = factor(metric, levels = c("Cohen's kappa", "Area under the ROC curve")))
  
##### Experiment in making Chris figure below #####

# All best models
model_best_all <- model_df_final %>%
  filter(balanced_type%in%c("Upsample", "Downsample", "SMOTE")|(species == "California sea lion" & weight == "75")|(species == "Harbor seal" & weight == "150")|(species == "Soupfin shark"& weight == "25")) %>%
  mutate(metric = factor(metric, levels = c("Cohen's kappa", "Area under the ROC curve"))) %>%
  select(-weight)

g_best <- ggplot(data = model_best_all, mapping = aes(x = mtry, y = mean)) +
  geom_line(aes(color = balanced_type)) +
  geom_point(data = select_model_final, aes(x = mtry, y = mean, color = balanced_type), shape = 20, size = 3) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  labs(x = "Number of variables (mtry)", y = "Value",tag = "A" ) +
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
  filter(model_id == "Area under the ROC curve-Preprocessor1_Model1-Weighted-California sea lion-1-75"|model_id == "Area under the ROC curve-Preprocessor1_Model7-Weighted-Harbor seal-7-150"|model_id == "Area under the ROC curve-Preprocessor1_Model2-Weighted-Soupfin shark-2-25")

model_best_weight_final<-rbind(model_best_weight_kappa, model_best_weight_roc)

g_best_weight <- ggplot(data = model_df_final %>% filter(balanced_type == "Weighted"), 
                        mapping = aes(x = mtry, y = mean, color = weight, group = weight)) +
  geom_line() +
  geom_point(data = model_best_weight_final, aes(x = mtry, y = mean, color = weight), shape = 20, size = 3) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  labs(x = "Number of variables (mtry)", y = "Value", tag = "B" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_gradientn(name = "Weight                ", 
                        colors = RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() +base_theme  

g_best_weight  


g <- gridExtra::grid.arrange(g_best, g_best_weight, ncol = 1)



ggsave(g, filename=file.path(plotdir, "FigSX_rf_model_selection.png"),
       width=5.5, height=6.5, units="in", dpi=600)

  
  
