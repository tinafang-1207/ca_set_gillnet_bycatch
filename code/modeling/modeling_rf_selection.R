

#### clean working space ####
rm(list = ls())

#### read in library ####
library(tidyverse)

#### read in model result ####

# sealion
output_sl_balanced <- readRDS("model_result/balanced_rf/california_sea_lion_model_balanced_rf.Rds")
output_sl_weighted <- readRDS("model_result/weighted_rf/california_sea_lion_model_weighted_rf.Rds")

# harbor seal
output_hs_balanced <- readRDS("model_result/balanced_rf/harbor_seal_model_balanced_rf.Rds")
output_hs_weighted <- readRDS("model_result/weighted_rf/harbor_seal_model_weighted_rf.Rds")

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
  
#### Make plots to show the result ####

# make the final table

model_df_final <- rbind(sl_balanced_df, sl_weighted_df, hs_balanced_df, hs_weighted_df) %>%
  rename(metric = .metric) %>%
  mutate(metric = recode_factor(metric, 
                                "kap" = "Cohen's kappa", 
                                "roc_auc" = "Area under the ROC curve"),
         balanced_type = recode_factor(balanced_type, 
                                "downsample" = "Downsample", 
                                "upsample" = "Upsample",
                                "smote" = "SMOTE"))


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

ref_df <- rbind(kappa_df, auc_df)

g_balanced <-ggplot(data = model_df_final %>% filter(balanced_type != "weighted"), 
                    mapping = aes(x = mtry, y = mean)) +
  geom_hline(data = ref_df, mapping = aes(yintercept = value), color = "grey70",linetype = "dashed") +
  geom_line(aes(color = balanced_type)) +
  labs(x = "Number of variables (mtry)", y = "Value" ) +
  facet_grid(metric~species, scales = "free_y") +
  scale_color_discrete(name = "Balance type") +
  theme_bw()+base_theme

g_balanced

g_weighted <- ggplot(data = model_df_final %>% filter(balanced_type == "weighted"), 
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

best_model <- model_df_final %>% 
  filter(.metric=="kap") %>% 
  arrange(balanced_type, desc(mean)) %>% 
  group_by(balanced_type, species) %>% 
  slice(1) %>% 
  mutate(balanced_type=factor(balanced_type, levels=c("upsample", "downsample", "smote", "weighted"))) %>% 
  arrange(balanced_type)


which.max(best_model$mean)

  
  
  
