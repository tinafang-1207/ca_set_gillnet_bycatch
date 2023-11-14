

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
  mutate(species = "california sea lion", weight = NA)

sl_weighted_df <- output_sl_weighted[["rf_weighted_final"]] %>%
  mutate(species = "california sea lion")

# harbor seal
hs_balanced_df <- output_hs_balanced[["rf_all_df"]] %>%
  mutate(species = "harbor seal", weight = NA)

hs_weighted_df <- output_hs_weighted[["rf_weighted_final"]] %>%
  mutate(species = "harbor seal")
  
#### Make plots to show the result ####

# make the final table

model_df_final <- rbind(sl_balanced_df, sl_weighted_df, hs_balanced_df, hs_weighted_df) %>%
  mutate(weight = as.factor(weight))


# make the plot

#theme

base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.text.x = element_text(angle = 90, hjust = 0.5),
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

g_balanced <-ggplot(data = model_df_final %>% filter(balanced_type != "weighted"), mapping = aes(x = mtry, y = mean)) +
  geom_line(aes(color = balanced_type)) +
  labs(x = "Number of variables(mtry)", y = )
  facet_wrap(~species +.metric, scales = "free_y") +
  theme_bw() +base_theme

g_balanced

g_weighted <- ggplot(data = model_df_final %>% filter(balanced_type == "weighted"), mapping = aes(x = mtry, y = mean)) +
  geom_line(aes(color = weight)) +
  facet_wrap(~species +.metric, scales = "free_y") +
  theme_bw() +base_theme

g_weighted


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

  
  
  
