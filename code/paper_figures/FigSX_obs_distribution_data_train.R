
# clean working environment
rm(list = ls())

# read in library
library(tidyverse)

# read in data
data <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds") 

# Reference lines
lats <- c(32, 33, 34, 35, 36, 37)

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


# process data
data_sp <- data %>%
  mutate(has_cm = ifelse(comm_name == "Common murre", "yes", "no"),
         has_cl = ifelse(comm_name == "California sea lion", "yes", "no"),
         has_hs = ifelse(comm_name == "Harbor seal", "yes", "no"),
         has_ss = ifelse(comm_name == "Soupfin shark", "yes", "no"))
  

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))


ggplot() +
  geom_point(data = data_sp, mapping = aes(x = long_dd, y = lat_dd, color = has_cm)) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("yes", "no"), values = c("red", "black"), name = "Has common murre") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  theme_bw() + base_theme

ggplot() +
  geom_point(data = data_sp, mapping = aes(x = long_dd, y = lat_dd, color = has_hs), shape = 1) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("yes", "no"), values = c("red", "black"), name = "Has common murre") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  theme_bw() + base_theme


ggplot() +
  geom_point(data = data_sp, mapping = aes(x = long_dd, y = lat_dd, color = has_cl), shape = 1) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("yes", "no"), values = c("red", "black"), name = "Has common murre") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  theme_bw() + base_theme

ggplot() +
  geom_point(data = data_sp, mapping = aes(x = long_dd, y = lat_dd, color = has_ss), shape = 1) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("yes", "no"), values = c("red", "black"), name = "Has common murre") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  theme_bw() + base_theme

###########################################################################
# plot the observer distribution for model training data

# read model result

output_sl_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/california_sea_lion_model_weighted_rf.Rds")

output_hs_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/harbor_seal_model_weighted_rf.Rds")

output_ss_weighted <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/weighted_rf_with_long/soupfin_shark_model_weighted_rf.Rds")

output_cm_balanced <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/model_output/balanced_rf_with_long/common_murre_model_balanced_rf.Rds")


# Extract training data

model_train_weighted_sl <- output_sl_weighted[["data_train"]]

model_train_weighted_hs <- output_hs_weighted[["data_train"]]

model_train_weighted_ss <- output_ss_weighted[["data_train"]]

model_train_balanced_cm <- output_cm_balanced[["data_train"]]

sl_data <- model_train_weighted_sl %>%
  mutate(species = "California sea lion") %>%
  select(-case_wts)

hs_data <- model_train_weighted_hs %>%
  mutate(species = "Harbor seal") %>%
  select(-case_wts)

ss_data <- model_train_weighted_ss %>%
  mutate(species = "Soupfin shark")%>%
  select(-case_wts)

cm_data <- model_train_balanced_cm %>%
  mutate(species = "Common murre")


data_all <- bind_rows(sl_data, hs_data, ss_data, cm_data)


g <-ggplot() +
  geom_point(data = data_all, mapping = aes(x = long_dd, y = lat_dd, color = response), shape = 1) +
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  scale_color_manual(breaks = c("0", "1"), values = c("lightgrey", "red"), name = "Has bycatch") +
  coord_sf(xlim = c(-124, -117), ylim = c(32, 38.5)) +
  facet_wrap(.~species) +
  theme_bw() + base_theme

g

plotdir <- "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/figure_reference_yutian/"

ggsave(g,filename=file.path(plotdir, "FigSX_data_train_obs_distribution.png"), 
       width=8, height=4, units="in", dpi=600)




















