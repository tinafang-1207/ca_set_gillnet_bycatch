
# clean working environment
rm(list = ls())

# read in packages
library(tidyverse)

# read in data

roc_training <- read.csv("model_result/roc_curve_training.csv")

roc_test <- read.csv("model_result/roc_curve.csv")

# combine roc dataframe

roc_training <- roc_training %>%
  mutate(type = "training")

roc_test <- roc_test %>%
  mutate(type = "test")

roc_all <- bind_rows(roc_training, roc_test)

# Find the optimum cut-off points

optimum_level <- roc_all %>%
  mutate(difference = sensitivity - (1-specificity)) %>%
  group_by(species, type) %>%
  filter(difference == max(difference))

########################################################
# PLOT ROC CURVE BELOW

# plotdir

plotdir <- "figures"

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))



g <- ggplot(data = roc_all, aes(x = 1-specificity, y = sensitivity))+
  geom_path()+
  geom_abline(lty = 3) +
  geom_point(aes(x = 1-specificity, y = sensitivity, color = .threshold, shape = type), size = 0.5) +
  geom_point(data = optimum_level, aes(x = 1-specificity, y = sensitivity), shape = 19) +
  scale_color_gradientn(name = "Threshold level", colors = RColorBrewer::brewer.pal(9, "Spectral") %>% rev(), breaks = c(0.1, 0.5, 0.9))+
  scale_shape_manual(name = "Data source", values = c(4, 19)) +
  labs(x = "False positive rate", y = "True positive rate") +
  facet_wrap(.~species) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_bw() + base_theme


g

ggsave(g, filename=file.path(plotdir, "FigSX_roc_curve_threshold.png"), 
       width=5.5, height=4, units="in", dpi=600)














