# clean working environment
rm(list = ls())

# read in packages
library(tidyverse)

# read csv
kappa_train <- read_csv("model_result/threshold_kappa_train_final.csv")
kappa_test <- read_csv("model_result/threshold_kappa_test_final.csv")

# Combine the data
kappa_orig <- bind_rows(kappa_train, kappa_test)

# Format original data
kappa <- kappa_orig %>%
  rename(Threshold = threshold,
         Kappa = estimate_kappa) %>%
  mutate(Dataset = ifelse(Dataset == "Train data", "Training data", "Test data"))

# select max kappa value

max_kappa_training <- kappa %>%
  filter(Dataset == "Training data") %>%
  group_by(Species) %>%
  filter(Kappa == max(Kappa))

max_kappa_test <- kappa %>%
  filter(Dataset == "Test data") %>%
  filter(Species == "California sea lion"&Threshold == 0.49|Species == "Harbor seal"&Threshold == 0.53|Species == "Harbor porpoise"&Threshold == 0.49|Species == "Common murre"&Threshold == 0.51|Species == "Northern elephant seal"&Threshold == 0.5)

max_kappa_final <- bind_rows(max_kappa_training, max_kappa_test)



# Base theme
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





# make figure

g <- ggplot(data = kappa, aes(x = Threshold, y = Kappa, color = Species)) +
  geom_line(aes(linetype = Dataset)) +
  geom_point(data = max_kappa_final, aes(x = Threshold, y = Kappa, color = Species)) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey") +
  geom_text(data = max_kappa_final %>% filter(Dataset == "Training data"), 
            aes(x = Threshold, y = Kappa, color = Species, label = Threshold), 
            vjust = -0.2,
            show.legend = FALSE) +
  labs(x = "Probability threshold", y = "Cohen's kappa") +
  theme_bw() + base_theme

g

plotdir <- "figures"

ggsave(g, filename=file.path(plotdir, "FigS14_rf_threshold_selection_kappa.png"),
       width=5.5, height=4, units="in", dpi=600)




