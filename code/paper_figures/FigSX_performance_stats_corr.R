

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
tabledir <- "tables"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(tabledir, "Table2_model_performance.xlsx"))


# Build data
################################################################################

# Build data
data2 <- data_orig %>% 
  select(species, n_bycatch, kappa_test, auc_test, kappa_train, auc_train) %>% 
  gather(key="metric", value="value", 3:ncol(.)) %>% 
  separate(col="metric", into=c("metric", "dataset"), sep="_") %>% 
  spread(key="metric", value="value") %>% 
  mutate(dataset=stringr::str_to_sentence(dataset))

# Key correlations (trainign data)
data2_train <- data2 %>% 
  filter(dataset=="Train")
cor(data2_train$n_bycatch, data2_train$kappa)^2
cor(data2_train$kappa, data2_train$auc)^2


# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     plot.tag=element_text(size=8),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size = unit(0.3, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))

g1 <- ggplot(data2, aes(x=n_bycatch, y=kappa, color=dataset, fill=dataset)) +
  geom_smooth(method="lm", se=F) +
  geom_point() +
  # Labels
  labs(x="Number of observed bycatch", y="Cohen's kappa", tag="A") +
  # Legeng
  scale_color_discrete(name="Dataset") +
  scale_fill_discrete(name="Dataset") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = c(0.8, 0.15))
g1

g2 <- ggplot(data2, aes(x=n_bycatch, y=auc, color=dataset, fill=dataset)) +
  geom_smooth(method="lm", se=F) +
  geom_point() +
  # Labels
  labs(x="Number of observed bycatch", y="Area under the curve (AUC)", tag="B") +
  # Legeng
  scale_color_discrete(name="Dataset") +
  scale_fill_discrete(name="Dataset") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g2

g3 <- ggplot(data2, aes(x=kappa, y=auc, color=dataset, fill=dataset)) +
  geom_smooth(method="lm", se=F) +
  geom_point() +
  # Labels
  labs(x="Cohen's kappa", y="Area under the curve (AUC)", tag="C") +
  # Legeng
  scale_color_discrete(name="Dataset") +
  scale_fill_discrete(name="Dataset") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none")
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, nrow=1)

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_performance_stats_corr.png"), 
       width=6.5, height=2.5, units="in", dpi=600)


