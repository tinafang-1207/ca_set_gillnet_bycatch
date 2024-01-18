
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Read data
################################################################################

data <- read.csv("model_result/marginal_effects.csv")

data_cat <- read.csv("model_result/categorical_marginal_effects.csv")



# Plot data
################################################################################

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

# Plot
g1 <- ggplot(data, aes(x=value, y=prob, color=species)) +
  facet_wrap(~variable, ncol=4, scales="free") +
  geom_line() +
  # Labels
  labs(x="Variable value", y="Marginal effect", tag = "A") +
  # Legend
  scale_color_discrete(name="Species") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = "bottom")
g1

g2 <- ggplot(data_cat, aes(x = value, y = prob, fill=species)) +
  facet_wrap(~variable) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x="Variable category", y="Marginal effect", tag = "B") +
  # Legend
  scale_color_discrete(name="Species") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"))
  
g2

g <- gridExtra::grid.arrange(g1, g2, ncol = 1)

g
  
# Export
ggsave(g, filename=file.path(plotdir, "Fig6_marginal_effects.png"), 
       width=7, height=6.5, units="in", dpi=600)


