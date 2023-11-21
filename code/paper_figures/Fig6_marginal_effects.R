
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
g <- ggplot(data, aes(x=value, y=prob, color=species)) +
  facet_wrap(~variable, ncol=4, scales="free") +
  geom_line() +
  # Labels
  labs(x="Variable value", y="Marginal effect") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.9, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_marginal_effects.png"), 
       width=6.5, height=3.75, units="in", dpi=600)


