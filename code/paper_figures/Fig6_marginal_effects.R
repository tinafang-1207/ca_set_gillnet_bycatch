
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)
library(cowplot)

# Directories
plotdir <- "figures"


# Read data
################################################################################

data <- read.csv("model_result/marginal_effects.csv")

data_cat <- read.csv("model_result/categorical_marginal_effects.csv")

# Reorder categorical data
################################################################################
data_cat <- data_cat %>%
  mutate(species = species %>% fct_reorder(prob))


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

species_color <- c("California sea lion" = "#FF3300", "Common murre" = "#669900", "Harbor seal" = "#33CCCC", "Soupfin shark" = "#9966FF")

main_plot <- ggplot(data, aes(x=value, y=prob, color=species)) +
  facet_wrap(~variable, ncol=4, scales="free") +
  geom_line() +
  # Labels
  labs(x="Variable value", y="Marginal effect") +
  # Legend
  scale_colour_manual(name="Species", values = species_color) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = "bottom")

main_plot


insert_plot <- ggplot(data_cat, aes(x = value, y = prob, fill=species)) +
  facet_wrap(~variable) +
  geom_bar(stat = "identity", position = "dodge") +
  # fill color
  scale_fill_manual(values = species_color) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
  
insert_plot

# use coplowt package, we can insert categorical plot inside

plot_with_insert <-
  ggdraw() +
  draw_plot(main_plot) +
  draw_plot(insert_plot, x = 0.755, y = 0.125, width = 0.25, height = 0.45)


  
# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig6_marginal_effects.png"), 
       width=5.37, height=4.19, units="in", dpi=600)


