
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

# specify the species color
species_color <- c("California sea lion" = "#1B9E77", "Common murre" = "#D95F02", "Harbor seal" = "#7570B3", "Northern elephant seal" = "#66A61E")

# create dataframe of vertical dashed lines
vline <- data.frame(xintercept = c(40, 60, 3, 8.5),
                    variable = c("Depth (fathoms)", "Depth (fathoms)", "Shore distance (km)", "Mesh size (cm)"))

main_plot <- ggplot(data, aes(x=value, y=prob, color=species)) +
  facet_wrap(~variable, ncol=3, scales="free") +
  geom_line() +
  geom_vline(data = vline, aes(xintercept = xintercept), linetype = "dashed", color = "gray") +
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

# use cowplot package, we can insert categorical plot inside

plot_with_insert <-
  ggdraw() +
  draw_plot(main_plot) +
  draw_plot(insert_plot, x = 0.67, y = 0.085, width = 0.33, height = 0.31)

plot_with_insert


  
# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects.png"), 
       width=6, height=6.5, units="in", dpi=600)








