
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

species_color <- c("California sea lion" = "#B83945", "Common murre" = "#4F845C", "Harbor seal" = "#E3E457", "Soupfin shark" = "#377483")


main_plot <- ggplot(data, aes(x=value, y=prob, color=species)) +
  facet_wrap(~variable, ncol=3, scales="free") +
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

# use cowplot package, we can insert categorical plot inside

plot_with_insert <-
  ggdraw() +
  draw_plot(main_plot) +
  draw_plot(insert_plot, x = 0.67, y = 0.085, width = 0.33, height = 0.31)

plot_with_insert


  
# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects.png"), 
       width=5.6, height=6.5, units="in", dpi=600)



######################## by species ############################

# plot directory
plotdir <- "presentations"


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


# California sea lion

species_color <- c("California sea lion" = "#B83945", "Common murre" = "#4F845C", "Harbor seal" = "#E3E457", "Soupfin shark" = "#377483")

main_plot <- ggplot(data %>% filter(species == "California sea lion"), aes(x=value, y=prob, color=species)) +
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


insert_plot <- ggplot(data_cat %>% filter(species == "California sea lion"), aes(x = value, y = prob, fill=species)) +
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
  draw_plot(insert_plot, x = 0.755, y = 0.125, width = 0.25, height = 0.45)

plot_with_insert

# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects_SL.png"), 
       width=5.6, height=4.19, units="in", dpi=600)

########################################################################

# Harbor seal


main_plot <- ggplot(data %>% filter(species == "Harbor seal"), aes(x=value, y=prob, color=species)) +
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


insert_plot <- ggplot(data_cat %>% filter(species == "Harbor seal"), aes(x = value, y = prob, fill=species)) +
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
  draw_plot(insert_plot, x = 0.755, y = 0.125, width = 0.25, height = 0.45)

plot_with_insert

# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects_HS.png"), 
       width=5.6, height=4.19, units="in", dpi=600)

########################################################################


# Common murre


main_plot <- ggplot(data %>% filter(species == "Common murre"), aes(x=value, y=prob, color=species)) +
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


insert_plot <- ggplot(data_cat %>% filter(species == "Common murre"), aes(x = value, y = prob, fill=species)) +
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
  draw_plot(insert_plot, x = 0.755, y = 0.125, width = 0.25, height = 0.45)

plot_with_insert

# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects_CM.png"), 
       width=5.6, height=4.19, units="in", dpi=600)

########################################################################


# Soupfin shark


main_plot <- ggplot(data %>% filter(species == "Soupfin shark"), aes(x=value, y=prob, color=species)) +
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


insert_plot <- ggplot(data_cat %>% filter(species == "Soupfin shark"), aes(x = value, y = prob, fill=species)) +
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
  draw_plot(insert_plot, x = 0.755, y = 0.125, width = 0.25, height = 0.45)

plot_with_insert

# Export
ggsave(plot_with_insert, filename=file.path(plotdir, "Fig5_marginal_effects_SS.png"), 
       width=5.6, height=4.19, units="in", dpi=600)







