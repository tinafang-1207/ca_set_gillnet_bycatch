
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Simulate data
################################################################################

# Species
species <- c("California sea lion",
             "Harbor porpoise",
             "Harbor seal",
             "Common murre",
             "Brandt's cormorant",
             "Northern elephant seal")

# Variables
variables <- c("Mesh size", 
               "Latitude", 
               "Temperature", 
               "Julian day", 
               "Depth",
               "Shore distance", 
               "Soak time")


# Build data
data <- expand.grid(species=species,
                    variable=variables) %>% 
  # Add importnace
  mutate(importance=runif(nrow(.), 0, 100))



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
g <- ggplot(data, aes(x=importance, 
                      y=tidytext::reorder_within(variable, importance, species))) +
  facet_wrap(~species, ncol=3, scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Variable importance", y="") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_variable_importance.png"), 
       width=6.5, height=4, units="in", dpi=600)


