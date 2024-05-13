
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# read in data

data <- read_csv("model_result/variable_importance.csv")

# Format data 

data <- data %>%
  mutate(variable = recode_factor(variable, 
                                  "yday"="Julian day",      
                                  "sst_c"="Temperature",             
                                  "lat_dd"="Latitude",
                                  "long_dd" = "Longitude",
                                  "mesh_size_in"="Mesh size",
                                  "shore_km"="Shore distance",
                                  "depth_fa"="Depth",    
                                  "soak_hr"="Soak time",
                                  "island_yn_X1" = "Island area?")) %>%
  mutate(category = case_when(variable %in% c("Mesh size", "Soak time")~"Fishing-related",
                              variable %in% c("Latitude", "Longitude","Depth", "Shore distance", "Island area?")~"Spatial",
                              variable %in% c("Julian day")~"Temporal",
                              variable%in% c("Temperature")~"Environmental"))

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

# set species color
category_color <- c("Spatial" = "#B83945", "Environmental" = "#4F845C", "Temporal" = "#E3E457", "Fishing-related" = "#377483")

# Plot
g <- ggplot(data, aes(x=importance, 
                      y=tidytext::reorder_within(variable, importance, species),
                      fill = category)) +
  facet_wrap(~species, ncol=2, scales="free") +
  geom_bar(stat="identity") +
  scale_fill_manual(name = "Category", values = category_color) +
  # Labels
  labs(x="Variable importance", y="Variables") +
  tidytext::scale_y_reordered() +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.y=element_blank())
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_variable_importance.png"), 
       width=5.5, height=4, units="in", dpi=600)


