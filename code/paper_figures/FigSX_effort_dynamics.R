

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
gisdatadir <- "data/gis_data"

# Read data
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))



# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # Add julian day
  mutate(week=lubridate::week(date)) %>% 
  # Effort by year-day
  group_by(year, week) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Prop
  group_by(year) %>% 
  mutate(ptrips=ntrips/sum(ntrips)) %>% 
  ungroup() %>% 
  # Reduce
  filter(year<=2021)
  



# Plot data
################################################################################


# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, 
            mapping=aes(x=week, y=ptrips, color=year, group=year)) +
  geom_line() +
  # Labels
  labs(x="Calendar week", y="Proportion of trips") +
  # Legend
  scale_color_gradientn(name="Year", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  # Theme
  theme_bw()
g


# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_set_gillnet_history.png"),
       width=6.5, height=4, units="in", dpi=600)



