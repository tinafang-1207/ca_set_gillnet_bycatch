

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris

# Read data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))


# Build data
################################################################################

# Calculate sets per trip
stats_trip <- data_orig %>% 
  # Add trip id
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  # Calculate sets per trip
  group_by(year, trip_id) %>% 
  summarize(nsets1=n_distinct(set_id),
            nsets2=max(set_num)) %>% 
  ungroup() %>% 
  # Record trips observed in year (sample size)
  group_by(year) %>% 
  mutate(ntrips=n_distinct(trip_id)) %>% 
  ungroup()

# Average
sets_per_trip_avg <- mean(stats_trip$nsets2)


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text = element_text(size=5),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats_trip,  aes(x=year, y=nsets1, group=year, fill=ntrips)) +
  geom_boxplot(outlier.shape=21, linewidth=0.5) +
  # Avergae line
  geom_hline(yintercept=sets_per_trip_avg, linewidth=0.8, linetype='dashed') +
  # Labels
  labs(x="Year", y="Number of sets per day\n(on observed fishing trips)") +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  scale_y_continuous(breaks=seq(0,12, 2), lim=c(0,NA)) +
  # Legend
  scale_fill_gradientn(name="Number of\nobserved trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +

  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sets_per_day_obs_data.png"), 
       width=4.5, height=3.5, units="in", dpi=600)






