

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
obs_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))
logs_orig <- readRDS(file=file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# Build data
################################################################################

# Calculate sets per trip
obs_sets_trip <- obs_orig %>% 
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
obs_sets_per_trip_avg <- mean(obs_sets_trip$nsets2)

# Calculate sets per trip
logs_sets_trip <- logs_orig %>% 
  # Calculate sets per trip
  group_by(year, trip_id) %>% 
  summarize(nsets1=n_distinct(set_id)) %>% 
  ungroup()

# Observed sets with bycatch of species of interest
obs <- obs_orig %>% 
  filter(comm_name %in% c("California sea lion", "Harbor seal", "Common murre", "Northern elephant seal", "Soupfin shark",
                          "Brandt's cormorant", "Harbor porpoise"))

# Max observed
obs_stats <- obs %>% 
  group_by(comm_name) %>% 
  summarize(max=max(n_caught)) %>% 
  arrange(desc(max))

# Order by max observed
obs_ordered <- obs %>% 
  mutate(comm_name=factor(comm_name, levels=obs_stats$comm_name))




# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text = element_text(size=5),
                   plot.tag = element_text(size=8),
                   plot.title = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Sets per trip (observer data) - by year
g1a <- ggplot(obs_sets_trip,  aes(x=year, y=nsets1, group=year, fill=ntrips)) +
  geom_boxplot(outlier.shape=21, linewidth=0.2, outlier.size = 1, outlier.stroke = 0.2) +
  # Avergae line
  # geom_hline(yintercept=obs_sets_per_trip_avg, linewidth=0.8, linetype='dashed') +
  # Labels
  labs(x="Year", y="Number of sets per day\n(on observed fishing trips)", tag="A") +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  scale_y_continuous(breaks=seq(0,12, 2), lim=c(0,NA)) +
  # Legend
  scale_fill_gradientn(name="# of observed trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position="top")) +

  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.25, 0.9),
        legend.key.size = unit(0.3, "cm"),
        legend.direction = "horizontal")
g1a

# Sets per trip (observer data) - all years
g1b <- ggplot(obs_sets_trip,  aes(y=nsets1)) +
  geom_boxplot(linewidth=0.2, outlier.size = 1) +
  # Labels
  labs(x=" ", y="", tag=" ") +
  scale_x_continuous(breaks=0, label="All") +
  scale_y_continuous(breaks=seq(0,12, 2), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank())
g1b

# Merge
g1 <- gridExtra::grid.arrange(g1a, g1b, widths=c(0.8, 0.2))
g1

# Pseudo-sets per trip (logbook data) - by year
g2a <- ggplot(logs_sets_trip,  aes(x=year, y=nsets1, group=year)) +
  geom_boxplot(linewidth=0.2, fill="grey90", outlier.size = 1) +
  # Avergae line
  # geom_hline(yintercept=obs_sets_per_trip_avg, linewidth=0.8, linetype='dashed') +
  # Labels
  labs(x="Year", y="Number of pseudo-sets per day\n(on logged fishing trips)", tag="B") +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  scale_y_continuous(breaks=seq(0,30, 2), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g2a

# Pseudo-sets per trip (logbook data) - all years
g2b <- ggplot(logs_sets_trip,  aes(y=nsets1)) +
  geom_boxplot(linewidth=0.2, fill="grey90", outlier.size = 1) +
  # Labels
  labs(x=" ", y="", tag=" ") +
  scale_x_continuous(breaks=0, label="All") +
  scale_y_continuous(breaks=seq(0,30, 2), lim=c(0,NA)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank())
g2b

# Merge
g2 <- gridExtra::grid.arrange(g2a, g2b, widths=c(0.8, 0.2))
g2

# Bycatch 
g3 <- ggplot(obs_ordered, aes(y=comm_name, x=n_caught)) +
  geom_boxplot(linewidth=0.2, fill="grey90", outlier.size = 1) +
  # Labels
  labs(y="", x="Number of bycatch in sets with bycatch", tag="C") +
  # Scales
  scale_x_continuous(trans="log10", breaks=c(1, 2, 5, 10, 20, 50, 100)) +
  # Theme
  theme_bw()  + my_theme +
  theme(axis.title.y=element_blank())
g3

# Merge
layout_matrix <- matrix(data=c(1,2,
                               3,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, heights=c(0.65, 0.35))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sets_per_trip_plus.png"), 
       width=6.5, height=4.5, units="in", dpi=600)






