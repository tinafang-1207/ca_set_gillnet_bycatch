

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Export directory
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge"

# Export key
data_raw_orig <- readRDS(file.path(outdir, "1983_2017_gillnet_observer_metadata_unimputed.Rds"))
data_imputed_orig <- readRDS(file.path(outdir, "1983_2017_gillnet_observer_metadata_all.Rds"))

# Build data
################################################################################

# Format imputed data
table(data_imputed_orig$net_type)
data_imputed <- data_imputed_orig %>% 
  # Set nets
  filter(!net_type %in% c("drift", "float")) 

# Format raw data
table(data$net_type)
data_raw <- data_raw_orig %>% 
  # Set nets
  filter(!net_type %in% c("drift", "float")) %>% 
  mutate(soak_day=soak_hr/24)

# Mesh size by target stats
mesh_stats <- data_raw %>% 
  group_by(target_spp) %>% 
  summarise(mesh_in=median(mesh_size_in, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(mesh_in))

# Order data by mesh size
data_raw1 <- data_raw %>% 
  mutate(target_spp=factor(target_spp, levels=mesh_stats$target_spp))

# Calculate sets per trip
stats_trip <- data_imputed_orig %>% 
  # Filter
  filter(!net_type %in% c("drift", "float") & mesh_size_in>=3.5) %>% 
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

# Base theme
base_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   plot.tag=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot mesh size
g1 <- ggplot(data_raw, aes(x=mesh_size_in)) +
  geom_histogram(breaks=seq(0, 30, 0.5)) +
  # Labels
  labs(x="Mesh size (in)", y="Number of observed sets", tag="A") +
  scale_x_continuous(breaks=c(0, 3.5, 8.5, 15, 20, 25, 30)) +
  # Theme
  theme_bw() + base_theme 
g1

# Plot
g2 <- ggplot(data_raw1, aes(y=target_spp, x=mesh_size_in)) +
  geom_boxplot(outlier.shape=21, linewidth=0.2, outlier.size = 0.3) +
  # Labels
  labs(x="Mesh size (in)", y="Target species", tag="B") +
  scale_x_continuous(breaks=c(3.5, 8.5, seq(0,30, 5))) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 0, hjust = 1))
g2

# Plot soak hour
g3 <- ggplot(data_raw, aes(x=soak_day)) +
  geom_histogram(breaks=seq(0, 10, 0.25)) +
  # Labels
  labs(x="Soak time (days)", y="Number of observed sets", tag="C") +
  scale_x_continuous(breaks=seq(0, 10, 1)) +
  # Theme
  theme_bw() + base_theme 
g3

# Plot depths
g4 <- ggplot(data_imputed, aes(x=depth_fa_orig, y=depth_fa)) +
  geom_point(shape=1, alpha=0.5, color="grey50") + 
  # Reference lines
  geom_abline(slope=c(1, 6)) +
  # Labels
  labs(x="Reported depth (fa)", y="Extracted depth (fa)", tag="D") +
  # Theme
  theme_bw() + base_theme 
g4

# Plot data
g5 <- ggplot(stats_trip,  aes(x=year, y=nsets1, group=year, fill=ntrips)) +
  geom_boxplot(outlier.shape=21, linewidth=0.2, outlier.size = 0.3) +
  # Avergae line
  geom_hline(yintercept=sets_per_trip_avg, linewidth=0.4, linetype='dashed') +
  # Labels
  labs(x="Year", y="Number of sets per day\n(on observed fishing trips)", tag="E") +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  scale_y_continuous(breaks=seq(0,12, 2), lim=c(0,NA)) +
  # Legend
  scale_fill_gradientn(name="Number of\nobserved trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position = "none",
        legend.key.size = unit(0.2, "cm"))
g5


# Merge
layout_matrix <- matrix(data=c(1,2,2,
                               3,4,5), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS2_observer_data_traits.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

