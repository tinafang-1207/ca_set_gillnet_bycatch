

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

# Build data
data_raw <- data_raw_orig %>% 
  # Set nets
  filter(!net_type %in% c("drift", "float"))

# Inspect
table(data$net_type)
mesh_stats <- data_raw %>% 
  group_by(target_spp) %>% 
  summarise(mesh_in=median(mesh_size_in, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(mesh_in))

data_raw1 <- data_raw %>% 
  mutate(target_spp=factor(target_spp, levels=mesh_stats$target_spp))
  

# Plot mesh size
g1 <- ggplot(data_raw, aes(x=mesh_size_in)) +
  geom_histogram(breaks=seq(0, 30, 0.5)) +
  # Labels
  labs(x="Mesh size (in)", y="Number of observed sets", tag="A") +
  # Theme
  theme_bw()
g1

# Plot
g2 <- ggplot(data_raw1, aes(y=target_spp, x=mesh_size_in)) +
  geom_boxplot() +
  # Labels
  labs(x="Mesh size (in)", y="Target species", tag="B") +
  # Theme
  theme_bw()
g2

# Plot soak hour
g3 <- ggplot(data_raw, aes(x=soak_hr)) +
  geom_histogram(breaks=seq(0, 240, 6)) +
  # Labels
  labs(x="Soak time (hr)", y="Number of observed sets", tag="C") +
  # Theme
  theme_bw()
g3

# Plot depths
g4 <- ggplot(data_imputed_orig, aes(x=depth_fa_orig, y=depth_fa)) +
  geom_point(shape=1, alpha=0.5) + 
  # Reference lines
  geom_abline(slope=c(1, 6)) +
  # Labels
  labs(x="Reported depth (fa)", y="Extracted depth (fa)", tag="B") +
  # Theme
  theme_bw()
g4



# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2)
g

