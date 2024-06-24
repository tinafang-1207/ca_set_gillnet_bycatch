

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir1 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris

# Read observer data
obs_orig <- readRDS(file=file.path(datadir1, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

# Read logbook data
logs_orig <- readRDS(file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))
logs_orig_all <- readRDS(file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data_imputed.Rds"))


# Build data
################################################################################

# Build observer data
obs <- obs_orig %>% 
  # Simplify
  select(set_id, target_spp, mesh_size_in) %>% 
  unique() %>% 
  # Specie sof interest
  filter(target_spp %in% c("California halibut", "White seabass", "Pacific angel shark")) %>% 
  # Add
  mutate(dataset="Observer data")
  
# Build logbooks data
logs <- logs_orig %>% 
  # Simplify
  select(set_id, target_spp, mesh_in)  %>% 
  unique() %>% 
  # Rename
  rename(mesh_size_in=mesh_in) %>% 
  # Reduce
  filter(target_spp %in% c("White Seabass", "Halibut", "Angel Shark")) %>% 
  mutate(target_spp=recode(target_spp,
                           "White Seabass"="White seabass", 
                           "Halibut"="California halibut",
                           "Angel Shark"="Pacific angel shark")) %>% 
  # Add
  mutate(dataset="Logbooks")

# Merge data
data <- bind_rows(obs, logs)


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
                   legend.position = "top",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data,  aes(y=target_spp, x=mesh_size_in, fill=dataset)) +
  geom_boxplot(outlier.shape=21, linewidth=0.5) +
  # Labels
  labs(x="Mesh size (inches)", y="") +
  scale_x_continuous(lim=c(0, 30),
                     breaks=c(seq(0,30,5), 3.5, 6, 8.5) %>% sort(), 
                     labels=c("0", "3.5", "5", "6", "8.5", "10", "15", "20", "25", "30")) +
  # Legend
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "FigS6_mesh_size_by_target.png"), 
       width=4.5, height=2.5, units="in", dpi=600)



# All set gillnet plot
################################################################################

# Build set key
set_key <- logs_orig_all %>% 
  select(year:target_spp) %>% 
  unique()

# Format set key
set_key1 <- set_key %>% 
  rowwise() %>% 
  mutate(target_spp1 = str_split(target_spp, ",")[[1]][1])

# Order
stats <- set_key1 %>% 
  group_by(target_spp1) %>% 
  summarize(mesh_in=median(mesh_in, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(mesh_in))
set_key2 <- set_key1 %>% 
  mutate(target_spp1=factor(target_spp1, levels=stats$target_spp1))

# Plot
ggplot(set_key2, aes(y=target_spp1, x=mesh_in)) +
  geom_boxplot() +
  # Ref line
  geom_vline(xintercept = 3.5) + 
  # Theme
  theme_bw()






