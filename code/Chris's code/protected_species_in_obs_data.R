

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read CDFW data
cdfw_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/cdfw_obs/processed/CDFW_1983_1989_gillnet_observer_data.Rds")
cdfw_key_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/cdfw_obs/processed/CDFW_1983_1989_gillnet_observer_set_info.Rds")

# Read SWFSC data
swfsc_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/swfsc_obs/processed/SWFSC_set_net_observer_data.Rds")
swfsc_key_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/swfsc_obs/processed/SWFSC_1990_2017_set_net_observer_trips.Rds")

# Read species key
spp_key <- read.csv("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_set_gillnet_bycatch/data/species_key_final.csv", as.is=T)


# Build data
################################################################################

# Identify set gillnet sets
cdfw_trips_use <- cdfw_key_orig %>% 
  filter(net_type=="set" & target_spp %in% c("California halibut", "Pacific angel shark", "White seabass"))

# Build CDFW data
# Number caught is correct - all but three discarded dead
cdfw <- cdfw_orig %>% 
  # Sets of interest
  filter(set_id %in% cdfw_trips_use$set_id) %>% 
  # Species of interest
  left_join(spp_key) %>% 
  filter(type %in% c("bird", "mammal", "turtle")) %>% 
  # Tabulate
  mutate(year=lubridate::year(date)) %>% 
  group_by(type, comm_name, year) %>% 
  summarise(n=sum(n_caught, na.rm=T)) %>% 
  ungroup() %>% 
  # Mutate
  mutate(dataset="CDFW")

# Identify set gillnet sets
swfsc_trips_use <- swfsc_key_orig %>% 
  filter(net_type!="float net" & target1_spp %in% c("Halibut, California", " Seabass, White", "Shark, Pacific Angel"))
freeR::complete(swfsc_trips_use)
table(swfsc_trips_use$net_type)
table(swfsc_trips_use$net_mesh_size_in)

# Build SWFSC data
swfsc <- swfsc_orig %>% 
  # Set gillnet
  filter(set_id %in% swfsc_trips_use$set_id) %>% 
  # Add type
  left_join(spp_key) %>% 
  filter(type %in% c("bird", "mammal", "turtle")) %>% 
  # These data don't record number for these guys -most dead though some alive
  mutate(n_caught=1) %>% 
  # Add date
  left_join(swfsc_trips_use %>% select(set_id, date_haul1)) %>%
  mutate(year=lubridate::year(date_haul1)) %>% 
  # Tabulate
  group_by(type, comm_name, year) %>% 
  summarise(n=sum(n_caught, na.rm=T)) %>% 
  ungroup() %>% 
  # Mutate
  mutate(dataset="SWFSC")

# Merge
data <- bind_rows(cdfw, swfsc)

# Stats
stats <- data %>% 
  # Summarize by species
  group_by(type, comm_name) %>% 
  summarise(n=sum(n, na.rm=T)) %>% 
  ungroup() %>% 
  # Arrange
  arrange(desc(n)) %>% 
  # Type
  mutate(comm_name=recode(comm_name,
                          "Sea Otter"="Sea otter")) %>% 
  mutate(type=recode_factor(type,
                             "bird"="Seabird",
                             "mammal"="Marine mammal",
                             "turtle"="Sea turtle"))



# Build data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   legend.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = c(0.8, 0.2),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(y=reorder(comm_name, n), x=n, fill=type)) +
  geom_bar(stat="identity") +
  # Labels
  geom_text(mapping=aes(x=n, y=comm_name, label=n), hjust=-0.2, size=2.4) +
  # Labels
  labs(x="Number of observed bycatch", y="") +
  # Reference line
  geom_hline(yintercept=nrow(stats)-5.5, linetype="dashed") +
  # Axes
  scale_x_continuous(trans="log10", lim=c(1, max(stats$n)*2)) +
  # Legend
  scale_fill_ordinal(name="") +
  # Theme
  theme_bw() + my_theme 
g

# Export
ggsave(g, filename=file.path(plotdir, "protected_species_bycatch_in_obs_data.png"), 
       width=6.5, height=4.5, units="in", dpi=600)






