

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
tabledir <- "tables"

# Read obersever data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))
logs_orig <- readRDS(file.path(datadir2, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))


# Build data
################################################################################

# Number of vessels fishing
nvessels_logs <- logs_orig %>% 
  # Filter
  filter(year<=2021) %>% 
  # Summarize
  group_by(year) %>%
  summarize(nvessels_tot=n_distinct(vessel_id))  %>%
  ungroup()

# Number of vessels observed
freeR::complete(data_orig)
yrs_missing_ids <- data_orig$year[is.na(data_orig$vessel_id)] %>% unique() %>% sort()
nvessels_obs <- data_orig %>% 
  group_by(year) %>% 
  summarize(nvessels=n_distinct(vessel_id)) %>% 
  mutate(missing_ids_yn=year %in% yrs_missing_ids)

# Final data
data <- nvessels_logs %>% 
  left_join(nvessels_obs) %>% 
  mutate(prop=nvessels/nvessels_tot)


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=9),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.key = element_rect(fill = NA, color=NA),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# By number
g1 <- ggplot(data %>% filter(!is.na(missing_ids_yn)), aes(x=year, y=nvessels, fill=missing_ids_yn)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of vessels\nwith observed trips", tag="A") +
  # Legend
  scale_fill_discrete(name="Count underestimated?", ) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position=c(0.7, 0.8))
g1

# By percent
g2 <- ggplot(data, aes(x=year, y=prop, fill=missing_ids_yn)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Percent of vessels\nwith observed trips", tag="B") +
  scale_y_continuous(labels=scales::percent_format()) +
  # Legend
  scale_fill_discrete(name="Count underestimated?") +
  # Theme
  theme_bw() + my_theme + 
  theme(legend.position="none")
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "percent_vessels_observed.png"), 
       width=6.5, height=3, units="in", dpi=600)

