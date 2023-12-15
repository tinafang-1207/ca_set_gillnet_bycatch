
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir1 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris
datadir2 <- "data/historical_estimates/processed"

# Read our effort data
effort_df <- read.csv(file=file.path(datadir1, "CA_3.5in_set_gillnet_effort_by_year.csv"))

# Read historical effort data
effort_hist_orig <- readRDS(file=file.path(datadir2, "ca_set_gillnet_effort_estimates_historical.Rds")) 

# Read our observer data
obs_orig <- readRDS("model_result/1981_2022_bycatch_estimate_ratio_no_strata.Rds")

# Read historical observer data
obs_hist_orig <- readRDS(file=file.path(datadir2, "ca_set_gillnet_bycatch_estimates_historical.Rds")) 


# Build data
################################################################################

# 1990 partial effort
effort_1990perk <- effort_hist_orig %>% 
  filter(year==1990 & reference=="Perkins et al. 1994")

effort_hist_use <- effort_hist_orig %>% 
  filter(!(year==1990 & reference=="Perkins et al. 1994"))

# Expand historical effort data to include NAs for missing years
effort_hist <- effort_df %>% 
  select(year) %>% 
  left_join(effort_hist_use %>% select(year, nvesseldays, nsets, nvesseldays_obs))

# Format our observer data
obs <- obs_orig %>% 
  select(comm_name, year, nbycatch) %>% 
  rename(bycatch_obs=nbycatch)

# Format historical observer data
obs_hist <- obs_hist_orig %>% 
  # Species of interest
  filter(species %in% c("Harbor porpoise", "Harbor seal", "Common murre", 
                        "Brandt's cormorant", "California sea lion", "Northern elephant seal")) %>% 
  # Simplify
  select(species, year, obs) %>% 
  # Rename
  rename(comm_name=species, bycatch_obs=obs)

# Extarct # of obs trips from our observer data
obs_trips <- obs_orig %>% 
  select(year, ntrips_obs) %>% 
  unique() %>% 
  rename(nvesseldays_obs=ntrips_obs)



# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=6),
                     axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=7),
                     axis.title.x=element_blank(),
                     legend.text=element_text(size=6),
                     legend.title=element_text(size=7),
                     strip.text=element_text(size=7),
                     plot.tag=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot effort
g1 <- ggplot(data=effort_df, mapping=aes(x=year, y=nvesseldays)) +
  # Our estimate
  geom_bar(stat="identity", fill="grey70") +
  # Historical estimates
  geom_path(data=effort_hist, linewidth=0.3) +
  geom_point(data=effort_hist, size=0.6) +
  geom_text(data=effort_hist %>% filter(year==1989), mapping=aes(x=year, y=nvesseldays), 
            label="Apr-Dec\nonly", hjust=1, nudge_x=-1, size=2.1) +
  geom_text(data=effort_hist %>% filter(year==1990), mapping=aes(x=year, y=nvesseldays), 
            label="Jan-Jun\nonly", hjust=1, nudge_x=-1, size=2.1) +
  # Labels
  labs(x="Year", y="Number of vessel days", tag="A") +
  # Theme
  theme_bw() + base_theme
g1

# Plot # of observed trips
g2 <- ggplot(data=obs_trips, mapping=aes(x=year, y=nvesseldays_obs)) +
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_line(data=effort_hist, linewidth=0.3) +
  geom_point(data=effort_hist, size=0.6) +
  # Labels
  labs(x="Year", y="Number of observed vessel days", tag="B") +
  # Theme
  theme_bw() + base_theme
g2

# Plot # of observed bycatch
g3 <- ggplot(data=obs, mapping=aes(x=year, y=bycatch_obs)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, ncol=3, scales="free_y", repeat.tick.labels = 'bottom') + 
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_line(data=obs_hist, linewidth=0.3) +
  geom_point(data=obs_hist, size=0.6) +
  # Labels
  labs(x="Year", y="Number of observed bycatch", tag="C") +
  # Theme
  theme_bw() + base_theme
g3  

# Arrange
layout_matrix <- matrix(data=c(1,3,
                               2,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, 
                             widths=c(0.3, 0.7),
                             layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_historical_vs_our_inputs.png"), 
       width=6.5, height=4, units="in", dpi=600)




# # Comparison of our set estimates and historic ones -- but we're clearly wrong
# g2 <- ggplot(data=effort_df, mapping=aes(x=year, y=nsets)) +
#   # Our estimate
#   geom_bar(stat="identity", fill="grey70") +
#   # Historical estimates
#   geom_path(data=effort_hist, mapping=aes(x=year, y=nsets)) +
#   geom_point(data=effort_hist, mapping=aes(x=year, y=nsets)) +
#   # Labels
#   labs(x="Year", y="Number of sets", tag="B") +
#   scale_x_continuous(seq(1980,2020,5)) +
#   # Theme
#   theme_bw() + base_theme
# g2


