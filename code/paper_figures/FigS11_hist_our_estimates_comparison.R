
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir1 <- "model_result"
datadir2 <- "data/historical_estimates/processed"

# Read data
data_orig <- readRDS(file=file.path(datadir1, "1981_2021_bycatch_estimate_ratio_stratified_w_historical.Rds"))

# Read historical data
data_hist_orig <- readRDS(file=file.path(datadir2, "ca_set_gillnet_bycatch_estimates_historical.Rds")) 


# Build data
################################################################################

# Study species
spp_do <- c("California sea lion", "Harbor seal", "Common murre", 
           "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise")


# Calculate recent averages
stats <- data_orig %>%
  # Calculate annual sum
  group_by(comm_name, year) %>%
  summarize(nbycatch=sum(nbycatch, na.rm=T)) %>% 
  ungroup() %>% 
  # Calculate recent average
  group_by(comm_name) %>% 
  summarize(ymax=max(nbycatch, na.rm=T),
            nbycatch_avg=mean(nbycatch[year %in% 2012:2021], na.rm=T)) %>% 
  ungroup() %>% 
  # Add label
  mutate(label=paste(round(nbycatch_avg, 0), "per yr")) %>% 
  # Arrange
  arrange(desc(nbycatch_avg)) %>% 
  mutate(comm_name=factor(comm_name, levels=comm_name))

# Order results
data <- data_orig %>% 
  mutate(comm_name=factor(comm_name, levels=spp_do)) %>% 
  mutate(strata=factor(strata, levels=c("Southern California", "Channel Islands", 
                                        "Ventura", "Morro Bay", "Monterey Bay", "San Francisco")))

# Format data
data_hist <- data_hist_orig %>% 
  # Filter
  filter(species %in% spp_do) %>% 
  # Rename
  rename(comm_name=species) %>% 
  # Simplify
  select(comm_name, year, mort) %>% 
  # Expand
  full_join(data_orig %>% select(year, comm_name)) %>% 
  # Arrange
  arrange(comm_name, year) %>% 
  # Rename
  rename(nbycatch=mort) %>% 
  # Factor species
  mutate(comm_name=factor(comm_name, levels=spp_do )) 


# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     axis.title.x=element_blank(),
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

# Plot
g <- ggplot(data, aes(x=year, y=nbycatch)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Our estimates
  geom_bar(stat="identity", mapping=aes(fill=strata), color="grey30", linewidth=0.1) +
  # geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), color="grey70", width=0) +
  # Historical estimates
  geom_point(data=data_hist, size=0.6) +
  geom_path(data=data_hist, linewidth=0.3) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  # Legend
  scale_fill_discrete(name="") +
  guides(fill = guide_legend(nrow = 1)) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS11_bycatch_estimates_comparison_hist_ours.png"), 
       width=6.5, height=4, units="in", dpi=600)


