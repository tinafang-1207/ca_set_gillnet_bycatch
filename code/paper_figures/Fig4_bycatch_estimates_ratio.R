
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
data_orig <- readRDS(file=file.path(datadir1, "1981_2022_bycatch_estimate_ratio_no_strata.Rds"))

# Read historical data
data_hist_orig <- readRDS(file=file.path(datadir2, "ca_set_gillnet_bycatch_estimates_historical.Rds")) 


# Build data
################################################################################

# Format historical
data_hist <- data_hist_orig %>% 
  # Species of interest
  filter(species %in% unique(data_orig$comm_name)) %>% 
  # Simplify
  select(species, year, mort) %>% #mort_lo, mort_hi) %>% 
  # Rename
  rename(comm_name=species) %>% 
  # Expand
  full_join(data_orig %>% select(comm_name, year)) %>% 
  # Add
  mutate(dataset='Historical studies') %>% 
  # Arrange
  select(dataset, everything()) %>% 
  arrange(dataset, comm_name, year)

# Build data
data <- data_orig %>% 
  # Add
  mutate(dataset='Our study') %>% 
  # Simplify
  select(dataset, comm_name, year, mort, mort_lo, mort_hi) %>% 
  # Add historical
  bind_rows(data_hist)


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
g <- ggplot(data, aes(x=year, y=mort, color=dataset)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Data
  geom_ribbon(mapping=aes(x=year, ymin=mort_lo, ymax=mort_hi, fill=dataset), 
              color=NA, alpha=0.6) +
  geom_path(linewidth=0.4) +
  geom_point(data=data %>% filter(dataset!="Our study"), size=0.6) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  # Legend
  scale_color_manual(name="Estimate source", values=c("purple", "darkgreen")) +
  scale_fill_manual(name="Estimate source", values=c("purple", "darkgreen")) +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_bycatch_estimates_ratio.png"), 
       width=6.5, height=4, units="in", dpi=600)


