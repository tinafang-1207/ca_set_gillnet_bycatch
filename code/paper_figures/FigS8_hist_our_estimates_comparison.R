
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

# Study species
spp_do <- c("Common murre", "California sea lion", "Harbor seal",
            "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise")

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
  arrange(comm_name, year)


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
g <- ggplot(data_orig, aes(x=year, y=mort)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Our estimates
  geom_bar(stat="identity", fill="grey70") +
  # geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), color="grey70", width=0) +
  # Historical estimates
  geom_point(data=data_hist, size=0.6) +
  geom_path(data=data_hist, linewidth=0.3) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS8_bycatch_estimates_comparison_hist_ours.png"), 
       width=6.5, height=4, units="in", dpi=600)


