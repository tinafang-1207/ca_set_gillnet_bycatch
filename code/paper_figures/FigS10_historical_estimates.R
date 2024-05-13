


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/historical_estimates/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(datadir, "ca_set_gillnet_bycatch_estimates_historical.Rds"))


# Build data
################################################################################

# Species
spp <- c("California sea lion", "Harbor seal", "Common murre", 
         "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise")

# Order data
data_ordered <- data_orig %>% 
  filter(species %in% spp) %>% 
  mutate(species=factor(species, levels=spp))


# Plot data
################################################################################

# Theme
my_theme2 <-  theme(axis.text=element_text(size=7),
                    axis.title = element_text(size=8),
                     strip.text=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered , aes(x=year, y=mort)) +
  # Facet
  lemon::facet_rep_wrap(~species, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), width=0) +
  # Labels
  # geom_text(data=stats_select, mapping=aes(y=ymax, label=mort_tot), x=2012, 
  #           size=2.4, hjust=1) +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  # Theme
  theme_bw() + my_theme2
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS10_historical_estimates.png"), 
       width=6.5, height=4, units="in", dpi=600)




