
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


# Build data
################################################################################

# Calculate recent averages
stats <- data_orig %>%
  # Calculate stats
  group_by(comm_name) %>%
  summarize(ymax=max(mort_hi),
            yuse=ymax*0.15,
            bycatch_avg=mean(mort[year%in%2013:2022])) %>% 
  ungroup() %>% 
  # Add label
  mutate(label=paste(round(bycatch_avg, 1), "/ yr")) %>% 
  # Arrange
  arrange(desc(bycatch_avg)) %>% 
  mutate(comm_name=factor(comm_name, levels=comm_name))

# Order results
data <- data_orig %>% 
  mutate(comm_name=factor(comm_name, levels=stats$comm_name))


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
g <- ggplot(data, aes(x=year, y=mort)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Our estimates
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), color="grey30", width=0, linewidth=0.3) +
  # Plot label
  geom_segment(data=stats, mapping=aes(yend=yuse), 
               x=2013, xend=2013, y=0, color="grey30", linewidth=0.2, linetype="solid") +
  geom_text(data=stats, mapping=aes(y=yuse, label=label),
            x=2013.5, hjust=0, size=2, color="grey30") +
  # Labels
  labs(x="", y="Estimated bycatch") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig4_bycatch_estimates_ratio.png"), 
       width=6.5, height=4, units="in", dpi=600)


