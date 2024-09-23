
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
data_orig <- readRDS(file=file.path(datadir1, "1981_2021_bycatch_estimate_ratio_stratified.Rds"))

# Read RF predictions
rf_orig <- read.csv(file=file.path(datadir1, "temporal_prediction.csv"), as.is=T) %>%
  rename(comm_name = species)


# Build data
################################################################################

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
  mutate(comm_name=factor(comm_name, levels=levels(stats$comm_name))) %>% 
  mutate(strata=factor(strata, levels=c("Southern California", "Channel Islands", 
                                        "Ventura", "Morro Bay", "Monterey Bay")))

# Order RF predictions
rf <- rf_orig %>% 
  mutate(comm_name=factor(comm_name, levels=levels(stats$comm_name)))


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
g <- ggplot(data, aes(x=year, y=nbycatch, fill=strata)) +
  # Facet
  lemon::facet_rep_wrap(~comm_name, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Our estimates
  geom_bar(stat="identity", color="grey30", linewidth=0.1) +
  # RF estimates
  geom_line(data=rf, mapping=aes(x=year, y= total_bycatch), inherit.aes = F) +
  # Reference lines
  geom_vline(xintercept=c(1987, 1994, 2002), linetype="dashed", color="grey50", linewidth=0.3) +
  # Plot label
  geom_text(data=stats, mapping=aes(y=ymax*0.97, label=label),
            x=2021, hjust=1, size=2.2, color="grey30", inherit.aes = F) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig3_bycatch_estimates.png"), 
       width=6.5, height=4, units="in", dpi=600)


