
# clean working environment
#############################################################################
rm(list = ls())

# read in package
#############################################################################
library(tidyverse)

# read in data
#############################################################################
data_orig <- read.csv("model_result/temporal_prediction_strata.csv")

year_orig <- read.csv("model_result/temporal_prediction.csv")

plotdir <- "figures"

# Format data
#############################################################################
data <- data_orig %>% 
  mutate(strata=factor(strata, levels=c("Southern California", "Channel Islands", 
                                        "Ventura", "Morro Bay", "Monterey Bay"))) %>%
  drop_na()

# plot data
###############################################################################

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

g <- ggplot(data, aes(x=year, y=total_bycatch, fill=strata)) +
  # Facet
  lemon::facet_rep_wrap(~species, scales="free_y", ncol=2, repeat.tick.labels = 'bottom') +
  # Our estimates
  geom_bar(stat="identity", color="grey30", linewidth=0.1) +
  # RF estimates
  geom_line(data=year_orig, mapping=aes(x=year, y= total_bycatch), inherit.aes = F) +
  # Reference lines
  geom_vline(xintercept=c(1987, 1994, 2002), linetype="dashed", color="grey80", linewidth=0.3) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))

g


ggsave(g, filename=file.path(plotdir, "FigSX_rf_temporal_bycatch_estimates_strata.png"), 
       width=5, height=4, units="in", dpi=600)






