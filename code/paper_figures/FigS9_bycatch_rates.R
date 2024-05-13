
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "model_result"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1981_2022_bycatch_rates.Rds"))


# Format data
################################################################################

# Strata
stratas <- c("Northern California", "San Francisco", "Monterey Bay", "Morro Bay",
             "Ventura", 'Channel Islands', "Southern California")

# Format data
data <- data_orig %>% 
  # Factor strata
  mutate(strata=factor(strata, levels=c(stratas)))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data, aes(x=year, y=rate_imp, color=strata)) +
  facet_wrap(~comm_name, ncol=3, scales="free_y") +
  geom_point(mapping=aes(shape=rate_type), size=1.5, stroke=0.2) +
  geom_line(linewidth=0.5) +
  # Labels
  labs(x="Year", y="Bycatch per trip") +
  # Legend
  scale_color_manual(name="Strata", drop=F, 
                     values=RColorBrewer::brewer.pal(nlevels(data$strata), "Set1")) +
  scale_shape_manual(name="Data type", values=c(1, 16)) +
  # Theme
  theme_bw() + my_theme 
g


# Export
ggsave(g, filename=file.path(plotdir, "FigS9_bycatch_rates.png"), 
       width=6.5, height=3.5, units="in", dpi=600)


