


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
species_do <- c("California sea lion", "Harbor seal", "Common murre", 
                "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise")

# Build data
data <- data_orig %>% 
  # Species of interest
  filter(species %in% species_do) %>% 
  # Calculate CV
  mutate(mort_cv_calc=mort_se_calc/mort) %>% 
  filter(!is.na(mort_cv_calc))

# Build stats
stats <- data %>% 
  group_by(species) %>% 
  summarize(mort_cv=median(mort_cv_calc)) %>% 
  ungroup() %>% 
  arrange(desc(mort_cv))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title = element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.position = "none",
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data , aes(x=mort_cv_calc, y=species %>% factor(., levels=stats$species), fill=species)) +
  geom_boxplot(alpha=0.5) +
  # Labels
  labs(x="CV of bycatch estimate", y="") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS10_historical_estimates.png"), 
       width=6.5, height=4, units="in", dpi=600)




