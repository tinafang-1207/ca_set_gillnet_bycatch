
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Simulate data
################################################################################

# Species
species <- c("California sea lion",
             "Harbor porpoise",
             "Harbor seal",
             "Common murre",
             "Brandt's cormorant",
             "Northern elephant seal")

# Loop through grid and simulate data
data_orig <- purrr::map_df(1:length(species), function(x){
  
  # Set the length of the random walk
  walk_length <- length(1981:2022)
  
  # Generate random values from a normal distribution (you can adjust mean and standard deviation as needed)
  random_values <- rnorm(walk_length, mean = 0, sd = 1)
  
  # Create the random walk by cumulatively summing the random values
  random_walk <- cumsum(random_values)
  
  # Build data
  data1 <- tibble(species=species[x],
                  year=1981:2022,
                  value=random_walk)
  
})

# Format data
data <- data_orig %>% 
  mutate(cv=0.1,
         sd=cv*abs(value),
         value_lo=value-1.96*sd,
         value_hi=value+1.96*sd)

# Plot data
################################################################################

# Theme
base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
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
g <- ggplot(data, aes(x=year, y=value)) +
  facet_wrap(~species, ncol=3, scales="free_y") +
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), fill="grey60", color=NA) +
  geom_line() +
  # Labels
  labs(x="Year", y="Estimated bycatch") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.9, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_bycatch_estimates_ratio.png"), 
       width=6.5, height=4, units="in", dpi=600)


