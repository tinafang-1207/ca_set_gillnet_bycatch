
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

# Variables
variables <- c("Mesh size (cm)", 
               "Latitude (°N)", 
               "Temperature (°C)", 
               "Julian day", 
               "Depth (fa)",
               "Shore distance (km)", 
               "Soak time (hr)")

# Generate grid
spp_var <- expand.grid(species=species,
                       variable=variables)

# Loop through grid and simulate data
data_orig <- purrr::map_df(1:nrow(spp_var), function(x){
  
  # Set the length of the random walk
  walk_length <- 200
  
  # Generate random values from a normal distribution (you can adjust mean and standard deviation as needed)
  random_values <- rnorm(walk_length, mean = 0, sd = 1)
  
  # Create the random walk by cumulatively summing the random values
  random_walk <- cumsum(random_values)
  
  # Build data
  data1 <- tibble(species=spp_var$species[x],
                  variable=spp_var$variable[x],
                  value=1:walk_length,
                  effect=random_walk)
  
})

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
g <- ggplot(data_orig, aes(x=value, y=effect, color=species)) +
  facet_wrap(~variable, ncol=4, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="Variable value", y="Marginal effect") +
  # Legend
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.key.size=unit(0.3, "cm"),
        legend.position = c(0.9, 0.2))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_marginal_effects.png"), 
       width=6.5, height=3.75, units="in", dpi=600)


