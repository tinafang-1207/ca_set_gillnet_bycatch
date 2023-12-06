

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "data/colonies/raw"
outdir <- "data/colonies/processed"

# Read data
data_orig <- sf::st_read(file.path(indir, "fw274np8584.shp"))


# Setup
################################################################################

# Filter
sdata <- data_orig %>% 
  filter(brco>0)

# 
g <- ggplot(sdata, aes(size=brco)) +
  geom_sf() +
  theme_bw()
g


# Filter
sdata <- data_orig %>% 
  filter(comu>0)

# 
g <- ggplot(sdata, aes(size=comu)) +
  geom_sf() +
  theme_bw()
g

