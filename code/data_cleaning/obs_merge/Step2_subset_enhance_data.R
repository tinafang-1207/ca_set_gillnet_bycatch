

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Export directory
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1983_2017_gillnet_observer_data.Rds"))


# Subset to data of interest
################################################################################

#  Set nets
data_set <- data_orig %>% 
  # Filter to set gillnets
  filter(net_type %in% c("set", "trammel 1 panel", "trammel 2 panel", "trammel 3 panel"))

# Inspect
table(data_set$target_spp)
table(data_set$mesh_size_in)

#  3.5" set nets
data_set_lg <- data_set %>% 
  # Filter to >3.5" mesh size
  filter(mesh_size_in >= 3.5)

# Inspect
table(data_set_lg$target_spp)

# Target species: California halibut, White seabass, Pacific angel shark
data_set_lg <- data_set %>% 
  # Filter to target species
  filter(target_spp %in% c("California halibut", "White seabass", "Pacific angel shark"))


# Export
################################################################################

# Export
saveRDS(data_set_lg, file=file.path(outdir, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))

