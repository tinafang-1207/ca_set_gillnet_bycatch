

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))

# Read block key
block_key <- readRDS("data/bathymetry/processed/block_key.Rds") %>% 
  mutate(shore_km=dist_m/1000)


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce
  filter(net_type=="Set") %>%
  # Add block distance from shore
  left_join(block_key %>% select(block_id, shore_km), by=c("block_id_num"="block_id")) %>% 
  # Arrange
  select(logbook_id:block_long_dd, shore_km, everything())


# Build data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))



