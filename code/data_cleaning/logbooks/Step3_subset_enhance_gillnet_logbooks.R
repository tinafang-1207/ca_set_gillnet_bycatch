

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
data_orig <- readRDS(file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data_imputed.Rds"))

# Read block key
block_key <- readRDS("data/bathymetry/processed/block_key.Rds")
strata_key <- readRDS("data/strata/block_strata_key.Rds") %>% 
  # Island if in CI strata or if four blocks around Farralon Islands
  mutate(island_yn=strata=="Channel Islands" | block_id %in% c(457, 458,467, 468)) %>% 
  select(-strata)

# Read block temperature key
sst_key <- readRDS("data/gis_data/1981_2022_block_sst_expperience.Rds") %>% 
  select(-sst_c) %>% 
  rename(sst_c=sst_c_imp)


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Reduce
  filter(net_type=="Set" & mesh_in >= 3.5) %>%
  # Add block distance from shore
  left_join(block_key %>% select(block_id, shore_km), by="block_id") %>% 
  # Add julian day
  mutate(yday=lubridate::yday(date)) %>% 
  # Add island y/n
  left_join(strata_key, by="block_id") %>% 
  # mutate(island_yn=ifelse(is.na(island_yn), F, island_yn)) %>% 
  # Add SST
  left_join(sst_key, by=c("date", "block_id")) %>% 
  # Arrange
  relocate(yday, .after=date) %>% 
  relocate(shore_km, .after=block_long_dd) %>% 
  relocate(island_yn, .after=shore_km) %>% 
  relocate(sst_c, .after=island_yn)

# Inspect
freeR::complete(data)



# Build data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))



