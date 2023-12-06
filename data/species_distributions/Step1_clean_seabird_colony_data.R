

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
indir <- "data/species_distributions/raw/seabird_colonies"
outdir <- "data/species_distributions/processed"

# Read data
data_orig <- sf::st_read(file.path(indir, "fw274np8584.shp"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Arrange quickly
  select(source:et_status, ftsp:laal, everything()) %>% 
  # Rename
  rename(colony_code_source=code,
         colony_code_fws=usfws,
         hotspot_yn=hotspot,
         species_n=nospecies,
         count_max=colsize,
         long_dd=longdd,
         lat_dd=latdd,
         latlong_modified_yn=et_status) %>% 
  # Arrange
  select(source, bioregion,
         colony, colony_code_source, colony_code_fws,
         lat_dd, long_dd, latlong_modified_yn, 
         hotspot_yn, species_n, count_max, 
         assp, bloy, blsk, blsp, brco, brpe, bvsw, caau, cate, comu, dcco,
         elte, fote, ftsp, hrgu, laal, lcsp, lesp, lete, peco, pigu, rhau, rote,
         tupu, unco, wegu, xamu,
         everything()) %>% 
  # Remove LCSP because no data and seems to be duplicate of LESP which has data
  select(-lcsp)
  
# Inspect
str(data)
freeR::complete(data)
colnames(data)

# Export data
saveRDS(data, file=file.path(outdir, "2010_seabird_colony_database.Rds"))


