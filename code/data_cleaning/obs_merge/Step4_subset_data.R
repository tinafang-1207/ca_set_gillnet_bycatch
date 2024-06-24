

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
data_orig <- readRDS(file=file.path(outdir, "1983_2017_gillnet_observer_data_with_sst.Rds"))
table(data_orig$dataset)


# Subset to data of interest
################################################################################

# Some final wrangling
data <- data_orig %>% 
  mutate(spp_type=case_when(comm_name=="Unidentified seabird" ~ "bird",
                        comm_name %in% c("Sea otter", "Unidentified marine mammal") ~ "mammal",
                        T ~ spp_type))

# Insepct
freeR::complete(data)
spp_key <- data %>% 
  count(spp_type, comm_name)

# Net types
table(data_orig$net_type)

#  Set nets
data_set <- data %>% 
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

# Completeness
freeR::complete(data_set_lg)

table(data_set_lg$dataset)


# Export
################################################################################

# Export
saveRDS(data_set_lg, file=file.path(outdir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

