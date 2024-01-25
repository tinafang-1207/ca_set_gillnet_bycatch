

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris

# Read data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))





# Build data
################################################################################

# Build data
data <- data_orig %>% 
  filter(comm_name %in% c("California sea lion", "Harbor seal", "Common murre", "Northern elephant seal", "Soupfin shark"))

# Plot data 
g <- ggplot(data, aes(y=n_caught)) +
  facet_wrap(~comm_name, scales="free_y") +
  geom_boxplot() +
  scale_y_continuous(trans="log2")
g
