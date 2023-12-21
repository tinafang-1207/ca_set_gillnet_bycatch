

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state_karin/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state_karin/processed"

# Read data
data1_orig <- readxl::read_excel(file.path(indir, "data1.xlsx")) # check depth ranges
data2_orig <- readxl::read_excel(file.path(indir, "data2.xlsx")) # check depth ranges, check counts
data4_orig <- readxl::read_excel(file.path(indir, "data4.xlsx")) # check depth ranges, check counts
data5_orig <- readxl::read_excel(file.path(indir, "data5.xlsx")) # check depth ranges, check counts

# Format data
str(data1_orig)
str(data2_orig)
str(data4_orig)
str(data5_orig)


data2 <- data2_orig %>% 
  mutate(depth=as.numeric(depth_max))
