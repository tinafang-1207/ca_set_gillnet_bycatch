
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/injury_mortality/raw"
outdir <- "data/injury_mortality/processed"
plotdir <- "figures"

# Source
# https://github.com/JimCarretta/MSI-data