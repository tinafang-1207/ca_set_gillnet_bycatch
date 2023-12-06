

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
outdir <- "data/species_distributions/processed"

# Read data
bathy_orig <- raster::raster("/Users/cfree/Dropbox/Chris/UCSB/projects/ca_set_gillnet_bycatch/data/gis_data/200mEEZ_BathyGrids/bd200fa_v2i")

# Identify cells between 0 and -50
bathy_use <- bathy_orig <= 0 & bathy_orig >= -50

# Convert the selected cells to a polygon
bathy_poly <- raster::rasterToPolygons(bathy_use, dissolve = TRUE)

# Plot the original raster and the resulting polygon for visualization
plot(bathy_orig, main = "Original Raster")
plot(bathy_poly, add = TRUE, col = "red", border = "blue")
