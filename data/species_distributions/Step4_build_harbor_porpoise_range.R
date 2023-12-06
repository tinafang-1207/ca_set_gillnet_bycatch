

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

# Project raster
bathy_proj <- bathy_orig %>% 
  raster::projectRaster(crs="+proj=longlat +datum=WGS84")

# Crop raster
bathy_cropped <- raster::crop(bathy_proj, raster::extent(-180, 180, 34.5, 39.1))

# Identify cells between 0 and -50
bathy_use <- bathy_cropped <= 0 & bathy_cropped >= -50

# Plot the original raster and the resulting polygon for visualization
raster::plot(bathy_use)

# Export raster
raster::writeRaster(bathy_use, file=file.path(outdir, "harbor_porpoise_range.tiff"))



# Convert the selected cells to a polygon
bathy_poly <- raster::rasterToPolygons(bathy_use, dissolve = TRUE)

plot(bathy_poly, add = TRUE, col = "red", border = "blue")
