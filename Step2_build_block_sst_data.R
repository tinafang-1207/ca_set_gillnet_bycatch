

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/gis_data"

# Read SST data
sst1 <- readRDS(file.path("data/gis_data/OISST_1980.Rds"))
sst2 <- readRDS(file.path("data/gis_data/OISST_1990.Rds"))
sst3 <- readRDS(file.path("data/gis_data/OISST_2000.Rds"))
sst4 <- readRDS(file.path("data/gis_data/OISST_2018.Rds"))

# Blocks
blocks_orig <- wcfish::blocks
blocks <- blocks_orig %>% 
  filter(!block_state %in% c("Oregon", "Washington"))


# Build data
################################################################################

# Merge SST data
sst_df <- bind_rows(sst1, sst2, sst3, sst4) %>% 
  mutate(date=lubridate::ymd(date)) %>% 
  na.omit() %>% 
  spread(key="date", value="sst")

# Get dates
dates <- colnames(sst_df)[3:ncol(sst_df)] %>% lubridate::ymd()

# Convert to a raster brick
sst_brick <- raster::rasterFromXYZ(sst_df, crs=sf::st_crs(blocks))

# Calculate means
data1 <- raster::extract(x=sst_brick, y=blocks, method="simple", fun="mean", na.rm=T)

# Format
data2 <- data1 %>% 
  as.data.frame() %>% 
  setNames(dates) %>% 
  mutate(block_id=blocks$block_id) %>% 
  select(block_id, everything()) %>% 
  gather(key="date", value="sst_c", 2:ncol(.)) %>% 
  na.omit() %>% 
  mutate(date=lubridate::ymd(date))

range(data2$date)


# Impute missing data
################################################################################

# Block/date temp
temp <- expand.grid(block_id=unique(data2$block_id),
                    date=seq(lubridate::ymd("1981-09-01"), lubridate::ymd("2022-04-01"), by="1 day")) 

# Impute
data3 <- temp %>% 
  left_join(data2, by=c("block_id", "date")) %>% 
  group_by(block_id) %>% 
  mutate(sst_c_imp=zoo::na.approx(sst_c)) %>% 
  ungroup()

freeR::complete(data3)


# Export data
################################################################################

# Export
saveRDS(data3, file=file.path(datadir, "1981_2022_block_sst_expperience.Rds"))





