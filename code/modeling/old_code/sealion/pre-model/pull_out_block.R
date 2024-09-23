
#### clean working environment####
rm(list = ls())

#### load in packages####
library(tidyverse)

### read in data
###########################

model_data_orig <-read.csv("data/confidential/processed/fig2_total_merge_final.csv")

blocks <- wcfish::blocks

# convert to spatial object 
blocks_sp <- sf::as_Spatial(blocks)

#filter fishing locations from model data
data_xy <- model_data_orig %>%
  select(haul_lat_dd, haul_long_dd) %>%
  unique() %>%
  filter(!is.na(haul_lat_dd) & !is.na(haul_long_dd))

#convert fishing locations to sp
data_xy_sp <- data_xy %>%
  sf::st_as_sf(coords = c("haul_long_dd", "haul_lat_dd"), crs=sf::st_crs(blocks), remove = F) %>%
  sf::as_Spatial()

# intersects with blocks
data_xy_block_ids <- sp::over(data_xy_sp, blocks_sp) %>%
  pull(block_id)

data_xy_block_df <- data_xy_block_ids %>%
  as.data.frame() %>%
  mutate(haul_lat_dd = data_xy$haul_lat_dd, haul_long_dd=data_xy$haul_long_dd) %>%
  rename(block_id = ".") %>%
  select(haul_long_dd, haul_lat_dd, block_id)

# join with block location

block_xy <- blocks %>%
  select(block_id, block_long_dd, block_lat_dd) %>%
  sf::st_drop_geometry()

data_block <- left_join(data_xy_block_df, block_xy, by = "block_id")

# join block location back with model data

model_data_orig_block <- left_join(model_data_orig, data_block, by = c("haul_lat_dd", "haul_long_dd"))

# save data

write.table(model_data_orig_block, file = "data/confidential/processed/fig2_total_merge_final_block.csv", row.names = F, sep = ",")





