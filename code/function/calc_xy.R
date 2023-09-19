calc_xy <- function(block, depth_fa){
  
  # Derive min/max depth
  max_depth_fa <- depth_fa + 1
  min_depth_fa <- depth_fa - 1
  
  block_do <- fishing_blocks %>%
    filter(block_id == block)
  
  bathy_200fa_WGS84_cropped <- crop(x = bathy_200fa_WGS84, y = block_do)
  
  bathy_200fa_WGS84_cropped_df <- as.data.frame(bathy_200fa_WGS84_cropped, xy = TRUE) %>%
    filter(!is.na(bd200fa_v2i))
  
  bathy_200fa_WGS84_depth_df <- bathy_200fa_WGS84_cropped_df %>%
    filter(bd200fa_v2i >= -max_depth_fa & bd200fa_v2i <= -min_depth_fa)
  
  bathy_200fa_WGS84_depth <- rasterFromXYZ(bathy_200fa_WGS84_depth_df)
  
  centroid <- colMeans(xyFromCell(bathy_200fa_WGS84_depth, 1:ncell(bathy_200fa_WGS84_depth)))
  
}