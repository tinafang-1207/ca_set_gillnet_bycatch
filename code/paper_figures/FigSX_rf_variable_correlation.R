
### Clean working environment ###
rm(list = ls())

### read in package ###
library(tidyverse)

### read in observer data ###
model_orig <- readRDS("/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

########################################################

model_predictor <- model_orig %>%
  select(lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  cor()

model_variables <-  model_orig %>%
  select(lat_dd, long_dd, depth_fa, soak_hr, mesh_size_in, shore_km, yday, sst_c, island_yn) %>%
  slice(1:10000)
  

g_corr <- ggcorrplot::ggcorrplot(
  model_predictor,
  method = "circle",
  type = 'upper',
  outline.col = "black"
) +
  theme_bw() +
  theme(legend.position = c(0.9, 0.25),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

g_corr


g <- GGally::ggpairs(model_variables)
  
g
  
  
  

#######################################################
# save the figure
plotdir <- "figures"

ggsave(g, filename=file.path(plotdir, "FigSX_rf_variable_correlation.png"), 
       width=8, height=8, units="in", dpi=600)


