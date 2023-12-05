
install.packages("rgdal")

library(rgdal)

hs <- readOGR(dsn = "data/gis_data/HS_pred_habitat", layer = "ds2163_b237_PredictedHabitatSuitability_CWHR.lyr")
