
# function to predict spatial risk

predict_spatial_risk <- function(best_model_fit, predict_data, spp){
  
  preds_fac <- predict(best_model_fit, predict_data)
  preds_prob <- predict(best_model_fit, predict_data, type = "prob")
  
  
  # combine columns to have final prediction dataframe
  predict_final <- predict_data %>%
    bind_cols(preds_fac) %>%
    bind_cols(preds_prob) %>%
    mutate(species = spp) %>%
    select(lat_dd, long_dd, yday, .pred_class, .pred_0, .pred_1, species)
  
  # calculate weight by julian day
  
  # weight will be vessel-day (will be the trip-id, use n_distinct(trip_id))
  logbook_weight <- logbook %>%
    # create julian day
    mutate(yday = lubridate::yday(date)) %>%
    # filter
    filter(year >=2010) %>%
    filter(net_type == "Set") %>%
    # count
    group_by(yday) %>%
    summarize(vessel_days_weight = n_distinct(trip_id))
  
  #combine the dataframe with weight and calculate the spatial risk
  predict_spatial_risk <- predict_final %>%
    left_join(logbook_weight, by = "yday") %>%
    group_by(lat_dd, long_dd) %>%
    summarize(spatial_risk = weighted.mean(.pred_1, vessel_days_weight)) %>%
    mutate(species = spp)
  
  return(predict_spatial_risk)
  
}
