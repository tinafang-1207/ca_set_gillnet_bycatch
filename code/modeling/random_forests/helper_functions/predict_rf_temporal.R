
# function to predict temporal numbers

predict_temporal <- function(best_model_fit, predict_data, spp){
  
  if(spp == "Common murre"){
    scalar <- 3
  }else{
    scalar <-1
  }
  
  preds_prob <- predict(best_model_fit, predict_data, type = "prob") %>%
    mutate(species = spp)
  
  #combine columns
  predict_final <- predict_data %>%
    bind_cols(preds_prob) %>%
    mutate(threshold = ifelse(species == "Common murre", 0.38, 0.5)) %>%
    mutate(bycatch_yn = ifelse(.pred_1>=threshold, 1, 0))
  
  # calculate bycatch numbers each year
  data_bycatch <- predict_final %>%
    mutate(bycatch_yn = as.numeric(as.character(bycatch_yn))) %>%
    group_by(year) %>%
    summarize(bycatch_sets = sum(bycatch_yn)) %>%
    mutate(total_bycatch = bycatch_sets*3*scalar)
  
  return(data_bycatch)
  
}




