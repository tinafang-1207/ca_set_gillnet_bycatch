
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
    mutate(threshold = case_when(spp == "California sea lion"~0.49,
                                 spp == "Harbor seal"~0.53,
                                 spp == "Common murre"~0.51,
                                 spp == "Northern elephant seal"~0.5)) %>%
    mutate(bycatch_yn = ifelse(.pred_1>=threshold, 1, 0))
  
  # calculate bycatch numbers each year
  data_bycatch <- predict_final %>%
    mutate(bycatch_yn = as.numeric(as.character(bycatch_yn))) %>%
    group_by(year,strata) %>%
    summarize(bycatch_sets = sum(bycatch_yn)) %>%
    mutate(total_bycatch = bycatch_sets*3*scalar)
  
  return(data_bycatch)
  
}




