
# function to predict temporal numbers

predict_temporal <- function(best_model_fit, predict_data, spp){
  
  if(spp == "Common murre"){
    scalar <- 3
  }else{
    scalar <-1
  }
  
  preds_fac <- predict(best_model_fit, predict_data)
  preds_prob <- predict(best_model_fit, predict_data, type = "prob")
  
  #combine columns
  predict_final <- predict_data %>%
    bind_cols(preds_fac) %>%
    bind_cols(preds_prob)
  
  # calculate bycatch numbers each year
  data_bycatch <- predict_final %>%
    rename(bycatch_yn=.pred_class) %>%
    mutate(bycatch_yn = as.numeric(as.character(bycatch_yn))) %>%
    group_by(year) %>%
    summarize(bycatch_sets = sum(bycatch_yn)) %>%
    mutate(total_bycatch = bycatch_sets*3*scalar)
  
  return(data_bycatch)
  
}




