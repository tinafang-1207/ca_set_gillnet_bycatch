
#### clean working environment ####


rm(list = ls())

### read in packages ###

library(tidyverse)

library(tidymodels)
library(vip)
library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

### read in data ###

model_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv") # yutian

model_orig <- read.csv("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/processed/fig2_total_merge_final.csv") # chris


### Source the function

# balanced rf
source("code/function/balanced_rf.R")

#weighted rf
source("code/function/weighted_rf.R")

### set up the species list ###

spp_do <- c("California sea lion", "Harbor seal")


########### Fit balanced rf below ###############

# set up output dir
outputdir_balanced_rf <- "model_result/balanced_rf"

i <- 1

output_balanced_rf_list <- list()

for (i in length(spp_do)) {
  
  spp_run <- spp_do[i]
  
  balanced_rf <- fit_balanced_rf_model(spp = spp_run, model_orig = model_orig)
  
  outfile_balanced_rf <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_balanced_rf.Rds")
  
  # it does not paste list from harbor seal
  if (i == 1){output_balanced_rf_list <- list(balanced_rf)}else{output_balanced_rf_list <- append(output_balanced_rf_list, list(balanced_rf))}
  
  # the saveRDS only save the last item.....
  
  #saveRDS(output_balanced_rf_list[[i]], file = file.path(outputdir_balanced_rf, outfile_balanced_rf))
  
  }

############## Fit weighted rf below ###############

# For now, will write a for loop once it worked

output_sl_weighted <- fit_weighted_rf_model(spp = "California sea lion", model_orig = model_orig)

output_hs_weighted <- fit_weighted_rf_model(spp = "Harbor seal", model_orig = model_orig)

saveRDS(output_sl_weighted, file = file.path("model_result/weighted_rf/california_sea_lion_model_weighted_rf.Rds"))

saveRDS(output_hs_weighted, file = file.path("model_result/weighted_rf/harbor_seal_model_weighted_rf.Rds"))



############ Compare the output and select the best model #########

stats <- rf_all_df %>% 
  filter(.metric=="kap") %>% 
  arrange(balanced_type, desc(mean)) %>% 
  group_by(balanced_type) %>% 
  slice(1) %>% 
  mutate(balanced_type=factor(balanced_type, levels=c("upsample", "downsample", "smote"))) %>% 
  arrange(balanced_type)

which.max(stats$mean)

weighted_hs_df <- output_hs_weighted[["rf_weighted_final"]]
weighted_sl_df <- output_sl_weighted[["rf_weighted_final"]]

rf_all_df_hs <- balanced_rf[["rf_all_df"]]




