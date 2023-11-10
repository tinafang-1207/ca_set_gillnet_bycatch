
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

spp_do <- c("California sea lion", "Harbor seal", "Brandt's cormorant")

# set up output dir
outputdir <- "model_result"

########### Fit balanced rf below ###############

i <- 1

for (i in length(spp_do)) {
  
  balanced_rf <- fit_balanced_rf_model(spp = spp_do[i], model_orig = model_orig)
  
  outfile_balanced_rf <- paste0(tolower(gsub(" ", "_", spp_do[i])), "_model_balanced_rf.Rds")
  
  saveRDS(balanced_rf, file=file.path(outputdir, outfile_balanced_rf) )
  
  }



# Run example

output <- readRDS("model_result/brandt's-cormorant_model_balanced_rf.Rds")

# Extract info

best_models <- output_harbor_seal[["best_models"]]

rf_all_df <- output_harbor_seal[["rf_all_df"]]


output_cormorant <- fit_balanced_rf_model(spp = "Brandt's cormorant", model_orig)

output_harbor_seal <- fit_balanced_rf_model(spp = "Harbor seal", model_orig)














# run example

output_sl_weighted <- fit_weighted_rf_model(spp = "California sea lion", model_orig)

# Extract data

rf_weighted_final <- output_sl_weighted[["rf_weighted_final"]]


############ Compare the output and select the best model #########

stats <- rf_all_df %>% 
  filter(.metric=="kap") %>% 
  arrange(balanced_type, desc(mean)) %>% 
  group_by(balanced_type) %>% 
  slice(1) %>% 
  mutate(balanced_type=factor(balanced_type, levels=c("upsample", "downsample", "smote"))) %>% 
  arrange(balanced_type)

which.max(stats$mean)








