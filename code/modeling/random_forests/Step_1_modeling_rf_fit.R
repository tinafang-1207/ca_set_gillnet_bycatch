
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

spp_do <- c("California sea lion", "Harbor seal", "Soupfin shark")


########### Fit balanced rf below ###############

# set up output dir for balance rf
outputdir_balanced_rf <- "model_result/balanced_rf"

i <- 1

for (i in 1:length(spp_do)) {
  
  spp_run <- spp_do[i]
  
  balanced_rf <- fit_balanced_rf_model(spp = spp_run, model_orig = model_orig)
  
  outfile_balanced_rf <- paste0(tolower(gsub(" ", "_", spp_run)), "_model_balanced_rf.Rds")
  
  saveRDS(balanced_rf, file = file.path(outputdir_balanced_rf, outfile_balanced_rf))
  
  }

############## Fit weighted rf below ###############

#set up output dir for weighted rf
outputdir_weighted_rf <- "model_result/weighted_rf"

i <- 1

for (i in 1:length(spp_do)) {
  
  spp_run <- spp_do[i]
  
  weighted_rf <- fit_weighted_rf_model(spp = spp_run, model_orig = model_orig)
  
  outfile_weighted_rf <- paste0(tolower(gsub(" ", "_", spp_run)), "_model_weighted_rf.Rds")
  
  saveRDS(weighted_rf, file = file.path(outputdir_weighted_rf, outfile_weighted_rf))
  
}



