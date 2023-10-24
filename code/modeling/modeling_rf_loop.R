
#### clean working environment ####

rm(list = ls())

### read in packages ###

library(tidyverse)

library(tidymodels)
library(vip)
#library(randomForest)
library(ranger)
library(DALEXtra)
library(themis)
library(workflows)

### read in data ###

model_orig <- read.csv("data/confidential/processed/fig2_total_merge_final.csv")

### set up the species ###

spp <- "California sea lion"

###

fit_rf_model <- function(spp, model_orig) {
  
  
}





















