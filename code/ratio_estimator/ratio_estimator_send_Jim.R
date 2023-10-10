
### clean working environment ###
rm(list = ls())

### load in package ###
library(tidyverse)

### read in logbook and observer data ###
lb_orig <- readRDS("data/confidential/processed/processed_CDFW_logbook/CDFW_1980_2022_gillnet_logbook_new_final.Rds")

obs_orig <- readRDS("data/confidential/processed/processed_obs_1980_2017/obs_1980_2017_final.Rds")

### Explore fishing efforts stratified by quarter and geographic regions ###



