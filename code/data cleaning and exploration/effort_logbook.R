
#### clean working environment

rm(list = ls())

#### read in package
library(tidyverse)

#### read in data
lb_orig <- readRDS("data/confidential/processed/CDFW_1980_2022_gillnet_logbook_new.Rds")

ob_orig <- read.csv("data/confidential/processed/fig2_total_merge_final_block.csv") %>%
  filter(data_source == "CDFW(1983-1989)")

non_exist <- anti_join(CDFW_1983_1989_gillnet_observer_data, ob_orig, by = "set_id")


