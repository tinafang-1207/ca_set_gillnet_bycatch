
### clean working environment ###
rm(list = ls())

### load in package ###
library(tidyverse)

### read in logbook and observer data ###
lb_orig <- readRDS("data/confidential/processed/processed_CDFW_logbook/CDFW_1980_2022_gillnet_logbook_new_final.Rds")

obs_orig <- readRDS("data/confidential/processed/processed_obs_1980_2017/obs_1980_2017_final.Rds")

block_key <- read.csv("data/keys/block_stratified_key.csv")

### Explore fishing efforts stratified by quarter and geographic regions ###

lb_stratify <- lb_orig %>%
  # join block key
  left_join(block_key, by = "block_id") %>%
  # separate by season
  separate(date, into = c("year_date", "month_date", "day_date"), sep = "-") %>%
  mutate(season = case_when(month_date%in% c("12", "01", "02")~"winter",
                            month_date%in% c("03", "04", "05")~"spring",
                            month_date%in% c("06", "07", "08")~"summer",
                            month_date%in% c("09", "10", "11")~"fall")) 

lb_effort <- lb_stratify %>%
  filter(mesh_size_in > 3.5) %>%
  mutate(vessel_day=paste(vessel_id_use, "-", date)) %>%
  mutate(set_id=paste(vessel_name, "-", vessel_id_use, "-", permit_num, "-", date, "-", block_id, "-", haul_depth_fa, "-", net_length_ft, "-", mesh_size_correct, "-", soak_hour)) %>%
  # Summarize by year & stratified region
  group_by(season, stratified_region) %>% 
  summarize(n_logbook_rows=n(),
            n_vessels=n_distinct(vessel_id_use),
            n_vessel_days=n_distinct(vessel_day),
            n_sets=n_distinct(set_id)) %>% 
  ungroup()


