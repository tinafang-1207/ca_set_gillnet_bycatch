
### clean working environment ###
rm(list = ls())

### load in package ###
library(tidyverse)

### read in data ###
lb_orig <- readRDS("data/confidential/original/gillnet_logbooks/processed/CDFW_2000_2020_gillnet_logbook_data.Rds")

#filter only to year 2000
lb_2000 <- lb_orig %>%
  filter(year == "2000") %>%
  filter(net_type == "Set") %>%
  #create unique identifier
  mutate(unique_id = paste(vessel_id, "-", vessel_name, "-", date)) %>%
  #filter out duplicated identifier (only keep trip info)
  filter(!duplicated(unique_id))

# assign stratum to lb_2000
lb_2000_stra <- lb_2000 %>%
  mutate(stratum = case_when(block_id %in% 437:650~"Central California",
                             block_id %in% c(684:690, 707:713, 760:762, 765, 806:807, 813:814, 829, 849:850, 867)~"Channel Islands",
                             block_id %in% c(718:918) & block_id %in% c(684:690, 707:713, 760:762, 765, 806:807, 813:814, 829, 849:850, 867, 776) == FALSE~"Southern California")) %>%
  mutate(stratum = ifelse(is.na(stratum), "Ventura", stratum)) %>%
  group_by(stratum) %>%
  count()
         
         