
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outputdir <- "model_result"
obsdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
logbookdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read effort time series
logs_orig <- readRDS(file.path(logbookdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# Read data
obs_orig <- readRDS(file=file.path(obsdir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

# Read block key
block_key <- readRDS("data/strata/block_strata_key.Rds")


# Compute bycatch rate
################################################################################

# ASSIGN BLOCK IDS NOT IN STRATA KEY A BLOCK ID

# Build annual effort by strata
effort <- logs_orig %>%
  # Add strata
  left_join(block_key) %>% 
  # Summarize
  group_by(year, strata) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>%
  ungroup() %>% 
  filter(!is.na(strata))



# Compute bycatch rate
################################################################################

# Stratas
stratas <- c("Monterey Bay", "Morro Bay", 
             "Ventura", "Channel Islands", "Southern California")

# Add strata
obs <- obs_orig %>% 
  # Add strata
  left_join(block_key) %>% 
  # Fill in missing strata
  mutate(strata=case_when(block_id %in% c(901, 904, 1032) ~ "Southern California",
                          T ~ strata))

# Inspect
freeR::complete(obs)
table(obs$strata)

# Retained halibut by year, strata
stats_halibut_kept <- obs %>% 
  # Calculate total catch by year, strata, and species
  group_by(year, strata) %>% 
  summarize(n_trips=n_distinct(trip_id),
            n_sets=n_distinct(set_id),
            n_halibut=sum(n_kept[comm_name=="California halibut"], na.rm=T)) %>% 
  ungroup()

# Discards by year, strata, species
stats_discards <- obs %>% 
  # Calculate number discarded
  mutate(n_discarded=n_caught-n_kept) %>% 
  # Calculate total catch by year, strata, and species
  group_by(year, strata, comm_name, spp_code_chr) %>% 
  summarize(n_discarded=sum(n_discarded, na.rm=T)) %>% 
  ungroup()

# Merge
stats <- stats_discards %>% 
  # Add retained halibut
  left_join(stats_halibut_kept) %>% 
  # Compute ratio
  mutate(ratio=n_discarded/n_halibut) %>% 
  # Filter to species of interest
  filter(comm_name %in% c("Giant sea bass", "Soupfin shark")) %>% 
  # Arrange
  select(strata, year, n_trips, n_sets, comm_name, everything()) %>% 
  arrange(strata, year, comm_name)


# Number of trips thresh
thresh_ntrips <- 10

# Estimate 
################################################################################



