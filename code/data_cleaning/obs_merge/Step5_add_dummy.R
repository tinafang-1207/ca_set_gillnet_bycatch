
# clean working environment
rm(list = ls())

# read in data
data_orig <- readRDS("/users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge/1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")

# categorize based on block_id

# mainland - all those not in Channel Island strata
# island - those in Channel Island strata

