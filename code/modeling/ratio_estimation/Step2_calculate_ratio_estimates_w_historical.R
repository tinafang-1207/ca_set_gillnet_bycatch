
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
obs_orig <- readRDS(file=file.path(obsdir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")) %>% 
  # Remove Karin data b/c don't know if it is complete
  filter(dataset!="State (Karin)")

# Read block key
block_key <- readRDS("data/strata/block_strata_key.Rds")

# Read historical rates
hist_rates_orig <- readxl::read_excel("data/historical_estimates/processed/historical_regional_bycatch_rates.xlsx", sheet=3)


# BUILD
# Type: Raw data, Report summary
# Species:
# Strata
# Year:
# Number sets
# Number trips
# Number bycatch
# Bycatch / trip


# Build each one
# Then combine
# Then expand
# Then apply

# Species
species <- c("California sea lion", "Northern elephant seal", "Harbor seal", "Harbor porpoise",
             "Common murre", "Brandt's cormorant")

# Stratas
stratas <- c("San Francisco", "Monterey Bay", "Morro Bay", 
             "Ventura", "Channel Islands", "Southern California")

# Calculate number of trips by year and strata
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
  # THIS IS TEMPORARY
  filter(!is.na(strata))


# Build rates from historical reports
################################################################################

# Build rates
rates_sum <- hist_rates_orig %>% 
  # Simplify
  select(-notes) %>% 
  # Gather
  gather(key="species", value="ncaught", 5:ncol(.)) %>% 
  # Format species
  mutate(species=recode(species, 
                        "sea_lion"="California sea lion",
                        "harbor_seal"="Harbor seal",
                        "harbor_porpoise"="Harbor porpoise")) %>% 
  # Add columns
  mutate(type="Report summary",
         ntrips_obs=nsets_obs/3, 
         catch_per_trip=ncaught / ntrips_obs) %>% 
  # Eliminate no data
  filter(!is.na(catch_per_trip)) %>% 
  # Arrange
  select(type, reference, species, strata, year, nsets_obs, ntrips_obs, ncaught, catch_per_trip, everything()) %>% 
  arrange(species, strata, year) %>% 
  # Add strata id
  mutate(strata_id=paste(year, strata, species, sep="-"))

# Check
# Must all be ones
check1 <- rates_sum %>% 
  count(species, strata, year)



# Build rates from raw data
################################################################################

# Add strata to observer data
obs <- obs_orig %>% 
  # Add strata
  left_join(block_key) %>% 
  # Fill in missing strata
  mutate(strata=case_when(block_id %in% c(901, 904, 1032) ~ "Southern California",
                          T ~ strata))

# Inspect
freeR::complete(obs)
table(obs$strata)


# Number of trips by year and strata
trips <- obs %>% 
  # Number of trips by year and strata
  group_by(year, strata) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup()

# Number of bycatch by year and strata
bycatch <- obs %>% 
  # Reduce to species of interest
  filter(!is.na(comm_name) & comm_name %in% species) %>% 
  # Number by year and strata
  group_by(year, strata, comm_name) %>% 
  summarize(nbycatch=sum(n_caught, na.rm=T)) %>% 
  ungroup() 

# Merge
rates_raw <- purrr::map_df(species, function(x){
  df <- trips %>% 
    mutate(species=x)
}) %>% 
  # Rename
  rename(ntrips_obs=ntrips) %>% 
  # Add columns
  mutate(type="Report summary",
         reference="Observer data") %>% 
  # Add bycatch
  left_join(bycatch, by=c("year", "strata", "species"="comm_name")) %>% 
  mutate(nbycatch=ifelse(is.na(nbycatch), 0, nbycatch)) %>% 
  rename(ncaught=nbycatch) %>% 
  # Calculate rate
  mutate(catch_per_trip=ncaught / ntrips_obs) %>% 
  # Arrange
  select(type, reference, species, strata, year, ntrips_obs, everything()) %>% 
  arrange(species, strata, year) %>% 
  # Add strata id
  mutate(strata_id=paste(year, strata, species, sep="-"))


# Merge rates
################################################################################

# Isolate rates from summaries that are not redudant with raw data
rates_sum_use <- rates_sum %>% 
  filter(!strata_id %in% rates_raw$strata_id) %>% 
  select(-nsets_obs)

# Merge
rates <- bind_rows(rates_sum_use, rates_raw) %>% 
  # Arrange
  arrange(species, strata, year) %>% 
  select(-strata_id)



# Expand rates
################################################################################

# Build template
years <- 1981:2022
temp <- expand.grid(species=species,
                    strata=stratas,
                    year=years) %>% 
  # Arrange
  arrange(species, strata, year)

# Number of trips thresh
thresh_ntrips <- 10

# Merge data
data <- temp %>% 
  # Add observed rates
  left_join(rates, by=c("year", "strata", "species")) %>% 
  # Add rate type
  mutate(rate_type=ifelse(!is.na(catch_per_trip) & ntrips_obs>=thresh_ntrips, "Observed", "Borrowed")) %>% 
  # Add rate use
  mutate(rate_use=ifelse(rate_type=="Observed", catch_per_trip, NA)) %>% 
  # Add assumed zeros
  mutate(rate_use=case_when(species=="Harbor porpoise" & strata %in% c("Ventura", "Channel Islands", "Southern California") ~ 0,
                            species=="Common murre" & strata %in% c("Channel Islands", "Southern California") ~ 0,
                            T ~ rate_use)) %>% 
  # Order
  mutate(strata=factor(strata, levels=stratas))

# Loop through data and fill
i <- 1
for(i in 1:nrow(data)){

  # Extract info
  species_do <- data$species[i]
  strata_do <- data$strata[i]
  year_do <- data$year[i]
  
  # IDentify years with data
  sdata <- data %>% 
    filter(species==species_do & strata==strata_do & !is.na(rate_use))
  years <- sdata$year
  
  # Identify which year is closest
  year_closest <- years[which.min(abs(years-year_do))]

  # Identify closest rate
  rate_closest <- sdata$rate_use[sdata$year==year_closest]
  rate_closest_source <- sdata$type[sdata$year==year_closest]
  if(length(rate_closest)==0){
    rate_closest <- NA
    rate_closest_source <- NA
  }
  
  # Record
  data$rate_imp[i] <- rate_closest
  data$rate_source[i] <- rate_closest_source
  
}

# Export data
saveRDS(data, file=file.path(outputdir, "1981_2022_bycatch_rates_w_historical.Rds"))



# Plot bycatch rate
################################################################################

# Plot data
ggplot(data, aes(x=year, y=rate_imp, color=strata)) +
  facet_wrap(~species, ncol=3, scales="free_y") +
  geom_point(mapping=aes(shape=rate_type)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Bycatch per trip") +
  # Legend
  scale_color_discrete(name="Strata") +
  scale_shape_manual(name="Data type", values=c(1, 16)) +
  # Theme
  theme_bw()


# Compute bycatch
################################################################################

# Compute bycatch
data1 <- data %>% 
  # Add number of logged trips
  left_join(effort) %>% 
  # Compute bycatch
  mutate(nbycatch=ntrips * rate_imp) %>% 
  # Remove 2022
  filter(year!=2022) %>% 
  # Rename
  rename(comm_name=species)


# Plot
ggplot(data1, aes(x=year, y=nbycatch, fill=strata)) +
  facet_wrap(~comm_name, ncol=3, scales="free_y") +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Estimated bycatch") +
  # Legend
  scale_fill_discrete(name="Strata") +
  # Theme
  theme_bw()


# Export bycatch
################################################################################

saveRDS(data1, file=file.path(outputdir, "1981_2021_bycatch_estimate_ratio_stratified_w_historical.Rds"))

