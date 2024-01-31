
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

# Species
species <- c("California sea lion", "Northern elephant seal", "Harbor seal", "Harbor porpoise",
             "Common murre", "Brandt's cormorant")

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

# Stratas
stratas <- c("Monterey Bay", "Morro Bay", 
             "Ventura", "Channel Islands", "Southern California")

# Number of trips by year and strata
trips <- obs %>% 
  group_by(year, strata) %>% 
  summarize(ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Factor strata
  mutate(strata=factor(strata, levels=stratas))

# Number of bycatch by year and strata
bycatch <- obs %>% 
  filter(comm_name %in% species) %>% 
  group_by(year, strata, comm_name) %>% 
  summarize(nbycatch=sum(n_caught, na.rm=T)) %>% 
  ungroup() %>% 
  # Factor strata
  mutate(strata=factor(strata, levels=stratas))

# Plot data
ggplot(bycatch, aes(x=year, y=strata, fill=nbycatch)) +
  facet_wrap(~comm_name) +
  geom_raster()

# Build template
years <- 1981:2022
temp <- expand.grid(comm_name=species,
                    strata=stratas,
                    year=years) %>% 
  # Arrange
  arrange(comm_name, strata, year)

# Number of trips thresh
thresh_ntrips <- 10

# Merge data
data <- temp %>% 
  # Add number of trips
  left_join(trips, by=c("strata", "year")) %>% 
  # Add number of bycatch
  left_join(bycatch, by=c("year", "strata", "comm_name")) %>% 
  # Compute rate
  mutate(rate_obs=nbycatch/ntrips) %>% 
  # Factor strata
  mutate(strata=factor(strata, levels=stratas)) %>% 
  # Add rate type
  mutate(rate_type=ifelse(!is.na(rate_obs) & ntrips>=thresh_ntrips, "Observed", "Borrowed")) %>% 
  # Add rate use
  mutate(rate_use=ifelse(rate_type=="Observed", rate_obs, NA)) %>% 
  # Add assumed zeros
  mutate(rate_use=case_when(comm_name=="Harbor porpoise" & strata %in% c("Ventura", "Channel Islands", "Southern California") ~ 0,
                            comm_name=="Common murre" & strata %in% c("Channel Islands", "Southern California") ~ 0,
                            T ~ rate_use))

# Loop through data and fill
for(i in 1:nrow(data)){

  # Extract info
  species_do <- data$comm_name[i]
  strata_do <- data$strata[i]
  year_do <- data$year[i]
  
  # IDentify years with data
  sdata <- data %>% 
    filter(comm_name==species_do & strata==strata_do & !is.na(rate_use))
  years <- sdata$year
  
  # Identify which year is closest
  year_closest <- years[which.min(abs(years-year_do))]

  # Identify closest rate
  rate_closest <- sdata$rate_use[sdata$year==year_closest]
  
  # Record
  data$rate_imp[i] <- rate_closest
  
}

# Export data
saveRDS(data, file=file.path(outputdir, "1981_2022_bycatch_rates.Rds"))



# Plot bycatch rate
################################################################################

# Plot data
ggplot(data, aes(x=year, y=rate_imp, color=strata)) +
  facet_wrap(~comm_name, ncol=3, scales="free_y") +
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
  rename(ntrips_obs=ntrips,
         nbycatch_obs=nbycatch) %>% 
  # Add number of logged trips
  left_join(effort) %>% 
  # Compute bycatch
  mutate(nbycatch=ntrips * rate_imp) %>% 
  # Remove 2022
  filter(year!=2022)


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

saveRDS(data1, file=file.path(outputdir, "1981_2021_bycatch_estimate_ratio_stratified.Rds"))

