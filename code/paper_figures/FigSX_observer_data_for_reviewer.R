

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris
tabledir <- "tables"

# Read obersever data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

# Read effort data
effort_df <- read.csv(file=file.path(datadir2, "CA_3.5in_set_gillnet_effort_by_year.csv")) %>% 
  rename(ntrips_tot=nvesseldays)

# Read species key
spp_key <- read.csv("data/keys/species_key_final.csv", as.is=T)

# Read block key
block_key <- readRDS("data/strata/block_strata_key.Rds")

# Read historical observer sample sizes
hist_obs_n <- readxl::read_excel("data/historical_estimates/processed/historical_regional_bycatch_rates.xlsx", sheet=2) 



# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Add trip id
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  # Add quarter
  mutate(month=lubridate::month(date),
         quarter=case_when(month %in% c(1,2,3) ~ "1",
                           month %in%c(4,5,6) ~ "2",
                           month %in% c(7,8,9) ~ "3",
                           month %in% c(10,11,12) ~ "4",
                           T ~ "Unknown")) %>% 
  # Add strata
  left_join(block_key, by="block_id") %>% 
  # Order strata
  mutate(strata=ifelse(is.na(strata), "Unassigned", strata))

freeR::complete(data)
table(data$strata)



# Stats by strata
stats_strata <- data %>% 
  # Eliminate unassigned strata
  filter(strata!="Unassigned") %>% 
  # Factor strata
  mutate(strata=gsub(" ", "\n", strata),
         strata=factor(strata, levels=c("San\nFrancisco",
                                        "Monterey\nBay",
                                        "Morro\nBay", 
                                        "Ventura",              
                                        "Channel\nIslands",     
                                        "Southern\nCalifornia"))) %>% 
  # Summarize by year and strata
  group_by(dataset, year, strata) %>% 
  summarize(nsets=n_distinct(set_id),
            ntrips=n_distinct(trip_id)) %>% 
  ungroup()


ggplot(stats_strata, aes(y=strata, x=year, fill=ntrips)) +
  geom_tile() +
  # X-axis
  scale_x_continuous(breaks=seq(1980,2020, 5)) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradientn(name="# of trips", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw()
  
