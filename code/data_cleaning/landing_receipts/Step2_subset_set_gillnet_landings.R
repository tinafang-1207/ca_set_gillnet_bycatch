

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1980_2022_landings_receipts.Rds"))

# Read logbooks
logdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
logs_orig <- readRDS(file=file.path(logdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))


# Format data
################################################################################

# Gear codes
gear_key <- data_orig %>% 
  select(gear_id, gear) %>% 
  unique() %>%
  arrange(gear_id)

# What gear are halibut, white seabass, and angel shark caught with?
stats1 <- data_orig %>% 
  filter(comm_name %in% c("California halibut", "White sea bass", "Pacific angel shark")) %>% 
  group_by(gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()



# Format data
data <- data_orig %>% 
  # Reduce to set gillnets (or potential nets)
  filter(gear %in% c("Trammel net", "Set gill net", "Large mesh set gn", "Small mesh set gn", "Entangling nets")) %>% 
  # Filter to vessels in logbooks
  filter(vessel_id %in% logs_orig$vessel_id_use) %>% 
  # mutate(trip_id = paste(vessel_id, date, sep="-")) %>% 
  # filter(trip_id %in% logs_orig$trip_id) %>% 
  # Classify species
  mutate(spp_catg=ifelse(comm_name %in% c("California halibut", "White sea bass", "Pacific angel shark"), comm_name, "Other"))

# Common species?
stats <- data %>% 
  group_by(spp_catg, comm_name) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd))

# Summarize
stats3 <- data %>% 
  group_by(year, spp_catg) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Plot data
g1 <- ggplot(stats3, aes(x=year, y=value_usd/1e6, fill=spp_catg)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Revenues (USD millions)") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme
  theme_bw()
g1


