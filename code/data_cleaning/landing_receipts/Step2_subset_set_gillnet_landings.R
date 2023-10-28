

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


# Format data
################################################################################

# Gear codes
gear_key <- data_orig %>% 
  select(gear_id, gear) %>% 
  unique() %>%
  arrange(gear_id)

# Format data
data <- data_orig %>% 
  # Reduce to set gillnets
  filter(gear %in% c("Trammel net", "Set gill net", "Large mesh set gn", "Small mesh set gn")) %>% 
  # Classify species
  mutate(spp_catg=ifelse(comm_name %in% c("California halibut", "White sea bass", "Pacific angel shark"), comm_name, "Other"))

# Summarize
stats <-data_orig %>% 
  filter(comm_name %in% c("California halibut", "White sea bass", "Pacific angel shark")) %>% 
  group_by(gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize
stats <- data %>% 
  group_by(year, spp_catg) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()


table(data$gear)

# Plot data
g1 <- ggplot(stats, aes(x=year, y=value_usd, fill=spp_catg)) +
  geom_bar(stat="identity") +
  # Theme
  theme_bw()
g1


