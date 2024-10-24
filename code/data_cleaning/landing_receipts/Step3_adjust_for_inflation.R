

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
data_orig <- readRDS(file=file.path(outdir, "1980_2022_landings_receipts_set_gillnet_use.Rds"))


# Build data
################################################################################

# Test inflation calculations
inf_test <- data_orig %>% 
  slice(1:10) %>% 
  select(date, value_usd) %>% 
  mutate(value_usd2022=priceR::afi(price=value_usd, 
                                   from_date=date, 
                                   country="US", 
                                   to_date=lubridate::ymd("2022-01-01")))

# Format receipts
spp_do <- c("California halibut", "White sea bass", "Pacific angel shark", "Other species")
data <- data_orig %>% 
  # Adjust for inflation
  mutate(value_usd2022=priceR::afi(price=value_usd, 
                                   from_date=date, 
                                   country="US", 
                                   to_date=lubridate::ymd("2022-01-01"))) %>% 
  # Categorize species
  mutate(spp_catg=ifelse(comm_name %in% spp_do, comm_name, "Other species")) %>% 
  # Select
  select(trip_id, year:gear, spp_catg, everything())

# Summarize
stats <- data %>% 
  # Summarize
  group_by(spp_catg, year) %>% 
  summarize(value_usd2022=sum(value_usd2022, na.rm = T)) %>% 
  ungroup() %>% 
  # Format
  mutate(spp_catg=factor(spp_catg, spp_do))

# Export data
saveRDS(data, file=file.path(outdir, "1980_2022_landings_receipts_set_gillnet_use_afi.Rds"))
saveRDS(stats, file=file.path(outdir, "1980_2022_set_gillnet_revenues_by_spp_catg_afi.Rds"))


# Plot data
################################################################################


#  Plot data
g <- ggplot(stats, aes(x=year, y=value_usd2022/1e6, fill=spp_catg)) +
  # Data
  geom_bar(stat="identity", color="grey30", linewidth=0.1,
           position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Year", y="Revenues (2022 USD millions)") +
  # Legend
  scale_fill_ordinal(name="") +
  guides(fill = guide_legend(reverse=TRUE)) +
  # Theme
  theme_bw()
g

