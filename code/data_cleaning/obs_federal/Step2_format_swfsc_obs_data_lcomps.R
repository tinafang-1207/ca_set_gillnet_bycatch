

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed"
plotdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/figures"

# Read data
list.files(indir)
data_orig2 <- read.csv(file.path(indir, "obs_setnet_measurement.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS(file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))


# Format length compositions (data 2)
################################################################################

# Format data 2
data2 <- data_orig2 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_num=set_number,
         spp_code=catch_species_code,
         comm_name=species_common_name,
         length_cm=measurement) %>%
  # Add set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Remove useless columns
  select(-c(measurement_units, condition)) %>%
  # Arrange
  select(trip_id, set_num, set_id, spp_code, everything()) %>%
  arrange(trip_id, set_num, set_id, spp_code)

# Inspect
str(data2)
freeR::complete(data2)

# Inspect columns
# sort(unique(data2$condition)) ## ALWAYS EMPTY
# sort(unique(data2$measurement_units)) ## EMPTY on "cm"
sort(unique(data2$disposition))

# Inspect species key
spp_key_check2 <- data2 %>%
  select(spp_code, comm_name) %>%
  unique()

# Plot lengths
# g <- ggplot(data2, aes(x=length_cm)) +
#   facet_wrap(~comm_name, ncol=8, scales = "free") +
#   geom_density() +
#   # Labels
#   labs(x="Length (cm)", y="Density") +
#   # Theme
#   theme_bw()
# g

# Export data
saveRDS(data2, file=file.path(outdir, "SWFSC_set_net_observer_length_comps.Rds"))


