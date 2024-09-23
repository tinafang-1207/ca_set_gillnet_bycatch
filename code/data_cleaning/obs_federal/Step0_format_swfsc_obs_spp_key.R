

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
spp_key_orig <- read.csv(file.path(indir, "SWFSC_observer_program_species_codes.csv"), as.is=T, na.strings="---")


# Setup
################################################################################

# Format data
spp_key <- spp_key_orig %>%
  # Rename
  rename(spp_code=code) %>%
  # Format category
  mutate(category=stringr::str_to_sentence(category)) %>%
  # Fix a few names
  mutate(comm_name_orig=recode(comm_name_orig,
                               "Fulmar,Northern"="Fulmar, Northern",
                               "Pipefish Bay"="Pipefish, Bay",
                               "Turbot Hornyhead"="Turbot, Hornyhead",
                               "Halibut, CaliforniaThe main"="Halibut, California",
                               "Rockfish Yellowtail"="Rockfish, Yellowtail",
                               "Sturgeon Unidentified"="Sturgeon, Unidentified",
                               "Hagfish Pacific"="Hagfish, Pacific",
                               "Surfperch Barred"="Surfperch, Barred",
                               "Surfperch Black"="Surfperch, Black",
                               "Surfperch Pile"="Surfperch, Pile",
                               "Stargazer Smooth"="Stargazer, Smooth",
                               "Halibut Pacific"="Halibut, Pacific",
                               "Dolphinfish, Mahi-Mahi, Dorado"="Dolphinfish",
                               "Shrimp, Spot, Spot Prawn"="Shrimp, Spot",
                               "Squid, Eggs"="Squid eggs",
                               "Porpoise Dall's"="Porpoise, Dall's")) %>%
  # Make name regular
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular")) %>%
  # Arrange
  select(category, spp_code, comm_name_orig, comm_name, sci_name, everything())

# Inspect
freeR::complete(spp_key)

# Export
saveRDS(spp_key, file=file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))
