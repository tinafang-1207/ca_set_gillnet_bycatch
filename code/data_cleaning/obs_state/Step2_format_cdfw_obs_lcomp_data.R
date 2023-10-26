

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/cdfw_obs/raw"
outdir <- "data/cdfw_obs/processed"
plotdir <- "data/cdfw_obs/figures"

# Read data
data_orig <- read.csv(file.path(indir, "GNM8389.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS("data/cdfw_keys/processed/CDFW_species_key.Rds")
port_key <- readRDS("data/cdfw_keys/processed/CDFW_port_key.Rds")


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(vessel_id=fgno,
         set_num=setno,
         spp_code_chr=species,
         fork_length_mm=length,
         total_length_mm=altlen,
         disposition=disp,
         maturity=mat) %>%
  # Format date
  mutate(date=recode(date, "87/01263"=""),
         date=lubridate::ymd(date)) %>%
  # Format sex
  mutate(sex=recode(sex,
                    "0"="unknown",
                    "1"="male",
                    "2"="female")) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name), by="spp_code_chr") %>%
  # Fill in some missing species names
  mutate(spp_code_chr=recode(spp_code_chr, "ZC0"="ZC")) %>%
  mutate(comm_name=case_when(spp_code_chr=="PV" ~ "Harbor seal", # Phoca vitulina
                             spp_code_chr=="ZC" ~ "California sea lion", # Zalophus californianus
                             spp_code_chr=="MA" ~ "Northern elephant seal", # Mirounga angustirostris
                             TRUE ~ comm_name)) %>%
  # Add set id
  mutate(set_id=paste(date, vessel_id, set_num, sep="-")) %>%
  # Arrange
  select(date, vessel_id, set_num, set_id, spp_code_chr, comm_name, everything()) %>%
  arrange(date, vessel_id, set_num, comm_name)


# Inspect data
################################################################################

# Inspect
str(data)
freeR::complete(data)

# Inspect
sort(unique(data$sex))
sort(unique(data$maturity))
sort(unique(data$disposition))

# Inspect unmatched species
data_spp <- data %>%
  # Unique species
  select(spp_code_chr, comm_name) %>%
  unique() %>%
  # Reduce to unmatched species
  filter(is.na(comm_name)) %>%
  # Arrange
  arrange(spp_code_chr)
data_spp$spp_code_chr


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_length_comps.Rds"))

