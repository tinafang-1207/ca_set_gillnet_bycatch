

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
data_orig <- read.csv(file.path(indir, "GNC8389.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS("data/cdfw_keys/processed/CDFW_species_key.Rds")
spp_key2 <- readRDS("data/swfsc_obs/processed/SWFSC_observer_program_spp_key.Rds")
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
         n_caught=tcat,
         n_discarded_dead=ddead,
         n_discarded_alive=dlive,
         n_damaged=nodam,
         n_kept=nokept,
         n_kept_sublegal=nokeptsl,
         n_sold=nosold) %>%
  # Format date
  mutate(date=lubridate::ymd(date)) %>%
  # Format number discarded alive
  mutate(n_discarded_alive=ifelse(n_discarded_alive %in% c("m", "M"), NA, n_discarded_alive),
         n_discarded_alive=as.numeric(n_discarded_alive)) %>%
  # Format number damaged
  mutate(n_damaged=ifelse(n_damaged %in% c("S", "M"), NA, n_damaged),
         n_damaged=as.numeric(n_damaged)) %>%
  # Format number kept
  mutate(n_kept=ifelse(n_kept %in% c("S"), NA, n_kept),
         n_kept=as.numeric(n_kept)) %>%
  # Add species name (fix a few codes first)
  mutate(spp_code_chr=recode(spp_code_chr,
                             "PPn"="pPN",
                             "aV"="aVE")) %>%
  left_join(spp_key2 %>% select(code, comm_name), by=c("spp_code_chr"="code")) %>%
  # Fill in missing common name
  mutate(comm_name=case_when(spp_code_chr=="152" ~ "Spiny dogfish shark",
                             TRUE ~ comm_name)) %>%
  # Add set id
  mutate(set_id=paste(date, vessel_id, set_num, sep="-")) %>%
  # Check data
  mutate(n_caught_calc=n_discarded_dead+n_discarded_alive+n_kept+n_kept_sublegal+n_sold, # n_damaged is redundant
         n_check=n_caught-n_caught_calc) %>%
  select(-c(n_caught_calc, n_check)) %>%
  # Arrange
  select(date, vessel_id, set_num, set_id, spp_code_chr, comm_name, everything()) %>%
  arrange(date, vessel_id, set_num, set_id, comm_name)


# Inspect data
################################################################################

# Inspect
str(data)
freeR::complete(data)

# Data
range(data$date)

# File links
sort(unique(data$m_file_link))
sort(unique(data$s_file_link))

# Inspect species species
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
saveRDS(data, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))

