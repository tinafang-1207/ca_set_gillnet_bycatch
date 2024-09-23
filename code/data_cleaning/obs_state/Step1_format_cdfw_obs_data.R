

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/processed"
plotdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state/figures"
keydir <- "data/keys"

# Read data
data_orig <- read.csv(file.path(indir, "GNC8389.csv"), as.is=T, na.strings="")

# Read keys
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
spp_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))
spp_key2 <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_federal/processed/SWFSC_observer_program_spp_key.Rds")

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
  mutate(n_discarded_alive=ifelse(n_discarded_alive %in% c("m", "M"), 0, n_discarded_alive),
         n_discarded_alive=as.numeric(n_discarded_alive)) %>%
  # Format number damaged
  mutate(n_damaged=ifelse(n_damaged %in% c("S", "M"), 0, n_damaged),
         n_damaged=as.numeric(n_damaged)) %>%
  # Format number kept
  mutate(n_kept=ifelse(n_kept %in% c("S") | is.na(n_kept), 0, n_kept),
         n_kept=as.numeric(n_kept)) %>%
  # Format number kept sublegal
  mutate(n_kept_sublegal=ifelse(is.na(n_kept_sublegal), 0, n_kept_sublegal)) %>%
  # Format number sold
  mutate(n_sold=ifelse(is.na(n_sold), 0, n_sold)) %>%
  # Add species name (fix a few codes first)
  mutate(spp_code_chr=recode(spp_code_chr,
                             "PPn"="pPN",
                             "aV"="aVE")) %>%
  left_join(spp_key2 %>% select(spp_code, comm_name), by=c("spp_code_chr"="spp_code")) %>%
  # Fill in missing common name
  mutate(comm_name=case_when(spp_code_chr=="152" ~ "Spiny dogfish shark",
                             TRUE ~ comm_name)) %>%
  # Add set id
  mutate(set_id=paste(date, vessel_id, set_num, sep="-")) %>%
  # Check data
  mutate(n_caught_calc=n_discarded_dead+n_discarded_alive+n_kept+n_kept_sublegal+n_sold, # n_damaged is redundant
         n_check=n_caught-n_caught_calc) %>%
  # select(-c(n_caught_calc, n_check)) %>%
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

# Inspect species 
data_spp <- data %>%
  # Unique species
  select(spp_code_chr, comm_name) %>%
  unique() %>%
  # Arrange
  arrange(spp_code_chr)
data_spp$spp_code_chr

# See if any species are duplicated within a set
check1 <- data %>% 
  count(set_id, comm_name) %>% 
  filter(n>1)


# Finalize data
################################################################################

# Finalize
data_out <- data %>% 
  # Simplify
  select(-c(m_file_link, s_file_link, n_caught_calc, n_check)) %>% 
  # Summarize results across set and species
  group_by(date, vessel_id, set_num, set_id, spp_code_chr, comm_name) %>% 
  summarize(across(where(is.numeric), sum), .groups = 'drop')

# Inspect
freeR::complete(data_out)

# Export data
################################################################################

# Export
saveRDS(data_out, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_data.Rds"))

