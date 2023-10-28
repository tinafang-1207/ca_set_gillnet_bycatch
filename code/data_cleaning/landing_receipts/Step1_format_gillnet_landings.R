

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed"

# Read species key
keydir <- "data/keys"
spp_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))


# Merge data
################################################################################

# Files 2 merge
files2merge <- list.files(indir)

# Merge data
data_orig <- purrr::map_df(files2merge, function(x){
  
  # Read data
  fdata <- read.csv(file.path(indir, x), na.strings = "") %>% 
    mutate_all(.funs = as.character)
  
})


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(receipt_id=landing_receipt_num,
         date=landing_date,      
         permit_state=state_permit_number,
         permit_gf=gf_permit_num,  
         port=port_name, 
         block_id=cdfw_block_id,   
         primary_gear=primary_gear_name,
         comm_name_orig=species_name, 
         spp_code=species_id, 
         landings_lbs=pounds,           
         price_usd=unit_price,         
         value_usd=total_price,        
         gear=gear_name,        
         condition_id=fish_condition_id,
         condition=fish_condition_name,
         use=use_name) %>% 
  # Convert to numeric
  mutate(across(.cols=c(landings_lbs, value_usd, price_usd), .fns=as.numeric)) %>% 
  mutate(across(.cols=c(vessel_id, port_id, block_id, business_id, spp_code, 
                        primary_gear_id, gear_id, condition_id, use_id), .fns=as.numeric)) %>% 
  # Convert date
  mutate(date=lubridate::mdy(date)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Format port
  mutate(port=stringr::str_to_title(port)) %>% 
  mutate(port_id=ifelse(port_id==-1, 0, port_id)) %>% 
  mutate(port=case_when(port_id==0 ~ "Invalid Or Unknown Port",
                        port_id==452 ~ "Princeton-Half Moon Bay",
                        T ~ port)) %>% 
  # Format species
  mutate(comm_name_orig=stringr::str_squish(comm_name_orig)) %>% 
  # Add species
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("spp_code"="spp_code_num")) %>% 
  # Format primary gear
  mutate(primary_gear=stringr::str_to_sentence(primary_gear) %>% stringr::str_squish(.),
         primary_gear_id=ifelse(is.na(primary_gear_id), 0, primary_gear_id),
         primary_gear=ifelse(primary_gear_id==0, "Unknown", primary_gear)) %>% 
  # Format gear
  mutate(gear=stringr::str_to_sentence(gear) %>% stringr::str_squish(.),
         gear_id=ifelse(is.na(gear_id) | gear_id==-1, 0, gear_id),
         gear=ifelse(gear_id==0, "Unknown", gear)) %>% 
  # Format use
  mutate(use=stringr::str_to_sentence(use),
         use_id=ifelse(use_id==0 | is.na(use_id), -1, use_id),
         use=ifelse(use_id==-1, "Invalid", use)) %>% 
  # Format condition
  mutate(condition=stringr::str_to_sentence(condition),
         condition=ifelse(condition_id==0, "Dead", condition)) %>% 
  # Arrange
  select(year, date, receipt_id, 
         vessel_id, fisher_id, permit_state, permit_gf,
         port_id, port, business_id, block_id,
         primary_gear_id, primary_gear, gear_id, gear, 
         spp_code, comm_name_orig, comm_name, 
         condition_id, condition, 
         use_id, use,
         landings_lbs, price_usd, value_usd,
         everything()) %>% 
  arrange(date, receipt_id)

# Inspect
str(data1)
freeR::complete(data1)


# Inspect keys
################################################################################

# Port key
port_key <- data1 %>% 
  count(port_id, port) %>% 
  arrange(port_id)
freeR::which_duplicated(port_key$port_id)

# Species key
spp_key_check <- data1 %>% 
  count(spp_code, comm_name_orig, comm_name) %>% 
  arrange(spp_code)

# Use key
use_key <- data1 %>% 
  count(use_id, use) %>% 
  arrange(use_id)
freeR::which_duplicated(use_key$use_id)

# Condition key
condition_key <- data1 %>% 
  count(condition_id, condition) %>% 
  arrange(condition_id)
freeR::which_duplicated(condition_key$condition_id)

# Primary gear key (75 is unknown)
primary_gear_key <- data1 %>% 
  count(primary_gear_id, primary_gear) %>% 
  arrange(primary_gear_id)
freeR::which_duplicated(primary_gear_key$primary_gear_id)

# Gear key
gear_key <- data1 %>% 
  count(gear_id, gear) %>% 
  arrange(gear_id)
freeR::which_duplicated(gear_key$gear_id)


# Export data
################################################################################

# Export data
saveRDS(data1, file=file.path(outdir, "1980_2022_landings_receipts.Rds"))

