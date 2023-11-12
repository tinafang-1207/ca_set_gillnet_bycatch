

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/comm_permits/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/comm_permits/processed"
plotdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/comm_permits/figures"

# Read data
list.files(indir)
data_orig <- read.csv(file.path(indir, "VesselPermitExtract.CSV"), as.is=T, na.strings = "")

# Read port key
port_key <- readRDS("data/keys/CDFW_port_key.Rds")


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(reg_doc_id=cf_doc,
         length_ft=length,
         beam_ft=beam,
         horsepower=horse_power,
         home_port_code=home_port,
         home_port_county=port_county,
         permit=vessel_permit,
         permit_issue_office_code=permit_issue_office,
         reg_issue_office_code=reg_issue_office,
         reg_issue_year=vessel_year) %>%
  # Format data
  mutate(permit_issue_date=lubridate::mdy(permit_issue_date),
         reg_issue_date=lubridate::mdy(reg_issue_date),
         effective_date=lubridate::mdy(effective_date)) %>%
  # Format county
  mutate(home_port_county=stringr::str_to_title(home_port_county)) %>%
  # Format permit
  mutate(permit=stringr::str_to_title(permit)) %>%
  # Add home port name
  left_join(port_key %>% select(port_code, port), by=c("home_port_code"="port_code")) %>%
  rename(home_port=port) %>%
  # Arrange
  select(vessel_id:home_port_code, home_port, everything())

# Inspect
str(data)
freeR::complete(data)

# Inspect
range(data$permit_issue_date, na.rm=T)
table(data$permit)
table(data$home_port_code)
table(data$home_port_county)
table(data$permit_issue_office_code)
table(data$reg_issue_office_code)

# Inspect port key
port_key_check <- data %>%
  select(home_port_code, home_port) %>%
  unique() %>%
  arrange(home_port_code)


# Vessel key
################################################################################

# Build vessel key
# Length, beam, tonnage, horsepower, passengers appears constant through time
# Home port changes through time
# Name doesn't appear to be unique
vessel_key <- data %>%
  group_by(vessel_id, length_ft, beam_ft, horsepower, tonnage, passengers) %>%
  summarize(n=n(),
            yr1=min(reg_issue_year),
            yr2=max(reg_issue_year),
            ncfdoc=n_distinct(reg_doc_id),
            cfdocs=paste(sort(unique(reg_doc_id)), collapse=", "),
            n_names=n_distinct(vessel_name),
            names=paste(sort(unique(vessel_name)), collapse=", ")) %>%
  ungroup()

# Confirm that vessel id is unique
anyDuplicated(vessel_key$vessel_id)
freeR::which_duplicated(vessel_key$vessel_id)

# Simplfiy vessel key
vessel_key_out <- vessel_key %>%
  rename(vessel_names=names) %>%
  select(-c(n, yr1, yr2))

# Permit types
permit_key <- data %>%
  select(permit) %>%
  unique() %>%
  arrange(permit)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_2000_2021_permit_data.Rds"))
saveRDS(vessel_key_out, file=file.path(outdir, "CDFW_vessel_key.Rds"))
write.csv(permit_key, file=file.path(outdir, "CDFW_permit_types.csv"), row.names=F)




