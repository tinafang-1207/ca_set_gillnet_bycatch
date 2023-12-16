

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
data_orig1 <- read.csv(file.path(indir, "obs_setnet_catch.csv"), as.is=T, na.strings="")

# Read species key
spp_key <- readRDS(file.path(outdir, "SWFSC_observer_program_spp_key.Rds"))


# Format counts (data 1)
################################################################################

# Categories recorded differently (protect spp)
protected_catg <- c("Marine mammals", "Sea turtles", "Seabirds")

# Format data 1
data1 <- data_orig1 %>%
  # Rename
  janitor::clean_names() %>%
  rename(trip_id=observer_trip_number,
         set_num=set_number,
         spp_code=catch_species_code,
         comm_name_orig=species_common_name,
         n_caught=total_catch_count,
         n_kept=total_kept_count,
         n_returned_alive=returned_alive_count,
         n_returned_dead=returned_dead_count,
         n_returned_unknown=returned_unknown_count,
         n_damaged_mammals=damage_by_marine_mammals_count,
         n_damaged=damage_total_count,
         tag_yn=was_tag_present,
         mammal_damage_yn=was_damaged_by_marine_mammals,
         condition=condition_description) %>%
  # Format species
  mutate(spp_code=ifelse(is.na(spp_code), "0", spp_code),
         spp_code=recode(spp_code,
                         "000"="0",
                         "001"="1",
                         "003"="3",
                         "019"="19",
                         "040"="40",
                         "050"="50",
                         "051"="51",
                         "055"="55",
                         "079"="79",
                         "080"="80",
                         "096"="96")) %>%
  # Add species info
  left_join(spp_key %>% select(spp_code, comm_name, sci_name, category), by="spp_code") %>%
  # Fill in missing species
  mutate(comm_name_orig=ifelse(spp_code=="152", "Shark, Spiny Dogfish", comm_name_orig),
         comm_name=ifelse(spp_code=="152", "Spiny dogfish shark", comm_name),
         sci_name=ifelse(spp_code=="152", "Squalus suckleyi", sci_name),
         category=ifelse(spp_code=="152", "Fish", category)) %>%
  # Format sex
  mutate(sex=ifelse(sex=="", "Unknown", sex),
         sex=recode(sex,
                    "U"="Unknown",
                    "M"="Male",
                    "F"="Female")) %>%
  # Format condition
  mutate(condition=ifelse(category %in% protected_catg & (condition=="" | is.na(condition)), "Unknown", condition)) %>%
  # Format tag
  mutate(tag_yn=recode(tag_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Format marine mammal damage?
  mutate(mammal_damage_yn=recode(mammal_damage_yn,
                       "Y"="yes",
                       "N"="no")) %>%
  # Record marine mammals in n caught column
  mutate(n_caught=ifelse(category %in% protected_catg & is.na(n_caught), 1, n_caught),
         n_returned_dead=ifelse(!is.na(condition) & condition=="Dead" & is.na(n_returned_dead), 1, n_returned_dead),
         n_returned_alive=ifelse(!is.na(condition) & condition=="Alive" & is.na(n_returned_alive), 1, n_returned_alive)) %>% 
  # Check totals
  # Total = Kept + returned alive + returned dead + returned unknown (damaged is redundant)
  mutate(n_caught_calc=n_kept+n_returned_alive+n_returned_dead+n_returned_unknown,
         n_caught_diff=n_caught-n_caught_calc) %>%
  # Fill in missing values given that they passed
  mutate(n_kept=ifelse(!is.na(n_caught) & is.na(n_kept), 0, n_kept),
         n_returned_alive=ifelse(!is.na(n_caught) & is.na(n_returned_alive), 0, n_returned_alive),
         n_returned_dead=ifelse(!is.na(n_caught) & is.na(n_returned_dead), 0, n_returned_dead),
         n_returned_unknown=ifelse(!is.na(n_caught) & is.na(n_returned_unknown), 0, n_returned_unknown)) %>% 
  # Remove b/c pass check
  # select(-c(n_caught_calc, n_caught_diff)) %>%
  # Build set id
  mutate(set_id=paste(trip_id, set_num, sep="-")) %>%
  # Arrange
  select(trip_id, set_num, set_id,
         category, spp_code, comm_name_orig, comm_name, sci_name,
         everything()) %>%
  arrange(trip_id, set_num, set_id, spp_code) 

# Inspect
str(data1)
freeR::complete(data1)

# Inspect
sort(unique(data1$tag_yn))
sort(unique(data1$mammal_damage_yn))
sort(unique(data1$condition_code))
sort(unique(data1$condition))
sort(unique(data1$sex))

# Species key
spp_key_check <- data1 %>%
  select(spp_code, comm_name) %>%
  unique() %>%
  arrange(spp_code)
freeR::which_duplicated(spp_key$spp_code)
freeR::which_duplicated(spp_key$comm_name)

# Export data
saveRDS(data1, file=file.path(outdir, "SWFSC_set_net_observer_data.Rds"))


# Summarize totals by set and spcies
################################################################################

# Build data
data1_sum <- data1 %>% 
  # Simplify
  select(trip_id:spp_code, comm_name, sci_name, n_caught:n_returned_unknown) %>% 
  # Summarize results across set and species
  group_by(trip_id, set_num, set_id, category, spp_code, comm_name, sci_name) %>%
  summarize(across(where(is.numeric), sum), .groups = 'drop')

# Inspect
freeR::complete(data1_sum)

# Confirm no duplicates
check2 <- data1_sum %>% 
  count(set_id, comm_name)
sum(check2$n>1)

# Export
saveRDS(data1_sum, file=file.path(outdir, "SWFSC_set_net_observer_data_summed.Rds"))


