

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state_karin/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_state_karin/processed"

# Read key
spp_key <- readRDS("data/keys/CDFW_species_key.Rds")
port_key <- readRDS("data/keys/CDFW_port_key.Rds")
block_key <- readRDS("data/strata/block_strata_key.Rds")
spp_key_mammals <- readxl::read_excel(file.path(indir, "mammal_codes.xlsx"))

# Read 1987-1990 counts
data1_orig <- readxl::read_excel(file.path(indir, "data1.xlsx")) # 1987-1990 meta-data
data2_orig <- readxl::read_excel(file.path(indir, "data2.xlsx")) # 1987-1990 counts w/ limited meta-data

# Read 1989-1990 counts with limited meta-data
data3b_orig <- readxl::read_excel(file.path(indir, "data3b.xlsx")) # 1989 Monterey mammal/seabird counts
data3c_orig <- readxl::read_excel(file.path(indir, "data3c.xlsx")) # 1990 Monterey mammal/seabird counts w/ limited meta-data

# Read 1987-1988 Monterey meta-data 
data4_orig <- readxl::read_excel(file.path(indir, "data4.xlsx")) # 1987 meta-data
data7_orig <- readxl::read_excel(file.path(indir, "data7.xlsx")) # 1988 meta-data

# Read 1987-1988 Monterey mammal/seabird counts
data5_orig <- readxl::read_excel(file.path(indir, "data5.xlsx")) # 1987 Monterey mammal counts
data6_orig <- readxl::read_excel(file.path(indir, "data6.xlsx")) # 1987 Monterey seabird counts
data8_orig <- readxl::read_excel(file.path(indir, "data8.xlsx")) # 1988 Monterey mammal counts
data9_orig <- readxl::read_excel(file.path(indir, "data9.xlsx")) # 1988 Monterey seabird counts

# Inspect
str(data1_orig)
str(data2_orig)
str(data3b_orig)
str(data3c_orig)
str(data4_orig)
str(data5_orig)
str(data6_orig)
str(data7_orig)
str(data8_orig)
str(data9_orig)


# Format 1987-1988 meta-data
################################################################################

# What is target species 199?

# Merge 1987-1988 data
sets8788 <- bind_rows(data4_orig, data7_orig) %>% 
  # Remove useless column
  select(-page) %>% 
  # Remove useless row from data7
  filter(net_length!=18650) %>% 
  # Add strata
  left_join(block_key) %>% 
  # Add port code
  left_join(port_key %>% select(port_code, port), by="port_code") %>% 
  relocate(port, .after=port_code) %>%
  # Add target species 
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("target_spp_code"="spp_code_num")) %>% 
  rename(target_spp=comm_name) %>% 
  # Set a few 0s to NAs
  mutate(depth_min=ifelse(depth_min==0, NA, depth_min),
         depth_max=ifelse(depth_max==0, NA, depth_max),
         mesh_size_in=ifelse(mesh_size_in==0, NA, mesh_size_in)) %>%
  # Build set id
  mutate(set_id=paste(date, vessel_id, set_num,sep="-")) %>% 
  # Arrange
  select(set_id, date, vessel_id, set_num, nsets, 
         strata, port_code, port, block_id, 
         target_spp_code, target_spp,
         depth_min, depth_max, 
         mesh_size_in, net_length, net_length_obs, everything()) %>% 
  #  Take the very first set data of repeated values
  group_by(set_id) %>% 
  slice(1) %>% 
  ungroup()

# Inspect
freeR::complete(sets8788)

# Any duplicate set ids
freeR::which_duplicated(sets8788$set_id) # NO B/C you took first unique value


# Merge 1987-1988 counts
################################################################################

# Merge counts
counts8788 <- bind_rows(data5_orig, data6_orig, data8_orig, data9_orig) %>% 
  # Add set number
  mutate(set_id=paste(date, vessel_id, set_num, sep="-")) %>% 
  # Add species 
  left_join(spp_key_mammals, by="spp_code") %>% 
  rename(species=comm_name) %>% 
  # Add target species
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("target_spp_code"="spp_code_num")) %>% 
  rename(target_spp=comm_name) %>% 
  # Arrange
  select(set_id, date, vessel_id, set_num, nsets, 
         target_spp_code, target_spp, 
         spp_code, species, ncaught, comments)

# Inspect
str(counts8788)
freeR::complete(counts8788)

# Do all of the sets in the counts data have set ids?
counts8788sets <- unique(counts8788$set_id)
counts8788sets[!counts8788sets %in% sets8788$set_id] # IF ZERO THEY ALL HAVE META-DATA


# Format 1989-1990 meta-data
################################################################################

data1_orig <- readxl::read_excel(file.path(indir, "data1.xlsx")) # 1987-1990 meta-data
data2_orig <- readxl::read_excel(file.path(indir, "data2.xlsx")) # 1987-1990 counts w/ limited meta-data

# Read 1989-1990 counts with limited meta-data
data3b_orig <- readxl::read_excel(file.path(indir, "data3b.xlsx")) # 1989 Monterey mammal/seabird counts
data3c_orig <- readxl::read_excel(file.path(indir, "data3c.xlsx")) # 1990 Monterey mammal/seabird counts w/ limited meta-data

# Format 1
sets8889a <- data1_orig %>% 
  # Simplify
  select(-column) %>% 
  # Reduce to 88-90 sets
  mutate(year=lubridate::year(date)) %>% 
  filter(year %in% 1988:1989) %>% 
  # Arrange
  select(year, date, set_num, block_id, location, everything())

# Format 2
sets8889b <- data2_orig %>% 
  # Simplify
  select(date:depth_max) %>% 
  unique() %>% 
  # Add set id
  mutate(set_id=paste(date, set_num, sep="-")) %>% 
  # Add missing block id
  mutate(block_id=ifelse(location=="Hurricane Pt" & !is.na(location), 532, block_id)) %>%
  # Add strata
  left_join(block_key) %>% 
  # Take the very first set data of repeated values
  group_by(set_id) %>% 
  slice(1) %>% 
  ungroup()

# Inspect
freeR::complete(sets8889b)
  
# Are set ids unique?
freeR::which_duplicated(sets8889b$set_id)


# Merge 1989-1990 counts
################################################################################

# Build data
counts8990 <- bind_rows(data3b_orig, data3c_orig) %>% 
  # Remove useless
  select(-note) %>% 
  # Add date
  mutate(date=paste(year, month, day, sep="-") %>% lubridate::ymd(.)) %>% 
  select(-c(year, month, day)) %>% 
  # Format locations
  mutate(location=recode(location,
                         "Ft Ord North"="Fort Ord North",         
                         "Ft Ord South"="Fort Ord South",         
                         "Hurricane Pt."="Hurricane Point", 
                         "Soquel Pt."="Soquel Point", 
                         "Terrace Pt."="Terrace Point")) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Add set id
  mutate(set_id=paste(date, set_num, sep="-")) %>% 
  # Arrange
  select(set_id, year, date, location, everything()) %>% 
  arrange(year, date, location, set_num) %>% 
  # Gather
  gather(key="species", value="ncaught", 7:ncol(.)) %>% 
  mutate(species=recode(species, 
                        "cormorant"="Unidentified cormorant",     
                        "elephant_seal"="Northern elephant seal", 
                        "guillemot"="Pigeon guillemot",    
                        "harbor_seal"="Harbor seal",
                        "murre"="Common murre",
                        "otter"="Sea otter",      
                        "porpoise"="Harbor porpoise",
                        "sea_lion"="California sea lion",    
                        "seabird"="Unidentified seabird"))

# Inspect
str(counts8990)
freeR::complete(counts8990)
table(counts8990$location)
table(counts8990$year)

# Confirm if all sets have metadata
counts8990sets <- sort(unique(counts8990$set_id))
counts8990sets[!counts8990sets %in% sets8889b$set_id] 


################################################################################
################################################################################
# Merge 1987-90 data
################################################################################
################################################################################

# Merge counts
counts <- bind_rows(counts8788, counts8990) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Reduce to count specific data
  select(year, date, vessel_id, set_num, set_id, species, ncaught, comments) %>% 
  # Arrange
  arrange(date, vessel_id, set_num) %>% 
  # Eliminate zero catch
  filter(!is.na(ncaught))

# Inspect
freeR::complete(counts)

# Merge sets
sets <- bind_rows(sets8788, sets8889b) %>% 
  # Remove useless
  select(-nsets) %>% 
  # Rename
  rename(net_length_fa=net_length,
         net_length_obs_fa=net_length_obs,
         depth_fa_min=depth_min,
         depth_fa_max=depth_max) %>% 
  # Arrange
  select(set_id, date, vessel_id, set_num, 
         strata, port_code, port, block_id, location,
         depth_fa_min, depth_fa_max, 
         target_spp_code, target_spp, 
         mesh_size_in, net_length_fa, net_length_obs_fa,
         everything())

# Inspect
freeR::complete(sets)



# Export data
################################################################################

# Export
saveRDS(counts, file=file.path(outdir, "1987_1990_observer_data_from_karin.Rds"))
saveRDS(sets, file=file.path(outdir, "1987_1990_observer_metadata_from_karin.Rds"))
write.csv(counts, file=file.path(outdir, "1987_1990_observer_data_from_karin.csv"), row.names = F)
write.csv(sets, file=file.path(outdir, "1987_1990_observer_metadata_from_karin.csv"), row.names = F)


