

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

# Read 1987-1990 counts
data1_orig <- readxl::read_excel(file.path(indir, "data1.xlsx")) # 1987-1990 meta-data
data2_orig <- readxl::read_excel(file.path(indir, "data2.xlsx")) # 1987-1990 counts w/ limited meta-data

# Read 1989-1990 counts with limited meta-data
data3b_orig <- readxl::read_excel(file.path(indir, "data3b.xlsx")) # 1989 counts w/ limited meta-data
data3c_orig <- readxl::read_excel(file.path(indir, "data3c.xlsx")) # 1990 counts w/ limited meta-data

# Read 1987-1988 meta-data 
data4_orig <- readxl::read_excel(file.path(indir, "data4.xlsx")) # 1987 meta-data
data7_orig <- readxl::read_excel(file.path(indir, "data7.xlsx")) # 1988 meta-data

# Read 1987-1988 counts
data5_orig <- readxl::read_excel(file.path(indir, "data5.xlsx")) # 1987 counts
data6_orig <- readxl::read_excel(file.path(indir, "data6.xlsx")) # 1987 counts
data8_orig <- readxl::read_excel(file.path(indir, "data8.xlsx")) # 1988 counts
data9_orig <- readxl::read_excel(file.path(indir, "data9.xlsx")) # 1988 counts


sum(nrow(data3b_orig), nrow(data3c_orig), nrow(data4_orig), nrow(data7_orig))



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


# Format overall meta-data
################################################################################

# Format overall 
sets0 <- data1_orig %>% 
  mutate(year=lubridate::year(date)) %>% 
  select(column, year, date, everything()) %>% 
  arrange(date)

table(sets0$year)


# Merge 1989-1990 counts
################################################################################

# Build data
counts1 <- bind_rows(data3b_orig, data3c_orig) %>% 
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
  # Arrange
  select(note, year, date, location, everything()) %>% 
  arrange(year, date, location, set_num)

# Inspect
str(counts1)
freeR::complete(counts1)
table(counts1$location)
table(counts1$year)

# Extract sets


# Merge 1987-1988 count files
################################################################################

# Build data
counts2 <- bind_rows(data5_orig, data6_orig, data8_orig, data9_orig) %>% 
  # Add species
  mutate(species=recode(spp_code, 
                        "20"="Harbor porpoise",
                        "25"="Harbor seal",
                        "26"="Elephant seal",
                        "27"="California sea lion",
                        "30"="Sea otter",
                        "39"="Unidentified marine mammal",
                        "60"="Common murre", 
                        "61"="Pigeon guillemot", 
                        "62"="Pelagic cormorant",
                        "63"="Brandt's cormorant",
                        "64"="Pacific loon",
                        "65"="Common loon",
                        "70"="Western grebe",
                        "72"="Unidentified cormorant",
                        "73"="Unidentified loon",
                        "79"="Unidentified seabird")) %>% 
  relocate(species, .after=spp_code) %>% 
  # Add trip and set ids
  mutate(trip_id=paste(date, vessel_id, sep="-"),
         set_id=paste(date, vessel_id, set_num, sep="-")) %>% 
  # Arrange
  select(trip_id, set_id, everything()) %>% 
  arrange(set_id)

# Inspect
str(counts2)
freeR::complete(counts2)


# Merge 1987-1988 meta-data files
################################################################################

# Build data
sets2 <- bind_rows(data4_orig, data7_orig) %>% 
  # Remove columns
  select(-page) %>% 
  # Remove blank rows
  filter(!is.na(date)) %>% 
  # Format date
  mutate(date=lubridate::ymd(date)) %>% 
  # Add target species
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("target_spp_code"="spp_code_num")) %>% 
  rename(target_spp=comm_name) %>% 
  relocate(target_spp, .after=target_spp_code) %>%
  # Add port code
  left_join(port_key %>% select(port_code, port), by="port_code") %>% 
  relocate(port, .after=port_code) %>%
  # Set a few 0s to NAs
  mutate(depth_min=ifelse(depth_min==0, NA, depth_min),
         depth_max=ifelse(depth_max==0, NA, depth_max),
         mesh_size_in=ifelse(mesh_size_in==0, NA, mesh_size_in)) %>%
  # Calculate percent observed
  mutate(obs_perc=net_length_obs/net_length) %>%
  # Add trip id and set id
  mutate(trip_id=paste(date, vessel_id, sep="-"),
         set_id=paste(date, vessel_id, set_num, sep="-")) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Arrange
  arrange(date) %>% 
  select(year, trip_id, set_id, everything())

# Inspect
str(sets2)
freeR::complete(sets2)

# Inspect
table(sets2$year)
range(sets2$date)
table(sets2$mesh_size_in)
range(sets2$net_length)
range(sets2$net_length_obs)
range(sets2$obs_perc)

# Set id unique
freeR::which_duplicated(sets2$set_id) # FIX THIS

# Check depths
sum(sets2$depth_max<sets2$depth_min, na.rm=T) # must be 0

# Check set counts
# 1987-09-25-33828 may be missing the 3rd set
check1 <- sets2 %>% 
  group_by(trip_id) %>% 
  summarize(nsets=unique(nsets),
            nsets_max=max(set_num),
            nsets_present=n()) %>% 
  mutate(max_check=nsets==nsets_max,
         present_check=nsets==nsets_present)






