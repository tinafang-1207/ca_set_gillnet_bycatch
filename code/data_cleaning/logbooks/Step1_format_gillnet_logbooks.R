

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Gillnet Logs_1981-2022_sn.xlsx"), 
                                col_types = "text", na = c("N/A", ""))

# Read species key
spp_key <- readRDS("data/keys/CDFW_species_key.Rds")

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()


# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(logbook_id=sn,
         vessel_id_orig=vessel_id, 
         boat_num=boatno,
         permit_id=permit,
         date=fishing_date,
         target_spp1=tarspc,
         target_spp2=final_target_species,
         net_type_orig1=drift_set,
         net_type_orig2=final_net_type_set_drift,
         block_id=fg_blocks,
         depth_fa=depths,
         net_length_fa=net_length, 
         mesh_size_in=mesh_size,
         buoy_line_depth_ft=bouy_line_depth,
         soak_hr=hours_net_soaked,
         catch_n=num_catch,
         catch_lb=weights,
         spp_code=mlds_species_code,
         comm_name1=common_name,
         comm_name2=final_mlds_common_name) %>% 
  # Remove empty columns
  select(-c(x25, x26)) %>% 
  # Format year
  mutate(year=as.numeric(year)) %>% 
  # Format date
  mutate(date=recode(date, 
                     "6/31/2021"="6/30/2021"),
         date=case_when(grepl("/", date) ~ lubridate::mdy(date) %>% as.character(),
                        !grepl("/", date) ~ openxlsx::convertToDate(date) %>% as.character(),
                        T ~ ""),
         date=lubridate::ymd(date)) %>% 
  # Format block id
  mutate(block_id=gsub("/", ", ", block_id), 
         block_id_num=as.numeric(block_id)) %>% 
  # Add block details
  left_join(blocks_df %>% select(block_id, block_type, block_lat_dd, block_long_dd), 
            by=c("block_id_num"="block_id")) %>% 
  # Format depth
  mutate(depth_fa_num=as.numeric(depth_fa)) %>% 
  # Format net length
  mutate(net_length_fa=case_when(net_length_fa=="Halibut" ~ NA_character_,
                                 net_length_fa=="1 Mile" ~ "880",
                                 T ~ net_length_fa),
         net_length_fa_num=as.numeric(net_length_fa)) %>% 
  # Format mesh size
  mutate(mesh_size_in=gsub("/", ", ", mesh_size_in),
         mesh_size_in_num=as.numeric(mesh_size_in)) %>% 
  # Format buoy line depth
  mutate(buoy_line_depth_ft=ifelse(buoy_line_depth_ft=="SM Mesh", NA, buoy_line_depth_ft),
         buoy_line_depth_ft_num=as.numeric(buoy_line_depth_ft)) %>% 
  # Format soak hour
  mutate(soak_hr=recode(soak_hr,
                        "20 mins"="0.33"),
         soak_hr_num=as.numeric(soak_hr)) %>% 
  # Format original net type 1
  mutate(net_type_orig1=recode(net_type_orig1,
                               "67"="Set", # Large Mesh Set Gn based on gear codes
                               "68"="Set", # Small Mesh Set Gn based on gear codes
                               "d"="Drift",
                               "D"="Drift",
                               "s"="Set",
                               "S"="Set")) %>% 
  # Create final net type
  # Rules: use original 2 (which is clean), then clean parts of original 1, then assign remainder Unknown
  mutate(net_type_final=case_when(!is.na(net_type_orig2) ~ net_type_orig2,
                                  is.na(net_type_orig2) &  net_type_orig1 %in% c("Set", "Drift") ~ net_type_orig1,
                                  T ~ "Unknown")) %>% 
  # Format target species 1
  # Rules: maintain order, assume that Y=Yellowtail and T=Thresher
  mutate(target_spp1=gsub("B, ", "Barracudea, ", target_spp1),
         target_spp1=gsub("H, ", "Halibut, ", target_spp1),
         target_spp1=gsub("W, ", "White Seabass, ", target_spp1),
         target_spp1=gsub("S, ", "Shark/Swordfish, ", target_spp1),
         target_spp1=gsub("X, ", "Soupfin Shark, ", target_spp1),
         target_spp1=gsub(", X", ", Soupfin Shark", target_spp1),
         target_spp1=gsub("YELTL", "Yellowtail", target_spp1),
         target_spp1=recode(target_spp1,
                            "B"="Barracuda",
                            "h"="Halibut",
                            "H"="Halibut",
                            "S"="Shark/Swordfish",
                            "T"="Thresher",
                            "w"="White Seabass",
                            "W"="White Seabass",
                            "Wite_Seabass"="White Seabass",
                            "X"="Soupfin Shark",
                            "Angel_Shark"="Angel Shark",
                            "Barracudea, W"="Barracuda, White Seabass",
                            "Barracudea, White Seabass, Soupfin Shark"="Barracuda, White Seabass, Soupfin Shark",
                            "Croaker_Kingfish"="White Croaker (Kingfish)",
                            "Halibut, S"="Halibut, Swordfish/Shark", 
                            "Halibut, Sole, S"="Halibut, Sole, Swordfish/Shark",
                            "Halibut, W"="Halibut, White Seabass",
                            "King Fish"="White Croaker (Kingfish)",
                            "Soupfin Shark, W"="Soupfin Shark, White Seabass",
                            "Shovelnose_Shark"="Shovelnose Shark",
                            "Swordfish /Shark"="Swordfish/Shark",
                            "Soupfin_Shark"="Soupfin Shark",
                            "White Seabass, H"="White Seabass, Halibut",
                            "White Seabass, T"="White Seabass, Thresher",
                            "White Seabass, Y"="White Seabass, Yellowtail",
                            "Yellowtail, H"="Yellowtail, Halibut",
                            "Y, T"="Yellowtail, Thresher")) %>% 
  # Format target species 2
  # Rules: maintain order
  mutate(target_spp2=recode(target_spp2,
                            "H"="Halibut",
                            "H, X, Sole"="Halibut, Soupfin Shark, Sole",
                            "H, X, Sole, Angel"="Halibut, Soupfin Shark, Sole, Angel Shark",
                            "S"="Shark/Swordfish",
                            "W"="White Seabass",
                            "W, H"="White Seabass, Halibut",
                            "W, X"="White Seabass, Soupfin Shark")) %>% 
  # Finalize target species
  mutate(target_spp=ifelse(!is.na(target_spp1), target_spp1, target_spp2)) %>% 
  # Format predator
  # Rules: maintain order, maybe/question mark = unknown
  mutate(predator=predator %>% stringr::str_squish(.) %>% stringr::str_to_sentence(.),
         predator=recode(predator, 
                         "150"="Shark",
                         "Birds"="Bird",
                         "Blue sharks"="Blue shark",
                         "Harbor"="Harbor seal", 
                         "Harbor seals"="Harbor seal",
                         "Harbor seal + slime eels"="Harbor seal, slime eel",
                         "Harbor seals and sea lions"="Harbor seal, sea lion",
                         "Mako"="Mako shark",
                         "National marine fisheries"="NMFS",
                         "Sea lions"="Sea lion",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Sea lions + slime eels"="Sea lion, slime eel",
                         "Sea lions and harbor seals"="Sea lion, harbor seal", 
                         "Sea lions and slime eels"="Sea lion, slime eel",
                         "Sea_lion"="Sea lion",
                         "Seal?"="Unknown",
                         "Seals"="Seal",
                         "Seals, sea lion"="Seal, sea lion",
                         "Seals/mako"="Seal, mako shark",
                         "Seal_bird"="Seal, bird",
                         "Slime eels"="Slime eel",
                         "Slime eels and harbor seals"="Slime eel, harbor seal",
                         "Slime eels and sea lions"="Slime eel, sea lion",
                         "?"="Unknown",
                         "Unspecified"="Unknown",
                         "Unknown, maybe seal"="Unknown")) %>% 
  # Format common name 1
  mutate(comm_name1=comm_name1 %>% stringr::str_squish(.) %>% stringr::str_to_title(.)) %>% 
  # Add missing species codes and fix incorrect species codes
  mutate(spp_code=case_when(spp_code=="1" ~ "001",   
                            spp_code=="2" ~ "002",   
                            spp_code=="3" ~ "003",   
                            spp_code=="4" ~ "004",   
                            spp_code=="5" ~ "005",   
                            spp_code=="6" ~ "006",   
                            spp_code=="8" ~ "008",   
                            spp_code=="9" ~ "009",  
                            spp_code=="15" ~ "015",  
                            spp_code=="17" ~ "017",  
                            spp_code=="19" ~ "019",  
                            spp_code=="40" ~ "040",  
                            spp_code=="41" ~ "041",  
                            spp_code=="42" ~ "042",  
                            spp_code=="43" ~ "043",  
                            spp_code=="50" ~ "050",  
                            spp_code=="51" ~ "051",  
                            spp_code=="52" ~ "052",  
                            spp_code=="55" ~ "055",  
                            spp_code=="80" ~ "080",  
                            spp_code=="81" ~ "081",  
                            spp_code=="90" ~ "091", # swordfish  
                            spp_code=="92" ~ "092",  
                            spp_code=="95" ~ "095",  
                            spp_code=="96" ~ "096",  
                            spp_code=="97" ~ "097",  
                            spp_code=="98" ~ "098", 
                            # spp_code=="241" ~ "235", # curlfin turbot
                            # spp_code=="242" ~ "236", # diamond turbot
                            # spp_code=="243" ~ "237", # C-O sole
                            # spp_code=="244" ~ "238", # hornyhead turbot
                            # spp_code=="471" ~ "", # green sturgeon
                            # spp_code=="472" ~ "", # white sturgeon
                            # spp_code=="650" ~ "", # rougheye rockfish
                            # spp_code=="677" ~ "", # shortraker rockfish
                            # spp_code=="945" ~ "", # Sea snails nei
                            spp_code=="980" ~ "",
                            spp_code=="1520" ~ "152",
                            spp_code=="154/ 179" ~ "154/179",
                            comm_name1=="Eastern Pacific Bonito" ~ "003",
                            comm_name1=="Rock Crabs" ~ "801",             
                            comm_name1=="Pacific Sierra" ~ "052",        
                            comm_name1=="Blt-Bullet Tuna" ~ "019",        
                            comm_name1=="Dlp-Dolphins Nei" ~ "481", # decided this is dolphinfish     
                            comm_name1=="Amphioxus Lancelets" ~ "915",    
                            comm_name1=="Hydrozoans" ~ "681",            
                            comm_name1=="Agars" ~ "951",                 
                            comm_name1=="Blue Mackerel" ~ "051",         
                            # comm_name1=="Sb" ~ ,              
                            # comm_name1=="X" ~ ,                      
                            # comm_name1=="Crab" ~ ,                   
                            # comm_name1=="Harbor Seal" ~ ,          
                            # comm_name1=="S" ~ ,                      
                            # comm_name1=="Grass Back" ~ ,             
                            # comm_name1=="Verde" ~ ,
                            # comm_name1=="Smooth Hound" ~ ,          
                            # comm_name1=="Grass Bass" ~ , 
                            comm_name2=="Crab,RockUnspecified" ~ "801",
                            comm_name2=="SeaUrchin,unspecified" ~ "",
                            T ~ spp_code)) %>% 
  mutate(spp_code=ifelse(spp_code=="", NA, spp_code)) %>% 
  # Add species
  left_join(spp_key %>% select(spp_code_chr, comm_name), by=c("spp_code"="spp_code_chr")) %>% 
  # Fill in missing common names
  mutate(comm_name=case_when(spp_code=="154/158" ~ "Brown smoothhound shark/Smooth hammerhead shark",
                             spp_code=="154/179" ~ "Brown smoothhound shark/Gray smoothhound shark", 
                             comm_name1=="Harbor Seal" ~ "Harbor seal",
                             comm_name1=="Sea Lion" ~ "Sea lion",
                             comm_name2=="SeaUrchin,unspecified" ~ "Unspecified sea urchin",
                             T ~ comm_name)) %>% 
  # # Vessel identifier
  # mutate(vessel_id_use=ifelse(!is.na(vessel_id), vessel_id, boat_num),
  #        vessel_id_use_type=ifelse(!is.na(vessel_id), "Vessel id", "Boat number")) %>% 
  # # Add trip id
  # mutate(trip_id=paste(vessel_id_use, date, sep="-")) %>% 
  # # Build set
  # mutate(set_id=paste(date, vessel_id_use, block_id, depth_fa, 
  #                     net_length_fa, mesh_size_in, buoy_line_depth_ft,
  #                     soak_hr, target_spp, sep="-")) %>% 
  # Arrange
  select(logbook_id, year, date,
         #trip_id, set_id, 
         #vessel_id_use, vessel_id_use_type, 
         vessel_id_orig, vessel_name, boat_num, permit_id,
         block_id, block_id_num, block_type, block_lat_dd, block_long_dd, 
         net_type_orig1, net_type_orig2, net_type_final,
         depth_fa, depth_fa_num, 
         net_length_fa, net_length_fa_num,
         mesh_size_in, mesh_size_in_num,
         buoy_line_depth_ft, buoy_line_depth_ft_num, 
         soak_hr, soak_hr_num,
         target_spp1, target_spp2, target_spp, 
         spp_code, comm_name1, comm_name2, comm_name,
         status, catch_n, catch_lb, predator,
         everything()) %>% 
  arrange(date) %>% 
  # Rename
  rename(comm_name_orig1=comm_name1,
         comm_name_orig2=comm_name2,
         net_type=net_type_final) 
  # Remove duplicates
  # not sure if this is actually a good idea
  # unique()

# Inspect
str(data1)
freeR::complete(data1)

# Year and date
table(data1$year)
range(data1$date, na.rm=T)

# Net type
# 1, 2, 3, 5, H, N, Q, W, X are unknown
table(data1$net_type_orig1)
table(data1$net_type_orig2)
table(data1$net_type)
net_type_key <- data1 %>% 
  count(net_type, net_type_orig1, net_type_orig2)

# Depth
sort(unique(data1$depth_fa)) # sometimes there are multiple entries
range(data1$depth_fa_num, na.rm=T)

# Block id
sort(unique(data1$block_id)) # sometimes there are multiple entries

# Net characteristics
sort(unique(data1$net_length_fa)) # sometimes there are multiple entries
sort(unique(data1$mesh_size_in))  # sometimes there are multiple entries
sort(unique(data1$buoy_line_depth_ft)) # sometimes there are multiple entries
sort(unique(data1$soak_hr)) # sometimes there are multiple entries

# Inspect net characteristics
range(data1$net_length_fa_num, na.rm=T)
range(data1$mesh_size_in_num, na.rm=T)
range(data1$buoy_line_depth_ft_num, na.rm=T)
range(data1$soak_hr_num, na.rm=T)

# Status
# 1, 2, 3, 4, 5 are all unknown
table(data1$status) 

# Predator
table(data1$predator)

# Target species
# 1, 4, 8, D, E, F, J, L, M, N, O, P, R, SW, Z are unknown
sort(unique(data1$target_spp1))
sort(unique(data1$target_spp2))
target_spp_key <- data1 %>% 
  count(target_spp, target_spp1, target_spp2)

# Caught species
spp_key_orig <- data1 %>% 
  select(spp_code, comm_name_orig1, comm_name_orig2, comm_name) %>% unique()
spp_key_orig$spp_code[is.na(spp_key_orig$comm_name)] %>% unique() %>% as.numeric() %>% sort()


# Build vessel key
################################################################################

# Vessel key 
vessel_key1 <- data1 %>% 
  # Unique id combos
  select(vessel_id_orig, boat_num) %>% 
  unique() %>% 
  # Count characters
  mutate(vessel_id_nchar=nchar(vessel_id_orig),
         boat_num_nchar=nchar(boat_num)) %>% 
  # Vessel id type
  mutate(vessel_id_type=case_when(grepl("CF", vessel_id_orig) ~ "DMV number",
                                  vessel_id_nchar<=5 ~ "Vessel id",
                                  vessel_id_nchar>5 ~ "Primary vessel number",
                                  T ~ "")) %>% 
  # Boat number type
  mutate(boat_num_type=case_when(boat_num_nchar<=5 ~ "Vessel id",
                                 boat_num_nchar>5 ~ "Primary vessel number",
                                 T ~ "")) %>% 
  # Add vessel id
  mutate(vessel_id=ifelse(vessel_id_type=="Vessel id", vessel_id_orig,
                          ifelse(boat_num_type=="Vessel id", boat_num, NA))) %>% 
  # Add primary vessel id
  mutate(primary_id=ifelse(vessel_id_type=="Primary vessel number", vessel_id_orig,
                           ifelse(boat_num_type=="Primary vessel number", boat_num, NA))) %>% 
  # Add DMV id
  mutate(dmv_id=ifelse(vessel_id_type=="DMV number", vessel_id_orig, NA)) %>% 
  # Add vessel id use
  mutate(vessel_id_use=ifelse(!is.na(vessel_id), vessel_id,
                              ifelse(!is.na(primary_id), primary_id, dmv_id)),
         vessel_id_use_type=ifelse(!is.na(vessel_id), "Vessel id",
                                   ifelse(!is.na(primary_id), "Primary vessel number", 
                                          ifelse(!is.na(dmv_id), "DMV number", NA))))

# PVNs
pvns <- vessel_key1 %>% 
  select(primary_id)  %>% 
  na.omit()
write.csv(pvns, file=file.path(outdir, "primary_vessel_ids_in_gillnet_logbook_data.csv"), row.names=F)
  
# Prepare for merging
vessel_key2 <- vessel_key1 %>% 
  select(vessel_id_use_type, vessel_id_use, vessel_id, primary_id, dmv_id, vessel_id_orig, boat_num) %>% 
  arrange(vessel_id_use_type)


# Add vessel key to data
################################################################################

# Expand data
data2 <- data1 %>% 
  # Add vessel ids
  left_join(vessel_key2) %>% 
  # Add trip id
  mutate(trip_id=paste(vessel_id_use, date, sep="-")) %>% 
  # Add set id
  mutate(set_id=paste(vessel_id_use, date, net_type, block_id, depth_fa,
                      net_length_fa, mesh_size_in, buoy_line_depth_ft, soak_hr,
                      target_spp, sep="-")) %>% 
  # Arrange
  select(logbook_id, year, date, trip_id, set_id,
         vessel_id_use_type, vessel_id_use, vessel_id, primary_id, dmv_id,
         everything())


# Set id checks
################################################################################

# How frequently does a set repeat a species and is therefore unlikely to be a unique set?
check1 <- data2 %>% 
  # Number of times species listed for set
  # If set id was perfect, it would be 1
  group_by(set_id, comm_name) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  filter(n>1) %>% 
  # Merge repeated sppp
  group_by(set_id) %>% 
  summarize(comm_names=paste(comm_name, collapse = ", "),
            nrepeats=max(n)) %>% 
  ungroup()

# Proportion of sets that are unlikely to be unique
nsets <- n_distinct(data2$set_id)
nrow(check1) / nsets * 100
table(check1$nrepeats)
mean(check1$nrepeats)

# Sets per trip
stats <- data2 %>% 
  group_by(year, trip_id) %>% 
  summarize(nsets=n_distinct(set_id)) %>% 
  ungroup()

# Plot histogram of sets per trip
ggplot(stats, aes(x=nsets)) +
  geom_histogram(breaks=seq(0, 30,1)) +
  labs(x="Number of sets per day", y="Frequency") +
  theme_bw()

# Average sets per trip over time
stats_avg <- stats %>% 
  group_by(year) %>% 
  summarise(nsets_avg=mean(nsets))

# Plot average sets per trip over time
ggplot(stats_avg, aes(x=year, y=nsets_avg)) +
  geom_bar(stat="identity") +
  theme_bw()



# Export data
################################################################################

# Export
saveRDS(data2, file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))



