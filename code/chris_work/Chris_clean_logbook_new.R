

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/confidential/gillnet_logbooks_2023/raw"
outdir <- "data/confidential/gillnet_logbooks_2023/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Gillnet Logs_1981-2022_sn.xlsx"), 
                                col_types = "text", na = c("N/A", ""))

# Read species key
spp_key <- read.csv("data/public/cdfw_keys/processed/CDFW_species_key.csv", as.is=T) %>% 
  mutate(spp_code_chr=as.character(spp_code_chr))



# Format data
################################################################################

# Format data
data1 <- data_orig %>% 
  # Rename columns
  janitor::clean_names("snake") %>% 
  rename(logbook_id=sn,
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
  mutate(block_id=gsub("/", ", ", block_id)) %>% 
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
  # Add missing species codes
  mutate(spp_code=case_when(spp_code=="1520" ~ "152",
                            spp_code=="154/ 179" ~ "154/179",
                            comm_name1=="Eastern Pacific Bonito" ~ "3",
                            comm_name1=="Rock Crabs" ~ "801",             
                            comm_name1=="Pacific Sierra" ~ "52",        
                            comm_name1=="Blt-Bullet Tuna" ~ "19",        
                            comm_name1=="Dlp-Dolphins Nei" ~ "481", # decided this is dolphinfish     
                            comm_name1=="Amphioxus Lancelets" ~ "915",    
                            comm_name1=="Hydrozoans" ~ "681",            
                            comm_name1=="Agars" ~ "951",                 
                            comm_name1=="Blue Mackerel" ~ "51",         
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
  # Add species
  left_join(spp_key %>% select(spp_code_chr, comm_name), by=c("spp_code"="spp_code_chr")) %>% 
  # Fill in missing common names
  mutate(comm_name=case_when(spp_code=="154/158" ~ "Brown smoothhound shark/Smooth hammerhead shark",
                             spp_code=="154/179" ~ "Brown smoothhound shark/Gray smoothhound shark", 
                             comm_name1=="Harbor Seal" ~ "Harbor seal",
                             comm_name2=="SeaUrchin,unspecified" ~ "Unspecified sea urchin",
                             T ~ comm_name)) %>% 
  # Vessel identifier
  mutate(vessel_id_use=ifelse(!is.na(vessel_id), vessel_id, boat_num),
         vessel_id_use_type=ifelse(!is.na(vessel_id), "Vessel id", "Boat number")) %>% 
  # Arrange
  select(logbook_id, year, date, 
         vessel_id_use, vessel_id_use_type, 
         vessel_id, vessel_name, boat_num, permit_id,
         block_id,
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
spp_key_orig$comm_name_orig1[is.na(spp_key_orig$comm_name)] %>% unique()


# Export data
################################################################################

# Export
saveRDS(data1, file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))



