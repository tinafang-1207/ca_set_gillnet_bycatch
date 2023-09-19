
# clean working enviornment
rm(list = ls())


# load in package
library(tidyverse)

# read in data
data_orig <- read_csv("data/confidential/gillnet_data_new/Gillnet Logs_1981-2022.csv")

data_orig <- readxl::read_excel("data/confidential/gillnet_data_new/Gillnet Logs_1981-2022.xlsx", 
                               col_types = "text", na = c("N/A", ""))

spp_key <- readRDS("data/confidential/original/CDFW_species_key.Rds")


# format data
data1 <- data_orig %>%
  janitor::clean_names("snake") %>%
  # rename
  rename(serial_num = sn,
         vessel_name = vessel_name,
         boat_num = boatno,
         vessel_id = vessel_id,
         permit_num = permit,
         date = fishing_date,
         year = year,
         target_spp = tarspc,
         final_target_spp = final_target_species,
         net_type = drift_set,
         final_net_type = final_net_type_set_drift,
         block_id = fg_blocks,
         haul_depth_fa = depths,
         net_length_ft = net_length,
         mesh_size_in = mesh_size,
         buoy_line_depth_fa = bouy_line_depth,
         soak_hour = hours_net_soaked,
         common_name = common_name,
         common_name_mlds = final_mlds_common_name,
         mlds_spp_code = mlds_species_code,
         status = status,
         catch_n = num_catch,
         catch_lb = weights,
         predator = predator) %>%
  #remove empty columns
  select(-c(x25, x26)) %>%
  #format date
  mutate(date = ifelse(grepl("/", date), date, NA) %>% lubridate::mdy(.) %>% as.Date(.)) %>%
  # Format block id
  mutate(block_id=gsub("/", ", ", block_id)) %>% 
  # Format depth
  mutate(haul_depth_fa=as.numeric(haul_depth_fa)) %>% 
  # Format net length
  mutate(net_length_ft=as.numeric(net_length_ft)) %>% 
  # Format mesh size
  mutate(mesh_size_in=gsub("/", ", ", mesh_size_in),
         mesh_size_in_num=as.numeric(mesh_size_in)) %>% 
  # Format buoy line depth
  mutate(buoy_line_depth_fa=ifelse(buoy_line_depth_fa=="SM Mesh", NA, buoy_line_depth_fa),
         buoy_line_depth_fa=as.numeric(buoy_line_depth_fa)) %>% 
  # Format soak hour
  mutate(soak_hour=as.numeric(soak_hour)) %>% 
  #format status
  mutate(status=stringr::str_to_sentence(status)) %>%
  #format net type
  mutate(net_type=recode(net_type, "D"="Drift", "S"="Set"),
         net_type=ifelse(net_type %in% c("1", "2", "3", "5", "67", "68") | is.na(net_type), final_net_type, net_type))%>%
  #format target_spp
  mutate(target_spp=gsub("H, ", "Halibut, ", target_spp),
         target_spp=gsub("B, ", "Barracuda, ", target_spp),
         target_spp=gsub("W, ", "White seabass, ", target_spp),
         target_spp=gsub("C, ", "White croaker, ", target_spp),
         target_spp=gsub("S, ", "Shark/swordfish, ", target_spp),
         target_spp=gsub("X, ", "Soupfin shark, ", target_spp),
         target_spp=gsub(", X", ", Soupfin shark", target_spp),
         target_spp=gsub("YELTL", "Yellowtail", target_spp)) %>%
  mutate(target_spp=recode(target_spp,
                           "B"="Barracuda",
                           "H"="Halibut",
                           "W"="White seabass",
                           "S"="Shark/swordfish",
                           "X"="Soupfin shark",
                           "Croaker_Kingfish"="White croaker",
                           "Barracuda, W"="Barracuda, White croaker",
                           "Halibut, Sole, S"="Halibut, Sole, Shark/swordfish",
                           "Halibut, W"="Halibut, White seabass",
                           "Soupfin shark, W"="Soupfin shark, White seabass",
                           "White seabass, H"="White seabass, Halibut",
                           "Yellowtail, H"="Yellowtail, Halibut",
                           "Halibut, S" = "Halibut, Shark/swordfish",
                           "Swordfish /Shark" = "Swordfish/Shark",
                           "Shark/swordfish" = "Swordfish/Shark",
                           "Wite_Seabass" = "White seabass",
                           "White Seabass" = "White seabass",
                           "Angel_Shark" = "Angel shark",
                           "Scoupfin_shark" = "Scoupfin shark",
                           "Soupfin_Shark" = "Scoupfin shark",
                           "Shovelnose_Shark" = "Shovelnose shark",
                           "1" = "Yellowfin tuna",
                           "4" = "Bluefin tuna",
                           "8" = "Bigeye tuna")) %>%
  #format common name
  left_join(spp_key %>% select(spp_code_num, comm_name), by=c("mlds_spp_code"="spp_code_num")) %>%
  mutate(comm_name=case_when(mlds_spp_code==90 ~ "Swordfish",
                             mlds_spp_code==241 ~ "Curlfin turbot",
                             mlds_spp_code==243 ~ "C-O turbot",
                             mlds_spp_code==677 ~ "Shortraker rockfish",
                             mlds_spp_code==980 ~ "Sea lion",
                             T ~ comm_name)) %>%
  #fill in missing common name
  mutate(comm_name=ifelse(is.na(comm_name), paste(common_name), comm_name)) %>%
  #format missing common name
  mutate(comm_name = recode(comm_name,
  "X" = "Scoupfin shark",
  "S" = "Shark/swordfish",
  "EASTERN PACIFIC BONITO" = "Eastern pacific bonito",
  "ROCK CRABS" = "Rock crabs",
  "SEA SNAILS NEI" = "Sea snails nei",
  "BLT-BULLET TUNA" = "Bullet tuna",
  "GREEN STURGEON" = "Green sturgeon",
  "PACIFIC SIERRA" = "Pacific sierra",
  "DIAMOND TURBOT" = "Diamond turbot",
  "ROUGHEYE ROCKFISH" = "Rougheye rockfish",
  "HYDROZOANS" = "Hydrozoans",
  "HORNYHEAD TURBOT" = "Hornyhead turbot",
  "AGARS" = "Agars",
  "WHITE STURGEON" = "White sturgeon",
  "AMPHIOXUS LANCELETS" = "Amphioxus lancelets",
  "DLP-DOLPHINS NEI" = "Dolphins")) %>%
  # format predators
  mutate(predator=stringr::str_to_sentence(predator),
         predator=recode(predator,
                         "?"="Unknown",
                         "Birds"="Seabird",
                         "Blue sharks"="Blue shark",
                         "Harbor seals"="Harbor seal",
                         "Harbor seals and sea lions"="Harbor seal, sea lion",
                         "Mako"="Mako shark",
                         "Sea  lions"="Sea lion",
                         "Sea lion - hagfish"="Sea lion, hagfish",
                         "Sea lions and harbor seals"="Sea lion, harbor seal",
                         "Sea lions and slime eels"="Sea lion, slime eel",
                         "Seal?"="Seal",
                         "Seals"="Seal",
                         "Seals, sea lion"="Seal, sea lion",
                         "Slime eels"="Slime eel",
                         "Slime eels and harbor seals" = "Slime eel, harbor seal",
                         "Slime eels and sea lions" = "Slime eel, sea lion",
                         "Seals/mako" = "Seal, mako",
                         "Sea lions + slime eels" = "Sea lion, slime eel",
                         "Harbor seal + slime eels" = "Slime eel, harbor seal ",
                         "Sea_lion" = "Sea lion",
                         "Seal_bird" = "Seal, seabird",
                         "Harbor" = "Harbor seal",
                         "150" = "Unspecified shark",
                         "Unknown, maybe seal"="Unknown",
                         "National marine fisheries"="NMFS",
                         "Sea lions"="Sea lion")) %>%
  #format catch_n
  mutate(catch_n=catch_n %>% stringr::str_trim(.) %>% as.numeric(.),
         catch_lb = catch_lb%>% stringr::str_trim(.)%>% as.numeric(.)) %>%
  # vessel identifier
  mutate(vessel_id_use=ifelse(!is.na(vessel_id), vessel_id, boat_num),
         vessel_id_use_type=ifelse(!is.na(vessel_id), "Vessel id", "Boat number")) %>% 
  #arrange columns
  select(serial_num, vessel_name, vessel_id, vessel_id_use, vessel_id_use_type, permit_num, date, year, 
         target_spp, net_type, block_id, haul_depth_fa, net_length_ft,
         mesh_size_in, buoy_line_depth_fa, soak_hour, comm_name, mlds_spp_code,
         status, catch_n, catch_lb, predator) %>%
  rename(spp_code = mlds_spp_code,
         common_name = comm_name)


range(data1$year)
range(data1$date)

  
#save cleaned logbook data
saveRDS(data1, file=file.path("data/confidential/processed/CDFW_1980_2022_gillnet_logbook_new.Rds"))




########## Chris Code ############
###########################################################################

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

data1_filter <- data1 %>%
  filter(net_type == "Set") %>%
  select(year, date, vessel_id_use, boat_num, mesh_size_in, target_spp)



