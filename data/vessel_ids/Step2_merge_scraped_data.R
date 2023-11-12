

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(RSelenium)

# Directories
indir <- "data/vessel_ids/raw"
outdir <- "data/vessel_ids/processed"

# Files 2 merge
files2merge <- list.files(indir)

# Build data
################################################################################

# Merge files
data_orig <- purrr::map_df(files2merge, function(x){
  df <- read.csv(file.path(indir, x)) %>% 
    mutate(filename=x)
})

# 
sort(unique(data_orig$attrbute))

# Non-alternate number attrbutes
attributes <- c("Depth", 
                "Hull Identification Number",          
                "IMO Number" ,                          
                "Length",                               
                "Manufacturer Hull Number",           
                "Primary Vessel Number",               
                "Service",                            
                "Vessel Call Sign",                   
                "Vessel Flag",                          
                "Vessel Name",
                "Alternate VINs",                     
                "Alternate VINs:",
                "Breadth",                            
                "Build Year" )

# Format data
data1 <- data_orig %>% 
  # Rename
  rename(attribute=attrbute) %>% 
  # Arrange
  select(filename, everything()) %>% 
  # Filter
  filter(attribute %in% attributes) %>% 
  filter(!attribute %in% c("Alternate VINs", "Alternate VINs:")) %>% 
  # Format attributes
  mutate(attribute=recode(attribute,
                          "Depth"="depth_ft",
                          "Hull Identification Number"="hin",          
                          "IMO Number"="imo_number",                          
                          "Length"="length_ft",            
                          "Manufacturer Hull Number"="manufacturer_hin",          
                          "Primary Vessel Number"='primary_id',               
                          "Service"="service",                        
                          "Vessel Call Sign"="call_sign",                  
                          "Vessel Flag"="flag",                         
                          "Vessel Name"="vessel_name", 
                          "Breadth"="breadth_ft",                            
                          "Build Year"="build_year")) %>% 
  # Spread
  spread(key="attribute", value="value") %>% 
  # Arrange
  select(filename, primary_id, vessel_name, imo_number, hin, manufacturer_hin, call_sign, 
         flag, service, length_ft, depth_ft, breadth_ft, build_year, everything()) %>% 
  # Format
  mutate(build_year=build_year %>% gsub("N/A", "", .) %>% as.numeric(.),
         length_ft=length_ft %>% gsub("N/A| ft", "", .) %>% as.numeric(.),
         breadth_ft=breadth_ft %>% gsub("N/A| ft", "", .) %>% as.numeric(.),
         depth_ft=depth_ft %>% gsub("N/A| ft", "", .) %>% as.numeric(.),
         imo_number=ifelse(imo_number=="N/A", NA, imo_number),
         hin=ifelse(hin=="N/A", NA, hin),
         manufacturer_hin=ifelse(manufacturer_hin=="N/A", NA, manufacturer_hin),
         call_sign=ifelse(call_sign=="N/A", NA, call_sign)) %>% 
  # Format primary id
  rename(primary_id_orig=primary_id) %>% 
  mutate(primary_id_orig=gsub("\\(U.S.\\))", "U.S.)", primary_id_orig)) %>% 
  separate(col=primary_id_orig, into=c("primary_id", "primary_id_type"), sep=" \\(", remove=F) %>% 
  mutate(primary_id_type=gsub(")", "", primary_id_type)) %>% 
  select(-primary_id_orig)
  

head(data1)


  