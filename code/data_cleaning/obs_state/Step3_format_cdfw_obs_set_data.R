

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
list.files(indir)
data_orig <- read.csv(file.path(indir, "GNS8389.csv"), as.is=T, na.strings="")

# Read species key
port_key <- readRDS(file.path(keydir, "CDFW_port_key.Rds"))
spp_key <- readRDS(file.path(keydir, "CDFW_species_key.Rds"))

# Get blocks blocks
blocks <- wcfish::blocks


# Helper functions
################################################################################

# Convert latitude
x <- data_orig$LAT[1]
conv_lat <- function(x){
  # Extract degrees
  deg <- substr(x, 1, 2) %>% as.numeric()
  # Extract minutes
  min <- substr(x, 3, nchar(x)) %>% as.numeric()
  # Export lat
  lat <- deg+min/60
  return(lat)

}

# Convert latitude
x <- data_orig$LONG[1]
conv_long <- function(x){
  # Extract degrees
  deg <- substr(x, 1, 3) %>% as.numeric()
  # Extract minutes
  min <- substr(x, 4, nchar(x)) %>% as.numeric()
  # Export lat
  long <- (deg+min/60)*-1
  return(long)
}


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(set_num=setno,
         vessel_id=fgno,
         complete_yn=complete,
         obs_type=otype,
         port_depart_code=dport,
         port_landing_code=lport,
         target_spp_code=tspec,
         lat_dd_orig=lat,
         long_dd_orig=long,
         net_orientation=norient,
         environment=environ,
         bottom_depth_fa=bdepth,
         time_pull=ptime,
         duration=setdur,
         net_type=ntype,
         net_material=nmat,
         net_length_fa=nlen,
         net_depth=ndepth,
         mesh_size1_in=msize1,
         mesh_size2_in=msize2,
         hanging_length_in=hlength,
         suspender_length_ft=slength,
         extender_length_ft=elength) %>%
  # Format date
  mutate(date=lubridate::mdy(date)) %>%
  # Format time pull
  mutate(time_pull=ifelse(time_pull=="0.059027778", NA, time_pull),
         time_pull=as.POSIXct(time_pull, format="%H:%M") %>% strftime(., format="%H:%M")) %>%
  # Format duration
  mutate(duration=recode(duration,
                         "48"="48:00",
                         "022:"="22:00",
                         "024:"="24:00"),
         duration=ifelse(nchar(duration)>6, substr(duration, 1, nchar(duration)-3), duration),
         duration=lubridate::hm(duration)) %>%
  # Add soak time
  mutate(soak_hr=lubridate::hour(duration) + lubridate::minute(duration)/60) %>% 
  # Format lat/long
  mutate(lat_dd=conv_lat(lat_dd_orig),
         long_dd=conv_long(long_dd_orig)) %>%
  # Formate complete
  mutate(complete_yn=recode(complete_yn,
                            "1"="yes", "0"="no")) %>%
  # Format observation type
  mutate(obs_type=recode(obs_type,
                         "1"="prearranged",
                         "2"="opportune on board",
                         "3"="opportune not on board",
                         "4"="opportune at sea",
                         "5"="prearranged at sea")) %>%
  # Format net orientation
  mutate(net_orientation=recode(net_orientation,
                                "1"="parallel",
                                "2"="perpendicular",
                                "3"="diagonal",
                                "4"="other",
                                "5"="???")) %>%
  # Format net type
  mutate(net_type=recode(net_type,
                         "1"="set",
                         "2"="drift",
                         "3"="float",
                         "4"="trammel 1 panel",
                         "5"="trammel 2 panel",
                         "6"="trammel 3 panel")) %>%
  # Format net material
  mutate(net_material=recode(net_material,
                             "1"="monofilament",
                             "2"="multifilament",
                             "3"="combination",
                             "4"="twisted mono")) %>%
  # Format environment
  mutate(environment=recode(environment,
                            "1"="inshore of kelp",
                            "2"="in kelp",
                            "3"="offshore of kelp",
                            "4"="no kelp")) %>%
  # Format target species key
  mutate(target_spp_code=stringr::str_pad(target_spp_code, width=3, pad="0", side="left")) %>%
  # Add species name
  left_join(spp_key %>% select(spp_code_chr, comm_name), by=c("target_spp_code"="spp_code_chr")) %>%
  rename(target_spp=comm_name) %>%
  # Add port names
  left_join(port_key %>% select(port_code, port), by=c("port_depart_code"="port_code")) %>%
  rename(port_depart=port) %>%
  left_join(port_key %>% select(port_code, port), by=c("port_landing_code"="port_code")) %>%
  rename(port_landing=port) %>%
  # Format sampler
  mutate(sampler=toupper(sampler) %>% gsub(" ", "", .) %>% gsub("\\.", "", .)) %>%
  # Add set id
  mutate(set_id=paste(date, vessel_id, set_num, sep="-")) %>%
  # Arrange
  select(date, vessel_id, set_num, set_id, complete_yn, obs_type,
         port_depart_code, port_depart,
         port_landing_code, port_landing,
         target_spp_code, target_spp, lat_dd, long_dd,
         dfs, area, environment, bottom_depth_fa, time_pull, duration, soak_hr,
         net_orientation, net_type, net_material, net_length_fa, net_depth,
         mesh_size1_in, mesh_size2_in, hanging_length_in, hratio, suspender_length_ft, extender_length_ft, sampler,
         everything()) %>%
  select(-c(long_dd_orig, lat_dd_orig))

# Are the set ids unique?
freeR::which_duplicated(data$set_id)


# Inspect data
################################################################################

# Inspect
str(data)
freeR::complete(data)

# Inspect more
range(data$date)
table(data$complete_yn)
table(data$obs_type)
table(data$port_depart)
table(data$port_landing) # 676, 740 not matched, probably typos
table(data$target_spp)
table(data$net_orientation)
table(data$net_type)
table(data$net_material)
table(data$sampler)

# Check lat/long
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
g <- ggplot(data, aes(x=long_dd, y=lat_dd, color=port_depart)) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
  geom_point() +
  coord_sf(xlim=c(-117, -121), ylim=c(32, 35)) +
  # Labels
  labs(x="", y="") +
  scale_color_discrete(name="Port of departure") +
  # Theme
  theme_bw()
g


# Add block id
################################################################################

# Lat/long key
latlong_key <- data %>% 
  select(lat_dd, long_dd, set_id) %>% 
  na.omit() %>% 
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(blocks))

# Grab block
block_index <- sf::st_intersects(x=latlong_key, y=blocks) %>% 
   as.numeric()
block_ids <- blocks$block_id[block_index]

# Add to key
latlong_key1 <- latlong_key %>% 
  sf::st_drop_geometry() %>% 
  mutate(block_id = block_ids)

# Add to data
data1 <- data %>% 
  # Add block id
  left_join(latlong_key1 %>% select(set_id, block_id), by="set_id") %>% 
  # Arrange
  select(date, vessel_id, set_num, set_id, complete_yn, obs_type,
         port_depart_code, port_depart,
         port_landing_code, port_landing,
         target_spp_code, target_spp, block_id, lat_dd, long_dd, everything())


# Export data
################################################################################

# Export
saveRDS(data1, file=file.path(outdir, "CDFW_1983_1989_gillnet_observer_set_info.Rds"))

