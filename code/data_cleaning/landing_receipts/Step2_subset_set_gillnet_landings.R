

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1980_2022_landings_receipts.Rds"))

# Read logbooks
logdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
logs_orig <- readRDS(file=file.path(logdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))


# Initial exploration
################################################################################

# Gear codes
gear_key <- data_orig %>% 
  select(gear_id, gear) %>% 
  unique() %>%
  arrange(gear_id)

# What gear are halibut, white seabass, and angel shark caught with?
stats1 <- data_orig %>% 
  filter(comm_name %in% c("California halibut", "White sea bass", "Pacific angel shark")) %>% 
  group_by(gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Filter 1: landings with set gillnets or "entangling nets"
################################################################################

# Build data
data1 <- data_orig %>% 
  # Reduce to set gillnets (or potential nets)
  filter(gear %in% c("Trammel net", "Set gill net", "Large mesh set gn", "Small mesh set gn", "Entangling nets")) %>% 
  # Recode gear
  mutate(gear=recode_factor(gear,
                            "Trammel net"="Trammel net",
                            "Set gill net"="Set gillnet",
                            "Large mesh set gn"="Large-mesh set gillnet",
                            "Small mesh set gn"="Small-mesh set gillnet",
                            "Entangling nets"="Entangling net"))

# Identify species
data1_spp <- data1 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data1_spp_order <- data1_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data1_stats1 <- data1 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data1_stats2 <- data1 %>% 
  left_join(data1_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data1_spp_order$spp_label))

# Plot data
g1a <- ggplot(data1_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 1") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g1a

# Plot data
g1b <- ggplot(data1_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.7))
g1b

# Merge
g1 <- gridExtra::grid.arrange(g1a, g1b, nrow=1)
g1


# Filter 2: vessels that submit set gillnet logbooks
################################################################################

# Set gillnet vessels
sgn_vessels <- logs_orig$vessel_id %>% unique() %>% sort()

# Build data
data2 <- data1 %>% 
  # Reduce to vessels that submit set gillnet logbooks
  filter(vessel_id %in% logs_orig$vessel_id)

# Identify species
data2_spp <- data2 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data2_spp_order <- data2_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data2_stats1 <- data2 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data2_stats2 <- data2 %>% 
  left_join(data2_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data2_spp_order$spp_label))

# Plot data
g2a <- ggplot(data2_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 2") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g2a

# Plot data
g2b <- ggplot(data2_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.7))
g2b

# Merge
g2 <- gridExtra::grid.arrange(g2a, g2b, nrow=1)
g2


# Filter 3: strict interpretation of logbooks
################################################################################

# Build data
data3 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% logs_orig$trip_id)

# Identify species
data3_spp <- data3 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data3_spp_order <- data3_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data3_stats1 <- data3 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data3_stats2 <- data3 %>% 
  left_join(data3_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data3_spp_order$spp_label))

# Plot data
g3a <- ggplot(data3_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 3") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g3a

# Plot data
g3b <- ggplot(data3_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.7))
g3b

# Merge
g3 <- gridExtra::grid.arrange(g3a, g3b, nrow=1)
g3



# Filter 4: looser interpretation of logbook dates
################################################################################

# Log trip key
log_trip_key <- logs_orig %>% 
  select(trip_id, vessel_id, date) %>% 
  unique() %>% 
  na.omit()

# Build expanded trip ids
nday_buffer <- 1
trip_id_key1 <- purrr::map_df(1:nrow(log_trip_key), function(x){
 
  # Trip to do
  vessel_id_do <- log_trip_key$vessel_id[x]
  date_do <- log_trip_key$date[x]
  
  # Dates
  dates <- seq(date_do-nday_buffer, date_do+nday_buffer, by="1 day")
  trips <- paste(vessel_id_do, dates, sep="-")
  
  # Out
  out <- tibble(trip_id=trips,
                vessel_id=vessel_id_do,
                date=dates)
  
})

# Build data
data4 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key1$trip_id)

# Identify species
data4_spp <- data4 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data4_spp_order <- data4_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data4_stats1 <- data4 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data4_stats2 <- data4 %>% 
  left_join(data4_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data4_spp_order$spp_label))

# Plot data
g4a <- ggplot(data4_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 4") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g4a

# Plot data
g4b <- ggplot(data4_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.7))
g4b

# Merge
g4 <- gridExtra::grid.arrange(g4a, g4b, nrow=1)
g4



# Filter 5: looser interpretation of logbook dates
################################################################################

# Build expanded trip ids
nday_buffer <- 2
trip_id_key2 <- purrr::map_df(1:nrow(log_trip_key), function(x){
  
  # Trip to do
  vessel_id_do <- log_trip_key$vessel_id[x]
  date_do <- log_trip_key$date[x]
  
  # Dates
  dates <- seq(date_do-nday_buffer, date_do+nday_buffer, by="1 day")
  trips <- paste(vessel_id_do, dates, sep="-")
  
  # Out
  out <- tibble(trip_id=trips,
                vessel_id=vessel_id_do,
                date=dates)
  
})

# Build data
data5 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key2$trip_id)

# Identify species
data5_spp <- data5 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data5_spp_order <- data5_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data5_stats1 <- data5 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data5_stats2 <- data5 %>% 
  left_join(data5_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data5_spp_order$spp_label))

# Plot data
g5a <- ggplot(data5_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 5") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g5a

# Plot data
g5b <- ggplot(data5_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.7))
g5a

# Merge
g5 <- gridExtra::grid.arrange(g5a, g5b, nrow=1)
g5



# Filter 6: looser interpretation of logbook dates
################################################################################

# Build expanded trip ids
nday_buffer <- 3
trip_id_key3 <- purrr::map_df(1:nrow(log_trip_key), function(x){
  
  # Trip to do
  vessel_id_do <- log_trip_key$vessel_id[x]
  date_do <- log_trip_key$date[x]
  
  # Dates
  dates <- seq(date_do-nday_buffer, date_do+nday_buffer, by="1 day")
  trips <- paste(vessel_id_do, dates, sep="-")
  
  # Out
  out <- tibble(trip_id=trips,
                vessel_id=vessel_id_do,
                date=dates)
  
})

# Build data
data6 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key3$trip_id)

# Identify species
data6_spp <- data6 %>% 
  group_by(comm_name) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(value_usd)) %>% 
  mutate(rank=1:n(),
         spp_label=ifelse(rank<=10, comm_name, "Other"),
         spp_rank=ifelse(rank<=10, rank, 11))
data6_spp_order <- data6_spp %>% 
  group_by(spp_label, spp_rank) %>% 
  summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(spp_rank)

# Summarize by gear
data6_stats1 <- data6 %>% 
  group_by(year, gear) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup()

# Summarize by species
data6_stats2 <- data6 %>% 
  left_join(data6_spp %>% select(comm_name, spp_label)) %>% 
  group_by(year, spp_label) %>% 
  summarize(landings_lbs=sum(landings_lbs, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>% 
  ungroup() %>% 
  mutate(spp_label=factor(spp_label, levels=data6_spp_order$spp_label))

# Plot data
g6a <- ggplot(data6_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title="Filter 4") +
  # Legend
  scale_fill_discrete(name="Gear") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.8,0.8))
g6a

# Plot data
g6b <- ggplot(data6_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
  geom_bar(stat="identity", color="grey30", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Revenues (USD millions)", title=" ") +
  # Legend
  scale_fill_discrete(name="Species") +
  # Theme 
  theme_bw() + my_theme +
  theme(legend.position = c(0.7,0.7))
g6b

# Merge
g6 <- gridExtra::grid.arrange(g6a, g6b, nrow=1)
g6

# Export data
save(log_trip_key, trip_id_key1, trip_id_key2, trip_id_key3,
     file=file.path(outdir, "1980_2022_landings_receipts_set_gillnet_options.Rdata"))


# Temporary
saveRDS(data6, file=file.path(outdir, "1980_2022_landings_receipts_set_gillnet_use.Rds"))


