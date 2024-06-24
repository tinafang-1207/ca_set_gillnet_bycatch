

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed" # Chris

# Read obersever data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds"))

# Read effort data
effort_df <- read.csv(file=file.path(datadir2, "CA_3.5in_set_gillnet_effort_by_year.csv")) %>% 
  rename(ntrips_tot=nvesseldays)

# Read species key
spp_key <- read.csv("data/keys/species_key_final.csv", as.is=T)

# Read block key
block_key <- readRDS("data/strata/block_strata_key.Rds")

# Read historical observer sample sizes
hist_obs_n <- readxl::read_excel("data/historical_estimates/processed/historical_regional_bycatch_rates.xlsx", sheet=2) 



# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Add trip id
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  # Add quarter
  mutate(month=lubridate::month(date),
         quarter=case_when(month %in% c(1,2,3) ~ "1",
                           month %in%c(4,5,6) ~ "2",
                           month %in% c(7,8,9) ~ "3",
                           month %in% c(10,11,12) ~ "4",
                           T ~ "Unknown")) %>% 
  # Add strata
  left_join(block_key, by="block_id") %>% 
  # Order strata
  mutate(strata=ifelse(is.na(strata), "Unassigned", strata))

freeR::complete(data)
table(data$strata)

# Totals
stats_tots <- data %>%
  # Rename
  rename(type=spp_type) %>% 
  # Format species name
  mutate(comm_name=recode(comm_name,
                          "Sea Otter"="Sea otter", 
                          "Unidentified bird"="Unidentified seabird")) %>%
  # Species totals
  group_by(type, comm_name) %>% 
  summarize(n=sum(n_caught)) %>% 
  ungroup() %>% 
  # Add species meta-data
  # left_join(spp_key %>% select(comm_name, type), by="comm_name") %>% 
  # Reduce to species of interest
  filter(type %in% c("bird", "mammal", "turtle") | comm_name %in% c("Giant sea bass", "Soupfin shark", "White shark")) %>% 
  # Recode type
  mutate(type=recode_factor(type,
                            "finfish"="Fish",
                            "shark/ray"="Fish",
                            "bird"="Seabird",
                            "mammal"="Marine mammal",
                            "turtle"="Sea turtle")) %>% 
  # Arrange
  arrange(desc(n))

# Number of sets
stats_nsets <- data %>%
  # Calcule annual effort obs
  group_by(year) %>% 
  summarize(nsets=n_distinct(set_id),
            ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Add total effort and calc prop
  left_join(effort_df %>% select(year, ntrips_tot)) %>% 
  mutate(prop_obs=ntrips/ntrips_tot,
         prop_obs_label=paste0(round(prop_obs*100, 1), "%"))

# Calculate sets per trip
stats_trip <- data %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  group_by(year, trip_id) %>% 
  summarize(nsets1=n_distinct(set_id),
            nsets2=max(set_num))

ggplot(stats_trip,  aes(x=year, y=nsets1, group=year)) +
  geom_boxplot() +
  labs(x="Year", y="Number of sets per day") +
  scale_y_continuous(breaks=seq(0,12, 2), lim=c(0,NA)) +
  theme_bw()

# Stats by strata
stats_strata <- data %>% 
  # Summarize by year and strats
  group_by(dataset, year, strata, quarter) %>% 
  summarize(nsets=n_distinct(set_id),
            ntrips=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Eliminate unassigned strata
  filter(strata!="Unassigned") %>% 
  # Factor strata
  mutate(strata=gsub(" ", "\n", strata),
         strata=factor(strata, levels=c("San\nFrancisco",
                                        "Monterey\nBay",
                                        "Morro\nBay", 
                                        "Ventura",              
                                        "Channel\nIslands",     
                                        "Southern\nCalifornia")))
  

# Year borrowing key
year_key <- tibble(year=c(1995:1998, 2001:2005,2008,2009, 2014:2016)) %>% 
  mutate(year_use=case_when(year == 1995 ~ 1994,
                            year == 1996 ~ 1994,
                            year == 1997 ~ 1999,
                            year == 1998 ~ 1999,
                            year == 2001 ~ 2000,
                            year == 2002 ~ 2000,
                            year == 2003 ~ 2000,
                            year == 2004 ~ 2006,
                            year == 2005 ~ 2006,
                            year == 2008 ~ 2007,
                            year == 2009 ~ 2010,
                            year == 2014 ~ 2013,
                            year == 2015 ~ 2017,
                            year == 2016 ~ 2017))

# Build data rescue/lost key
data_rescued <- stats_strata %>% 
  # Reduce to rescued data
  filter(dataset=="State (Karin)") %>% 
  # Add rescue tag
  mutate(status="Rescued") %>% 
  mutate(quarter=as.numeric(quarter)) %>% 
  # Simplify
  select(status, year, strata, quarter, ntrips, nsets)
data_lost <- hist_obs_n %>% 
  # Add status tag
  mutate(status="Lost") %>% 
  # Rename
  rename(nsets=n_sets) %>% 
  # Estimate number of trips
  mutate(ntrips=nsets/3) %>% 
  # Simplify
  select(status, year, strata, quarter, ntrips, nsets) %>% 
  # Remove ones that are in other data
  filter(!strata %in% c("Channel Islands", "Southern California")) %>% 
  filter(! (strata=="Morro Bay" & quarter %in% 3:4 & year==1986) ) %>% 
  # Factor strata
  mutate(strata=recode_factor(strata,
                              "San Francisco"="San\nFrancisco",       
                              "Monterey Bay"="Monterey\nBay",         
                              "Morro Bay"="Morro\nBay",            
                              "Ventura"="Ventura",              
                              "Channel Islands"="Channel\nIslands",     
                              "Southern California"="Southern\nCalifornia"))
lost_rescue_key <- bind_rows(data_rescued, data_lost) %>% 
  mutate(status=factor(status, levels=c("Lost", "Rescued", "Provided")))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=6),
                   strip.text = element_text(size=5),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot observer counts
g1 <- ggplot(stats_tots, aes(y=reorder(comm_name, n), x=n, fill=type)) +
  geom_bar(stat="identity") +
  # Labels
  geom_text(mapping=aes(x=n, y=comm_name, label=n), hjust=-0.2, size=2.2) +
  # Labels
  labs(x="Number of observed bycatch", y="", tag="A") +
  # Reference line
  geom_hline(yintercept=nrow(stats_tots)-8.5, linetype="dashed") +
  # Axes
  scale_x_continuous(trans="log10", lim=c(1, max(stats_tots$n)*2.5)) +
  # Legend
  scale_fill_ordinal(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.y=element_blank(),
        legend.position = c(0.7, 0.1),
        legend.key.size = unit(0.3, "cm"))
g1

# Plot data
g2 <- ggplot(stats_nsets, aes(x=year, y=ntrips)) +
  geom_bar(stat="identity") +
  # Plot perc obs labels
  geom_text(mapping=aes(label=prop_obs_label), color="grey30", size=1.7, angle=90, hjust=-0.1) +
  # Plot borrowed year labels
  # geom_text(data=year_key, mapping=aes(x=year, y=0, label=year_use), color="grey70", size=1.7, angle=90, hjust=0) +
  # Labels
  labs(x="", y="Number of observed trips", tag="B") +
  # Axes
  scale_y_continuous(lim=c(0, 1050), breaks=seq(0,1000,200)) +
  scale_x_continuous(breaks=stats_nsets$year) +
  # scale_x_continuous(breaks=seq(1980,2020,5)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Plot data
g3 <- ggplot(stats_strata, aes(x=as.character(year), y=quarter, fill=ntrips)) +
  facet_wrap(~strata, ncol=1, strip.position="right") +
  geom_tile() +
  # Plot data status
  geom_point(data=lost_rescue_key, aes(as.character(year), y=quarter, shape=status), size=0.8, stroke=0.2) +
  # Labels
  labs(x="", y="Quarter", tag="C") +
  # Legend
  scale_shape_manual(name="Data status", values=c(1, 16, NA), drop=F) +
  scale_fill_gradientn(name="# of trips\nobserved", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(shape= guide_legend(order=1),
         fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2, order=2)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        legend.key.size = unit(0.3, "cm"),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g3

# Merge data
layout_matrix <- matrix(c(1,2,
                          1,3), byrow = T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3,
                             heights=c(0.37, 0.63),
                             layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_observer_data.png"), 
       width=6.5, height=4.5, units="in", dpi=600)






