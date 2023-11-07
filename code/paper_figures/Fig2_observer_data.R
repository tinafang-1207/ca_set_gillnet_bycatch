

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris

# Read data
data_orig <- readRDS(file=file.path(datadir, "1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds"))

# Read species key
spp_key <- read.csv("data/keys/species_key_final.csv", as.is=T)


# Build data
################################################################################

# Block ids
ci_blocks <- c(684:690, 707:713, 813:814, 760:762, 806:807, 829, 850, 849, 867, 765)
ventura_blocks <- c(651:663, 664:677, 697, 776, 691:696, 714:717, 701:706, 678:683) 

# Format data
data <- data_orig %>% 
  # Add quarter
  mutate(month=lubridate::month(date),
         quarter=case_when(month %in% c(12,1,2) ~ "Q1",
                           month %in%c(3,4,5) ~ "Q2",
                           month %in% c(6,7,8) ~ "Q3",
                           month %in% c(9,10,11) ~ "Q4",
                           T ~ "Unknown")) %>% 
  # Add strata
  mutate(strata=case_when(block_id %in% ci_blocks ~ "Channel Islands",
                          block_id %in% ventura_blocks ~ "Ventura",
                          block_id <= 650 ~ "Central California",
                          T ~ "Southern California")) %>% 
  # Order strata
  mutate(strata=factor(strata, levels=c("Central California", "Ventura", 
                                        "Channel Islands", "Southern California")))

# Totals
stats_tots <- data %>%
  # Species totals
  group_by(comm_name) %>% 
  summarize(n=sum(n_caught)) %>% 
  ungroup() %>% 
  # Add species meta-data
  left_join(spp_key %>% select(comm_name, type), by="comm_name") %>% 
  # Format species name
  mutate(comm_name=recode(comm_name,
                          "Sea Otter"="Sea otter")) %>% 
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
  group_by(year) %>% 
  summarize(nsets=n_distinct(set_id)) %>% 
  ungroup()

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
  group_by(year, strata, quarter) %>% 
  mutate(nsets=n_distinct(set_id))


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text = element_text(size=7),
                   plot.tag = element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot observer countss
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
g2 <- ggplot(stats_nsets, aes(x=year, y=nsets)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of observed sets", tag="B") +
  # Axes
  scale_x_continuous(breaks=stats_nsets$year) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Plot data
g3 <- ggplot(stats_strata, aes(x=as.character(year), y=quarter, fill=nsets)) +
  facet_wrap(~strata, ncol=1) +
  geom_tile() +
  # Labels
  labs(x="", y="Quarter", tag="C") +
  # Legend
  scale_fill_gradientn(name="# of sets\nobserved", colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev()) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
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
                             heights=c(0.45, 0.55),
                             layout_matrix=layout_matrix)
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig2_observer_data.png"), 
       width=6.5, height=4.5, units="in", dpi=600)






