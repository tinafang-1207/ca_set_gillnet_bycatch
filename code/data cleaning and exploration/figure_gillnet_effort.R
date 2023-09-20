

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)


# Directories


# Read data
data_orig <- readRDS("data/confidential/processed/CDFW_1980_2022_gillnet_logbook_new.Rds")

block <- wcfish::blocks

# Build data
################################################################################

# Net type
table(data_orig$net_type)

# Build data
data <- data_orig %>% 
  # Reduce to set fishery
  filter(net_type=="Set" & year!=2022) %>%
  # Add vessel days
  mutate(vessel_day=paste(vessel_id_use, date, sep="-"),
         set_id=paste(date, vessel_id_use, block_id, net_type, 
                      haul_depth_fa, net_length_ft, mesh_size_in, 
                      buoy_line_depth_fa, soak_hour, target_spp, sep="-"))

  
  # Summarize by year
  group_by(year) %>% 
  summarize(n_logbook_rows=n(),
            n_vessels=n_distinct(vessel_id_use),
            n_vessel_days=n_distinct(vessel_day),
            n_sets=n_distinct(set_id)) %>% 
  ungroup()

# Export data
write.csv(data, file=file.path(datadir, "1981_2022_ca_set_gillnet_effort.csv"), row.names=F)


# Plot for stratified region in Central & Southern California

block_key <- read.csv("data/block_stratified_key.csv")

block_format <- block %>%
  filter(block_state == "California") %>%
  left_join(block_key, by = "block_id") %>%
  filter(block_type %in% c("Inshore", "Midshore"))

usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")

mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")


g_region <- ggplot() +
  geom_sf(data = block_format, aes(fill = stratified_region)) +
  geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  scale_fill_discrete(name = "Stratified Region") +
  coord_sf(xlim = c(-122, -117), ylim = c(32, 37)) +
  my_theme

g_region

# Plot for number of vessel days by stratified region from 1981 - 2022
  # mesh size >= 8.5 inches




# Build data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=9),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=9),
                   plot.tag = element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot 
g1 <- ggplot(data, aes(x=year, y=n_logbook_rows)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of logbook rows", tag="A") +
  # Theme
  theme_bw() + my_theme
g1

# Plot 
g2 <- ggplot(data, aes(x=year, y=n_vessels)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels", tag="B") +
  # Theme
  theme_bw() + my_theme
g2

# Plot 
g3 <- ggplot(data, aes(x=year, y=n_vessel_days)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessel days", tag="C") +
  # Theme
  theme_bw() + my_theme
g3

# Plot 
g4 <- ggplot(data, aes(x=year, y=n_sets)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of sets", tag="D") +
  # Theme
  theme_bw() + my_theme
g4


# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow=2)
g

# Export
ggsave(g, filename=file.path(plotdir, "figure_gillnet_effort.png"), 
       width=6.5, height=4.5, units="in", dpi=600)


