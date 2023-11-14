

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
gisdatadir <- "data/gis_data"

# Read data
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))
outdir <- "data/effort_for_jim"

# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Set gillnet
  filter(net_type=="Set" & year!=2022) %>% 
  # Add vessel days
  mutate(vessel_day=paste(vessel_id_use, date, sep="-")) %>% 
  # Summarize by year
  group_by(year) %>% 
  summarize(n_vessels=n_distinct(vessel_id_use),
            n_vessel_days=n_distinct(vessel_day)) %>% 
  ungroup()

# Do any years violate the rule of three?
sum(data$n_vessels<3)

# Export data
write.csv(data, file=file.path(outdir, "1981_2022_ca_set_gillnet_effort.csv"), row.names=F)


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
g1 <- ggplot(data, aes(x=year, y=n_vessels)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels", tag="A") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot 
g2 <- ggplot(data, aes(x=year, y=n_vessel_days)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessel days", tag="B") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(outdir, "figure_gillnet_effort.png"), 
       width=6.5, height=2.25, units="in", dpi=600)


