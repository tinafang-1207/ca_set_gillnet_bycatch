


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
datadir <- "data/julian_beeson"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Julian_Beeson_1995_Tables4_5.xlsx"))


# Format data
################################################################################

# Column names 
colnames <- expand.grid(metric=c("obs", "est", "cv"),
                        year=1990:1995) %>% 
                        mutate(name=paste(metric, year, sep="_")) %>% pull(name)


# Rename
data <- data_orig %>% 
  # Rename
  setNames(c("table", "metric", "species", colnames )) %>% 
  # Gather
  gather(key="stat_year", value="value", 4:ncol(.)) %>% 
  # Format stat year
  separate(stat_year, into=c("stat", "year"), sep="_") %>% 
  mutate(year=as.numeric(year)) %>% 
  # Spread
  spread(key="stat", value="value") %>% 
  # Rename
  rename(obs_n=obs, est_n=est, est_cv=cv) %>% 
  # Arrange
  select(table, metric, species, year, obs_n, est_n, est_cv)

# Inspect
str(data)

# Stats
stats <- data %>% 
  filter(metric=="Mortality") %>% 
  group_by(species) %>% 
  summarize(est_n_max=max(est_n, na.rm=T),
            est_n_tot=sum(est_n, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(est_n_tot)) %>% 
  mutate(species=factor(species, level=species))

# Order data
data_ordered <- data %>% 
  mutate(species=factor(species, level=stats$species))


# Format data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered %>% filter(metric=="Mortality"), 
            mapping=aes(x=year, y=est_n)) +
  # Facet
  facet_wrap(~species, ncol=4, scale="free_y") +
  # Plot time series
  geom_line() +
  geom_point() +
  # Plot total
  geom_text(data=stats, mapping=aes(y=est_n_max, label=est_n_tot, x=1991)) %>% 
  # Labels
  labs(x="Year", y="Total mortality") +
  # Axes
  lims(y=c(0,NA)) +
  # Theme
  theme_bw() + my_theme
g



