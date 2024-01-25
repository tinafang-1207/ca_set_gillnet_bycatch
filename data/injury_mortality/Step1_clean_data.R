
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/injury_mortality"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "Carretta_etal_2022_Tables.xlsx"))
key <- readxl::read_excel(file.path(datadir, "Carretta_etal_2022_Tables.xlsx"), sheet=2)


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  rename(source_orig=source) %>% 
  left_join(key, by=c("source_orig")) %>% 
  select(species, type, source, cases, msi)

# Reduce to species of interest
sort(unique(data$species))
data_use <- data %>% 
  filter(species %in% c("California sea lion", "Northern elephant seal", "Harbor seal", "Harbor porpoise")) %>% 
  filter(msi>0)

data1 <- data_use %>% 
  filter(species %in% c("California sea lion", "Harbor porpoise")) %>% 
  mutate(species=recode(species,
                        "Harbor porpoise"="Harbor\nporpoise"))
data2 <- data_use %>% 
  filter(!species %in% c("California sea lion", "Harbor porpoise"))

# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   axis.title.y=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data1, aes(x=msi, 
                     y=tidytext::reorder_within(source, msi, species), 
                     fill=type)) +
  ggh4x::facet_grid2(species~., scales="free", space="free_y", independent="x") +
  geom_bar(stat="identity") + 
  # Label
  tidytext::scale_y_reordered() +
  labs(x="Number of observed mortality and\nserious injury incidents (2016-2020)", y="") +
  # Legend
  scale_fill_discrete(name="Source type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        legend.key.size = unit(0.3, "cm"))
g1

# Plot data
g2 <- ggplot(data2, aes(x=msi, 
                        y=tidytext::reorder_within(source, msi, species), 
                        fill=type)) +
  ggh4x::facet_grid2(species~., scales="free", space="free_y", independent="x") +
  geom_bar(stat="identity") + 
  # Label
  tidytext::scale_y_reordered() +
  labs(x="Number of observed mortality and\nserious injury incidents (2016-2020)", y="") +
  # Legend
  scale_fill_discrete(name="Source type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.65, 0.1),
        legend.key.size = unit(0.3, "cm"))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigSX_sources_of_mortality.png"), 
       width=6.5, height=4.5, units="in", dpi=600)

