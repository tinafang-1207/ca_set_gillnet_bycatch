
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
datadir <- "data/injury_mortality/raw"
outdir <- "data/injury_mortality/processed"
plotdir <- "figures"

# Source
# https://github.com/JimCarretta/MSI-data

# Read data
list.files(datadir)
data07_orig <- read.csv(file.path(datadir, "Anthropogenic.MSI.cases.2007-2022.csv"))
data16_orig <- read.csv(file.path(datadir, "2016_2020_5yr_MSI.csv"))
data17_orig <- read.csv(file.path(datadir, "2017_2021_5yr_MSI.csv"))
data18_orig <- read.csv(file.path(datadir, "2018_2022_5yr_MSI.csv"))
lfish <- read.csv(file.path(datadir, "List_of_Fisheries.csv"))

# Read key
type_key <- readxl::read_excel(file.path(datadir, "type_key.xlsx"))


# Format data
################################################################################

# Format data 
data <- data07_orig %>% 
  # Rename
  janitor::clean_names("snake") %>%
  rename(county=county_locale,
         stock=stock_or_area,
         injury_initial=initial_injury_assessment,
         injury_final=final_injury_assessment,
         msi=msi_value,
         type_orig=interaction_type) %>% 
  # Add date
  mutate(date=lubridate::ymd(paste(year, month, day, sep="-"))) %>% 
  # Format species
  mutate(species=stringr::str_to_sentence(species),
         species=recode(species, 
                        "Common dolphin, long-beaked"="Long-beaked common dolphin", 
                        "Common dolphin, short-beaked"="Short-beaked common dolphin",
                        "Common dolphin, unidentified"="Unidentified common dolphin")) %>% 
  # Format stock
  mutate(stock=stringr::str_to_title(stock)) %>% 
  # Format county
  mutate(county=stringr::str_to_title(county)) %>% 
  # Add interaction type
  left_join(type_key, by=c("type_orig")) %>% 
  rename(interaction_type=type_new,
         interaction_catg=category) %>% 
  # Arrange
  select(year, month, day, date, 
         state, county,
         species, stock, 
         interaction_catg, interaction_type, type_orig, 
         outcome, 
         injury_final, injury_initial, comments,
         everything()) %>% 
  # Arrange
  arrange(year, month, day)

# Inspect
table(data$species)
table(data$county)
range(data$date, na.rm=T)

# Export
saveRDS(data, file=file.path(outdir, "2007_2022_injury_mortality_data.Rds"))


# Visualize data
################################################################################

# Stats
stats <- data %>% 
  # State and species of interest
  filter(species %in% c("California sea lion", "Harbor seal", "Northern elephant seal", "Harbor porpoise")) %>% 
  # Summarize
  group_by(species, interaction_catg, interaction_type) %>% 
  summarise(msi=sum(msi)) %>% 
  ungroup() %>% 
  mutate(msi_yr=msi/length(2007:2022)) %>% 
  # Remove zeros
  filter(msi!=0)

# Split data
stats1 <- stats %>% 
  filter(species %in% c("California sea lion", "Harbor porpoise")) %>% 
  mutate(species=recode(species,
                        "Harbor porpoise"="Harbor\nporpoise"))
stats2 <- stats %>% 
  filter(!species %in% c("California sea lion", "Harbor porpoise"))

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
g1 <- ggplot(stats1, aes(x=msi_yr, 
                        y=tidytext::reorder_within(interaction_type, msi, species), 
                        fill=interaction_catg)) +
  ggh4x::facet_grid2(species~., scales="free", space="free_y", independent="x") +
  geom_bar(stat="identity") + 
  # Label
  tidytext::scale_y_reordered() +
  labs(x="Average annual observed mortality and\nserious injury incidents (2007-2022)", y="") +
  # Legend
  scale_fill_discrete(name="Source type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        legend.key.size = unit(0.3, "cm"))
g1

# Plot data
g2 <- ggplot(stats2, aes(x=msi_yr, 
                        y=tidytext::reorder_within(interaction_type, msi, species), 
                        fill=interaction_catg)) +
  ggh4x::facet_grid2(species~., scales="free", space="free_y", independent="x") +
  geom_bar(stat="identity") + 
  # Label
  tidytext::scale_y_reordered() +
  labs(x="Average annual observed mortality and\nserious injury incidents (2007-2022)", y="") +
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
ggsave(g, filename=file.path(plotdir, "FigSX_sources_of_mortality_full.png"), 
       width=6.5, height=6.5, units="in", dpi=600)




