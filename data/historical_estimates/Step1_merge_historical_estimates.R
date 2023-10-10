


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/historical_estimates/raw"
outdir <- "data/historical_estimates/processed"
plotdir <- "data/historical_estimates/figures"

# Read data
jb_orig <- readxl::read_excel(file.path(indir, "Julian_Beeson_1995_Tables4_5.xlsx"))
cam99_orig <- readxl::read_excel(file.path(indir, "Cameron_1999.xlsx"))
cam00_orig <- readxl::read_excel(file.path(indir, "Cameron_2000.xlsx"))
car01_orig <- readxl::read_excel(file.path(indir, "Carretta_2001.xlsx"))
car03_orig <- readxl::read_excel(file.path(indir, "Carretta_2003.xlsx"))
car10_orig <- readxl::read_excel(file.path(indir, "Carretta_2010.xlsx"))
car11_orig <- readxl::read_excel(file.path(indir, "Carretta_2011.xlsx"))
car12_orig <- readxl::read_excel(file.path(indir, "Carretta_2012.xlsx"))


# Format data
################################################################################

# Format Julian & Beeson 1995
jb <- jb_orig %>% 
  # Gather
  gather(key="metric_year", value="value", 5:ncol(.)) %>% 
  # Seperate
  separate(metric_year, into=c("variable", "year"), sep="_", remove=T, convert=T) %>% 
  # Spread
  spread(key="variable", value="value") %>% 
  # Remove entanglement
  filter(metric=="Mortality") %>% 
  select(-metric) %>% 
  # Rename
  rename(mort=est, mort_cv=cv) %>% 
  # Reduce
  filter(!is.na(mort) & mort!=0)

# Format Cameron 1999
cam99 <- cam99_orig %>% 
  # Remove observed (b/c from other years) 
  select(-obs)

# Format Cameron 2000
cam00 <- cam00_orig %>% 
  # Exclude observer data
  select(reference:species, mort:mort_cv) %>% 
  # Select the preferred harbor porpoise row
  filter(species!="Harbor Porpoise, excluding unidentified cetacean")

# Format Carretta 2001
car01 <- car01_orig %>% 
  # Merge regions
  group_by(reference, species, year) %>% 
  summarise(mort_sum=sum(mort),
            mort_cv=weighted.mean(mort_cv, w=mort)) %>% 
  ungroup() %>% 
  rename(mort=mort_sum) %>% 
  # Add table
  mutate(table="Tables 1&2")

# Format Carretta 2003
car03 <- car03_orig

# Format Carretta 2010
car10 <- car10_orig

# Format Carretta 2011
car11 <- car11_orig

# Format Carretta 2012
car12 <- car12_orig %>% 
  mutate(kill_day=kill_100days/100) %>% 
  select(-kill_100days)


# Merge and format data
################################################################################

# Merge data
data_merge <- bind_rows(jb, cam99, cam00, car01, car03, car10, car11, car12)

# Format data
data <- data_merge %>% 
  # Format species
  mutate(species=species %>% stringr::str_trim() %>% stringr::str_to_sentence(),
         species=recode(species, 
                        "Brandt’s cormorant"="Brandt's cormorant",
                        "Brandt‘s cormorant"="Brandt's cormorant",
                        "Brandt's cormortant"="Brandt's cormorant",
                        "Common dolphin (unknown stock)"="Common dolphin",
                        "Green/black turtle"="Green/black sea turtle",
                        "Harbor porpoise, including unidentified cetacean"="Harbor porpoise",
                        "Leatherback turtle"="Leatherback sea turtle",
                        "Loggerhead turtle"="Loggerhead sea turtle",
                        "Unid. seabird"="Unidentified bird",
                        "Unid. pinniped"="Unidentified pinniped",
                        "Unidentified turtle"="Unidentified sea turtle")) %>% 
  # Derive missing SEs and variances
  # var = SE^2
  # CV = se / mean
  mutate(mort_se_calc=ifelse(!is.na(mort_se), mort_se, mort_cv*mort),
         mort_var_calc=ifelse(!is.na(mort_var), mort_var, 
                              ifelse(!is.na(mort_se), mort_se^2, (mort_cv*mort)^2))) %>% 
  # Derive low/high values
  mutate(mort_lo=pmax(0, mort-mort_se_calc*1.96),
         mort_hi=mort+mort_se_calc*1.96) %>% 
  # Arrange
  select(reference:species, year, obs, 
         kill_day, kill_day_se, kill_100sets, kill_100sets_se,
         mort, mort_var, mort_se, mort_cv, everything())
  
# Inspect
str(data)
freeR::complete(data)

# Inspect species
sort(unique(data$species))

# Export data
saveRDS(data, file=file.path(outdir, "ca_set_gillnet_bycatch_estimates_historical.Rds"))


# Plot all data
################################################################################

# Stats
stats <- data %>% 
  group_by(species) %>% 
  summarize(mort=sum(mort, na.rm=T),
            ymax=max(mort_hi)) %>% 
  ungroup() %>% 
  arrange(desc(mort)) %>% 
  mutate(species=factor(species, species))

# Order data
data_ordered <- data %>% 
  mutate(species=factor(species, levels=stats$species))

# Theme
my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   axis.title=element_text(size=10),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_ordered, aes(x=year, y=mort)) +
  # Facet
  facet_wrap(~species, scales="free_y", ncol=5) +
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), width=0) +
  # Labels
  geom_text(data=stats, mapping=aes(y=ymax, label=mort), x=2012, 
            size=2.4, hjust=1) +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "historical_estimates_all.pdf"), 
       width=8.5, height=11.5, units="in", dpi=600)


# Plot some data
################################################################################

# Reduce data
data_select <- data_ordered %>% 
  filter(species %in% c("Common murre", "California sea lion", "Harbor seal",
                        "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise"))
stats_select <- stats %>% 
  filter(species %in% c("Common murre", "California sea lion", "Harbor seal",
                        "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise"))

# Theme
my_theme2 <-  theme(axis.text=element_text(size=8),
                   axis.title=element_text(size=10),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(data_select, aes(x=year, y=mort)) +
  # Facet
  facet_wrap(~species, scales="free_y", ncol=3) +
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), width=0) +
  # Labels
  geom_text(data=stats_select, mapping=aes(y=ymax, label=mort), x=2012, 
            size=2.4, hjust=1) +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  # Theme
  theme_bw() + my_theme2
g

# Export plot
ggsave(g, filename=file.path(plotdir, "historical_estimates_select.png"), 
       width=6.5, height=4, units="in", dpi=600)




