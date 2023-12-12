


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
bar83_orig <- readxl::read_excel(file.path(indir, "Barlow_etal_1994_Table3.xlsx"))
jb_orig <- readxl::read_excel(file.path(indir, "Julian_Beeson_1998_Tables4_5.xlsx"))
cam99_orig <- readxl::read_excel(file.path(indir, "Cameron_Forney_1999.xlsx"))
cam00_orig <- readxl::read_excel(file.path(indir, "Cameron_Forney_2000.xlsx"))
car01_orig <- readxl::read_excel(file.path(indir, "Carretta_2001.xlsx"))
car02_orig <- readxl::read_excel(file.path(indir, "Carretta_2002.xlsx"), sheet="Table 3")
car03_orig <- readxl::read_excel(file.path(indir, "Carretta_Chivers_2003.xlsx"), sheet = "Table 3")
car04_orig <- readxl::read_excel(file.path(indir, "Carretta_Chivers_2004.xlsx"))
car07_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2009.xlsx"))
car10_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2012a.xlsx")) 
car11_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2012b.xlsx"))
car12_orig <- readxl::read_excel(file.path(indir, "Carretta_etal_2014.xlsx"))



# Format Perkins data
################################################################################

# Read data
perk1 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 1)
perk2 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 2)
perk3 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 3)
perk4 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 4)
perk5 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 5)
perk6 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 6)
perk7 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 7)
perk8 <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 8)

# Merge ones w/out parentheses
perkA <- bind_rows(perk2, perk5)

# Merge ones w/ parentheses (all mortalities)
perkB_orig <- bind_rows(perk6, perk7, perk8)

# Clean B
perkB <- perkB_orig %>% 
  # Clean species
  mutate(species=stringr::str_to_sentence(species),
         species=recode(species, 
                        "Da11's porpoise"="Dall's porpoise",
                        "N. Right whale dolphin"="Northern right whale dolphin",    
                        "Northern elephant seals"="Northern elephant seal",    
                        "Pac. Whited-sided dolphin"="Pacific white-sided dolphin",  
                        "Par.. Whited-sided dolphin"="Pacific white-sided dolphin", 
                        "Paw.. Whited-sided dolphin"="Pacific white-sided dolphin")) %>% 
  # Gather
  gather(key="fishery", value="mort_orig", 6:ncol(.)) %>% 
  # Seperate
  mutate(mort_orig=gsub("\\.", "x", mort_orig)) %>% 
  separate(col="mort_orig", into=c("mort", "mort_se", sep=" \\("), remove = F) %>% 
  mutate(mort=as.numeric(mort),
         mort_se=gsub("x", ".", mort_se) %>% as.numeric()) %>% 
  select(reference:mort_se) %>% 
  # Reduce to total set fishery
  filter(fishery=="set_total") %>% 
  select(-c(fishery, mort_orig)) %>% 
  # Reduce
  filter(!is.na(mort))
  
# Inspect
sort(unique(perkB$species))


# Format Forney data
################################################################################

# Read data
forn01_orig <- readxl::read_excel(file.path(indir, "Forney_etal_2001.xlsx"))

# Format data
forn01 <- forn01_orig %>% 
  gather(key="metric", value="value", 3:ncol(.)) %>% 
  separate(metric, into=c("year", "metric"), sep="_") %>% 
  spread(key="metric", value="value") 

ggplot(forn01, aes(x=year, y=est, color=analysis, group=analysis)) +
  facet_wrap(~species, scale="free_y") +
  geom_line() +
  theme_bw()

# Format other data
################################################################################

# Format Barlow et al. 1994
bar83 <- bar83_orig %>% 
  # Remove effort
  filter(category!="Effort") %>% 
  # Rename
  rename(species=value) %>% 
  # Gather
  gather(key="year", value="value", 5:ncol(.)) %>% 
  mutate(year=as.numeric(year)) %>% 
  # Spread
  mutate(category=recode(category,
                         "Observed marine mammal mortality"="obs",
                         "Estimated marine mammal mortality"="mort")) %>% 
  spread(key="category", value="value") %>% 
  # Remove 1990 and after
  filter(year<1990)

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
  filter(!is.na(mort) & mort!=0) %>% 
  # Don't use 1990 estimates for species with 1990 data in Perkins et al 1994
  filter(!(year == 1990 & species %in% perkB$species))

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
  # Format species
  mutate(species=recode(species, 
                        "Elephant seal"="Northern elephant seal",
                        "Unid. pinniped"="Unidentified pinniped")) %>% 
  # Merge regions
  group_by(reference, species, year) %>% 
  summarise(mort_sum=sum(mort),
            mort_cv=weighted.mean(mort_cv, w=mort)) %>% 
  ungroup() %>% 
  rename(mort=mort_sum) %>% 
  # Add table
  mutate(table="Tables 1&2")

# Format Carretta 2002
car02 <- car02_orig %>% 
  # Remove 2000
  select(-mort_2000) %>% 
  # Rename 2001
  rename(mort=mort_2001) %>% 
  mutate(year=2001) 

# Format Carretta 2003
car03 <- car03_orig %>% 
  # Simplify
  select(-mort_2001) %>% 
  # Rename
  rename(mort=mort_2002,
         mort_var=mort_2002_var,
         mort_se=mort_2002_se,
         mort_cv=mort_2002_cv) %>% 
  # Add 
  mutate(year=2002) %>% 
  # Format species
  mutate(species=recode(species,
                        "Unid. Common dolphin"="Unidentified common dolphin", 
                        "N. elephant seal"="Northern elephant seal",
                        "Unid. pinniped"="Unidentified pinniped"))
  

# Format Carretta 2004
car04 <- car04_orig

# Format Carretta 2007
car07 <- car07_orig %>% 
  mutate(kill_100sets=bycatch_set*100,
         kill_100sets_var=bycatch_set_var*100) %>% 
  select(-c(bycatch_set, bycatch_set_var))

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
data_merge <- bind_rows(bar83, perkB, jb, cam99, cam00, car01, car02, car03, car04, car07, car10, car11, car12)

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
                        "N. elephant seal"="Northern elephant seal",
                        "Unid. Common dolphin"="Unidentified common dolphin",
                        "Unid. seabird"="Unidentified bird",
                        "Unid. pinniped"="Unidentified pinniped",
                        "Unidentified turtle"="Unidentified sea turtle",
                        "Southern sea otters"="Southern sea otter",
                        "Sea otter"="Southern sea otter")) %>% 
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
  select(reference, table, species, year, obs, 
         kill_day, kill_day_se, kill_100sets, kill_100sets_se,
         mort, mort_var, mort_se, mort_cv, everything()) %>% 
  # Remove missing
  filter(!is.na(mort))
  
# Inspect
str(data)
freeR::complete(data)

# Inspect species
sort(unique(data$species))

# Export data
saveRDS(data, file=file.path(outdir, "ca_set_gillnet_bycatch_estimates_historical.Rds"))

# Reference stats
ref_stats <- data %>% 
  group_by(reference) %>% 
  summarize(years=paste(unique(year), collapse=", "),
            species=paste(unique(species), collapse=", ")) %>% 
  ungroup() %>% 
  arrange(years)
ref_stats


# Plot all data
################################################################################

# Stats
stats <- data %>% 
  group_by(species) %>% 
  summarize(mort_tot=sum(mort, na.rm=T),
            ymax1=max(mort_hi, na.rm=T),
            ymax2=max(mort, na.rm=T)) %>% 
  ungroup() %>% 
  rowwise() %>% 
  mutate(ymax=pmax(ymax1, ymax2, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(desc(mort_tot)) %>% 
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
  geom_text(data=stats, mapping=aes(y=ymax, label=mort_tot), x=2012,
            size=2.4, hjust=1) +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
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
my_theme2 <-  theme(axis.text=element_text(size=7),
                    axis.title = element_text(size=8),
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
  lemon::facet_rep_wrap(~species, scales="free_y", ncol=3, repeat.tick.labels = 'bottom') +
  # Data
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(ymin=mort_lo, ymax=mort_hi), width=0) +
  # Labels
  # geom_text(data=stats_select, mapping=aes(y=ymax, label=mort_tot), x=2012, 
  #           size=2.4, hjust=1) +
  scale_x_continuous(breaks=seq(1980,2020,5)) +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  # Theme
  theme_bw() + my_theme2
g

# Export plot
ggsave(g, filename=file.path(plotdir, "historical_estimates_select.png"), 
       width=6.5, height=4, units="in", dpi=600)




