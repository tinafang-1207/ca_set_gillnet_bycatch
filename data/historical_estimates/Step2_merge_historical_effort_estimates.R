


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
perk_orig <- readxl::read_excel(file.path(indir, "Perkins_etal_1994_Tables1_8.xlsx"), 4)
jb_orig <- readxl::read_excel(file.path(indir, "Julian_Beeson_1998_Tables4_5.xlsx"), sheet="Effort")
cam99_orig <- readxl::read_excel(file.path(indir, "Cameron_Forney_1999.xlsx"), sheet="Effort")
cam00_orig <- readxl::read_excel(file.path(indir, "Cameron_Forney_2000.xlsx"), sheet="Effort")
car01_orig <- readxl::read_excel(file.path(indir, "Carretta_2001.xlsx"), sheet="Effort")
car02_orig <- readxl::read_excel(file.path(indir, "Carretta_2002.xlsx"), sheet="Effort")
car03_orig <- readxl::read_excel(file.path(indir, "Carretta_Chivers_2004.xlsx"), sheet="Effort")
car07_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2009.xlsx"), sheet="Effort")
car10_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2012a.xlsx"), sheet="Effort") 
car11_orig <- readxl::read_excel(file.path(indir, "Carretta_Enriquez_2012b.xlsx"), sheet="Effort")
car12_orig <- readxl::read_excel(file.path(indir, "Carretta_etal_2014.xlsx"), sheet="Effort")


# Format data
################################################################################

# Format Barlow et al. 1994
bar83 <- bar83_orig %>% 
  # Reduce to effort
  filter(category=="Effort") %>% 
  select(-category) %>% 
  # Gather
  rename(metric=value) %>% 
  gather(key="year", value="value", 4:ncol(.)) %>% 
  mutate(year=as.numeric(year)) %>% 
  # Spread
  spread(key="metric", value="value") %>% 
  # Rename
  rename("psets_obs"="% observed net pulls",
         "nvesseldays"="Effort in days",       
         "nsets"="Est. no. net pulls",
         "nsets_obs"="No. observed net pulls") %>% 
  # Exclude >1990 (in Julian & Beeson 1999)
  filter(year<1990)

# Format Perkins et al. 1994
perk <- perk_orig %>% 
  select(reference, table, year, set_total) %>% 
  rename(nvesseldays=set_total)

# Format Julian & Beeson 1995
jb <- jb_orig %>% 
  # Add reference and table
  mutate(reference="Julian & Beeson 1995",
         table="Table 5") %>% 
  select(reference, table, everything()) %>% 
  # Gather
  gather(key="year", value="value", 4:ncol(.)) %>% 
  mutate(year=as.numeric(year)) %>% 
  # Spread
  spread(key="metric", value="value") %>% 
  # Rename
  rename("pvesseldays_obs"="Percent observer coverage",
         "nvesseldays"="Estimated days effort",       
         "nvesseldays_obs"="Observed days effort",
         "ntrips_obs"="Observed trips effort") 

# Format Cameron 1999
cam99 <- cam99_orig

# Format Cameron 2000
cam00 <- cam00_orig

# Format Carretta 2001
car01 <- car01_orig 

# Format Carretta 2002
car02 <- car02_orig

# Format Carretta 2003
car03 <- car03_orig

# Format Carretta 2007
car07 <- car07_orig
 
# Format Carretta 2010
car10 <- car10_orig

# Format Carretta 2011
car11 <- car11_orig

# Format Carretta 2012
car12 <- car12_orig


# Merge and format data
################################################################################

# Merge data
data_merge <- bind_rows(bar83, perk, jb, cam99, cam00, car01, car02, car03, car07, car10, car11, car12)

# Format data
data <- data_merge %>% 
  # Arrange
  select(reference, table, year, 
         nvessels, mesh_in_avg,
         nvesseldays, nvesseldays_obs, pvesseldays_obs,
         nsets, nsets_obs, psets_obs,
         everything()) %>% 
  arrange(year)

# Are years unique?
freeR::which_duplicated(data$year)

# Inspect
str(data)
freeR::complete(data)


# Export data
saveRDS(data, file=file.path(outdir, "ca_set_gillnet_effort_estimates_historical.Rds"))






