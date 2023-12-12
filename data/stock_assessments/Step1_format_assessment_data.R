

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/stock_assessments/raw"
outdir <- "data/stock_assessments/processed"

# Read data thief data
eseal_orig <- readxl::read_excel(file.path(indir, "northern_elephant_seal_datathief.xlsx"))
hseal_orig <- readxl::read_excel(file.path(indir, "harbor_seal_datathief.xlsx"))
slion_orig <- readxl::read_excel(file.path(indir, "california_sea_lion_datathief.xlsx"))

# Read original data
porp_orig <- readxl::read_excel(file.path(indir, "Porpoise_AbundSummary_by_Stock.xlsx"), sheet="Format")

# Read bird data
brandt_orig <- readRDS(file=file.path(outdir, "Capitola_et_2012_brandts_cormorant_data.Rds"))
murre_orig <- readRDS(file=file.path(outdir, "Carter_et_2001_common_murre_data.Rds"))


# Format data
################################################################################

# Format data
eseal <- eseal_orig %>% 
  rename(n_mid=n_animals) %>% 
  mutate(species="Northern elephant seal",
         stock="Central California",
         units="Number of births",
         type="Observed")
hseal <- hseal_orig %>% 
  rename(n_mid=n_animals) %>% 
  mutate(species="Harbor seal",
         stock="California",
         units="Number of seals",
         type="Observed")
slion <- slion_orig %>% 
  mutate(species="California sea lion",
         stock="US West Coast",
         region="US West Coast",
         units="Number of sea lions",
         type="Observed") 

# Format porpoise data
porp <- porp_orig %>% 
  mutate(stock=region) %>% 
  mutate(species="Harbor porpoise",
         units="Number of porpoises",
         type="Observed") 

# Format Brandt data
brandt <- brandt_orig %>% 
  # Summarize across colonies
  group_by(region, year, type) %>% 
  summarize(n_mid=sum(nests_n)) %>% 
  ungroup() %>% 
  # Add
  mutate(stock="Central California") %>% 
  mutate(species="Brandt's cormorant",
         units="Number of nests")

# Format murre data
murre <- murre_orig %>% 
  # Summarize across colonies
  group_by(region, year, type) %>% 
  summarize(n_mid=sum(count)) %>% 
  ungroup() %>% 
  # Add
  mutate(stock="Central California") %>% 
  mutate(species="Common murre",
         units="Number of breeding birds")



# Interpolate data
################################################################################

# Test data
df <- eseal %>% 
  filter(region=="Channel Islands")

# Funtion to interpolate missing values
interpolate_n <- function(df){
  
  # Grab species, region, units
  spp <- df$species %>% unique()
  stock <- df$stock %>% unique()
  reg <- df$region %>% unique()
  units <- df$units %>% unique()
  
  # Create a complete sequence of years from the minimum to maximum
  complete_years <- seq(min(df$year), max(df$year), by = 1)
  
  # Create a template dataframe with all years
  template_df <- data.frame(species=spp,
                            stock=stock,
                            region=reg,
                            units=units,
                            year = complete_years,
                            type="Observed")
  
  # Merge the template dataframe with the original dataframe
  merged_df <- merge(template_df, df %>% select(year, n_mid), by = "year", all.x = TRUE) %>% 
    # Add a column to indicate obs/inter
    mutate(type=ifelse(is.na(n_mid), "Interpolated", type)) %>% 
    # Arrange
    select(species, stock, region, units, year, type, everything())
  
  # Linearly interpolate missing values
  interpolated_df <- merged_df %>%
    arrange(year) %>%
    mutate(n_mid = zoo::na.approx(n_mid, na.rm = FALSE))
  
  # Return
  return(interpolated_df)
  
}

# Interpolate
eseal_ci <- interpolate_n(eseal %>% filter(region=="Channel Islands"))
eseal_ccal <- interpolate_n(eseal %>% filter(region=="Central California"))
hseal_main <- interpolate_n(hseal %>% filter(region=="Mainland"))
hseal_ci <- interpolate_n(hseal %>% filter(region=="Channel Islands"))


# Merge data
################################################################################

# Merge
data <- bind_rows(eseal_ci, eseal_ccal, hseal_main, hseal_ci, slion, porp, brandt, murre) %>% 
  # Add stock
  mutate(stock_label=paste0(species, " (", stock, ")")) %>% 
  # Arrange
  select(species, stock, stock_label, region, year, units, type, n_mid, n_lo, n_hi) %>% 
  # Filter
  filter(!is.na(n_mid))


# Export
saveRDS(data, file=file.path(outdir, "stock_assessment_data.Rds"))


# Plot data
################################################################################

# Plot data
options(scipen = 999)
g <- ggplot(data, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Labels
  labs(x="Year", y="Population size") +
  scale_x_continuous(lim=c(NA, 2020)) +
  scale_alpha_manual(values=c(0.7, 1)) +
  # Legend 0.7)) +
  # Theme
  theme_bw() +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g





