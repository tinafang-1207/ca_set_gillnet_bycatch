
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
logbookdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read effort time series
effort_orig <- read.csv(file.path(logbookdir, "1981_2022_ca_set_gillnet_effort.csv"), as.is=T)

# Read observer data
obs_orig <- readRDS("/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/processed/processed_obs_1980_2017/obs_1980_2017_final.Rds")

# Function
################################################################################

species <-"California sea lion"
est_batch <- function(species){
  
  # Build data
  obs <- obs_orig %>% 
    # Add year
    mutate(year=lubridate::year(date)) %>% 
    # Summarize by year
    group_by(year) %>% 
    summarize(nsets_obs=n_distinct(set_id),
              nsets_obs_w_bycatch=n_distinct(set_id[comm_name==species]),
              nbycatch=sum(n_caught[comm_name==species])) %>% 
    ungroup()
  
  # Add data
  data1 <- effort_orig %>% 
    # Reduce
    select(year, n_sets) %>% 
    rename(nsets_tot=n_sets) %>% 
    # Add observer info
    left_join(obs, by="year") %>% 
    # Proportion observed
    mutate(p_obs=nsets_obs / nsets_tot) %>% 
    # Calculate kill rate
    mutate(kill_per_set=nbycatch / nsets_obs)
  
  # Data key
  year_ratio_key <- data1 %>% 
    select(year, kill_per_set) %>% 
    na.omit() %>% 
    rename(kill_per_set_use=kill_per_set)
  
  # Reference year key
  ref_year_key <- purrr::map_df(data1$year, function(x){
    ref_year <- year_ratio_key$year[which(abs(year_ratio_key$year - x) == min(abs(year_ratio_key$year - x)))]
    tibble(year=x, 
           ref_year=ref_year)
  })
  
  # Build data
  data2 <- data1 %>% 
    # Add reference year and ratio
    left_join(ref_year_key, by="year") %>% 
    left_join(year_ratio_key, by=c("ref_year"="year")) %>% 
    # Compute mortality
    mutate(mort=nsets_tot*kill_per_set_use)
  
  # Plot data
  g <- ggplot(data2, mapping=aes(x=year, y=mort)) +
    geom_bar(stat="identity") +
    theme_bw()
  g
  
  
}