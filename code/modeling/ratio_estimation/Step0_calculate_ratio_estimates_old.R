
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
outputdir <- "model_result"
obsdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/obs_merge" # Chris
logbookdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read effort time series
effort_orig <- read.csv(file.path(logbookdir, "CA_3.5in_set_gillnet_effort_by_year.csv"), as.is=T)

# Read data (originally said 1983_2017_gillnet_observer_data_3.5in_set_halibut.Rds)
obs_orig <- readRDS(file=file.path(obsdir, "1983_2017_gillnet_observer_data_with_sst_3.5in_set.Rds")) 

# Read year key
year_key <- readxl::read_excel("code/modeling/ratio_estimation/year_key.xlsx")


# Function
################################################################################

# Example species
species <-"California sea lion"

# Estimate bycatch using non-stratified ratio estimator
est_bycatch <- function(species){
  
  # Build observer data summary
  obs <- obs_orig %>% 
    # Add year
    mutate(year=lubridate::year(date)) %>% 
    # Add trip id
    mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
    # Summarize by year
    group_by(year) %>% 
    summarize(ntrips_obs=n_distinct(trip_id),
              ntrips_w_bycatch_obs=n_distinct(trip_id[comm_name==species]),
              nbycatch=sum(n_caught[comm_name==species])) %>% 
    ungroup()
  
  # Build data
  data1 <- effort_orig %>% 
    # Reduce
    select(year, nvesseldays) %>% 
    rename(ntrips_tot=nvesseldays) %>% 
    # Add observer info
    left_join(obs, by="year") %>% 
    # Proportion observed
    mutate(prop_obs=ntrips_obs / ntrips_tot) %>% 
    # Calculate kill rate
    mutate(kill_per_trip=nbycatch / ntrips_obs)
  
  # Extract kill rates for years with data
  year_ratio_key <- data1 %>% 
    select(year, kill_per_trip) %>% 
    na.omit() %>% 
    rename(kill_per_trip_use=kill_per_trip)
  
  # Bootstrap
  x <- 1983
  kill_rate_var_df <- purrr::map_df(year_ratio_key$year, function(x){
    
    # Number of trips total
    ntrips_tot <- effort_orig$nvesseldays[effort_orig$year==x]
    kill_per_trip <- year_ratio_key$kill_per_trip_use[year_ratio_key$year==x]
    
    # Results
    results_vec_obs <- obs_orig %>% 
      # Filter to year
      filter(year==x) %>% 
      # Add trip id
      mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
      # Summarize
      group_by(trip_id) %>% 
      summarize(nbycatch=sum(n_caught[comm_name==species])) %>% 
      ungroup() %>% 
      # Pull
      pull(nbycatch)
    
    # Sample vector and calculate kill rate
    niter <- 1000
    kill_rates <- purrr::map_vec(1:niter, function(x){
      
      # Sampled results
      results_vec_tot <- sample(x=results_vec_obs, size=ntrips_tot, replace = T)
      
      # Record stats
      kill_per_trip <- sum(results_vec_tot) / length(results_vec_tot)
      
    })
    
    # Plot
    df <- tibble(x=1:length(kill_rates), y=kill_rates)
    g <- ggplot(df, aes(x="", y=y)) +
      geom_boxplot() +
      geom_point(data=tibble(x="", y=kill_per_trip), color="red", size=4) +
      # lims(y=c(0,NA)) +
      labs(x="", y="Kill per trip") +
      theme_bw()
    g
    
    # Compute stats
    kill_rate_var <- var(kill_rates)
    kill_rate_sd <- sd(kill_rates)
    kill_rate_avg <- mean(kill_rates)
    kill_rate_cv <- kill_rate_sd / kill_rate_avg
    
    # Record
    out <- tibble(year=x,
                  kill_rate_var=kill_rate_var,
                  kill_rate_sd=kill_rate_sd,
                  kill_rate_cv=kill_rate_cv)
    

  })
  
  # Add variance estimates to key
  year_ratio_key1 <- year_ratio_key %>% 
    # Add kill rate stats
    left_join(kill_rate_var_df) %>% 
    # Compute kill rate CI
    mutate(kill_rate_lo = kill_per_trip_use - kill_rate_sd * 1.96,
           kill_rate_hi = kill_per_trip_use + kill_rate_sd * 1.96)
  
  ggplot() +
    geom_errorbar(data=year_ratio_key1, aes(x=year, ymin=kill_rate_lo, ymax=kill_rate_hi)) +
    geom_point(data=year_ratio_key1, aes(x=year, y=kill_per_trip_use))
  
  
  
  # Build data borrowing kill rates from reference years
  data2 <- data1 %>% 
    # Add reference year
    left_join(year_key, by="year") %>% 
    mutate(year_use=ifelse(is.na(year_use), year, year_use)) %>% 
    # Add reference ratio
    mutate(kill_rate_type=ifelse(year==year_use, "Available", "Borrowed")) %>% 
    left_join(year_ratio_key1, by=c("year_use"="year")) %>% 
    # Compute mortality
    mutate(mort=ntrips_tot*kill_per_trip_use) %>% 
    # Compute mortality CI
    mutate(mort_var=ntrips_tot*kill_rate_var,
           mort_sd=sqrt(mort_var),
           mort_cv=mort_sd / mort,
           mort_lo = ntrips_tot * kill_rate_lo, # mort-mort_sd*1.96,
           mort_hi = ntrips_tot * kill_rate_hi #mort+mort_sd*1.96
           ) %>% 
    # Add species
    mutate(comm_name=species) %>% 
    # Arrange
    select(comm_name, everything())
  
  # Plot data
  g <- ggplot(data2, mapping=aes(x=year, y=mort)) +
    geom_ribbon(mapping=aes(x=year, ymin=mort_lo, ymax=mort_hi), fill="grey80") +
    geom_line() +
    theme_bw()
  g
  
  # Return
  return(data2)
  
}


# Build data
################################################################################

# Species to do
spp_do <- c("Common murre", "California sea lion", "Harbor seal",
            "Brandt's cormorant", "Northern elephant seal", "Harbor porpoise")

# Generate estimates
data <- purrr::map_df(spp_do, function(x){
  est_bycatch(species=x)
})

# Export
saveRDS(data, file=file.path(outputdir, "1981_2022_bycatch_estimate_ratio_no_strata.Rds"))


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(x=year, y=mort)) +
  facet_wrap(~comm_name, ncol=3, scales="free_y") +
  geom_line() +
  # Labels
  labs(x="Year", y="Bycatch estimate") +
  # Theme
  theme_bw()
g










