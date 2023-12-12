

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "data/stock_assessments/raw"
outdir <- "data/stock_assessments/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Carter_etal_2001_TableH3.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Rename
  rename(colony=colony_complex) %>% 
  # Gather
  gather(key="year", value="count", 2:ncol(.)) %>% 
  mutate(year=as.numeric(year), 
         region="Central California") %>% 
  # Filter
  filter(colony!="Total") %>% 
  # Remove 1979 b/c unable to interpolate
  filter(year!=1979)






# Impute data
################################################################################

# Colonies
colonies <- unique(data$colony)
x <- colonies[1]
data_interpolated <- purrr::map_df(colonies, function(x){
  
  # Subset data
  sdata <- data %>% 
    filter(colony==x & !is.na(count))
  
  # Create a complete sequence of years from the minimum to maximum
  complete_years <- seq(min(sdata $year), max(sdata$year), by = 1)
  
  # Create a template dataframe with all years
  template_df <- data.frame(region=sdata$region %>% unique(),
                            colony=sdata$colony %>% unique(),
                            year=complete_years,
                            type="Observed")
  
  # Merge the template dataframe with the original dataframe
  merged_df <- merge(template_df, sdata %>% select(year, count), by = "year", all.x = TRUE) %>% 
    # Add a column to indicate obs/inter
    mutate(type=ifelse(is.na(count), "Interpolated", type))
  
  # Linearly interpolate missing values
  interpolated_df <- merged_df %>%
    arrange(year) %>%
    mutate(count = zoo::na.approx(count, na.rm = FALSE))
  
  
})

# Export
saveRDS(data_interpolated, file=file.path(outdir, "Carter_et_2001_common_murre_data.Rds"))


# Plot data
################################################################################

# Calculate stats
stats <- data_interpolated %>% 
  group_by(year, type) %>% 
  summarize(count=sum(count, na.rm=T)) %>% 
  ungroup()

# Plot data
g <- ggplot(stats, aes(x=year, y=count, fill=type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of nests", title="Common murre") +
  scale_x_continuous(breaks=seq(1980, 2020, 5), lim=c(NA, 2020)) +
  # Legend
  scale_fill_discrete(name="") +
  # scale_alpha_manual(name="", values=c(0.7, 1)) +
  # Theme
  theme_bw()
g








