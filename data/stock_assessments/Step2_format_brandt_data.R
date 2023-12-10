

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
data_orig1 <- readxl::read_excel(file.path(indir, "Capitola_etal_2012_table1&2.xlsx"), sheet="Table 1", na="ND")
data_orig2 <- readxl::read_excel(file.path(indir, "Capitola_etal_2012_table1&2.xlsx"), sheet="Table 2", na="ND")
colony_key_orig <- readxl::read_excel(file.path(indir, "Capitola_etal_2012_table1&2.xlsx"), sheet="Colonies")


# Format data
################################################################################

# Format data
data1 <- data_orig1 %>% 
  gather(key="year", value="nests_n", 4:ncol(.)) %>% 
  mutate(year=as.numeric(year))

# Format data
data2 <- data_orig2 %>% 
  gather(key="year", value="nests_n", 4:ncol(.)) %>% 
  mutate(year=as.numeric(year))

# Merge
data <- bind_rows(data1, data2) %>% 
  # Recode colonies
  mutate(colony=recode(colony,
                       "Pup Rock and Adj. Mainland"="Pup Rock & Adj. Mainland",
                       "Two Rks S of Piedras Blancas"="Two Rocks S of Piedras Blancas")) %>% 
  # Add colont info
  left_join(colony_key_orig)

# Inspect
freeR::complete(data)

# Inspect colonies
sort(unique(data$colony))


# Impute data
################################################################################

# Colonies
colonies <- unique(data$colony)
x <- colonies[1]
data_interpolated <- purrr::map_df(colonies, function(x){
  
  # Subset data
  sdata <- data %>% 
    filter(colony==x & !is.na(nests_n))
  
  # Create a complete sequence of years from the minimum to maximum
  complete_years <- seq(min(sdata $year), max(sdata$year), by = 1)
  
  # Create a template dataframe with all years
  template_df <- data.frame(region=sdata$region %>% unique(),
                            colony=sdata$colony %>% unique(),
                            year=complete_years,
                            type="Observed")
  
  # Merge the template dataframe with the original dataframe
  merged_df <- merge(template_df, sdata %>% select(year, nests_n), by = "year", all.x = TRUE) %>% 
    # Add a column to indicate obs/inter
    mutate(type=ifelse(is.na(nests_n), "Interpolated", type))
  
  # Linearly interpolate missing values
  interpolated_df <- merged_df %>%
    arrange(year) %>%
    mutate(nests_n = zoo::na.approx(nests_n, na.rm = FALSE))
  
  
})

# Export
saveRDS(data_interpolated, file=file.path(outdir, "Capitola_et_2012_brandts_cormorant_data.Rds"))

# Plot data
################################################################################

# Calculate stats
stats <- data_interpolated %>% 
  group_by(year, region, type) %>% 
  summarize(nests_n=sum(nests_n, na.rm=T)) %>% 
  ungroup()


# Plot data
g <- ggplot(stats, aes(x=year, y=nests_n, fill=region, alpha=type)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="Year", y="Number of nests", title="Brandt's cormorant") +
  scale_x_continuous(breaks=seq(1980, 2020, 5), lim=c(NA, 2020)) +
  # Legend
  scale_fill_discrete(name="") +
  scale_alpha_manual(name="", values=c(0.7, 1)) +
  # Theme
  theme_bw()
g








