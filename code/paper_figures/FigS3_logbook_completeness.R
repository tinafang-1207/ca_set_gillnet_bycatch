

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"

# Read data 
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))


# Build data
################################################################################

# Build set keys
table(data_orig$net_type)
sets <- data_orig %>% 
  # Flter
  filter(net_type=="Set") %>% 
  # Set meta-data
  select(logbook_id:target_spp_first) %>% 
  unique() %>% 
  # Meta-data of interest
  select(date, block_id, depth_fa_num, 
         net_length_fa_num, mesh_size_in_num,
         soak_hr_num, target_spp, target_spp_first)

# Add 1st target
sets1 <- sets %>% 
  # Reduce to species longer than two character
  filter(nchar(target_spp_first)>=3) %>% 
  # Calculate median
  group_by(target_spp_first) %>% 
  mutate(mesh_med=median(mesh_size_in_num, na.rm=T)) %>% ungroup() %>% 
  mutate(target_spp_first=stringr::str_to_sentence(target_spp_first))

# Completeness stats
comp_percs <- freeR::complete(sets) / nrow(sets)
comp_df <- tibble(variable=names(comp_percs),
                  perc=comp_percs %>% as.numeric()) %>% 
  filter(perc!=0 & !variable%in%c("date", "target_spp_first")) %>% 
  # Format
  mutate(variable=recode(variable,
                         "block_id"=" Block id",              
                         "depth_fa_num"="Depth (fathoms)",          
                         "net_length_fa_num"="Net length (fathoms)",      
                         "mesh_size_in_num"="Mesh size (in)", 
                         "soak_hr_num"="Soak time (hr)",          
                         "target_spp"="Target species")) %>% 
  # Order
  arrange(desc(perc)) 


# Plot data
################################################################################

# Base theme
hist_theme <- theme(axis.text=element_text(size=6),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=7),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=6),
                    plot.tag=element_text(size=7),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

boxplot_theme <-   theme(axis.text=element_text(size=6),
                         axis.title=element_text(size=7),
                         legend.text=element_text(size=5),
                         legend.title=element_text(size=6),
                         plot.tag=element_text(size=7),
                         plot.title = element_blank())

# Plot data
g1 <- ggplot(comp_df, aes(y=reorder(variable, desc(perc)), x=perc)) +
  geom_point() +
  # Labels
  labs(x="Percent incomplete", y="Data attribute", tag="A") +
  scale_x_continuous(trans="log10", 
                     lim=c(0.001, 1), 
                     breaks=c(0.001, 0.01, 0.1, 1),
                     labels=c("0.1%", "1%", "10%", "100%")) +
  # Theme
  theme_bw() + boxplot_theme
g1

# Plot 
g2 <- ggplot(sets, aes(x=soak_hr_num/24, y=after_stat(count)/1000)) +
  geom_histogram(binwidth = 0.25) +
  # Labels
  labs(x="Soak time (days)", y="Thousands of pseudo-sets", tag="B") +
  scale_x_continuous(breaks=seq(0, 11, 1), lim=c(0, 11),
                     labels=c(breaks=seq(0, 10, 1), "≥11")) +
  # Plot maximum
  geom_vline(xintercept=10, linetype="dotted", color="grey30") +
  # Theme
  theme_bw() + hist_theme
g2

# Plot 
g3 <- ggplot(sets, aes(x=depth_fa_num, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0,2000,10)) +
  # Labels
  labs(x="Depth (fathoms)", y="Thousands of pseudo-sets", tag="C") +
  scale_x_continuous(lim=c(0,300),
                     breaks=seq(0,300,100),
                     labels=c(seq(0,200, 100), "≥300")) +
  # Theme
  theme_bw() + hist_theme
g3

# Plot 
g4 <- ggplot(sets1, aes(x=mesh_size_in_num, y=reorder(target_spp_first, desc(mesh_med)))) +
  geom_boxplot(outlier.shape=21, linewidth=0.2, outlier.size = 0.3) +
  # Labels
  labs(x="Mesh size (in)", y="Target species", tag="D") +
  scale_x_continuous(breaks=c(0, 8.5, 20, 30),
                     labels=c("0", "8.5", "20", "30")) +
  # Theme
  theme_bw() + boxplot_theme
g4

# Plot 
g5 <- ggplot(sets, aes(x=mesh_size_in_num, y=after_stat(count)/1000)) +
  geom_histogram() +
  # Labels
  labs(x="Mesh size (in)", y="Thousands of pseudo-sets", tag="E") +
  scale_x_continuous(breaks=c(0, 3.5, 8.5, seq(15, 35, 5)),
                     label=c("0", "3.5", "8.5", "15", "20", "25", "30", "35")) +
  # Theme
  theme_bw() + hist_theme
g5

# Plot 
g6 <- ggplot(sets, aes(x=net_length_fa_num, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0, 12000,100)) +
  # Labels
  labs(x="Net length (fathoms)", y="Thousands of pseudo-sets", tag="F") +
  scale_x_continuous(lim=c(0,5000),
                     breaks=seq(0,5000,1000),
                     labels=c(seq(0,4000,1000), "≥5000")) +
  # Plot maximum
  geom_vline(xintercept=2000, linetype="dotted", color="grey30") +
  # Theme
  theme_bw() + hist_theme
g6

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6,  ncol=3)
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS3_logbook_data_imputation.png"), 
       width=6.5, height=4.5, units="in", dpi=600)



