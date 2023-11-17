

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
plotdir <- "figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data.Rds"))

# Read block key
block_key <- readRDS("data/bathymetry/processed/block_key.Rds") 


# Build data
################################################################################

# Format data
data <- data_orig %>% 
  # Simplify data
  # Keep the numeric variables converted to numeric
  # Keep the final categorical variables (delete the original)
  select(logbook_id, year, date, 
         trip_id, set_id, 
         vessel_id_use_type, vessel_id_use, 
         block_id_num, block_type, block_lat_dd, block_long_dd, 
         net_type, depth_fa_num, net_length_fa_num, mesh_size_in_num, buoy_line_depth_ft_num, soak_hr_num, 
         target_spp, spp_code, comm_name, status, catch_n, catch_lb, predator) %>% 
  # Rename
  rename(vessel_id=vessel_id_use,
         vessel_id_type=vessel_id_use_type,
         block_id=block_id_num,
         depth_fa=depth_fa_num, 
         net_length_fa=net_length_fa_num, 
         mesh_size_in=mesh_size_in_num, 
         buoy_line_depth_ft=buoy_line_depth_ft_num, 
         soak_hr=soak_hr_num) 


# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag=element_text(size=7),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g1 <- ggplot(data, aes(x=depth_fa, fill=net_type, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0,1000,10)) +
  # Reference line
  # geom_vline(xintercept=96, linetype="dashed", color="grey30") +
  # Labels
  labs(x="Depth (fathoms)", y="Frequency\n(1000s of logbook entries)", tag="A") +
  # Legend
  scale_fill_discrete(name="Net type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.8, 0.8))
g1

# Plot data
g2 <- ggplot(data, aes(x=soak_hr, fill=net_type, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0, 24*10, 3)) +
  # Reference line
  geom_vline(xintercept=96, linetype="dashed", color="grey30", linewidth=0.3) +
  # Labels
  labs(x="Soak time (hours)", y="Frequency\n(1000s of logbook entries)", tag="B") +
  scale_x_continuous(breaks=seq(0, 24*10, 24)) +
  # Legend
  scale_fill_discrete(name="Net type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot data
g3 <- ggplot(data, aes(x=mesh_size_in, fill=net_type, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0,30,1)) +
  # Reference line
  # geom_vline(xintercept=14, linetype="dashed", color="grey30") +
  # Labels
  labs(x="Mesh size (inches)", y="Frequency\n(1000s of logbook entries)", tag="C") +
  scale_x_continuous(breaks=seq(0, 30, 5)) +
  # Legend
  scale_fill_discrete(name="Net type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Plot data
g4 <- ggplot(data, aes(x=net_length_fa, fill=net_type, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0,10000,100)) +
  # Reference line
  geom_vline(xintercept=2000, linetype="dashed", color="grey30", linewidth=0.3) +
  # Labels
  labs(x="Net length (fathoms)", y="Frequency\n(1000s of logbook entries)", tag="D") +
  scale_x_continuous(breaks=seq(0, 10000, 1000)) + # goes to 10000
  # Legend
  scale_fill_discrete(name="Net type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g4

# Plot data
g5 <- ggplot(data, aes(x=buoy_line_depth_ft, fill=net_type, y=after_stat(count)/1000)) +
  geom_histogram(breaks=seq(0,1000,10)) +
  # Reference line
  # geom_vline(xintercept=96, linetype="dashed", color="grey30") +
  # Labels
  labs(x="Buoy line depth (feet)", y="Frequency\n(1000s of logbook entries)", tag="E") +
  scale_x_continuous(breaks=seq(0, 1000, 100)) + # goes to 1000
  # Legend
  scale_fill_discrete(name="Net type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g5

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, ncol=2)

# Export data
ggsave(g, filename=file.path(plotdir, "FigSX_gillnet_logbook_value_histograms.png"), 
       width=6.5, height=4.75, units="in", dpi=600)


# Vessel stats
stats <- data %>% 
  group_by(vessel_id, net_type) %>% 
  summarize(lengths=n_distinct(net_length_fa),
            meshes=n_distinct(mesh_size_in),
            soaks=n_distinct(soak_hr)) %>% 
  ungroup() %>% 
  gather(key="metric", value="value", 3:ncol(.))

# Plot
g <- ggplot(stats, mapping = aes(y=metric, x=value, fill=net_type)) +
  geom_boxplot() +
  scale_x_continuous(trans="log2", breaks=c(1, 2, 5,10,20,50,100))
g


# Impute data
################################################################################

# Maxes
soak_cap <- 96
length_cap <- 2000

# Build set key
set_key <- data %>% 
  select(year:target_spp) %>% 
  unique()
freeR::which_duplicated(set_key$set_id)

# 3 levels
# Target level - vessel, net, target
# Vessel level - vessel, net
# Fleet level - net, target

# Fleet stats
fleet_stats <- set_key %>% 
  group_by(net_type, target_spp) %>% 
  summarize(soak_hr_med3=median(soak_hr[soak_hr <= soak_cap], na.rm=T),
            mesh_size_in_med3=median(mesh_size_in, na.rm=T),
            net_length_fa_med3=median(net_length_fa[net_length_fa <= length_cap], na.rm=T)) %>% 
  ungroup()

# Vessel stats
vessel_stats <- set_key %>% 
  group_by(vessel_id, net_type) %>% 
  summarize(soak_hr_med2=median(soak_hr[soak_hr <= soak_cap], na.rm=T),
            mesh_size_in_med2=median(mesh_size_in, na.rm=T),
            net_length_fa_med2=median(net_length_fa[net_length_fa <= length_cap], na.rm=T)) %>% 
  ungroup()

# Target stats
target_stats <- set_key %>% 
  group_by(vessel_id, net_type, target_spp) %>% 
  summarize(soak_hr_med1=median(soak_hr[soak_hr <= soak_cap], na.rm=T),
            mesh_size_in_med1=median(mesh_size_in, na.rm=T),
            net_length_fa_med1=median(net_length_fa[net_length_fa <= length_cap], na.rm=T)) %>% 
  ungroup() %>% 
  # Add vessel and fleet stats
  left_join(vessel_stats) %>% 
  left_join(fleet_stats)

# Format data
data2 <- data %>% 
  # Add depth info
  left_join(block_key) %>% 
  # Impute depth
  mutate(depth_fa_imp=case_when(depth_fa > depth_fa_max ~ depth_fa_max,
                                is.na(depth_fa) ~ depth_fa_med,
                                T ~ depth_fa), 
         depth_fa_imp_source=case_when(depth_fa > depth_fa_max ~ "Block maximum",
                                       is.na(depth_fa) & !is.na(depth_fa_med) ~ "Block median",
                                       !is.na(depth_fa) & depth_fa <= depth_fa_max ~ "Reported",
                                       T ~ "Unknown")) %>% 
  # Add stats
  left_join(target_stats) %>% 
  # Impute soak time
  mutate(soak_hr_imp=case_when(!is.na(soak_hr) ~ soak_hr,
                               is.na(soak_hr) & !is.na(soak_hr_med1) ~ soak_hr_med1,
                               is.na(soak_hr) & is.na(soak_hr_med1) & !is.na(soak_hr_med2)~ soak_hr_med2,
                               is.na(soak_hr) & is.na(soak_hr_med1) & is.na(soak_hr_med2) & !is.na(soak_hr_med3) ~ soak_hr_med3,
                               T ~ NA),
         soak_hr_imp_source=case_when(!is.na(soak_hr) ~ "Reported",
                                       is.na(soak_hr) & !is.na(soak_hr_med1) ~ "Target-level",
                                       is.na(soak_hr) & is.na(soak_hr_med1) & !is.na(soak_hr_med2) ~ "Vessel-level",
                                       is.na(soak_hr) & is.na(soak_hr_med1) & is.na(soak_hr_med2) & !is.na(soak_hr_med3) ~ "Fleet-level",
                                       T ~ "Unknown")) %>% 
  # Impute mesh size
  mutate(mesh_size_in_imp=case_when(!is.na(mesh_size_in) ~ mesh_size_in,
                               is.na(mesh_size_in) & !is.na(mesh_size_in_med1) ~ mesh_size_in_med1,
                               is.na(mesh_size_in) & is.na(mesh_size_in_med1) & !is.na(mesh_size_in_med2)~ mesh_size_in_med2,
                               is.na(mesh_size_in) & is.na(mesh_size_in_med1) & is.na(mesh_size_in_med2) & !is.na(mesh_size_in_med3) ~ mesh_size_in_med3,
                               T ~ NA),
         mesh_size_in_imp_source=case_when(!is.na(mesh_size_in) ~ "Reported",
                                      is.na(mesh_size_in) & !is.na(mesh_size_in_med1) ~ "Target-level",
                                      is.na(mesh_size_in) & is.na(mesh_size_in_med1) & !is.na(mesh_size_in_med2) ~ "Vessel-level",
                                      is.na(mesh_size_in) & is.na(mesh_size_in_med1) & is.na(mesh_size_in_med2) & !is.na(mesh_size_in_med3) ~ "Fleet-level",
                                      T ~ "Unknown")) %>% 
  # Impute mesh size
  mutate(net_length_fa_imp=case_when(!is.na(net_length_fa) ~ net_length_fa,
                                    is.na(net_length_fa) & !is.na(net_length_fa_med1) ~ net_length_fa_med1,
                                    is.na(net_length_fa) & is.na(net_length_fa_med1) & !is.na(net_length_fa_med2)~ net_length_fa_med2,
                                    is.na(net_length_fa) & is.na(net_length_fa_med1) & is.na(net_length_fa_med2) & !is.na(net_length_fa_med3) ~ net_length_fa_med3,
                                    T ~ NA),
         net_length_fa_imp_source=case_when(!is.na(net_length_fa) ~ "Reported",
                                           is.na(net_length_fa) & !is.na(net_length_fa_med1) ~ "Target-level",
                                           is.na(net_length_fa) & is.na(net_length_fa_med1) & !is.na(net_length_fa_med2) ~ "Vessel-level",
                                           is.na(net_length_fa) & is.na(net_length_fa_med1) & is.na(net_length_fa_med2) & !is.na(net_length_fa_med3) ~ "Fleet-level",
                                           T ~ "Unknown")) %>% 
  # Apply caps
  mutate(soak_hr_imp=ifelse(!is.na(soak_hr) & soak_hr>soak_cap, soak_cap, soak_hr_imp),
         soak_hr_imp_source=ifelse(!is.na(soak_hr) & soak_hr>soak_cap, "Capped", soak_hr_imp_source),
         net_length_fa_imp=ifelse(!is.na(net_length_fa) & net_length_fa>length_cap, length_cap, net_length_fa_imp),
         net_length_fa_imp_source=ifelse(!is.na(net_length_fa) & net_length_fa>length_cap, "Capped", net_length_fa_imp_source))

# Imputation frequency
table(data2$depth_fa_imp_source) / nrow(data2) * 100
table(data2$soak_hr_imp_source) / nrow(data2) * 100
table(data2$mesh_size_in_imp_source) / nrow(data2) * 100
table(data2$net_length_fa_imp_source) / nrow(data2) * 100



# Simplify data
################################################################################

# Build data
data3 <- data2 %>% 
  # Simplify data
  select(logbook_id, year, date, 
         trip_id, set_id, 
         vessel_id_type, vessel_id, 
         block_id, block_type, block_lat_dd, block_long_dd, 
         net_type, 
         depth_fa_imp, 
         depth_fa_imp_source, 
         net_length_fa_imp, 
         net_length_fa_imp_source,
         mesh_size_in_imp, 
         mesh_size_in_imp_source,
         buoy_line_depth_ft, 
         soak_hr_imp, 
         soak_hr_imp_source,
         target_spp, spp_code, comm_name, status, catch_n, catch_lb, predator) %>% 
  # Rename
  rename(depth_fa=depth_fa_imp, 
         depth_source=depth_fa_imp_source, 
         net_length_fa=net_length_fa_imp, 
         net_length_source=net_length_fa_imp_source,
         mesh_in=mesh_size_in_imp, 
         mesh_source=mesh_size_in_imp_source,
         soak_hr=soak_hr_imp, 
         soak_source=soak_hr_imp_source)


# Plot
################################################################################

# Build new set key
set_key2 <- data3 %>% 
  select(year:target_spp) %>% 
  unique()

# Build imputation proportions
imp_props1 <- set_key2 %>% 
  # Simplify
  select(set_id, depth_source) %>% 
  # Summarize
  mutate(variable="      Depth") %>% 
  count(variable, depth_source) %>% 
  rename(nsets=n, 
         source=depth_source) %>% 
  mutate(props=nsets/sum(nsets)) %>% 
  # Format
  mutate(source=factor(source, levels=c("Reported", "Block maximum", "Block median", "Unknown")))
  

# Build imputation proportions
imp_props2 <- set_key2 %>% 
  # Simplify
  select(set_id, soak_source, mesh_source, net_length_source) %>% 
  # Gather
  gather(key="variable", value="source", 2:ncol(.)) %>% 
  # Summarize
  group_by(variable, source) %>% 
  summarise(nsets=n()) %>% 
  ungroup() %>% 
  group_by(variable) %>% 
  mutate(props=nsets/sum(nsets)) %>% 
  ungroup() %>% 
  # Format
  mutate(variable=recode_factor(variable,
                                "soak_source"="Soak time",
                                "mesh_source"="Mesh size",
                                "net_length_source"="Net length")) %>% 
  mutate(source=factor(source, levels=c("Reported", "Capped", "Target-level", "Vessel-level", "Fleet-level", "Unknown")))


# Plot data
g1 <- ggplot(imp_props1, aes(y=variable, x=props, fill=source)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Proportion of pseduo-sets", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_ordinal(name="Source") +
  # Theme
  theme_bw() +
  theme(axis.title.x=element_blank())
g1

# Plot data
g2 <- ggplot(imp_props2, aes(y=variable, x=props, fill=source)) +
  geom_bar(stat="identity", position = position_fill(reverse = TRUE)) +
  # Labels
  labs(x="Proportion of pseduo-sets", y="") +
  scale_x_continuous(labels=scales::percent) +
  # Legend
  scale_fill_ordinal(name="Source") +
  # Theme
  theme_bw() 
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.3, 0.7), ncol=1)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "CDFW_1981_2020_gillnet_logbook_data_imputed.Rds"))



