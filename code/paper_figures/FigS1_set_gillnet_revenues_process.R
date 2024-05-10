

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
plotdir <-"figures"
indir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/raw"
outdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1980_2022_landings_receipts.Rds"))

# Read logbooks
logdir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
logs_orig <- readRDS(file=file.path(logdir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# Load data
load(file=file.path(outdir, "1980_2022_landings_receipts_set_gillnet_options.Rdata"))


# Build data
################################################################################

# Build data
data1 <- data_orig %>% 
  # Reduce to set gillnets (or potential nets)
  filter(gear %in% c("Trammel net", "Set gill net", "Large mesh set gn", "Small mesh set gn", "Entangling nets")) %>% 
  # Recode gear
  mutate(gear=recode_factor(gear,
                            "Trammel net"="Trammel net",
                            "Set gill net"="Set gillnet",
                            "Large mesh set gn"="LM set gillnet",
                            "Small mesh set gn"="SM set gillnet",
                            "Entangling nets"="Entangling net")) %>% 
  # Recode species
  mutate(comm_name=recode(comm_name,
                          "Bocaccio/chilipepper rockfish group"="Bocaccio/chilipepper"))

# Build data
data2 <- data1 %>% 
  # Reduce to vessels that submit set gillnet logbooks
  filter(vessel_id %in% logs_orig$vessel_id)

# Build data
data3 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% logs_orig$trip_id)

# Build data
data4 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key1$trip_id)

# Build data
data5 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key2$trip_id)

# Build data
data6 <- data2 %>% 
  mutate(trip_id=paste(vessel_id, date, sep="-")) %>% 
  filter(trip_id %in% trip_id_key3$trip_id)


# Function
################################################################################

# Plot data function
data <- data1
plot_data <- function(data, plot_tag, plot_title){
  
  # Build data
  #######################################
  
  # Identify species
  data_spp <- data %>% 
    group_by(comm_name) %>% 
    summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(desc(value_usd)) %>% 
    mutate(rank=1:n(),
           spp_label=ifelse(rank<=10, comm_name, "Other"),
           spp_rank=ifelse(rank<=10, rank, 11))
  
  # Order species
  data_spp_order <- data_spp %>% 
    group_by(spp_label, spp_rank) %>% 
    summarize(value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    arrange(spp_rank)
  
  # Summarize by gear
  data_stats1 <- data %>% 
    group_by(year, gear) %>% 
    summarize(landings_lbs=sum(landings_lbs, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() 
  
  # Summarize by species
  data_stats2 <- data %>% 
    left_join(data_spp %>% select(comm_name, spp_label)) %>% 
    group_by(year, spp_label) %>% 
    summarize(landings_lbs=sum(landings_lbs, na.rm=T),
              value_usd=sum(value_usd, na.rm=T)) %>% 
    ungroup() %>% 
    mutate(spp_label=factor(spp_label, levels=data_spp_order$spp_label))
  
  # Plot data
  #######################################
  
  # Setup theme
  my_theme <-  theme(axis.text=element_text(size=5),
                     axis.title=element_text(size=6),
                     axis.title.x=element_blank(),
                     legend.text=element_text(size=5),
                     legend.title=element_blank(),
                     plot.title=element_text(size=6),
                     plot.tag=element_text(size=6, face="bold"),
                     plot.tag.position = c(0,0.98),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key.size=unit(0.1, "cm"),
                     legend.background = element_rect(fill=alpha('blue', 0)))
  
  # Plot data
  g1 <- ggplot(data_stats1, aes(x=year, y=value_usd/1e6, fill=gear)) +
    geom_bar(stat="identity", color="grey30", linewidth=0.2) +
    # Labels
    labs(x="Year", y="Revenues (USD millions)", title=plot_title, tag=plot_tag) +
    # Legend
    scale_fill_discrete(name="Gear") +
    # Theme 
    theme_bw() + my_theme +
    theme(legend.position = c(0.72 ,0.88))
  g1
  
  # Plot data
  g2 <- ggplot(data_stats2, aes(x=year, y=value_usd/1e6, fill=spp_label)) +
    geom_bar(stat="identity", color="grey30", linewidth=0.2) +
    # Labels
    labs(x="Year", y="Revenues (USD millions)", title=" \n ", tag=" ") +
    # Legend
    scale_fill_discrete(name="Species") +
    # Theme 
    theme_bw() + my_theme +
    theme(legend.position = c(0.7, 0.77),
          axis.title.y=element_blank())
  g2
  
  # Merge
  g <- gridExtra::grid.arrange(g1, g2, nrow=1)
  g
  
}

# Plot data
################################################################################

# Individual plots
g1 <- plot_data(data=data1,
                plot_tag="A",
                plot_title=" \nEntangling net vessels")
g2 <- plot_data(data=data2,
                plot_tag="B",
                plot_title=" \nSet gillnet vessels")
g3 <- plot_data(data=data3,
                plot_tag="C",
                plot_title="Set gillnet vessels\n(0 day logged trip buffer)")
g4 <- plot_data(data=data4,
                plot_tag="D",
                plot_title="Set gillnet vessels\n(1 day logged trip buffer)")
g5 <- plot_data(data=data5,
                plot_tag="E",
                plot_title="Set gillnet vessels\n(2 day logged trip buffer)")
g6 <- plot_data(data=data6,
                plot_tag="F",
                plot_title="Set gillnet vessels\n(3 day logged trip buffer)")

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, g6)

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_set_gillnet_revenues_process.png"), 
       width=6.5, height=6.5, units="in", dpi=600)


