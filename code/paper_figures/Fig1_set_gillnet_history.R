

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
gisdatadir <- "data/gis_data"

# Read data
datadir <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# Read landings reciepts
datadir2 <- "/Users/cfree/Dropbox/ca_set_gillnet_bycatch/confidential/landings_receipts/processed"
receipts_orig <- readRDS(file=file.path(datadir2, "1980_2022_set_gillnet_revenues_by_spp_catg_afi.Rds")) %>% 
  filter(year!=2022)

# World
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# Blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

# Strata 
block_key <- readRDS("data/strata/block_strata_key.Rds")

# State waters
state_waters <- readRDS(file.path(gisdatadir, "CA_state_waters_polyline.Rds"))


# Format data
################################################################################

# Inspect
# freeR::complete(data_orig)

# Reg years
reg_years <- c(1987, 1994, 2002)
reg_dates <- lubridate::ymd(c("1987-05-14",
                              "1994-01-01",
                              "2002-04-26"))

# Block ids
ci_blocks <- c(684:690, 707:713, 813:814, 760:762, 806:807, 829, 850, 849, 867, 765)
ventura_blocks <- c(651:663, 664:677, 697, 776, 691:696, 714:717, 701:706, 678:683) 

# Build data
data <- data_orig %>%
  # Add period
  # mutate(period=cut(year, breaks=c(1980, reg_years, 2023), 
  #                   labels=c("1. 1981-1986", "2. 1987-1993", "3. 1994-2001", "4. 2002-present"), right=F)) %>% 
  mutate(period=cut(date, breaks=c(lubridate::ymd("1980-01-01"), reg_dates, lubridate::ymd("2023-12-31")), 
                    labels=c("1. 1981-1986", "2. 1987-1993", "3. 1994-2001", "4. 2002-present"), right=F)) %>% 
  # Add quarter
  mutate(month=lubridate::month(date),
         quarter=case_when(month %in% c(12,1,2) ~ "Winter",
                           month %in%c(3,4,5) ~ "Spring",
                           month %in% c(6,7,8) ~ "Summer",
                           month %in% c(9,10,11) ~ "Fall",
                           T ~ "Unknown")) %>% 
  # Add strata
  left_join(block_key) %>% 
  # Filter
  filter(year<=2021)

# Inspect period key
period_key <- data %>% 
  count(period, year)

# Build annual time series
stats_yr <- data %>%
  # Summarize
  group_by(year) %>%
  summarize(nsets=n_distinct(set_id),
            nvesseldays=n_distinct(trip_id),
            nvessels=n_distinct(vessel_id))  %>%
  ungroup()

# Build annual effort by strata
stats_yr_strata <- data %>%
  # Summarize
  group_by(year, strata) %>% 
  summarize(nsets=n_distinct(set_id),
            nvesseldays=n_distinct(trip_id),
            nvessels=n_distinct(vessel_id))  %>%
  ungroup() %>% 
  # Make sure non-confidential 
  filter(nvessels>=3) %>% 
  # Format strata
  mutate(strata=ifelse(is.na(strata), "Unknown", strata),
         strata=factor(strata, levels=c("Southern California",
                                        "Channel Islands",
                                        "Ventura",
                                        "Morro Bay",
                                        "Monterey Bay",
                                        "San Francisco",
                                        "Unknown")))

# Export data
write.csv(stats_yr, file=file.path(datadir, "CA_3.5in_set_gillnet_effort_by_year.csv"), row.names=F)

# Build block data
stats_blocks <- data %>%
  # Summarize by period and block
  group_by(period, block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nvesseldays=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Calculate proportion by period
  group_by(period) %>% 
  mutate(prop=nvesseldays/sum(nvesseldays)) %>% 
  ungroup() %>% 
  # Add block props
  mutate(block_id=as.numeric(block_id)) %>% 
  left_join(blocks_df %>% select(block_id, block_type, block_lat_dd, block_long_dd), by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  # Adjust longitude to stagger
  mutate(long_adj=recode(period,
                         "1. 1981-1986"=3,
                         "2. 1987-1993"=2,
                         "3. 1994-2001"=1,
                         "4. 2002-present"=0,
                         ) %>% as.numeric()) %>% 
  mutate(block_long_dd_adj = block_long_dd- long_adj) %>% 
  # Remove confidential data
  filter(nvessels>=3)

# Spatialize
stats_blocks_sf <- stats_blocks %>% 
  left_join(blocks %>% select(block_id)) %>% 
  sf::st_as_sf()

# Inspect year key
count(data, period, year)


# Plot data
################################################################################

# Bays
bays_df <- matrix(c("San\nFrancisco", 37.8, -122.1,
                    "Monterey\nBay", 36.8, -121.7,
                    "Morro\nBay", 35.4, -120.7,
                    "Ventura", 34.7, -120,
                    "Southern\nCalifornia", 34.2,  -118.5), ncol=3, byrow = T) %>% 
  as.data.frame() %>% setNames(c("bay", "lat_dd", "long_dd")) %>% 
  mutate(lat_dd=as.numeric(lat_dd),
         long_dd=as.numeric(long_dd)) %>% 
  # Add period
  mutate(period="1. 1981-1986" %>% factor(levels=levels(data$period)))

# Reg data
reg_data <- tibble(year=reg_years,
                   yval=c(380, 280, 180),
                   label=c("1987\n40 fathom depth restriction",
                           "1994\nMainland state waters exclusion",
                           "2002\n60 fathom depth\nrestriction"))

# Base theme
base_theme <- theme(axis.text=element_text(size=7),
                    axis.text.y = element_text(angle = 90, hjust = 0.5),
                    axis.title=element_text(size=8),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=7),
                    strip.text = element_text(size=8),
                    plot.tag =element_text(size=9),
                    plot.title=element_blank(),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.key = element_rect(fill = NA, color=NA),
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Strata lines
lats <- c(33.84, 34.5, 35.66, 37.33, 38.84)
xends <- c(-121.5, -122, -122.5, -124, -125)
lat_df <- tibble(x=-117,
                 xend=xends,
                 y=lats,
                 yend=lats)

# Plot map
g1 <- ggplot(data=stats_blocks_sf, mapping=aes(fill=prop)) +
  facet_wrap(~period, nrow=1) +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # State waters
  geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
  # Plot Point Arguello
  # geom_hline(yintercept = 34.577201, linewidth=0.2) +
  # geom_segment(x=-122.5, xend=-117, y=34.577201, yend=34.577201, linewidth=0.2) +
  geom_segment(data=lat_df, mapping=aes(y=y, yend=yend, x=x, xend=xend), inherit.aes = F, linewidth=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  # Plot bay labels
  geom_text(data=bays_df, mapping = aes(x=long_dd, y=lat_dd, label=bay), 
            inherit.aes = F, color="grey30",fontface="italic",  
            hjust=0, nudge_x=0.1, size=1.8, lineheight = 0.8) +
  # Labels
  labs(x="", y=" ", tag="A") + #subtitle="Blocks visited by fewer than 3 vessels hidden to comply with rule-of-three") +
  # Crop
  coord_sf(xlim = c(-124, -117), ylim = c(32.3, 38.5)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -118, 2)) +
  scale_y_continuous(breaks=seq(32, 38, 2)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       labels = scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text=element_text(size=6),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.81, 0.25),
        plot.subtitle=element_text(size=4, face="italic"))
g1

# Number of vessels
g2 <- ggplot(stats_yr, aes(x=year, y=nvessels)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", linewidth=0.25) +
  geom_text(data=reg_data, mapping=aes(x=year, y=yval, label=label), 
            color="grey30", vjust=1, hjust=0, size=1.8, nudge_x=1) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Number of vessels", tag="B") +
  lims(y=c(0, NA)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank())
g2

# # Number of vessel days (line graph version)
# g3 <- ggplot(stats_yr, aes(x=year, y=nvesseldays)) +
#   # Reference lines
#   geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", linewidth=0.25) +
#   # Data
#   geom_line() +
#   # Labels
#   labs(x="Year", y="Number of vessel days", tag="C") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title.x=element_blank())
# g3

# Number of vessel days (bar graph version)
g3 <- ggplot(stats_yr_strata, aes(x=year, y=nvesseldays, fill=strata)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", linewidth=0.25) +
  # Data
  geom_bar(stat="identity", color="grey30", linewidth=0.1,
           position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Year", y="Number of vessel days", tag="C") +
  # Legend
  scale_fill_ordinal(name="") +
  guides(fill = guide_legend(reverse=TRUE)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.18, "cm"))
g3

# Revenues
g4 <- ggplot(receipts_orig, aes(x=year, y=value_usd2022/1e6, fill=spp_catg)) +
  # Reference lines
  geom_vline(xintercept=reg_years, linetype="dotted", color="grey70", linewidth=0.25) +
  # Data
  geom_bar(stat="identity", color="grey30", linewidth=0.1,
           position = position_stack(reverse = TRUE)) +
  # Labels
  labs(x="Year", y="Ex-vessel revenues\n(2022 USD millions)", tag="D") +
  scale_y_continuous(breaks=seq(0,12,2)) +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(4, "Set3")) +
  guides(fill = guide_legend(reverse=TRUE)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        legend.position = c(0.7, 0.8),
        legend.key.size = unit(0.18, "cm"))
g4


# Merge plots
layout_matrix <- matrix(c(1,1,1,
                          2,3,4), ncol=3, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, 
                             layout_matrix=layout_matrix, heights=c(0.57, 0.43))
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig1_set_gillnet_history.png"),
       width=6.5, height=4, units="in", dpi=600)




# # Merge plots
# layout_matrix <- matrix(c(1,3,
#                           1,4,
#                           2,5), ncol=2, byrow=T)
# g <- gridExtra::grid.arrange(g1, g2, g3, g4, #g5,
#                              layout_matrix=layout_matrix)
# g

# # Plot map
# g1 <- ggplot() +
#   # Blocks
#   # geom_sf(data=blocks, fill=NA, color="grey70", size=0.1) +
#   # Plot data
#   geom_point(data=stats_blocks, 
#              mapping=aes(x=block_long_dd_adj, y=block_lat_dd, color=period, size=prop)) +
#   # State waters
#   geom_sf(data=state_waters, color="grey40", size=0.15) +
#   # Plot Point Arguello
#   geom_hline(yintercept = 34.577201, lwd=0.2) +
#   # Land
#   geom_sf(data = usa, fill = "grey85", col = "white", size = 0.2) +
#   geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
#   # Labels
#   labs(x="", y="", tag="A") +
#   # Crop
#   # coord_sf(xlim = c(-122.5, -117), ylim = c(32.3, 37)) + # southern CA
#   coord_sf(xlim = c(-126.5, -117), ylim = c(32.3, 38.5)) +
#   # Legend
#   scale_color_discrete(name="Period") +
#   scale_size_continuous(name="% of trips", labels = scales::percent) +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title=element_blank(),
#         legend.key.size = unit(0.3, "cm"),
#         legend.position = c(0.8, 0.8))
# g1


# # Amount of landings
# g5 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
#   geom_line() +
#   # Labels
#   labs(x="Year", y="Landings (millions of lbs)", tag="D") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title.x=element_blank())
# g5
# 
# # Amount of revenues
# g6 <- ggplot(data, aes(x=year, y=landings_lb/1e6)) +
#   geom_line() +
#   # Labels
#   labs(x="Year", y="Revenues (USD millions)", tag="E") +
#   # Theme
#   theme_bw() + base_theme +
#   theme(axis.title.x=element_blank())
# g6