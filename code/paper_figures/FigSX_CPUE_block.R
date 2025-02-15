
### clean working environment ###
rm(list = ls())

### load in packages ###
library(tidyverse)

### read in data ###
datadir <- "/Users/yutianfang/Dropbox/ca_set_gillnet_bycatch/confidential/logbooks/processed/"
data_orig <- readRDS(file.path(datadir, "CDFW_1981_2020_gillnet_logbook_data_use.Rds"))

# world
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")

# blocks
blocks <- wcfish::blocks
blocks_df <- blocks %>% sf::st_drop_geometry()

block_key <- readRDS("data/strata/block_strata_key.Rds")

# State waters
state_waters <- readRDS(file.path("data/gis_data/CA_state_waters_polyline.Rds"))

# risk contour
sl_contour <- sf::st_read("data/gis_data/predict_risk_contour/sealion_predict_risk_contour.shp")
hs_contour <- sf::st_read("data/gis_data/predict_risk_contour/harbor_seal_predict_risk_contour.shp")


# Format data
###############################################

# Reg years
reg_years <- c(1987, 1994, 2002)
reg_dates <- lubridate::ymd(c("1987-05-14",
                              "1994-01-01",
                              "2002-04-26"))

# Block ids
ci_blocks <- c(684:690, 707:713, 813:814, 760:762, 806:807, 829, 850, 849, 867, 765)
ventura_blocks <- c(651:663, 664:677, 697, 776, 691:696, 714:717, 701:706, 678:683)

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
  filter(year >= 2002 & year <= 2021) %>%
  filter(period == "4. 2002-present") %>%
  mutate(fishing_season = case_when(quarter %in% c("Spring", "Fall", "Winter")~"Other months",
                                    quarter == "Summer"~"Hotspot season"))
  

# build block data
stats_blocks <- data %>%
  # Summarize by period and block
  group_by(fishing_season, block_id) %>% 
  summarize(nvessels=n_distinct(vessel_id),
            nvesseldays=n_distinct(trip_id)) %>% 
  ungroup() %>% 
  # Calculate proportion by period
  group_by(fishing_season) %>% 
  mutate(prop=nvesseldays/sum(nvesseldays)) %>% 
  ungroup() %>% 
  # Add block props
  mutate(block_id=as.numeric(block_id)) %>% 
  left_join(blocks_df %>% select(block_id, block_type, block_lat_dd, block_long_dd), by="block_id") %>% 
  filter(block_type=="Inshore") %>% 
  # Remove confidential data
  filter(nvessels>=3)

stats_blocks_sf <- stats_blocks %>% 
  left_join(blocks %>% select(block_id)) %>% 
  sf::st_as_sf()

# Plot data
################################################################################

# Bays
bays_df <- matrix(c("San\nFrancisco", 37.8, -122.1,
                    "Monterey\nBay", 36.8, -121.7,
                    "Morro\nBay", 35.4, -120.7,
                    "Ventura", 34.7, -120,
                    "Southern\nCalifornia", 34.2,  -118.5,
                    "Channel\nIslands", 33, -122.2), ncol=3, byrow = T) %>% 
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
g1 <- ggplot() +
  geom_sf(data = stats_blocks_sf, mapping = aes(fill = prop)) +
  facet_wrap(~fishing_season, nrow = 1) +
  # Blocks
  geom_sf(color="grey50", linewidth=0.1) +
  # State waters
  #geom_sf(data=state_waters, color="grey30", linewidth=0.2, inherit.aes = F) +
  # Plot Point Arguello
  # geom_hline(yintercept = 34.577201, linewidth=0.2) +
  # geom_segment(x=-122.5, xend=-117, y=34.577201, yend=34.577201, linewidth=0.2) +
  geom_segment(data=lat_df, mapping=aes(y=y, yend=yend, x=x, xend=xend), inherit.aes = F, linewidth=0.2) +
  # Land
  geom_sf(data = usa, fill = "grey85", col = "white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", linewidth=0.2, inherit.aes = F) +
  geom_sf(data = sl_contour, color = "red") +
  geom_sf(data = hs_contour, color = "blue") +
  # Crop
  coord_sf(xlim = c(-122, -117), ylim = c(32.3, 35.5)) +
  # Axes
  scale_x_continuous(breaks=seq(-124, -118, 2)) +
  scale_y_continuous(breaks=seq(32, 38, 2)) +
  # Legend
  scale_fill_gradientn(name="% of trips", 
                       colors=RColorBrewer::brewer.pal(9, "Spectral") %>% rev(),
                       trans="log10",
                       breaks=c(0.01, 0.1, 1, 10)/100,
                       labels = c("0.01%", "0.1%", "1%", "10%")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", frame.linewidth = 0.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text=element_text(size=6),
        axis.title.x=element_blank(),
        legend.key.size = unit(0.25, "cm"),
        legend.position = c(0.6, 0.25),
        plot.subtitle=element_text(size=4, face="italic"))
g1


