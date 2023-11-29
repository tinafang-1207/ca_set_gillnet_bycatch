

# Read data
################################################################################

# Clear workspace
rm(list = ls())
options(scipen=999)

# Packages
library(tidyverse)

# Directories
indir <- "data/stock_assessments/raw"
outdir <- "data/stock_assessments/processed"
plotdir <- "figures"

# Read data
data <- readRDS(file=file.path(outdir, "stock_assessment_data.Rds")) %>% 
  mutate(type=recode_factor(type,
                            "Observed"="Reported",
                            "Interpolated"="Interpolated"))

# Read PBRs 
pbrs_orig <- readxl::read_excel(file.path(outdir, "pbrs.xlsx")) %>% 
  rename(region=stock) %>% 
  mutate(stock=ifelse(species=="Harbor porpoise", paste0(species, " (", region, ")"), species))


# Build PBR key
################################################################################

# PBR key
pbr_key <- data %>% 
  # XY coord
  group_by(stock, year) %>% 
  summarise(n_hi=sum(n_hi),
            n_mid=sum(n_mid)) %>% 
  ungroup() %>% 
  group_by(stock) %>% 
  summarize(x=min(year),
            y1=max(n_hi),
            y2=max(n_mid),
            y=pmax(y1, y2, na.rm = T) * 0.95) %>% 
  ungroup() %>% 
  select(-c(y1, y2)) %>% 
  # Add PBR
  left_join(pbrs_orig %>% select(stock, pbr)) %>% 
  mutate(pbr_label=paste0("PBR: ", pbr))
  


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_blank(),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.margin = margin(-7, 0, -7, 0),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Sea lion
data1 <- data %>% filter(stock=="California sea lion")
pbr1 <- pbr_key %>% filter(stock=="California sea lion")
g1 <- ggplot(data1, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # PBR
  geom_text(data=pbr1, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.2, color="black") +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g1

# Sea lion
data2 <- data %>% 
  filter(stock=="Northern elephant seal") %>% 
  mutate(region=recode_factor(region,
                              "Central California"="Mainland",
                              "Channel Islands"="Channel Islands"))
pbr2 <- pbr_key %>% filter(stock=="Northern elephant seal")
g2 <- ggplot(data2, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # PBR
  geom_text(data=pbr2, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.2, color="black", inherit.aes = F) +
  # Labels
  labs(x="Year", y="Number of births") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_fill_ordinal(name="") +
  scale_alpha_manual(name="", values=c(1,0.6)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.28, 0.7))
g2

# Harbor seal
data3 <- data %>% filter(stock=="Harbor seal") %>% 
  mutate(region=factor(region, levels=c("Mainland", "Channel Islands")))
pbr3 <- pbr_key %>% filter(stock=="Harbor seal")
g3 <- ggplot(data3, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # PBR
  geom_text(data=pbr3, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.2, color="black", inherit.aes = F) +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_fill_ordinal(name="") +
  scale_alpha_manual(name="", values=c(1,0.6)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Harbor porpoise 
data4 <- data %>% filter(stock=="Harbor porpoise (Monterey Bay)")
pbr4 <- pbr_key %>% filter(stock=="Harbor porpoise (Monterey Bay)")
g4 <- ggplot(data4, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # PBR
  geom_text(data=pbr4, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.2, color="black") +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g4

# Harbor porpoise 
data5 <- data %>% filter(stock=="Harbor porpoise (Morro Bay)")
pbr5 <- pbr_key %>% filter(stock=="Harbor porpoise (Morro Bay)")
g5 <- ggplot(data5, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0, color="grey30", linewidth=0.2) +
  # PBR
  geom_text(data=pbr5, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.2, color="black") +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g5

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, ncol=3)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig8_population_sizes.png"), 
       width=6.5, height=4, units="in", dpi=600)


