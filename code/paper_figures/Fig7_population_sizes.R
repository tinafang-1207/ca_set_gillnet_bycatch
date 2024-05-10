

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
  mutate(region=recode(region, 
                       "North of Piedras Blancas"="North of PB",
                       "South of Piedras Blancas"="South of PB")) %>% 
  mutate(stock_label=recode(stock_label,
                            "Harbor porpoise (San Francisco-Russian River)"="Harbor porpoise (SF-Russian River)")) %>% 
  mutate(stock_label=gsub(" \\(", "\n(", stock_label)) %>% 
  mutate(type=recode_factor(type,
                            "Observed"="Reported",
                            "Interpolated"="Interpolated"))
unique(data$stock_label)

# Read PBRs 
pbrs_orig <- readxl::read_excel(file.path(outdir, "pbrs.xlsx")) %>% 
  mutate(stock=recode(stock,
                     "San Francisco-Russian River"="SF-Russian River")) %>% 
  mutate(stock_label=paste0(species, "\n(", stock, ")"))


# Build PBR key
################################################################################

# PBR key
pbr_key <- data %>% 
  # XY coord
  group_by(stock_label, species, year) %>% 
  summarise(n_hi=sum(n_hi),
            n_mid=sum(n_mid)) %>% 
  ungroup() %>% 
  group_by(stock_label, species) %>% 
  summarize(x=min(year),
            y1=max(n_hi),
            y2=max(n_mid),
            y=pmax(y1, y2, na.rm = T) * 0.97) %>% 
  ungroup() %>% 
  select(-c(y1, y2)) %>% 
  # Add PBR
  left_join(pbrs_orig %>% select(stock_label, pbr), by="stock_label") %>% 
  mutate(pbr_label=paste0("PBR: ", prettyNum(pbr, ",")))
  


# Plot data
################################################################################

# Reg years
reg_years <- c(1987, 1994, 2002)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=7),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_blank(),
                   strip.text=element_text(size=6),
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
data1 <- data %>% filter(species=="California sea lion")
pbr1 <- pbr_key %>% filter(species=="California sea lion")
g1 <- ggplot(data1, aes(x=year, y=n_mid)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr1, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black") +
  # Labels
  labs(x="Year", y="Number of sea lions") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g1

# Sea lion
data2 <- data %>% 
  filter(species=="Northern elephant seal") %>% 
  mutate(region=recode_factor(region,
                              "Central California"="Mainland",
                              "Channel Islands"="Channel Islands"))
pbr2 <- pbr_key %>% filter(species=="Northern elephant seal")
g2 <- ggplot(data2, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr2, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black", inherit.aes = F) +
  # Labels
  labs(x="Year", y="Number of births") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_fill_ordinal(name="") +
  scale_alpha_manual(name="", values=c(1,0.6)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.31, 0.72))
g2

# Harbor seal
data3 <- data %>% filter(species=="Harbor seal") %>% 
  mutate(region=factor(region, levels=c("Mainland", "Channel Islands")))
pbr3 <- pbr_key %>% filter(species=="Harbor seal")
g3 <- ggplot(data3, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", position=position_stack(reverse = TRUE)) +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr3, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black", inherit.aes = F) +
  # Labels
  labs(x="Year", y="Number of seals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_fill_ordinal(name="") +
  scale_alpha_manual(name="", values=c(1,0.6)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g3

# Harbor porpoise 
data4 <- data %>% filter(stock_label=="Harbor porpoise\n(Morro Bay)")
pbr4 <- pbr_key %>% filter(stock_label=="Harbor porpoise\n(Morro Bay)")
g4 <- ggplot(data4, aes(x=year, y=n_mid)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr4, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black") +
  # Labels
  labs(x="Year", y="Number of porpoises") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g4

# Harbor porpoise 
data5 <- data %>% filter(stock_label=="Harbor porpoise\n(Monterey Bay)")
pbr5 <- pbr_key %>% filter(stock_label=="Harbor porpoise\n(Monterey Bay)")
g5 <- ggplot(data5, aes(x=year, y=n_mid)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr5, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black") +
  # Labels
  labs(x="Year", y="Number of porpoises") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g5

# Harbor porpoise 
data6 <- data %>% filter(stock_label=="Harbor porpoise\n(SF-Russian River)")
pbr6 <- pbr_key %>% filter(stock_label=="Harbor porpoise\n(SF-Russian River)")
g6 <- ggplot(data6, aes(x=year, y=n_mid)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", fill="grey70") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  geom_text(data=pbr6, mapping=aes(x=x, y=y, label=pbr_label), 
            hjust=0, size=2.0, color="black") +
  # Labels
  labs(x="Year", y="Number of porpoises") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g6

# Brandt's cormorant
data7 <- data %>% filter(stock_label=="Brandt's cormorant\n(Central California)")
pbr7 <- pbr_key %>% filter(stock_label=="Brandt's cormorant\n(Central California)")
g7 <- ggplot(data7, aes(x=year, y=n_mid, fill=region, alpha=type)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # PBR
  # geom_text(data=pbr7, mapping=aes(x=x, y=y, label=pbr_label), 
  #           hjust=0, size=2.0, color="black", inherit.aes = F) +
  # Labels
  labs(x="Year", y="Number of nests") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_fill_manual(name="", values=RColorBrewer::brewer.pal(2, "Set1")) +
  scale_alpha_manual(name="", values=c(1,0.6), guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.26, 0.92))
g7

# Common murre
data8 <- data %>% filter(stock_label=="Common murre\n(Central California)")
g8 <- ggplot(data8, aes(x=year, y=n_mid, alpha=type)) +
  facet_wrap(~stock_label, scale="free", ncol=3) +
  geom_bar(stat="identity", position = position_stack(reverse = TRUE)) +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), 
                width=0, color="grey30", linewidth=0.2) +
  # Ref line
  geom_vline(xintercept=reg_years-0.5, color="grey75", linetype="dotted", linewidth=0.2) +
  # Labels
  labs(x="Year", y="Number of breeding birds") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Legend
  scale_alpha_manual(name="", values=c(1,0.6), guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.26, 0.92))
g8

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, g3, g7, g6, g4, g5, g8, ncol=4)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig7_population_sizes.png"), 
       width=6.5, height=3.7, units="in", dpi=600)


