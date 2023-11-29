

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
data <- readRDS(file=file.path(outdir, "stock_assessment_data.Rds"))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.position = c(0.2, 0.8),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Sea lion
data1 <- data %>% filter(stock=="California sea lion")
g1 <- ggplot(data1, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g1

# Sea lion
data2 <- data %>% filter(stock=="Northern elephant seal")
g2 <- ggplot(data2, aes(x=year, y=n_mid, fill=region)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Labels
  labs(x="Year", y="Number of births") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g2

# Harbor seal
data3 <- data %>% filter(stock=="Harbor seal")
g3 <- ggplot(data3, aes(x=year, y=n_mid, fill=region)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g3

# Harbor porpoise 
data4 <- data %>% filter(stock=="Harbor porpoise (Monterey Bay)")
g4 <- ggplot(data4, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
  # Labels
  labs(x="Year", y="Number of animals") +
  scale_x_continuous(breaks=seq(1950,2020,10), lim=c(NA, 2020)) +
  # Theme
  theme_bw() + my_theme
g4

# Harbor porpoise 
data5 <- data %>% filter(stock=="Harbor porpoise (Morro Bay)")
g5 <- ggplot(data5, aes(x=year, y=n_mid)) +
  facet_wrap(~stock, scale="free", ncol=3) +
  geom_bar(stat="identity") +
  geom_errorbar(mapping=aes(x=year, ymin=n_lo, ymax=n_hi), width=0) +
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



