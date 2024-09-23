
# clean working space
################################################################################
rm(list = ls())

# read in packages
################################################################################
library(tidyverse)

# read in data
################################################################################
rf_orig <- read.csv("model_result/temporal_prediction_strata.csv")

re_orig <- readRDS("model_result/1981_2021_bycatch_estimate_ratio_stratified.Rds")

# set plot directory
################################################################################
plotdir <- "figures"

# modify data 
################################################################################

rf_data <- rf_orig %>%
  mutate(method = "Random Forest") %>%
  mutate(strata=factor(strata, levels=c("Southern California", "Channel Islands", 
                                        "Ventura", "Morro Bay", "Monterey Bay"))) %>%
  select(-bycatch_sets) %>%
  drop_na()


re_data <- re_orig %>%
  filter(comm_name %in% c("California sea lion", "Harbor seal", "Common murre", "Northern elephant seal")) %>%
  select(comm_name, strata, year, nbycatch) %>%
  rename(species = comm_name,
         total_bycatch = nbycatch) %>%
  mutate(strata=factor(strata, levels=c("Southern California", "Channel Islands", 
                                        "Ventura", "Morro Bay", "Monterey Bay"))) %>%
  mutate(method = "Ratio Estimation") 

data_all <- rbind(re_data, rf_data) %>%
  mutate(method = factor(method, levels = c("Ratio Estimation", "Random Forest")))


# plot data
################################################################################

base_theme <-  theme(axis.text=element_text(size=7),
                     axis.title=element_text(size=8),
                     axis.title.x=element_blank(),
                     legend.text=element_text(size=7),
                     legend.title=element_text(size=8),
                     strip.text=element_text(size=8),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(data_all, aes(x = year, y = total_bycatch, fill = strata)) +
  # Facet
  facet_grid(species~method, scale = "free_y") +
  # shading
  geom_rect(data = data_all %>% filter(species == "California sea lion"), aes(xmin = 1980, xmax = 1990, ymin = 0, ymax = Inf), alpha = 0.1, fill = "grey80") +
  geom_rect(data = data_all %>% filter(species == "Harbor seal"), aes(xmin = 1980, xmax = 2000, ymin = 0, ymax = Inf), alpha = 0.1, fill = "grey80") +
  geom_rect(data = data_all %>% filter(species == "Common murre"), aes(xmin = 1980, xmax = 1992, ymin = 0, ymax = Inf), alpha = 0.1, fill = "grey80") +
  geom_rect(data = data_all %>% filter(species == "Northern elephant seal"), aes(xmin = 1980, xmax = 2000, ymin = 0, ymax = Inf), alpha = 0.1, fill = "grey80") +
  # estimates
  geom_bar(stat="identity", color="grey30", linewidth=0.1) +
  # Labels
  labs(x="", y="Estimated bycatch") +
  scale_fill_discrete(name="") +
  # Theme
  theme_bw() + base_theme +
  theme(legend.position="top",
        legend.key.size=unit(0.3, "cm"))

g

ggsave(g, filename=file.path(plotdir, "FigS16_re_rf_strata_comparison.png"), 
       width=5.5, height=6, units="in", dpi=600)











  


