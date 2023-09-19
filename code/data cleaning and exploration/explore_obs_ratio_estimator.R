
### clean working environment ###
rm(list = ls())

### load in packages ###

library(tidyverse)

### read in data ###

predict_data_final <- readRDS("model_result/logbook_new_prediction.Rds")

obs_orig <- read_csv("data/confidential/processed/fig3_merge_obs_and_trip_data_all.csv")

noaa_est <- read_csv("data/confidential/processed/noaa_ratio_estimator_1990_2000.csv") %>%
  mutate(year = as.numeric(year))


obs_format <- obs_orig %>%
  filter(comm_name == "California sea lion") %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  group_by(year) %>%
  summarize(obs_sl_bycatch = sum(n_discarded_total)) %>%
  mutate(year = as.numeric(year))

predict_time <- predict_data_final %>%
  separate(set_id, into = c("vessel_name", "vessel_id_use", "permit_num", "year", "month", "day"), sep = "-") %>%
  mutate(season = case_when(month%in% c("12", "01", "02")~"winter",
                            month%in% c("03", "04", "05")~"spring",
                            month%in% c("06", "07", "08")~"summer",
                            month%in% c("09", "10", "11")~"fall")) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(have_sl_bycatch = ifelse(sl_bycatch_class== "1", "yes", "no")) %>%
  group_by(year, season, have_sl_bycatch) %>%
  mutate(sl_bycatch_class = as.numeric(sl_bycatch_class)) %>%
  summarize(set_number = sum(sl_bycatch_class))

# make plot 

base_theme <- theme(axis.text=element_text(size=6),
                    axis.title=element_text(size=8),
                    axis.title.x=element_blank(),
                    legend.text=element_text(size=6),
                    legend.title=element_text(size=8),
                    strip.text=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(), 
                    axis.line = element_line(colour = "black"))


ggplot(data = predict_time %>% filter(have_sl_bycatch == "yes")) +
  geom_bar(aes(x = year, y = set_number, fill = season), stat = "identity") +
  scale_x_continuous(breaks = seq(1981, 2022, 5)) +
  scale_fill_discrete(name = "Season") +
  geom_point(data = obs_format, color = "blue", shape = 18, aes(x = year, y = obs_sl_bycatch)) +
  geom_point(data = noaa_est, color = "red",shape = 20, aes(x = year, y = sl_bycatch_estimate)) +
  labs(y = "Estimated California sea lion bycatch") +
  theme_bw() + base_theme

class(obs_format$year)




obs_2007 <- obs_orig %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  filter(year == "2017")

sealion_set <- obs_orig %>%
  separate(date, into = c("year", "month", "day"), sep = "-") %>%
  filter(comm_name == "California sea lion") %>%
  group_by(set_id) %>%
  summarize(sealion_set = sum(n_discarded_total))

ggplot(data = sealion_set, aes(x = sealion_set)) +
  geom_histogram(binwidth = 1)
  
  
  
